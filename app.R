library(shinydashboard)
library(tidyverse)
library(drc)
library(lubridate)
library(scales)
library(DBI)

forecast_days <- 100

# Connect to PostgreSQL
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = "c19",
  host = "c19.truthly.com",
  port = 5432,
  user = "c19"
)

input_data <- dbGetQuery(db, "
  SELECT
    region,
    report_date::date AS date,
    deaths::numeric,
    ROW_NUMBER() OVER (PARTITION BY region ORDER BY report_date)::integer AS day
  FROM deaths_per_region
  WHERE deaths > 0
  ORDER BY 1,2
")

dbDisconnect(db)

regions <- unique(input_data$region)

ui <- dashboardPage(
  dashboardHeader(title = "Coronalyzer"),
  dashboardSidebar(
    sliderInput("date",
                "Date:",
                min = as.Date("2020-04-02"),
                max = Sys.Date(),
                value = Sys.Date(),
                animate = animationOptions(interval=1000)
    )
  ),
  dashboardBody(
#    tags$style(type = "text/css", "#graphCurve {height: calc(100vh - 200px) !important; width: calc(100vw - 400px) !important;}"),
    fluidRow(
      box(
        plotOutput("graphCurve")
      )
    ),
    hr(),
    print("Author: Joel Jakobsson <joel@truthly.com>")
  )
  
)

setVal <- function(val, key, value) {
  df <- val()
  if (is.null(df)) {
    df <- data.frame()
  }
  df[1,key] <- value
  val(df)
}

getVal <- function(val, key) {
  df <- val()
  df[1,key]
}

server <- function(input, output, session) {
  shinyOptions(cache = memoryCache(max_size = 100e6))
  graphCurveX <- reactiveVal()
  graphCurveY <- reactiveVal()
  
  output$graphCurve <- renderCachedPlot({
    
    updateSliderInput(session, "date", min = min(input_data$date))
    updateSliderInput(session, "date", max = max(input_data$date))

    result <- NULL
    
    for (r in regions) {
      data <- input_data %>%
        filter(region == r)
      
      model_data <- filter(data, date <= input$date)

      if (count(model_data) == 0 | min(model_data$deaths) == max(model_data$deaths)) {
        next
      }

      predict_to_day <- max(data$day) + 1 + forecast_days
      predict_from_day <-  max(data$day) + 1
      min_date <- min(data$date)

      model <- drm(deaths ~ day, data = model_data, fct = LL.4(fixed=c(NA,NA,NA,NA)))
      model_summary <- paste(capture.output(summary(model)),collapse="\n")
      steepness <- model$coefficients["b:(Intercept)"]
      deceased <- model$coefficients["d:(Intercept)"]
      inflection <- model$coefficients["e:(Intercept)"]
      data <- rbind(
        data,
        expand.grid(region=r,deaths=NA,date=NA,day=seq(predict_from_day,predict_to_day))
      )
      # Formula is: round(deceased - deceased/(1 + (day/inflection)^(-steepness)))

      data$predict <- round(predict(model, newdata=data))
      data$date <- min_date + data$day - 1
  
      result <- rbind(result, data)

    }

    sum <- result %>%
      group_by(date) %>%
      summarise(region = "Sweden", deaths = sum(deaths), predict=sum(predict))
    sum$day <- sum$date - min(sum$date) + 1
    result <- rbind(result, sum)

    labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')
    
    plot <- ggplot(result, aes(x=date)) +
      geom_point(aes(y=deaths, color=region, alpha=0.8)) +
      geom_line(aes(y=predict, color=region, alpha=0.8)) +
      geom_text(data = result %>% group_by(region) %>% filter(date == last(date)), aes(label = paste(region, predict), 
                                                                                       x = date + 1,
                                                                                       y = predict,
                                                                                       color = region), hjust = "inward") +
      guides(alpha = FALSE, fill = FALSE) +
      labs(x = "Date", y = "Deaths") +
      theme_minimal() +
      geom_vline(aes(xintercept = Sys.Date()), color="black") +
      geom_vline(aes(xintercept = input$date), color="black") +
      ggtitle(paste0("COVID-19 - Input <= ", input$date)) +
      scale_y_log10(labels = labels_f) +
      coord_cartesian(ylim = c(1,10000))

    print(plot)        
  
  },
    cacheKeyExpr = {list(input$date)},
    sizePolicy = sizeGrowthRatio(width = 1416, height = 640, growthRate = 1.1)
  )
  
}

shinyApp(ui = ui, server = server)
