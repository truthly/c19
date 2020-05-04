library(shinydashboard)
library(tidyverse)
library(drc)
library(lubridate)
library(scales)
library(DBI)
library(pool)
library(DT)

# Connect to PostgreSQL
pool <- dbPool(
  drv = RPostgres::Postgres(),
  dbname = "c19",
  host = "c19.truthly.com",
  port = 5432,
  user = "c19",
  minSize = 5,
  maxSize = 50
)

regions <- dbGetQuery(pool, "
  SELECT DISTINCT region
  FROM deaths_per_region
  ORDER BY 1
")$region


ui <- dashboardPage(
  dashboardHeader(title = "Coronalyzer"),
  dashboardSidebar(
    sliderInput("date",
                "Date:",
                min = as.Date("2020-04-02"),
                max = Sys.Date(),
                value = Sys.Date(),
                animate = animationOptions(interval=1000)
    ),
    sliderInput("forecast_days",
                "Forecast:",
                min = 1,
                max = 1000,
                value = 200
    )
  ),
  dashboardBody(
#    tags$style(type = "text/css", "#graphCurve {height: calc(100vh - 200px) !important}"),
    fluidRow(
      box(width = "640",
        plotOutput("graphCurve")
      )
    ),
    fluidRow(
      box(width = "640",
          DT::dataTableOutput("dataTable")
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
  graphCurveX <- reactiveVal()
  graphCurveY <- reactiveVal()
  dataTable <- reactiveVal()
  
  output$graphCurve <- renderPlot({
    
    input_data <- dbGetQuery(pool, "
      SELECT
        region,
        report_date::date AS date,
        deaths::numeric,
        ROW_NUMBER() OVER (PARTITION BY region ORDER BY report_date)::integer AS day
      FROM deaths_per_region
      WHERE deaths > 0
      ORDER BY 1,2
    ")

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

      predict_to_day <- max(data$day) + input$forecast_days
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
#      data <- data %>% group_by(region) %>% arrange(date) %>% mutate(new_deaths = deaths - lag(deaths))
  
      result <- rbind(result, data)

    }

    sum <- result %>%
      group_by(date) %>%
      summarise(region = "Sweden", deaths = sum(deaths), predict=sum(predict))
    sum$day <- sum$date - min(sum$date) + 1
    result <- rbind(result, sum)

    dataTable(sum %>%
        group_by(region) %>%
        arrange(date) %>%
        mutate(new_deaths = deaths - lag(deaths), new_deaths_predicted = predict - lag(predict)) %>%
        filter(date >= input$date) %>%
        rename(
          total_deaths_predicted=predict,
          total_deaths_actual=deaths,
          new_deaths_actual=new_deaths
        ) %>%
        mutate(
          diff_total_deaths=total_deaths_predicted-total_deaths_actual,
          diff_new_deaths=new_deaths_predicted-new_deaths_actual) %>%
        mutate(
          total_deaths_predicted = if_else(date > input$date, total_deaths_predicted, NULL),
          new_deaths_predicted = if_else(date > input$date, new_deaths_predicted, NULL),
          diff_total_deaths = if_else(date > input$date, diff_total_deaths, NULL),
          diff_new_deaths = if_else(date > input$date, diff_new_deaths, NULL),
        ) %>%
        dplyr::select(-`day`)
    )
    
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
      scale_y_log10(labels = labels_f) +
      coord_cartesian(ylim = c(1,10000))

    print(plot)        
  
  })
  
  output$dataTable <- renderDataTable({
    datatable(dataTable(), options = list(paging = FALSE))
  })
  
}

shinyApp(ui = ui, server = server)
