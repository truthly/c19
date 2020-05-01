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
  SELECT region, date, deaths, day FROM (
    SELECT
      region,
      report_date::date AS date,
      deaths::numeric,
      ROW_NUMBER() OVER (PARTITION BY region ORDER BY report_date)::integer AS day,
      MIN(deaths) OVER (PARTITION BY region),
      MAX(deaths) OVER (PARTITION BY region)
    FROM deaths_per_region
    WHERE deaths > 0
  ) AS X
  WHERE MAX > MIN
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
                animate = animationOptions(interval=500)
    ),
    radioButtons("scale","Scale:", c("Linear"="lin","Logarithmic"="log")),
    actionButton("lockScales","Lock scales", icon = icon("lock"))
  ),
  dashboardBody(
    tags$style(type = "text/css", "#graphCurve {height: calc(100vh - 200px) !important; width: calc(100vw - 400px) !important;}"),
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
  graphCurveX <- reactiveVal()
  graphCurveY <- reactiveVal()
  
  output$graphCurve <- renderPlot({
    
    updateSliderInput(session, "date", min = min(input_data$date))
    updateSliderInput(session, "date", max = max(input_data$date))

    result <- NULL

    for (r in regions) {
      print(r)
      data <- input_data %>%
        filter(region == r)
        
      first_case <- as.Date(min(data$date))
    
      model_data <- filter(data, date <= input$date)
      
      model <- drm(deaths ~ day, data = model_data, fct = LL.4(fixed=c(NA,NA,NA,NA)))
      model_summary <- paste(capture.output(summary(model)),collapse="\n")
      steepness <- model$coefficients["b:(Intercept)"]
      deceased <- model$coefficients["d:(Intercept)"]
      inflection <- model$coefficients["e:(Intercept)"]
      inflection_date <- first_case + as.integer(inflection) - 1
#      end_day <- max(as.integer(2*inflection), max(model_data$day) + forecast_days)
      end_day <- max(model_data$day) + forecast_days
      fits <- expand.grid(region=r,date=NA,day=seq(max(model_data$day)+1,end_day))
      # Formula is: round(deceased - deceased/(1 + (day/inflection)^(-steepness)))
      pm <- predict(model, newdata=fits, interval="confidence", level=0.68)
      pm2 <- predict(model, newdata=fits, interval="confidence", level=0.95)
      fits$deaths <- round(pm[,1])
      cur_max <- max(model_data$deaths)
      fits$deathsmin <- if_else(pm[,2] < cur_max, cur_max, pm[,2])
      fits$deathsmax <- if_else(pm[,3] < cur_max, cur_max, pm[,3])
      fits$deathsmin2 <- if_else(pm2[,2] < cur_max, cur_max, pm2[,2])
      fits$deathsmax2 <- if_else(pm2[,3] < cur_max, cur_max, pm2[,3])
      data$deathsmin <- NA
      data$deathsmax <- NA
      data$deathsmin2 <- NA
      data$deathsmax2 <- NA
      data <- rbind(data, fits)
      # Convert day from integer to date
      data$date <- first_case + data$day - 1
    
      result <- rbind(result, data)
    
    }

#    result$region <- as.factor(result$region)
    
    sum <- result %>%
      group_by(date) %>%
      summarise(region = "Sweden", deaths = sum(deaths), deathsmin=NA, deathsmax=NA, deathsmin2=NA, deathsmax2=NA)
    sum$day <- as.integer(sum$date - min(sum$date)) + 1
    
    print(sum)
print(result)
    result <- rbind(result, sum)
        
    plot <- ggplot(result, aes(x=date)) +
      geom_line(aes(y=deaths, color=region, alpha=0.8)) +
      geom_text(data = result %>% group_by(region) %>% filter(date == last(date)), aes(label = paste(region, deaths), 
                                                                   x = date + 1,
                                                                   y = deaths,
                                                                   color = region)) +
      geom_ribbon(aes(ymin=deathsmin, ymax=deathsmax, fill=region), alpha=0.1) +
#      geom_ribbon(aes(ymin=deathsmin2, ymax=deathsmax2, fill=region), alpha=0.1) +
      guides(alpha = FALSE, fill = FALSE) +
      labs(x = "Date", y = "Deaths") +
      theme_bw() +
      geom_vline(aes(xintercept = Sys.Date()), color="black") +
      ggtitle(paste0("COVID-19 - Input <= ", input$date))

    labels_f <- scales::number_format(accuracy = 1, decimal.mark = ',')

    if (input$scale == "log") {
      plot <- plot + scale_y_log10(labels = labels_f)
    } else {
      plot <- plot + scale_y_continuous(labels = labels_f)
    }
    
    if (input$lockScales %% 2 == 0) {
      updateActionButton(session, "lockScales", label = "Lock scales", icon = icon("lock"))
      # unlocked
      gpb <- ggplot_build(plot)
      graphCurveX(c(
        as.Date(gpb$layout$panel_scales_x[[1]]$range$range[1],origin="1970-01-01"),
        as.Date(gpb$layout$panel_scales_x[[1]]$range$range[2],origin="1970-01-01")
      ))
      graphCurveY(c(
        gpb$layout$panel_scales_y[[1]]$range$range[1],
        gpb$layout$panel_scales_y[[1]]$range$range[2]
      ))
    } else {
      plot <- plot + coord_cartesian(ylim = graphCurveY(), xlim =graphCurveX()) 
      updateActionButton(session, "lockScales", label = "Unlock scales", icon = icon("lock-open"))
    }
    
    print(plot)        
  
  })
  
}

shinyApp(ui = ui, server = server)
