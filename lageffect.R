library(tidyverse)
library(DBI)

# This script generates the three types of graphs showing the lag effect
# between report_date and death_date, i.e. how long time it takes until new deaths
# shows up in the numbers.

# Connect to PostgreSQL
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = "c19",
  host = "c19.truthly.com",
  port = 5432,
  user = "c19"
)

data <- dbGetQuery(db, "
  SELECT
    report_date,
    death_date,
    deaths,
    CASE WHEN report_date > MIN(report_date) OVER () THEN report_date - death_date END AS lag_effect,
    deaths - COALESCE(LAG(deaths) OVER (PARTITION BY death_date ORDER BY report_date),0) AS new_deaths
  FROM deaths
")

dbDisconnect(db)

min_date <- min(data$report_date)
max_date <- max(data$report_date)

data$lag_effect <- as.integer(data$lag_effect)

plot1 <- ggplot(data %>% filter(new_deaths > 0 & report_date > min_date)) +
  geom_point(aes(x=death_date, y=report_date, size=new_deaths, color=lag_effect)) +
  theme_minimal() +
  labs(x = "Avliden_datum", color = "Eftersläpning", y = "Rapportdatum", size="Nya dödsfall") +
  ggtitle("Folkhälsomyndigheten - Covid19 Historik Excel - Avlidna per dag") +
  scale_color_gradientn(colours = terrain.colors(10)) +
  scale_y_date(breaks = "1 day")

data$report_date <- as.factor(data$report_date)

plot2 <- ggplot(data, aes(x=death_date)) +
  geom_line(aes(y=deaths, color=report_date)) +
  theme_minimal() +
  ggtitle("Folkhälsomyndigheten - Covid19 - Avlidna per dag") +
  labs(x = "Datum avliden", color = "Rapportdatum", y = "Antal avlidna")

data$lag_effect <- pmin(14, data$lag_effect)
data$lag_effect <- factor(data$lag_effect, levels = sort(unique(data$lag_effect), decreasing = TRUE))

plot3 <- ggplot(data, aes(x=death_date)) +
  geom_col(aes(y=new_deaths, fill=lag_effect), position = position_stack()) +
  theme_minimal() +
  labs(x = "Datum avliden", fill = "Eftersläpning", y = "Antal avlidna") +
  ggtitle("Folkhälsomyndigheten - Covid19 - Avlidna per dag") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-06")), aes(y=30, label="6/4: Fallen ligger på knappt 30 om dan."), hjust = "inward") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-07")), aes(y=40, label="7/4: Vi ligger på ett snitt på 40 fall per dygn."), hjust = "inward") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-08")), aes(y=45, label="8/4: Nu ligger vi på 45 eller högre."), hjust = "inward") +
  geom_label(data=data.frame(death_date=as.Date("2020-04-20")), aes(y=60, label="20/4: Vi ligger i snitt på 60 fall om dagen."), hjust = "inward") +
  scale_y_continuous(breaks = seq(0,100,by=10))


plot1
plot2
plot3

ggsave(paste0("graphs/",max_date,"a.png"), plot1, width = 14, height = 8, dpi = 300)
ggsave(paste0("graphs/",max_date,"b.png"), plot2, width = 14, height = 8, dpi = 300)
ggsave(paste0("graphs/",max_date,"c.png"), plot3, width = 14, height = 8, dpi = 300)
