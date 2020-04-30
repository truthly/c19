library(readxl)
library(tidyverse)
library(data.table)
library(DBI)

date <- as.Date("2020-04-02")
filename <- paste0("files/",date,".xlsx")

# Connect to PostgreSQL
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = "c19",
  host = "c19.truthly.com",
  port = 5432,
  user = "c19update"
)

# Start a new database transaction
dbBegin(db)

while (file.exists(filename)) {
  print(filename)
  data <- read_xlsx(filename, sheet="Totalt antal per region") %>%
    rename(region = Region, deaths = Totalt_antal_avlidna) %>%
    mutate(report_date = date) %>%
    select(-`Totalt_antal_fall`, -`Fall_per_100000_inv`, -`Totalt_antal_intensivvårdade`) %>%
    na.omit()

  # Write the data to the existing SQL table named "deaths"
  dbAppendTable(db, "deaths_per_region", data)
  
  # Process next date
  date <- date + 1
  filename <- paste0("files/",date,".xlsx")
}


# Commit the database transaction
dbCommit(db)

# Disconnect from the database
dbDisconnect(db)
