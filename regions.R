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

while (file.exists(filename)) {
  print(filename)
  data <- read_xlsx(filename, sheet="Totalt antal per region") %>%
    rename(region = Region, deaths = Totalt_antal_avlidna) %>%
    mutate(report_date = date) %>%
    dplyr::select(-`Totalt_antal_fall`, -`Fall_per_100000_inv`, -`Totalt_antal_intensivvårdade`) %>%
    na.omit()

  if (dbGetQuery(db,paste0("SELECT COUNT(*) FROM deaths_per_region WHERE report_date = '",date,"'"))$count == 0) {
    # Start a new database transaction
    dbBegin(db)
    # Write the data to the existing SQL table named "deaths"
    dbAppendTable(db, "deaths_per_region", data)
    # Commit the database transaction
    dbCommit(db)
  }
  
  # Process next date
  date <- date + 1
  filename <- paste0("files/",date,".xlsx")
}


# Disconnect from the database
dbDisconnect(db)
