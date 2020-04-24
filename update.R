library(readxl)
library(tidyverse)
library(data.table)
library(DBI)

# This script is responsible for downloading new reports from FHM
# and uploading the date to the PostgreSQL database.

# The URL below is linked from https://www.folkhalsomyndigheten.se/smittskydd-beredskap/utbrott/aktuella-utbrott/covid-19/bekraftade-fall-i-sverige/
url <- "https://www.arcgis.com/sharing/rest/content/items/b5e7488e117749c19881cce45db13f7e/data"

# Download Excel-file to temp file
xls_file <- tempfile()
utils::download.file(url, xls_file, mode = "wb")

# Read Excel-file and extract data from the sheet named "Antal avlidna per dag"
data <- read_excel(
  xls_file,
  sheet = "Antal avlidna per dag",
  col_names = c("death_date","deaths"),
  col_types = c("date","numeric"),
  skip = 1,
  na = c("Uppgift saknas")
) %>%
  drop_na()

# Get list of all sheet names
sheets <- excel_sheets(xls_file)

# Extract report_date from sheet named e.g. "FOHM 23 Apr 2020"
report_date <- parse_date(
  sub("^FOHM ", "", sheets[grep("[0-9][0-9]? [A-Z][a-z][a-z] 202[0-9]",sheets)]),
  "%d %b %Y"
)

# Connect to PostgreSQL
db <- dbConnect(
  RPostgres::Postgres(),
  dbname = "c19",
  host = "c19.truthly.com",
  port = 5432,
  user = "c19update"
)

# Get date for last report
last_date <- as.Date(dbGetQuery(db, "SELECT MAX(report_date) FROM deaths")$max)

# If the downloaded report is not more recent than the last report in the database, abort.
stopifnot(report_date > last_date)

# Set report_date in the data
data$report_date <- report_date

# Start a new database transaction
dbBegin(db)

# Write the data to the existing SQL table named "deaths"
dbAppendTable(db, "deaths", data)

# Commit the database transaction
dbCommit(db)

# Disconnect from the database
dbDisconnect(db)
