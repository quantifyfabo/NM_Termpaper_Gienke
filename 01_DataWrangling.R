# Load Data for Paper
library(tidyverse)
library(readxl)
library(dplyr)
library(lubridate)




emdat_raw <- read_excel('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/Datasets/EM-Dat2023_2025_Raw.xlsx')
ts_data_clean <- read.csv('/Users/fabiangi/Documents/Goethe Uni/Semester 3/VP Naturkatastrophen/Paper_Project/Datasets/TS_EN_v1.csv')

# date format in ts data
ts_data_clean$date <- as.Date(ts_data_clean$date)

# Prepare Dateformat
emdat_clean <- emdat_raw%>%
  mutate(end_year = as.integer(`End Year`)) %>%
  filter(end_year != 2023)

emdat_clean$start_year  <- as.integer(emdat_clean$`Start Year`)
emdat_clean$start_month <- as.integer(emdat_clean$`Start Month`)
emdat_clean$start_day   <- as.integer(emdat_clean$`Start Day`)

emdat_clean$end_year  <- as.integer(emdat_clean$`End Year`)
emdat_clean$end_month <- as.integer(emdat_clean$`End Month`)
emdat_clean$end_day   <- as.integer(emdat_clean$`End Day`)


# Substitute NA in Dates
emdat_clean$start_month[is.na(emdat_clean$start_month)] <- 1
emdat_clean$start_day[is.na(emdat_clean$start_day)]     <- 1

emdat_clean$end_month[is.na(emdat_clean$end_month)] <- emdat_clean$start_month[is.na(emdat_clean$end_month)]
emdat_clean$end_day[is.na(emdat_clean$end_day)]     <- emdat_clean$start_day[is.na(emdat_clean$end_day)]


# Create Date Variable from Day, Month, Year Variable
emdat_clean$start_date <- make_date(
  year  = emdat_clean$start_year,
  month = emdat_clean$start_month,
  day   = emdat_clean$start_day
)

emdat_clean$end_date <- make_date(
  year  = emdat_clean$end_year,
  month = emdat_clean$end_month,
  day   = emdat_clean$end_day
)

# Prepare Time Window of the Event (Start Date to End Date with +-7 Days)
window_days <- 7

emdat_clean$window_start <- emdat_clean$start_date - window_days
emdat_clean$window_end   <- emdat_clean$end_date   + window_days

emdat_clean$window_length_days <-
  as.numeric(emdat_clean$window_end - emdat_clean$window_start)



# Select relevant Variables and rename
emdat_events <- emdat_clean %>%
  select(
    `DisNo.`,
    Country,
    ISO,
    Region,
    Subregion,
    `Disaster Group`,
    `Disaster Type`,
    `Disaster Subtype`,
    start_date,
    end_date,
    window_start,
    window_end,
    window_length_days,
    `Total Deaths`,
    `No. Injured`,
    `Total Affected`
  ) %>%
  rename(
    event_id         = `DisNo.`,
    country          = Country,
    iso              = ISO,
    region           = Region,
    subregion        = Subregion,
    disaster_group   = `Disaster Group`,
    disaster_type    = `Disaster Type`,
    disaster_subtype = `Disaster Subtype`,
    deaths           = `Total Deaths`,
    injured          = `No. Injured`,
    affected         = `Total Affected`
  )



# Test
emdat_events$start_date  <- as.Date(emdat_events$start_date, origin = "1970-01-01")
emdat_events$window_end  <- as.Date(emdat_events$window_end, origin = "1970-01-01")

# make ts date column as.Date type
ts_data_clean$date <- as.Date(ts_data_clean$date)
