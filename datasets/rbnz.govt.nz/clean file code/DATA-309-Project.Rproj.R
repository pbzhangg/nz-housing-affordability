# scripts/clean_employment.R

# 1. Load libraries
library(tidyverse)
library(lubridate)

# 2. Read raw data
emp_raw <- read_csv(
  "/Volumes/arjun/Data309/Data/RBNZ/Employment and Unemployment.csv",
  show_col_types = FALSE
)

# 3. Clean & filter
emp_clean <- emp_raw %>%
  # 3.1 Rename columns to friendly names
  rename(
    date              = `Date`,
    unemployment_rate = `Unemployment rate (%)`
  ) %>%
  # 3.2 Parse the date (DD-MM-YYYY)
  mutate(date = dmy(date)) %>%
  # 3.3 Keep only dates from 2015-01-01 through today
  filter(date >= ymd("2015-01-01")) %>%
  # 3.4 Ensure numeric type
  mutate(unemployment_rate = as.numeric(unemployment_rate)) %>%
  # 3.5 Keep only the two columns we need
  select(date, unemployment_rate)

# 4. Write out cleaned CSV
dir.create("data_processed", showWarnings = FALSE)
write_csv(emp_clean, "data_processed/employment_unemployment.csv")
