# Load libraries
library(readr)
library(tidyverse)
library(zoo)

# Read raw data
income_data <- read.csv("datasets/infometrics.co.nz/mean-household-income.csv")

avg_income_yearly <- income_data %>%
  # Rename columns
  rename(year = `Year`,
         nz_level = `New.Zealand.Level`,
         wlg_level = `Wellington.Region.Level`,
         akl_level = `Auckland.Level`,
         chc_level = `Christchurch.City.Level`
  ) %>% 
  # Filter for only years after 2015 (2015 inclusive)
  filter(year >= 2015) %>%
  # Only keep the needed columns
  select(year, nz_level, wlg_level, 
         akl_level, chc_level) %>%
  mutate(date = as.Date(paste0(year, "-04-01")))

# Create a time series object
ts_yearly <- zoo(avg_income_yearly %>% select(-year, -date), order.by = avg_income_yearly$date)

# Create the quarterly sequence to interpolate to
start_date <- as.Date(paste0(min(avg_income_yearly$year) - 1, "-04-01"))
end_date <- max(avg_income_yearly$date)
quarterly_dates <- seq.Date(from = start_date, to = end_date, by = "3 months")
ts_quarterly <- zoo(, order.by = quarterly_dates)

# Merge and linearly interpolate
ts_interpolated <- merge(ts_yearly, ts_quarterly) %>%
  na.approx()

# Convert to a dataframe
avg_income_quarterly <- fortify.zoo(ts_interpolated) %>%
  rename(date = Index) %>%
  mutate(
    year = year(date - months(3)),
    quarter = quarter(date, fiscal_start = 4)
  )

# Remove the extra quarterly entries (same as the original yearly data points)
avg_income_quarterly_clean <- avg_income_quarterly %>%
  filter(!((quarter == 4) & (date %in% avg_income_yearly$date)))

# Final formatting and saving
avg_income_quarterly_clean <- avg_income_quarterly_clean %>%
  select(date, everything(), -year, -quarter)

# Clean and filter raw data for average annual change in household income
income_change <- income_data %>%
  # Rename columns
  rename(year = `Year`,
         nz_change = `New.Zealand.Absolute.change`,
         wlg_change = `Wellington.Region.Absolute.change`,
         akl_change = `Auckland.Absolute.change`,
         chc_change = `Christchurch.City.Absolute.change`
  ) %>% 
  # Filter for only years after 2015 (2015 inclusive)
  filter(year >= 2015) %>%
  # Only keep the needed columns
  select(year, nz_change, wlg_change, 
         akl_change, chc_change) %>%
  mutate(date = as.Date(paste0(year, "-04-01")))

# Create a time series object
ts_yearly_change <- zoo(income_change %>% select(-year, -date), order.by = income_change$date)

# Create the quarterly sequence to interpolate to
start_date <- as.Date(paste0(min(income_change$year) - 1, "-04-01"))
end_date <- max(income_change$date)
quarterly_dates <- seq.Date(from = start_date, to = end_date, by = "3 months")
ts_quarterly_change <- zoo(, order.by = quarterly_dates)

# Merge and linearly interpolate
ts_interpolated_change <- merge(ts_yearly_change, ts_quarterly_change) %>%
  na.approx()

# Convert to a dataframe
income_change_quarterly <- fortify.zoo(ts_interpolated_change) %>%
  rename(date = Index) %>%
  mutate(
    year = year(date - months(3)),
    quarter = quarter(date, fiscal_start = 4)
  )

# Remove the extra quarterly entries (same as the original yearly data points)
income_change_quarterly_clean <- income_change_quarterly %>%
  filter(!((quarter == 4) & (date %in% income_change$date)))

# Final formatting and saving
income_change_quarterly_clean <- income_change_quarterly_clean %>%
  select(date, everything(), -year, -quarter)

# Write out cleaned CSV files
dir.create("datasets/infometrics.co.nz/processed_data", showWarnings = FALSE)
write_csv(avg_income_quarterly_clean, "datasets/infometrics.co.nz/processed_data/avg_quarterly_income.csv")
write_csv(income_change_quarterly_clean, "datasets/infometrics.co.nz/processed_data/avg_quarterly_income_change.csv")
