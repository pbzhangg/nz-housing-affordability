# scripts/clean_house_prices.R

# Paths
raw_dir       <- "/Volumes/arjun/Data309/DATA-309-Project/datasets/rbnz.govt.nz"
processed_dir <- "data_processed"
dir.create(processed_dir, showWarnings = FALSE)

# Libs
library(tidyverse)
library(lubridate)

# Read
hp_raw <- read_csv(file.path(raw_dir, "House prices.csv"), show_col_types = FALSE)

# Clean + filter 2015-01-01 to 2025-12-31
hp_clean <- hp_raw %>%
  rename(
    date = Date,
    house_price_annual_pct_change = `House prices (annual % change)`
  ) %>%
  mutate(
    date = dmy(date),
    house_price_annual_pct_change = as.numeric(house_price_annual_pct_change)
  ) %>%
  filter(date >= as.Date("2015-01-01"),
         date <= as.Date("2025-12-31")) %>%
  arrange(date)

# Write
write_csv(hp_clean, file.path(processed_dir, "house_prices_clean.csv"))
