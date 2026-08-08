library(tidyverse)
library(dplyr)
library(stringr)
library(lubridate)


auckland_wellington_christchurch <- read_csv("datasets/infometrics.co.nz/house prices/auckland_wellington_christchurch.csv")
#regions <- read_csv("datasets/infometrics.co.nz/house prices/regions.csv")
#regions2 <- read_csv("datasets/infometrics.co.nz/house prices/regions2.csv")
rural_urban <- read_csv("datasets/infometrics.co.nz/house prices/rural_urban.csv")

all_regions_wide <- list(
  auckland_wellington_christchurch,
  rural_urban
  #regions
) %>%
  reduce(full_join, by = "Year") %>%
  mutate(Year = ymd(paste0(Year, "-01-01")))


all_regions_clean <- all_regions_wide %>%
  select(Year, contains("Level")) %>%
  rename_with(~ str_remove_all(., " Level")) %>%
  rename_with(~ str_trim(.))

all_regions_long <- all_regions_clean %>%
  pivot_longer(
    cols = -Year,
    names_to = "Region",
    values_to = "House_Price"
  )

write.csv(all_regions_long, "avg_house_price_long.csv")
write.csv(all_regions_wide, "avg_house_price_wide.csv")