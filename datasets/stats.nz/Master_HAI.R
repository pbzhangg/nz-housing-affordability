library(readr); library(dplyr); library(lubridate); library(zoo)

# 1) Read files
master <- read_csv("master_quarterly_2018_2025_fullnames.csv") %>%
  mutate(date = ymd(date),
         qtr  = as.yearqtr(date))

hai_wide <- read_csv("HAI_2018_2025_WIDE.csv") %>%
  mutate(date = ymd(date),
         qtr  = as.yearqtr(date)) %>%
  select(-date)

# 2) (Safety) collapse to one row per quarter if needed
hai_qtr <- hai_wide %>%
  group_by(qtr) %>%
  summarise(across(everything(), ~mean(.x, na.rm = TRUE)), .groups = "drop")

# 3) Join on quarter
master_plus_hai <- master %>%
  left_join(hai_qtr, by = "qtr") %>%
  arrange(qtr)

# 4) Save
write_csv(master_plus_hai, "MASTER_PLUS_HAI_WIDE.csv")