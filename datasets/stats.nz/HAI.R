library(tidyr)
library(readr)
library(dplyr)
library(stringr)

# df is your filtered long table from above
df_wide <- df %>%
  rename(
    mortgage = `Mortgage Affordability Index`,
    deposit  = `Deposit Affordability Index`,
    rent     = `Rent Affordability Index`
  ) %>%
  # one row per date; columns for each metric x region
  pivot_wider(
    id_cols = date,
    names_from = Area_Name,
    values_from = c(mortgage, deposit, rent),
    names_glue = "{.value}_{Area_Name}"
  ) %>%
  arrange(date)

# make safe column names (New Zealand -> New_Zealand)
names(df_wide) <- gsub("\\s+", "_", names(df_wide))

write_csv(df_wide, "HAI_2018_2025_WIDE.csv")