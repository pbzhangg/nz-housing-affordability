library(tidyverse)
library(lubridate)

read_and_clean_dataset <- function(file_path) {
  # Read dataset
  df <- read_csv(file_path, show_col_types = FALSE)
  
  # Detect a date column (any column with 'date' or 'Date' in its name)
  date_col <- names(df)[str_detect(names(df), regex("date", ignore_case = TRUE))]
  
  if (length(date_col) > 0) {
    # Try to detect date format automatically
    df <- df %>%
      mutate(!!date_col := parse_date_time(.data[[date_col]], 
                                           orders = c("ymd", "dmy", "mdy"),
                                           tz = "UTC"))
  }
  
  # Only apply area classification if Area_Name exists
  if ("Area_Name" %in% names(df)) {
    df <- df %>%
      mutate(Category = case_when(
        Area_Name %in% c("Auckland") ~ "Auckland Urban",
        Area_Name %in% c("Rodney", "Franklin", "Papakura") ~ "Auckland Rural",
        
        Area_Name == "Christchurch City" ~ "Canterbury Urban",
        Area_Name %in% c("Ashburton District", "Hurunui District", "MacKenzie District",
                         "Selwyn District", "Timaru District", "Waimakariri District", 
                         "Waimate District", "Canterbury") ~ "Canterbury Rural",
        
        Area_Name %in% c("Wellington City", "Hutt City", "Upper Hutt City", "Porirua City") ~ "Wellington Urban",
        Area_Name %in% c("Kapiti Coast District", "Masterton District", 
                         "South Wairarapa District", "Carterton District", "Wellington") ~ "Wellington Rural",
        
        TRUE ~ "Other"
      ))
  } else {
    df <- df %>%
      mutate(Category = "National")
  }
  
  # Drop completely NA rows
  df <- df %>% drop_na()
  
  return(df)
}

aggregate_yearly_grouped <- function(df) {
  # Detect date column
  date_col <- names(df)[str_detect(names(df), regex("date", ignore_case = TRUE))]
  
  if (length(date_col) == 0) {
    stop("Dataset must contain a 'date' column to aggregate by year.")
  }
  
  # Extract Year
  df <- df %>%
    mutate(Year = year(.data[[date_col]]))
  
  # Identify numeric columns (exclude Year)
  numeric_cols <- df %>% select(where(is.numeric)) %>% select(-Year) %>% names()
  
  # Determine grouping columns
  group_cols <- "Year"
  if ("Area_Name" %in% names(df)) {
    group_cols <- c(group_cols, "Area_Name")
  }
  if ("Category" %in% names(df)) {
    group_cols <- c(group_cols, "Category")
  }
  if ("region" %in% names(df)) {
    group_cols <- c(group_cols, "region")
  }
  
  # Group and compute averages
  df_yearly <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(across(all_of(numeric_cols), ~ mean(.x, na.rm = TRUE)), .groups = "drop")
  
  return(df_yearly)
}

read_clean_aggregate <- function(file_path) {
  # Extract file name for prefix
  prefix <- tools::file_path_sans_ext(basename(file_path))
  
  # Read and clean
  df <- read_and_clean_dataset(file_path) %>%
    aggregate_yearly_grouped()
  
  # Prefix all columns except Year
  df <- df %>%
    rename_with(~ paste0(prefix, "_", .x), -Year)
  
  return(df)
}

# data.govt
files <- c(
  "datasets/data.govt.nz/Housing_Affordability_Indices.csv",
  "datasets/data.govt.nz/Banks_Mortgage_Rates.csv",
  "datasets/data.govt.nz/House_Price_Index.csv",
  "datasets/data.govt.nz/Value_of_Housing_Stock.csv"
)

data_govt_all_dfs <- map(files, read_clean_aggregate)

data_govt_master_dataset <- reduce(data_govt_all_dfs, full_join, by = "Year")


# figure.nz
figure_all_dfs <- read_and_clean_dataset("datasets/figure.nz/unemployment_wide.csv")
figure_all_dfs_master_dataset <- aggregate_yearly_grouped(figure_all_dfs)

# infometrics
files <- c(
  "datasets/infometrics.co.nz/processed_data/avg_quarterly_income_change.csv",
  "datasets/infometrics.co.nz/processed_data/avg_quarterly_income.csv",
  "datasets/infometrics.co.nz/house prices/avg_house_price_wide.csv"
)

infometrics_all_dfs <- map(files, read_clean_aggregate)

infometrics_master_dataset <- reduce(infometrics_all_dfs, full_join, by = "Year")

# rbnz.govt
rbnz_all_dfs <- read_and_clean_dataset("datasets/rbnz.govt.nz/data_processed/Merging of all files.csv")
rbnz_all_dfs_master_dataset <- aggregate_yearly_grouped(rbnz_all_dfs)

# stats.nz
stats_nz_all_dfs <- read_and_clean_dataset("datasets/stats.nz/MASTER_PLUS_HAI_WIDE.csv")
stats_nz_all_dfs_master_dataset <- aggregate_yearly_grouped(stats_nz_all_dfs)

# final merge
prefix_columns <- function(df, prefix) {
  df %>%
    rename_with(~ paste0(prefix, "_", .x), -Year)
}

data_govt_master_dataset_prefixed   <- prefix_columns(data_govt_master_dataset, "govt")
figure_master_dataset_prefixed      <- prefix_columns(figure_all_dfs_master_dataset, "figure")
infometrics_master_dataset_prefixed <- prefix_columns(infometrics_master_dataset, "infometrics")
rbnz_master_dataset_prefixed        <- prefix_columns(rbnz_all_dfs_master_dataset, "rbnz")
stats_nz_master_dataset_prefixed    <- prefix_columns(stats_nz_all_dfs_master_dataset, "statsnz")

master_df <- reduce(
  list(
    data_govt_master_dataset_prefixed,
    figure_master_dataset_prefixed,
    infometrics_master_dataset_prefixed,
    rbnz_master_dataset_prefixed,
    stats_nz_master_dataset_prefixed
  ),
  full_join,
  by = "Year"
)

#write_csv(master_df, "master_dataset.csv")