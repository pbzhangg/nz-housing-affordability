library(dplyr)
library(lubridate)
library(stringr)


load_files <- function(folder_path) {
  files <- list.files(path = folder_path, pattern = "\\.csv$", full.names = TRUE)
  
  named_files <- setNames(lapply(files, read.csv, colClasses = "character"), basename(files))
  
  df <- bind_rows(named_files, .id = "source_file") %>%
    mutate(
      Date_str = substr(source_file, 1, 7),
      Date = ymd(paste0(Date_str, "-01")))
  return(df)
}

df <- load_files("datasets/oneroof.co.nz/")

df <- df %>%
  mutate(Region = coalesce(Region, Location)) %>%
  mutate(Local_authority = coalesce(Territorial.local.authority, TLA)) %>%
  mutate(Average_property_value = coalesce(Average.property.value, Current.average.property.value)) %>%
  mutate( Suburb = coalesce(Suburb, District)) %>%
  mutate(Average_property_value = str_replace_all(Average_property_value, "[$,]", ""),
    Average_property_value = as.numeric(Average_property_value)
  ) %>%
  filter(!is.na(Average_property_value),
         !is.na(Local_authority)) %>%
  select(
    Suburb,
    Local_authority,
    Average_property_value,
    Date)
  
write_csv(df, 'datasets/oneroof.co.nz/merged_oneroof_dataset.csv')