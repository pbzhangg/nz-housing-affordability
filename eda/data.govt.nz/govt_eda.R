library(readxl)
library(tidyverse)

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

mortgage <- read.csv('datasets/data.govt.nz/Banks_Mortgage_Rates.csv')
housing_index <- read.csv('datasets/data.govt.nz/House_Price_Index.csv')
housing_affordability <- read.csv('datasets/data.govt.nz/Housing_Affordability_Indices.csv')
local_housing <- read.csv('datasets/data.govt.nz/Local_Housing_Statistics.csv')
housing_stock <- read.csv('datasets/data.govt.nz/Value_of_Housing_Stock.csv')