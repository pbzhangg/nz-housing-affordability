library(ggplot2)
library(dplyr)

raw_df <- read.csv("datasets/master dataset/master_dataset.csv", stringsAsFactors = FALSE)

names(raw_df) <- c(
  "Year",
  "Region",              # govt_Housing_Affordability_Indices_Area_Name
  "HAI_Category",          # govt_Housing_Affordability_Indices_Category
  "Mortgage_Index",    # govt_Housing_Affordability_Indices_Mortgage.Affordability.Index
  "Deposit_Index",     # govt_Housing_Affordability_Indices_Deposit.Affordability.Index
  "Rent_Index",        # govt_Housing_Affordability_Indices_Rent.Affordability.Index
  "Mortgage_Category",     # govt_Banks_Mortgage_Rates_Category
  "Mortgage_Floating_Rate",# govt_Banks_Mortgage_Rates_Floating.rate
  "Mortgage_2yr_Fixed_Rate",# govt_Banks_Mortgage_Rates_2.year.fixed.rate
  "HPI_Category",          # govt_House_Price_Index_Category
  "HPI_Annual_Change",     # govt_House_Price_Index_House.prices..annual...change.
  "HVS_Category",          # govt_Value_of_Housing_Stock_Category
  "HVS_House_Values",      # govt_Value_of_Housing_Stock_House.values
  "Figure_Category",       # figure_Category
  "Unemployment_Auckland",       # figure_Auckland
  "Unemployment_Christchurch",   # figure_Christchurch
  "Unemployment_Wellington",     # figure_Wellington
  "Income_Change_Category",# infometrics_avg_quarterly_income_change_Category
  "Income_Change_NZ",      # infometrics_avg_quarterly_income_change_nz_change
  "Income_Change_Wellington", # infometrics_avg_quarterly_income_change_wlg_change
  "Income_Change_Auckland",   # infometrics_avg_quarterly_income_change_akl_change
  "Income_Change_Christchurch", # infometrics_avg_quarterly_income_change_chc_change
  "Income_Category",       # infometrics_avg_quarterly_income_Category
  "Income_NZ_Level",       # infometrics_avg_quarterly_income_nz_level
  "Income_Wellington_Level", # infometrics_avg_quarterly_income_wlg_level
  "Income_Auckland_Level",   # infometrics_avg_quarterly_income_akl_level
  "Income_Christchurch_Level", # infometrics_avg_quarterly_income_chc_level
  "House_Price_Category",  # infometrics_avg_house_price_wide_Category
  "House_Price_Unknown",   # infometrics_avg_house_price_wide_...1
  "ChCh_House_Level",      # infometrics_avg_house_price_wide_Christchurch.City.Level
  "ChCh_House_Change",     # infometrics_avg_house_price_wide_Christchurch.City...Change
  "ChCh_House_Abs_Change", # infometrics_avg_house_price_wide_Christchurch.City.Absolute.change
  "Akl_House_Level",       # infometrics_avg_house_price_wide_Auckland.Level
  "Akl_House_Change",      # infometrics_avg_house_price_wide_Auckland...Change
  "Akl_House_Abs_Change",  # infometrics_avg_house_price_wide_Auckland.Absolute.change
  "Wlg_House_Level",       # infometrics_avg_house_price_wide_Wellington.City.Level
  "Wlg_House_Change",      # infometrics_avg_house_price_wide_Wellington.City...Change
  "Wlg_House_Abs_Change",  # infometrics_avg_house_price_wide_Wellington.City.Absolute.change
  "Metro_House_Level",     # infometrics_avg_house_price_wide_Metro.areas.Level
  "Metro_House_Change",    # infometrics_avg_house_price_wide_Metro.areas...Change
  "Metro_House_Abs_Change",# infometrics_avg_house_price_wide_Metro.areas.Absolute.change
  "Rural_House_Level",     # infometrics_avg_house_price_wide_Rural.areas.Level
  "Rural_House_Change",    # infometrics_avg_house_price_wide_Rural.areas...Change
  "Rural_House_Abs_Change",# infometrics_avg_house_price_wide_Rural.areas.Absolute.change
  "RBNZ_Category",         # rbnz_Category
  "RBNZ_House_Price_Change", # rbnz_house_price_annual_pct_change
  "RBNZ_House_Value_Index", # rbnz_house_value_index
  "RBNZ_Debt_Servicing_Pct", # rbnz_debt_servicing_pct
  "RBNZ_Floating_Rate",    # rbnz_floating_rate
  "RBNZ_2yr_Fixed_Rate",   # rbnz_two_year_fixed_rate
  "RBNZ_CPI_Change",       # rbnz_cpi_annual_pct_change
  "RBNZ_Unemployment_Rate",# rbnz_unemployment_rate
  "StatsNZ_Category",      # statsnz_Category
  "StatsNZ_OCR",           # statsnz_official_cash_rate
  "Rent_Akl",              # statsnz_rent_auckland
  "Rent_Cant",             # statsnz_rent_canterbury
  "Rent_Wlg",              # statsnz_rent_wellington
  "Consents_Akl",          # statsnz_consents_auckland
  "Consents_Cant",         # statsnz_consents_canterbury
  "Consents_Wlg",          # statsnz_consents_wellington
  "Population_Akl",        # statsnz_population_auckland
  "Population_Cant",       # statsnz_population_canterbury
  "Population_Wlg",        # statsnz_population_wellington
  "Rent_YoY_Akl",          # statsnz_rent_yoy_auckland
  "Rent_YoY_Cant",         # statsnz_rent_yoy_canterbury
  "Rent_YoY_Wlg",          # statsnz_rent_yoy_wellington
  "Rent_Index_Akl",        # statsnz_rent_index_auckland
  "Rent_Index_Cant",       # statsnz_rent_index_canterbury
  "Rent_Index_Wlg",        # statsnz_rent_index_wellington
  "Consents_Per_1000_Akl", # statsnz_consents_per_1000_auckland
  "Consents_Per_1000_Cant",# statsnz_consents_per_1000_canterbury
  "Consents_Per_1000_Wlg", # statsnz_consents_per_1000_wellington
  "Mortgage_Akl",          # statsnz_mortgage_Auckland
  "Mortgage_Cant",         # statsnz_mortgage_Canterbury
  "Mortgage_NZ",           # statsnz_mortgage_New_Zealand
  "Mortgage_Wlg",          # statsnz_mortgage_Wellington
  "Deposit_Akl",           # statsnz_deposit_Auckland
  "Deposit_Cant",          # statsnz_deposit_Canterbury
  "Deposit_NZ",            # statsnz_deposit_New_Zealand
  "Deposit_Wlg",           # statsnz_deposit_Wellington
  "Rent_Akl2",             # statsnz_rent_Auckland
  "Rent_Cant2",            # statsnz_rent_Canterbury
  "Rent_NZ",               # statsnz_rent_New_Zealand
  "Rent_Wlg2"              # statsnz_rent_Wellington
)

clean_df <- raw_df %>%
  select(-contains("Category")) %>%
  mutate(Category = case_when(
    Region %in% c("Auckland") ~ "Auckland Urban",
    Region %in% c("Rodney", "Franklin", "Papakura") ~ "Auckland Rural",
    
    Region == "Christchurch City" ~ "Canterbury Urban",
    Region %in% c("Ashburton District", "Hurunui District", "MacKenzie District",
                  "Selwyn District", "Timaru District", "Waimakariri District", 
                  "Waimate District", "Canterbury") ~ "Canterbury Rural",
    
    Region %in% c("Wellington City", "Hutt City", "Upper Hutt City", "Porirua City") ~ "Wellington Urban",
    Region %in% c("Kapiti Coast District", "Masterton District", 
                  "South Wairarapa District", "Carterton District", "Wellington") ~ "Wellington Rural",
    
    TRUE ~ "Other"
  )) %>%
  select(Year, Region, Category, everything())

na_prop <- clean_df %>%
  summarise(across(everything(), ~ mean(is.na(.)))) %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "na_prop")

na_prop %>% arrange(desc(na_prop))

threshold <- 0.6
vars_to_drop <- na_prop %>% filter(na_prop > threshold) %>% pull(variable)

clean_master_df <- clean_df %>%
  select(-all_of(vars_to_drop)) %>%
  filter(Year >= 2015)





library(dplyr)

clean_region_df <- clean_master_df %>%
  mutate(
    # House price variables
    House_Level = case_when(
      Category %in% c("Auckland Urban", "Auckland Rural") ~ Akl_House_Level,
      Category %in% c("Canterbury Urban", "Canterbury Rural") ~ ChCh_House_Level,
      Category %in% c("Wellington Urban", "Wellington Rural") ~ Wlg_House_Level,
      Category == "Other" ~ rowMeans(select(cur_data(), Akl_House_Level, ChCh_House_Level, Wlg_House_Level), na.rm = TRUE)
    ),
    House_Change = case_when(
      Category %in% c("Auckland Urban", "Auckland Rural") ~ Akl_House_Change,
      Category %in% c("Canterbury Urban", "Canterbury Rural") ~ ChCh_House_Change,
      Category %in% c("Wellington Urban", "Wellington Rural") ~ Wlg_House_Change,
      Category == "Other" ~ rowMeans(select(cur_data(), Akl_House_Change, ChCh_House_Change, Wlg_House_Change), na.rm = TRUE)
    ),
    House_Abs_Change = case_when(
      Category %in% c("Auckland Urban", "Auckland Rural") ~ Akl_House_Abs_Change,
      Category %in% c("Canterbury Urban", "Canterbury Rural") ~ ChCh_House_Abs_Change,
      Category %in% c("Wellington Urban", "Wellington Rural") ~ Wlg_House_Abs_Change,
      Category == "Other" ~ rowMeans(select(cur_data(), Akl_House_Abs_Change, ChCh_House_Abs_Change, Wlg_House_Abs_Change), na.rm = TRUE)
    ),
    # Unemployment
    Unemployment = case_when(
      Category %in% c("Auckland Urban", "Auckland Rural") ~ Unemployment_Auckland,
      Category %in% c("Canterbury Urban", "Canterbury Rural") ~ Unemployment_Christchurch,
      Category %in% c("Wellington Urban", "Wellington Rural") ~ Unemployment_Wellington,
      Category == "Other" ~ rowMeans(select(cur_data(), Unemployment_Auckland, Unemployment_Christchurch, Unemployment_Wellington), na.rm = TRUE)
    ),
    # Income
    Income_Change = case_when(
      Category %in% c("Auckland Urban", "Auckland Rural") ~ Income_Change_Auckland,
      Category %in% c("Canterbury Urban", "Canterbury Rural") ~ Income_Change_Christchurch,
      Category %in% c("Wellington Urban", "Wellington Rural") ~ Income_Change_Wellington,
      Category == "Other" ~ rowMeans(select(cur_data(), Income_Change_Auckland, Income_Change_Christchurch, Income_Change_Wellington), na.rm = TRUE)
    ),
    Income_Level = case_when(
      Category %in% c("Auckland Urban", "Auckland Rural") ~ Income_Auckland_Level,
      Category %in% c("Canterbury Urban", "Canterbury Rural") ~ Income_Christchurch_Level,
      Category %in% c("Wellington Urban", "Wellington Rural") ~ Income_Wellington_Level,
      Category == "Other" ~ rowMeans(select(cur_data(), Income_Auckland_Level, Income_Christchurch_Level, Income_Wellington_Level), na.rm = TRUE)
    )
  ) %>%
  select(Year, Region, Category, Mortgage_Index, Deposit_Index, Rent_Index,
         Mortgage_Floating_Rate, Mortgage_2yr_Fixed_Rate, HPI_Annual_Change,
         HVS_House_Values, RBNZ_House_Price_Change, RBNZ_House_Value_Index,
         RBNZ_Debt_Servicing_Pct, RBNZ_Floating_Rate, RBNZ_2yr_Fixed_Rate,
         RBNZ_CPI_Change, RBNZ_Unemployment_Rate,
         House_Level, House_Change, House_Abs_Change,
         Unemployment, Income_Change, Income_Level)


names(clean_region_df) <- c(
  "Year",                          # Year of observation
  "Region",                        # Name of the area
  "Category",                      # Urban / Rural / Other category
  "Mortgage_Index",            # Housing Affordability: Mortgage Index
  "Deposit_Index",             # Housing Affordability: Deposit Index
  "Rent_Index",                # Housing Affordability: Rent Index
  "Mortgage_Floating_Rate",        # Floating mortgage interest rate (%)
  "Mortgage_2yr_Fixed_Rate",       # 2-year fixed mortgage interest rate (%)
  "HPI_Annual_Change",             # House Price Index: annual % change
  "Housing_Stock_House_Values",              # Value of Housing Stock (median or total)
  "RBNZ_House_Price_Annual_Change",# Reserve Bank: annual house price % change
  "RBNZ_House_Value_Index",        # Reserve Bank: House Value Index
  "RBNZ_Debt_Servicing_Pct",       # Reserve Bank: Debt servicing as % of income
  "RBNZ_Floating_Rate",            # Reserve Bank: floating mortgage rate (%)
  "RBNZ_2yr_Fixed_Rate",           # Reserve Bank: 2-year fixed mortgage rate (%)
  "RBNZ_CPI_Annual_Change",        # Reserve Bank: CPI annual change (%)
  "RBNZ_Unemployment_Rate",        # Reserve Bank: unemployment rate (%)
  "Avg_House_Price_Level",         # Average house price level for the region
  "Avg_House_Price_Change",        # Annual change in average house price
  "Avg_House_Price_Abs_Change",    # Absolute change in average house price
  "Unemployment_Rate",             # Unemployment rate for the region
  "Income_Quarterly_Change",       # Quarterly change in income for the region
  "Income_Level"                   # Income level for the region
)

#write_csv(clean_region_df, "master_dataset_clean.csv")