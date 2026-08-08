library(tidyverse)
library(lubridate)

housing_affordability <- read.csv('datasets/data.govt.nz/Housing_Affordability_Indices.csv')

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

housing_affordability <- housing_affordability %>%
  mutate(date = ymd(date)) %>%
  drop_na()

housing_affordability <- housing_affordability %>%
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

housing_affordability_long <- housing_affordability %>%
  pivot_longer(
    cols = c(Mortgage.Affordability.Index,
             Deposit.Affordability.Index,
             Rent.Affordability.Index),
    names_to = 'IndexType',
    values_to = 'IndexValue'
  ) %>%
  mutate(IndexType = recode(IndexType,
                            "Mortgage.Affordability.Index" = "Mortgage Affordability",
                            "Deposit.Affordability.Index" = "Deposit Affordability",
                            "Rent.Affordability.Index" = "Rent Affordability"))

housing_affordability_avg <- housing_affordability_long %>%
  group_by(date, Category, IndexType) %>%
  summarise(IndexValue = mean(IndexValue, na.rm = TRUE), .groups = "drop")

ggplot(housing_affordability_avg, aes(x = date, y = IndexValue, color = IndexType)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  facet_wrap(~ Category, scales = "fixed") +
  labs(
    title = "Affordability Indexes Over Time by Region Category",
    subtitle = 'Shaded region shows COVID period (2020-2023)',
    x = "Date",
    y = "Index Value",
    color = "Index Type"
  ) +
  theme_bw() +
  theme(
    strip.text = element_text(size = 12, face = "bold"),
    legend.position = "bottom"
  )

deposit_plot <- housing_affordability_avg %>%
  filter(IndexType == "Deposit Affordability") %>%
  ggplot(aes(x = date, y = IndexValue, color = Category)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(title = "Deposit Affordability Over Time",
       x = "Date", y = "Index Value", color = "Region Category") +
  theme_bw()

mortgage_plot <- housing_affordability_avg %>%
  filter(IndexType == "Mortgage Affordability") %>%
  ggplot(aes(x = date, y = IndexValue, color = Category)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(title = "Mortgage Affordability Over Time",
       x = "Date", y = "Index Value", color = "Region Category") +
  theme_bw()

rent_plot <- housing_affordability_avg %>%
  filter(IndexType == "Rent Affordability") %>%
  ggplot(aes(x = date, y = IndexValue, color = Category)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(title = "Rent Affordability Over Time",
       x = "Date", y = "Index Value", color = "Region Category") +
  theme_bw()


housing_affordability_urban_rural <- housing_affordability_long %>%
  mutate(UrbanRural = case_when(
    Category %in% c("Auckland Urban", "Canterbury Urban", "Wellington Urban") ~ "Urban",
    Category %in% c("Auckland Rural", "Canterbury Rural", "Wellington Rural") ~ "Rural",
    TRUE ~ "Other"
  )) %>%
  group_by(date, UrbanRural, IndexType) %>%
  summarise(IndexValue = mean(IndexValue, na.rm = TRUE), .groups = "drop")

deposit_plot_ur <- housing_affordability_urban_rural %>%
  filter(IndexType == "Deposit Affordability") %>%
  ggplot(aes(x = date, y = IndexValue, color = UrbanRural)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(title = "Deposit Affordability Over Time (Urban vs Rural)",
       x = "Date", y = "Index Value", color = "Area Type") +
  theme_bw()

mortgage_plot_ur <- housing_affordability_urban_rural %>%
  filter(IndexType == "Mortgage Affordability") %>%
  ggplot(aes(x = date, y = IndexValue, color = UrbanRural)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(title = "Mortgage Affordability Over Time (Urban vs Rural)",
       x = "Date", y = "Index Value", color = "Area Type") +
  theme_bw()

rent_plot_ur <- housing_affordability_urban_rural %>%
  filter(IndexType == "Rent Affordability") %>%
  ggplot(aes(x = date, y = IndexValue, color = UrbanRural)) +
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(title = "Rent Affordability Over Time (Urban vs Rural)",
       x = "Date", y = "Index Value", color = "Area Type") +
  theme_bw()


deposit_plot
mortgage_plot
rent_plot
deposit_plot_ur
mortgage_plot_ur
rent_plot_ur

plots <- list(
  Deposit_Affordability = deposit_plot,
  Mortgage_Affordability = mortgage_plot,
  Rent_Affordability = rent_plot,
  Deposit_Affordability_Urban_Rural = deposit_plot_ur,
  Mortgage_Affordability_Urban_Rural = mortgage_plot_ur,
  Rent_Affordability_Type_Urban_Rural = rent_plot_ur
)

# for (name in names(plots)) {
#   ggsave(
#     filename = paste0(name, ".png"),
#     plot = plots[[name]],
#     width = 12,
#     height = 6,
#     dpi = 600,
#     bg = "white"
#   )
# }