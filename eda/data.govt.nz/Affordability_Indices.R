# https://www.hud.govt.nz/stats-and-insights/change-in-housing-affordability-indicators/about-the-indicators

library(tidyverse)

housing_affordability <- read.csv('datasets/data.govt.nz/Housing_Affordability_Indices.csv')

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

colSums(is.na(housing_affordability)) # Check for NA values
summary(housing_affordability) # Check data structure

housing_affordability <- housing_affordability %>% # Set data type
  mutate(date = ymd(date)) %>%
  drop_na()

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
  group_by(date, IndexType) %>%
  summarise(IndexValue = mean(IndexValue, na.rm = TRUE), .groups = "drop")

ggplot(housing_affordability_avg, aes(x = date, y = IndexValue, color = IndexType)) + # Create Plot
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(
    title = "Affordability Indexes Over Time",
    subtitle = 'Shaded region shows COVID year (2020-2023)',
    x = "Date",
    y = "Index Value",
    color = "Index Type"
  ) +
  theme_bw()

# ggsave(
#   filename = "Affordability Indices Over Time.png",
#   plot = last_plot(),
#   width = 12,
#   height = 6,
#   dpi = 600,
#   bg = 'white'
# )