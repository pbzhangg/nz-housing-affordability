library(tidyverse)

# Read data set
mortgage <- read.csv('datasets/data.govt.nz/Banks_Mortgage_Rates.csv')

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

# Mortgage Data set
colSums(is.na(mortgage)) # Check for NA values
summary(mortgage) # Check data structure

mortgage <- mortgage %>% # Set data type
  mutate(Date = dmy(Date))

mortgage_long <- mortgage %>% # Reshape for easier plotting
  pivot_longer(
    cols = c(Floating.rate, X2.year.fixed.rate),
    names_to = "RateType",
    values_to = "RateValue"
  ) %>%
  mutate(RateType = recode(RateType,
                           "Floating.rate" = "Floating",
                           "X2.year.fixed.rate" = "2-Year Fixed"))

ggplot(mortgage_long, aes(x = Date, y = RateValue, color = RateType)) + # Create Plot
  annotate('rect',
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = 'gray', alpha = 0.33) + 
  geom_line(size = 1.2) +
  labs(
    title = "NZ Mortgage Rates Over Time",
    subtitle = 'Shaded region shows COVID year (2020-2023)',
    x = "Date",
    y = "Interest Rate (%)",
    color = "Rate Type"
  ) +
  theme_bw()

# ggsave(
#   filename = "Fixed vs. Floating Morgage Rates.png",
#   plot = last_plot(),
#   width = 12,
#   height = 6,
#   dpi = 600,
#   bg = 'white'
# )

mortgage_yoy <- mortgage_long %>% # Calculate year over year changes
  arrange(RateType, Date) %>%
  group_by(RateType) %>%
  mutate(
    YoY_Change = RateValue - lag(RateValue)
  ) %>%
  ungroup()

ggplot(mortgage_yoy, aes(x = Date, y = YoY_Change, fill = RateType)) + # plot yoy changes
  geom_col(position = "dodge",) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  labs(
    title = "Year-over-Year Change in NZ Mortgage Rates",
    x = "Date",
    y = "Change in Interest Rate (percentage points)",
    fill = "Rate Type"
  ) +
  theme_bw() + 
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33)


# ggsave(
#   filename = "Fixed vs. Floating Morgage Rates YoY.png",
#   plot = last_plot(),
#   width = 12,
#   height = 6,
#   dpi = 600,
#   bg = 'white'
# )