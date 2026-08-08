library(tidyverse)
library(lubridate)
library(scales)

# Load data
housing_index <- read.csv('datasets/data.govt.nz/House_Price_Index.csv')

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

housing_index <- housing_index %>%
  mutate(
    Date = dmy(Date),
    House.prices..annual...change. = as.numeric(House.prices..annual...change.)
  ) %>%
  rename(
    Annual_HousePrice_Change = House.prices..annual...change.
  )

ggplot(housing_index, aes(x = Date, y = Annual_HousePrice_Change, 
                          fill = Annual_HousePrice_Change >= 0)) +
  geom_col(show.legend = FALSE) +
  geom_hline(yintercept = 0, color = "black", linetype = "dashed") +
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33) +
  scale_fill_manual(values = c("tomato", "steelblue")) +
  scale_x_date(date_labels = "%Y", date_breaks = "2 years") +
  labs(
    title = "Annual % Change in NZ House Prices",
    subtitle = "Grey area = COVID period",
    x = "Year",
    y = "Annual % Change (%)"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# ggsave(
#   filename = "Annual Percent Change in House Prices.png",
#   plot = last_plot(),
#   width = 12,
#   height = 6,
#   dpi = 600,
#   bg = 'white'
# )