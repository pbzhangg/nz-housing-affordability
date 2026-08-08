library(tidyverse)

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

housing_stock <- read.csv('datasets/data.govt.nz/Value_of_Housing_Stock.csv')

housing_stock <- housing_stock %>%
  mutate(Date = dmy(Date))

ggplot(data = housing_stock, aes(x = Date, y = House.values)) +
  geom_col(fill = 'steelblue',
           width = 70) +
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.4)+
  theme_bw() +
  labs(title = 'Total value of NZ houses',
       subtitle = 'Shaded region shows COVID year (2020-2023)',
       x = 'Date',
       y = '$m')

# ggsave(
#   filename = "Total value of NZ houses.png",
#   plot = last_plot(),
#   width = 12,
#   height = 6,
#   dpi = 600,
#   bg = 'white'
# )