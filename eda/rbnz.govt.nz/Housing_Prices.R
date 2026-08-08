library(tidyverse)
library(lubridate)
library(scales)

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")
covid_start_year <- year(covid_start) + (yday(covid_start) - 1) / 365
covid_end_year   <- year(covid_end) + (yday(covid_end) - 1) / 365

rbnz <- read_csv("datasets/rbnz.govt.nz/data_processed/Merging of all files.csv") %>%
  mutate(quarter = dmy(quarter)) %>%
  drop_na

ggplot(rbnz, aes(x = quarter, y = house_price_annual_pct_change)) +
  geom_line(color = "steelblue", linewidth = 1) +
  geom_point(color = "steelblue", size = 2) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(labels = label_percent(scale = 1)) +
  labs(
    title = "Annual % Change in House Prices",
    subtitle = 'Shaded region shows COVID year (2020-2023)',
    x = '',
    y = "Annual Change (%)"
  ) +
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33) +
  theme_bw()

ggsave(
  filename = "Annual Percent Change in House Prices.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 600,
  bg = 'white'
)

rbnz_yearly <- rbnz %>%
  arrange(quarter) %>%
  mutate(year = lubridate::year(quarter)) %>%
  group_by(year) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  mutate(
    growth_factor = 1 + house_price_annual_pct_change / 100,
    cumulative_index = cumprod(growth_factor),
    cumulative_pct_change = (cumulative_index - 1) * 100
  )


ggplot(rbnz_yearly, aes(x = year, y = cumulative_pct_change)) +
  geom_line(color = "darkorange", linewidth = 1) +
  geom_point(color = "darkorange", size = 2) +
  scale_x_continuous(breaks = pretty(rbnz_yearly$year)) +
  scale_y_continuous(limits = c(0, 120),
    breaks = seq(0, 120, by = 20),
    labels = scales::label_percent(scale = 1)
  ) +
  labs(
    title = "NZ House Price Growth Since 2015",
    subtitle = "Shaded region shows COVID year (2020-2023)",
    x = 'Year',
    y = "Cumulative Growth (%)"
  ) +
  annotate("rect",
           xmin = covid_start_year, xmax = covid_end_year,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33) +
  theme_bw()

ggsave(
  filename = "NZ House Price Growth Since 2015.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 600,
  bg = 'white'
)