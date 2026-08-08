source('datasets/infometrics.co.nz/house prices/merge_house_prices.R')

# Define Covid Date Range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

ggplot(all_regions_long, aes(x = Year, y = House_Price, color = Region)) +
  geom_line(size = 1) +
  labs(
    title = "Average House Prices by Region (2005–2025)",
    subtitle = 'Shaded region shows COVID year (2020-2023)',
    x = "Year",
    y = "House Price (NZD)",
    color = "Region"
  ) +
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33) + 
  theme_bw() +
  theme(legend.position = "right")

ggsave(
  filename = "Yearly average house price by region.png",
  plot = last_plot(),
  width = 12,
  height = 6,
  dpi = 600,
  bg = 'white'
)