library(tidyverse)
library(scales)

local_housing <- read.csv('datasets/data.govt.nz/Local_Housing_Statistics.csv')

unique(local_housing$series) # Show variables in dataset

testing <- local_housing %>% # Replace series == 'x' with desired variable
  filter(series == "Renting Household Proportion") %>%
  mutate(date = ymd(date)) %>%
  mutate(value = as.numeric(gsub(",", "", value)))

ggplot(testing, aes(x = date, y = value, group = 1)) +
  geom_point(color = "blue", size = 2) +
  geom_line(color = "blue", linewidth = 1) +
  scale_y_continuous(
    breaks = pretty_breaks(n = 10), # Number of y ticks
    labels = scales::comma
  ) +
  labs(
    title = "",
    x = "Date",
    y = ""
  ) +
  theme_minimal()