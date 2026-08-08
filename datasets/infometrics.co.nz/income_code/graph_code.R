# Load libraries
library(ggplot2)
library(readr)

dir.create("datasets/infometrics.co.nz/graphs", showWarnings = FALSE, recursive = TRUE)

# Define COVID year range
covid_start <- as.Date("2020-01-01")
covid_end   <- as.Date("2023-08-01")

# Read cleaned average household income data
avg_income <- read_csv("datasets/infometrics.co.nz/processed_data/avg_quarterly_income.csv",
                       show_col_types = FALSE)

# Combine regions into a single column 'Region' and average income values into a single column 'Avg_Income'
avg_income_df <- avg_income %>%
  pivot_longer(cols = starts_with("nz_level") | starts_with("akl_level") |
                 starts_with("chc_level") | starts_with("wlg_level"),
               names_to = "Region", values_to = "Avg_Income")

# Rename regions for the legend
avg_income_df$Region <- recode(avg_income_df$Region, 
                               "nz_level" = "New Zealand",
                               "akl_level" = "Auckland",
                               "chc_level" = "Christchurch",
                               "wlg_level" = "Wellington")

# Plot the average household income data
avg_income_plot <- ggplot(avg_income_df, aes(x = date, y = Avg_Income,
                                             color = Region, group = Region)) +
  geom_line(size = 1) + 
  labs(
    title = "Average Household Income in New Zealand Since 2015",
    x = "Year",
    y = "Average Household Income ($NZD)",
    color = "Region") + 
  theme_bw() +
  # Show and label every year from 2015 to 2025 in x axis
  scale_x_date(
    breaks = seq.Date(from = min(avg_income_df$date), to = max(avg_income_df$date), by = "1 year"),
    labels = scales::date_format("%Y")  # Format to display only the year
  ) +
  # Set custom colours for each region
  scale_color_manual(values = c("New Zealand" = "purple", 
                                "Christchurch" = "green", 
                                "Wellington" = "blue", 
                                "Auckland" = "red")) +
  # Add shaded region for COVID period
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33)

# Read cleaned average change in household income data
income_change <- read_csv("datasets/infometrics.co.nz/processed_data/avg_quarterly_income_change.csv",
                       show_col_types = FALSE)
# Combine regions into a single column 'Region' and change in income values into a single column 'Change'
income_change_df <- income_change %>%
  pivot_longer(cols = starts_with("nz_change") | starts_with("akl_change") |
                 starts_with("chc_change") | starts_with("wlg_change"),
               names_to = "Region", values_to = "Change")

# Rename regions for the legend
income_change_df$Region <- recode(income_change_df$Region, 
                                  "nz_change" = "New Zealand",
                                  "akl_change" = "Auckland",
                                  "chc_change" = "Christchurch",
                                  "wlg_change" = "Wellington")

# Plot the change in household income data
income_change_plot <- ggplot(income_change_df, aes(x = date, y = Change,
                                                   color = Region, group = Region)) +
  geom_line(size = 1) + 
  labs(
    title = "Average Change in Household Income in New Zealand Since 2015",
    x = "Year",
    y = "Household Income ($NZD)",
    color = "Region") + 
  theme_bw() +
  # Show and label every year from 2015 to 2025 in x axis
  scale_x_date(
    breaks = seq.Date(from = min(income_change_df$date), to = max(income_change_df$date), by = "1 year"),
    labels = scales::date_format("%Y")  # Format to display only the year
  ) +
  # Set custom colours for each region
  scale_color_manual(values = c("New Zealand" = "purple", 
                                "Christchurch" = "green", 
                                "Wellington" = "blue", 
                                "Auckland" = "red")) +
  # Add shaded region for COVID period
  annotate("rect",
           xmin = covid_start, xmax = covid_end,
           ymin = -Inf, ymax = Inf,
           fill = "grey", alpha = 0.33)

# Save plots as png files
ggsave("datasets/infometrics.co.nz/graphs/average_income.png", avg_income_plot, width = 9, height = 5, dpi = 200, bg = "white")
ggsave("datasets/infometrics.co.nz/graphs/average_income_change.png", income_change_plot, width = 9, height = 5, dpi = 200, bg = "white")  
