library(tidyverse)
library(lubridate)
library(scales)

path <- "MASTER_PLUS_HAI_WIDE.csv"   
df <- read_csv(path, show_col_types = FALSE) %>% mutate(date = ymd(date))

# ---- 1) WIDE -> LONG with an explicit 'region' -----------------------------
lc_regions <- c("auckland","canterbury","wellington","new_zealand")
rent_driver_cols <- names(df)[grepl(paste0("^rent_(", paste(lc_regions, collapse="|"), ")$"), names(df))]
if (length(rent_driver_cols)) {
  names(df)[match(rent_driver_cols, names(df))] <- sub("^rent_", "rentDriver_", rent_driver_cols)
}

region_suffix_pat <- "(Auckland|Canterbury|Wellington|New[_ ]?Zealand|auckland|canterbury|wellington|new_zealand)$"

df_long <- df %>%
  pivot_longer(
    cols = matches(region_suffix_pat),
    names_to   = c("metric","region_raw"),
    names_pattern = paste0("^(.*)_", region_suffix_pat),
    values_to  = "value"
  ) %>%
  mutate(
    region = case_when(
      region_raw %in% c("Auckland","auckland")        ~ "Auckland",
      region_raw %in% c("Canterbury","canterbury")    ~ "Canterbury",
      region_raw %in% c("Wellington","wellington")    ~ "Wellington",
      region_raw %in% c("New_Zealand","New Zealand",
                        "new_zealand")                ~ "New Zealand",
      TRUE ~ region_raw
    )
  ) %>%
  select(-region_raw) %>%
  pivot_wider(names_from = metric, values_from = value)

# ---- 2) Tidy features / auto-detect columns --------------------------------
# Mortgage HAI column 
mortgage_col <- dplyr::case_when(
  "mortgage" %in% names(df_long) ~ "mortgage",
  any(grepl("^mortgage(_hai)?$", names(df_long), ignore.case = TRUE)) ~
    names(df_long)[grepl("^mortgage(_hai)?$", names(df_long), ignore.case = TRUE)][1],
  TRUE ~ NA_character_
)
stopifnot(!is.na(mortgage_col))

# OCR column
ocr_col <- dplyr::case_when(
  "official_cash_rate" %in% names(df_long) ~ "official_cash_rate",
  "OCR" %in% names(df_long)                ~ "OCR",
  TRUE                                     ~ NA_character_
)

# Choose driver rent column 
rent_driver_col <- dplyr::case_when(
  "rentDriver" %in% names(df_long) ~ "rentDriver",
  "rent" %in% names(df_long)       ~ "rent",
  TRUE                             ~ NA_character_
)

df_long <- df_long %>%
  mutate(period = if_else(date < ymd("2020-03-31"), "Pre-COVID", "Post-COVID")) %>%
  group_by(region) %>%
  arrange(date, .by_group = TRUE) %>%
  mutate(
    consents_l4   = if ("consents"   %in% names(.)) lag(consents, 4) else NA_real_,
    population_yoy= if ("population" %in% names(.)) (population / lag(population, 4) - 1) * 100 else NA_real_
  ) %>%
  ungroup()

# ---- 3) Charts --------------------------------------------------------------
plain <- theme_minimal(base_size = 12) +
  theme(legend.position = "bottom", panel.grid.minor = element_blank())
covid_start <- as.Date("2020-03-31")

# (1) Mortgage HAI trend (Q4)
p_hai_trend <- ggplot(df_long, aes(date, .data[[mortgage_col]], colour = region)) +
  geom_line(linewidth = 0.9, alpha = 0.9) +
  geom_vline(xintercept = covid_start, linetype = "dashed") +
  labs(title = "Mortgage Affordability Index (HAI) over time",
       subtitle = "Dashed line marks 2020Q1 (COVID shock). Downward drift post-2020 across all regions.",
       x = "Quarter", y = "Mortgage HAI (↑ = more affordable)", colour = "Region") +
  scale_x_date(date_breaks = "1 year", labels = label_date("%Y")) +
  plain

# (2) OCR trend (context)
if (!is.na(ocr_col)) {
  p_ocr_trend <- df_long %>%
    group_by(date) %>%
    summarise(ocr = first(.data[[ocr_col]]), .groups = "drop") %>%
    ggplot(aes(date, ocr)) +
    geom_line(linewidth = 0.9) +
    geom_vline(xintercept = covid_start, linetype = "dashed") +
    labs(title = "Official Cash Rate (OCR)",
         subtitle = "Higher OCR periods align with lower mortgage affordability.",
         x = "Quarter", y = "OCR (%)") +
    scale_x_date(date_breaks = "1 year", labels = label_date("%Y")) +
    plain
}

# (3) HAI vs OCR (Q1: OCR effect)
if (!is.na(ocr_col)) {
  p_scatter_ocr <- ggplot(df_long, aes(.data[[ocr_col]], .data[[mortgage_col]], colour = region)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = "lm", se = FALSE) +
    facet_wrap(~ region, scales = "free") +
    labs(title = "Mortgage HAI vs OCR",
         subtitle = "Negative slope in each region: when OCR rises, mortgage affordability falls.",
         x = "OCR (%)", y = "Mortgage HAI") +
    plain + theme(legend.position = "none")
}

# (4) HAI vs Rents (Q1: rent pressure)
if (!is.na(rent_driver_col)) {
  p_scatter_rent <- ggplot(df_long, aes(.data[[rent_driver_col]], .data[[mortgage_col]], colour = region)) +
    geom_point(alpha = 0.5, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
    facet_wrap(~ region, scales = "free") +
    labs(title = "Mortgage HAI vs Rents (driver)",
         subtitle = "Higher rents align with worse mortgage affordability (negative slope).",
         x = "Rent (driver)", y = "Mortgage HAI") +
    plain + theme(legend.position = "none")
}

# (5) HAI vs Consents (lagged) — supply timing
if ("consents_l4" %in% names(df_long)) {
  p_scatter_consents <- ggplot(df_long, aes(consents_l4, .data[[mortgage_col]], colour = region)) +
    geom_point(alpha = 0.5, na.rm = TRUE) +
    geom_smooth(method = "lm", se = FALSE, na.rm = TRUE) +
    facet_wrap(~ region, scales = "free") +
    labs(title = "Mortgage HAI vs Building Consents (t−4 quarters)",
         subtitle = "Supply effects show with lags; patterns differ by region.",
         x = "Consents (lagged 1 year)", y = "Mortgage HAI") +
    plain + theme(legend.position = "none")
}

# (6) Pre vs Post COVID bars (Q4)
prepost <- df_long %>%
  mutate(period = if_else(date < ymd("2020-03-31"), "Pre-COVID", "Post-COVID")) %>%
  group_by(region, period) %>%
  summarise(mortgage_mean = mean(.data[[mortgage_col]], na.rm = TRUE), .groups = "drop")

p_prepost <- ggplot(prepost, aes(period, mortgage_mean, fill = period)) +
  geom_col(width = 0.7) +
  facet_wrap(~ region) +
  labs(title = "Mortgage HAI: Pre vs Post COVID averages",
       subtitle = "Shows a level drop post-2020 across all three regions.",
       x = NULL, y = "Average Mortgage HAI") +
  plain + theme(legend.position = "none")

# ---- 4) Save ----------------------------------------------------------------
dir.create("figures", showWarnings = FALSE)
ggsave("figures/01_hai_trend.png", p_hai_trend, width = 10, height = 6, dpi = 300)
if (exists("p_ocr_trend"))        ggsave("figures/02_ocr_trend.png", p_ocr_trend, width = 10, height = 6, dpi = 300)
if (exists("p_scatter_ocr"))      ggsave("figures/03_hai_vs_ocr_by_region.png", p_scatter_ocr, width = 10, height = 6, dpi = 300)
if (exists("p_scatter_rent"))     ggsave("figures/04_hai_vs_rent_by_region.png", p_scatter_rent, width = 10, height = 6, dpi = 300)
if (exists("p_scatter_consents")) ggsave("figures/05_hai_vs_consents_lag4_by_region.png", p_scatter_consents, width = 10, height = 6, dpi = 300)
ggsave("figures/06_prepost_covid_hai.png", p_prepost, width = 10, height = 6, dpi = 300)

message("Saved charts to the 'figures/' folder.")