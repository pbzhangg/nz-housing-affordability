library(tidyverse)
library(lubridate)
library(janitor)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
numify  <- function(x) if (is.numeric(x)) x else readr::parse_number(as.character(x))

# HLFS-style quarter start: Mar/Jun/Sep/Dec (stamp as the 1st)
quarter_start_hlfs <- function(d) {
  y <- year(d); m <- month(d)
  qm <- case_when(m <= 3 ~ 3L, m <= 6 ~ 6L, m <= 9 ~ 9L, TRUE ~ 12L)
  as.Date(sprintf("%d-%02d-01", y, qm))
}

# Robust reader: pull YEAR and ERP from a Stats NZ-style CSV
read_erp <- function(path, region_out) {
  df <- suppressMessages(readr::read_csv(path, show_col_types = FALSE)) %>% clean_names()
  
  # year/period column
  year_col <- names(df)[str_detect(names(df), "(^|_)(year|period|date|category)($|_)")][1] %||% names(df)[1]
  yrs <- stringr::str_extract(as.character(df[[year_col]]), "\\d{4}") %>% readr::parse_integer()
  
  # ERP column candidates
  pop_cands <- names(df)[str_detect(
    names(df),
    "estimated.*resident.*population|resident.*population|^population$|erp"
  )]
  if (length(pop_cands) == 0) {
    # fallback: most-variable numeric column
    num_cols <- df %>% select(where(is.numeric))
    pop_col <- if (ncol(num_cols) > 0) names(num_cols)[which.max(map_dbl(num_cols, ~ sd(.x, na.rm = TRUE)))] else NA_character_
  } else {
    # prefer the most specific "estimated resident population"
    pref <- pop_cands[str_detect(pop_cands, "estimated.*resident.*population")]
    pop_col <- (pref[1]) %||% pop_cands[1]
  }
  
  erp <- numify(df[[pop_col]])
  
  tibble(region = region_out, year = yrs, erp = erp) %>%
    filter(!is.na(year)) %>%
    mutate(erp = as.numeric(erp))
}

# ---------- inputs (Christchurch -> Canterbury) ----------
files  <- c("population-of-auckland-r.csv",
            "population-of-christchur.csv",
            "population-of-wellington.csv")
regions <- c("Auckland", "Canterbury", "Wellington")

pop_yearly <- map2_dfr(files, regions, read_erp) %>%
  filter(between(year, 2018, 2025)) %>%
  group_by(region, year) %>%
  summarise(erp = dplyr::last(na.omit(erp)), .groups = "drop")

# Ensure full grid 2018–2025 for each region, carry last known forward (e.g., fill 2025 if missing)
pop_yearly_full <- tidyr::expand_grid(region = regions, year = 2018:2025) %>%
  left_join(pop_yearly, by = c("region","year")) %>%
  group_by(region) %>%
  arrange(year, .by_group = TRUE) %>%
  tidyr::fill(erp, .direction = "down") %>%
  ungroup()

# Quarter grid and join by year (step function within each year)
qtr_grid <- tibble(date = seq(as_date("2018-01-01"), as_date("2025-12-31"), by = "1 month")) %>%
  mutate(date = quarter_start_hlfs(date)) %>%
  distinct(date) %>%
  filter(month(date) %in% c(3,6,9,12)) %>%
  mutate(year = year(date)) %>%
  tidyr::expand_grid(region = regions)

pop_qtr <- qtr_grid %>%
  left_join(pop_yearly_full, by = c("region","year")) %>%
  select(date, region, erp) %>%
  mutate(erp = as.integer(round(erp, 0))) %>%
  pivot_wider(names_from = region, values_from = erp) %>%
  arrange(date) %>%
  # ensure all three region columns exist and order them
  { for (nm in c("Auckland","Canterbury","Wellington")) if (!nm %in% names(.)) .[[nm]] <- NA_integer_; . } %>%
  select(date, Auckland, Canterbury, Wellington)

# ---------- write ----------
out_file <- "population_quarterly_2018_2025.csv"
readr::write_csv(pop_qtr, out_file)

cat("Wrote:", out_file,
    "\nRows:", nrow(pop_qtr),
    "\nDate range:", format(min(pop_qtr$date), "%Y-%m-%d"), "→", format(max(pop_qtr$date), "%Y-%m-%d"), "\n")