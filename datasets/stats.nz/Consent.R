library(tidyverse)
library(lubridate)
library(janitor)

# ----------------- helpers -----------------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
numify  <- function(x) if (is.numeric(x)) x else readr::parse_number(as.character(x))

# HLFS-style quarter start: Mar/Jun/Sep/Dec (stamp as the 1st)
quarter_start_hlfs <- function(d) {
  y <- year(d); m <- month(d)
  qm <- case_when(m <= 3 ~ 3L, m <= 6 ~ 6L, m <= 9 ~ 9L, TRUE ~ 12L)
  as.Date(sprintf("%d-%02d-01", y, qm))
}

parse_date_smart <- function(x) {
  d <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Y-m-d","d/m/Y","m/d/Y","Y/m/d","Y-m","m-Y","b Y","Y","d-b-Y"),
    exact = FALSE
  ))
  if (all(is.na(d))) d <- suppressWarnings(readr::parse_datetime(as.character(x)))
  as_date(d)
}

standardise_region <- function(x) {
  x <- tolower(x)
  case_when(
    str_detect(x, "auck")        ~ "Auckland",
    str_detect(x, "well")        ~ "Wellington",
    str_detect(x, "christ|canter") ~ "Canterbury",   # <- Christchurch mapped to Canterbury
    TRUE                         ~ NA_character_
  )
}

# ----------------- read -----------------
src <- "Consent.csv"
stopifnot(file.exists(src))

raw <- suppressMessages(readr::read_csv(src, show_col_types = FALSE)) %>% clean_names()
nm  <- names(raw)

# find a date-like column or year column
date_col <- nm[str_detect(nm, "(^|_)(date|period|month|time|year)($|_)")][1] %||% nm[1]

# detect likely region columns (keep only the three we need)
region_cols_idx <- str_which(nm, "(?i)auck|well|christ|canter")
region_cols <- nm[region_cols_idx]
stopifnot(length(region_cols) >= 1)

# ----------------- tidy to long -----------------
long0 <- raw %>%
  mutate(date_any = parse_date_smart(.data[[date_col]])) %>%
  mutate(year_guess = suppressWarnings(parse_integer(as.character(.data[[date_col]])))) %>%
  mutate(date_coalesced = coalesce(date_any, make_date(year_guess, 6, 30))) %>%  # mid-year if only year
  select(date = date_coalesced, all_of(region_cols)) %>%
  pivot_longer(-date, names_to = "region_raw", values_to = "value") %>%
  mutate(
    region = standardise_region(region_raw),
    value  = numify(value)
  ) %>%
  filter(!is.na(region), !is.na(date))

# Decide if source is monthly/quarterly vs annual (for aggregation rule)
has_months <- long0 %>%
  mutate(m = month(date)) %>%
  summarise(nm = n_distinct(m, na.rm = TRUE)) %>%
  pull(nm) > 1

# ----------------- quarterise -----------------
if (has_months) {
  # MONTHLY/QUARTERLY SOURCE -> sum within each quarter
  cons_qtr <- long0 %>%
    mutate(qtr = quarter_start_hlfs(date)) %>%
    filter(qtr >= as_date("2018-01-01"), qtr <= as_date("2025-12-31")) %>%
    group_by(qtr, region) %>%
    summarise(dwellings_consented = sum(value, na.rm = TRUE), .groups = "drop")
} else {
  # ANNUAL SOURCE -> carry-forward (YE-March mapping)
  # First, place the observed annual point on a YE-March date if month is NA/Jan etc.
  annual <- long0 %>%
    mutate(date = if_else(month(date) == 3 & day(date) == 31,
                          date, make_date(year(date), 3, 31)))
  # Build quarter grid and join each quarter to the applicable YE-March
  q_grid <- tibble(date = seq(as_date("2018-01-01"), as_date("2025-12-31"), by = "1 month")) %>%
    mutate(qtr = quarter_start_hlfs(date)) %>%
    distinct(qtr) %>%
    filter(month(qtr) %in% c(3,6,9,12)) %>%
    transmute(qtr = qtr) %>%
    crossing(region = c("Auckland","Canterbury","Wellington")) %>%
    mutate(yemar = if_else(month(qtr) >= 4,
                           make_date(year(qtr) + 1, 3, 31),
                           make_date(year(qtr), 3, 31)))
  cons_qtr <- q_grid %>%
    left_join(annual %>% select(region, yemar = date, value), by = c("region","yemar")) %>%
    transmute(qtr, region, dwellings_consented = value)
}

# ----------------- wide, integer, ordered -----------------
cons_qtr_wide <- cons_qtr %>%
  mutate(dwellings_consented = as.integer(round(dwellings_consented, 0))) %>%
  pivot_wider(names_from = region, values_from = dwellings_consented) %>%
  arrange(qtr) %>%
  # ensure all 3 region columns exist
  { for (nm in c("Auckland","Canterbury","Wellington")) if (!nm %in% names(.)) .[[nm]] <- NA_integer_; . } %>%
  select(date = qtr, Auckland, Canterbury, Wellington)

# keep range strictly 2018–2025
cons_qtr_wide <- cons_qtr_wide %>%
  filter(date >= as_date("2018-01-01"), date <= as_date("2025-12-31"))

# ----------------- write -----------------
out <- "consents_quarterly_2018_2025.csv"
readr::write_csv(cons_qtr_wide, out)

cat("Wrote:", out,
    "\nRows:", nrow(cons_qtr_wide),
    "\nDate range:", format(min(cons_qtr_wide$date), "%Y-%m-%d"), "→", format(max(cons_qtr_wide$date), "%Y-%m-%d"), "\n")