library(tidyverse)
library(lubridate)
library(janitor)

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
numify  <- function(x) if (is.numeric(x)) x else readr::parse_number(as.character(x))

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
std_region <- function(x) {
  x <- tolower(x)
  case_when(
    str_detect(x, "auck")          ~ "Auckland",
    str_detect(x, "christ|canter") ~ "Canterbury",  # Christchurch → Canterbury
    str_detect(x, "well")          ~ "Wellington",
    TRUE                           ~ NA_character_
  )
}

# ---------- READ (WIDE) ----------
src <- "Rent.csv"; stopifnot(file.exists(src))
raw <- suppressMessages(readr::read_csv(src, show_col_types = FALSE)) %>% clean_names()
nm  <- names(raw)

date_col <- nm[str_detect(nm, "(^|_)(date|period|month|time|year)($|_)")][1] %||% nm[1]
region_cols <- nm[str_detect(nm, "(?i)auck|well|christ|canter")]
stopifnot(length(region_cols) >= 1)

# ---------- WIDE → LONG → QUARTERLY ----------
rent_qtr <- raw %>%
  mutate(date_raw = parse_date_smart(.data[[date_col]])) %>%
  select(date_raw, all_of(region_cols)) %>%
  pivot_longer(-date_raw, names_to = "region_raw", values_to = "rent_raw") %>%
  mutate(
    region  = std_region(region_raw),
    rent    = numify(rent_raw),
    qtr     = quarter_start_hlfs(date_raw)
  ) %>%
  filter(!is.na(region), !is.na(qtr),
         qtr >= as_date("2018-01-01"), qtr <= as_date("2025-12-31")) %>%
  group_by(qtr, region) %>%                                  # avg within quarter (works if monthly or already quarterly)
  summarise(rent = mean(rent, na.rm = TRUE), .groups = "drop") %>%
  mutate(rent = as.integer(round(rent, 0))) %>%
  pivot_wider(names_from = region, values_from = rent) %>%
  arrange(qtr) %>%
  { for (nm in c("Auckland","Canterbury","Wellington")) if (!nm %in% names(.)) .[[nm]] <- NA_integer_; . } %>%
  select(date = qtr, Auckland, Canterbury, Wellington)

# ---------- WRITE ----------
out <- "rent_quarterly_2018_2025.csv"
readr::write_csv(rent_qtr, out)
cat("Wrote:", out,
    "\nRows:", nrow(rent_qtr),
    "\nDate range:", format(min(rent_qtr$date), "%Y-%m-%d"), "→", format(max(rent_qtr$date), "%Y-%m-%d"), "\n")