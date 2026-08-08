
library(tidyverse)
library(lubridate)
library(janitor)

# ---------- helpers ----------
`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x
numify  <- function(x) if (is.numeric(x)) x else readr::parse_number(as.character(x))
fmt2    <- function(x) ifelse(is.na(x), NA_character_, sprintf("%.2f", x))

# HLFS-style quarter start: Mar/Jun/Sep/Dec (stamp as the 1st)
quarter_start_hlfs <- function(d) {
  y <- year(d); m <- month(d)
  qm <- dplyr::case_when(m <= 3 ~ 3L, m <= 6 ~ 6L, m <= 9 ~ 9L, TRUE ~ 12L)
  as.Date(sprintf("%d-%02d-01", y, qm))
}

parse_date_smart <- function(x) {
  d <- suppressWarnings(lubridate::parse_date_time(
    x,
    orders = c("Y-m-d","d/m/Y","m/d/Y","Y/m/d","d-b-Y","b Y","Y-m","m-Y","Y/b"),
    exact = FALSE, tz = "UTC"
  ))
  if (all(is.na(d))) d <- suppressWarnings(readr::parse_datetime(as.character(x), locale = readr::locale(tz="UTC")))
  as_date(d)
}

# ---------- read ----------
src <- "OCR.csv"   
stopifnot(file.exists(src))

raw <- suppressMessages(readr::read_csv(src, show_col_types = FALSE)) %>% clean_names()
nm  <- names(raw)

# pick likely columns (case-insensitive)
pick_col <- function(cands, fallback_contains = NULL) {
  hit <- nm[tolower(nm) %in% tolower(cands)]
  if (length(hit)) return(hit[1])
  if (!is.null(fallback_contains)) {
    hit2 <- nm[str_detect(tolower(nm), tolower(fallback_contains))]
    if (length(hit2)) return(hit2[1])
  }
  NULL
}

date_col <- pick_col(c("date","period","month","time","obs_date"), "date") %||% nm[1]
ocr_col  <- pick_col(c("ocr","official cash rate","offical cash rate","o.c.r"), "ocr")
stopifnot(!is.null(ocr_col))

# ---------- tidy → quarterly ----------
df_q <- tibble(
  date_raw = parse_date_smart(raw[[date_col]]),
  ocr_raw  = numify(raw[[ocr_col]])
) %>%
  filter(!is.na(date_raw)) %>%
  mutate(qtr = quarter_start_hlfs(date_raw)) %>%
  filter(qtr >= as_date("2018-01-01"), qtr <= as_date("2025-12-31")) %>%
  group_by(qtr) %>%
  summarise(ocr = mean(ocr_raw, na.rm = TRUE), .groups = "drop") %>%
  arrange(qtr) %>%
  mutate(ocr = round(ocr, 2),
         ocr = fmt2(ocr)) %>%             # keep exactly 2 decimals in file
  select(date = qtr, ocr)

# sanity check: non-empty and quarterly sequence
stopifnot(nrow(df_q) > 0)

# ---------- write ----------
out_file <- "ocr_quarterly_2018_2025.csv"
readr::write_csv(df_q, out_file)

cat("Wrote:", out_file, "\nRows:", nrow(df_q),
    "\nDate range:", format(min(df_q$date), "%Y-%m-%d"), "→", format(max(df_q$date), "%Y-%m-%d"), "\n")