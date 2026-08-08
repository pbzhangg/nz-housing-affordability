
library(tidyverse)
library(lubridate)
library(janitor)

# ---- helpers ----
std_region_wide <- function(df) {
  nm <- names(df)
  tolow <- tolower(nm)
  nm[tolow == "date"]        <- "date"
  nm[tolow == "auckland"]    <- "Auckland"
  nm[tolow == "wellington"]  <- "Wellington"
  # map any christchurch -> Canterbury (your rule)
  nm[tolow %in% c("christchurch","canterbury")] <- "Canterbury"
  names(df) <- nm
  df
}
yoY <- function(x, k = 4) (x - dplyr::lag(x, k)) / dplyr::lag(x, k) * 100

# ---- read inputs ----
ocr <- readr::read_csv("ocr_quarterly_2018_2025.csv", show_col_types = FALSE) |>
  clean_names() |>
  transmute(date = as_date(date),
            official_cash_rate = as.numeric(ocr)) |>
  filter(year(date) >= 2018, year(date) <= 2025)

consents <- readr::read_csv("consents_quarterly_2018_2025.csv", show_col_types = FALSE) |>
  clean_names() |> std_region_wide() |>
  transmute(date = as_date(date),
            consents_auckland   = as.integer(round(Auckland,   0)),
            consents_canterbury = as.integer(round(Canterbury, 0)),
            consents_wellington = as.integer(round(Wellington, 0))) |>
  filter(year(date) >= 2018, year(date) <= 2025)

population <- readr::read_csv("population_quarterly_2018_2025.csv", show_col_types = FALSE) |>
  clean_names() |> std_region_wide() |>
  transmute(date = as_date(date),
            population_auckland   = as.integer(round(Auckland,   0)),
            population_canterbury = as.integer(round(Canterbury, 0)),
            population_wellington = as.integer(round(Wellington, 0))) |>
  filter(year(date) >= 2018, year(date) <= 2025)

rent <- readr::read_csv("rent_quarterly_2018_2025.csv", show_col_types = FALSE) |>
  clean_names() |> std_region_wide() |>
  transmute(date = as_date(date),
            rent_auckland   = as.integer(round(Auckland,   0)),
            rent_canterbury = as.integer(round(Canterbury, 0)),
            rent_wellington = as.integer(round(Wellington, 0))) |>
  filter(year(date) >= 2018, year(date) <= 2025)

# ---- merge (inner to keep aligned quarter stamps) ----
master <- rent |>
  inner_join(ocr,        by = "date") |>
  inner_join(consents,   by = "date") |>
  inner_join(population, by = "date") |>
  arrange(date)

# ---- derived metrics ----
master <- master |>
  mutate(
    # YoY % vs same quarter last year
    rent_yoy_auckland   = round(yoY(rent_auckland),   1),
    rent_yoy_canterbury = round(yoY(rent_canterbury), 1),
    rent_yoy_wellington = round(yoY(rent_wellington), 1)
  )

# Rent index (first non-NA = 100) per region
base_akl  <- master$rent_auckland  [which(!is.na(master$rent_auckland ))[1]]
base_cant <- master$rent_canterbury[which(!is.na(master$rent_canterbury))[1]]
base_wlg  <- master$rent_wellington[which(!is.na(master$rent_wellington))[1]]

master <- master |>
  mutate(
    rent_index_auckland   = if (!is.na(base_akl)  && base_akl  != 0) round(100 * rent_auckland   / base_akl,  1) else NA_real_,
    rent_index_canterbury = if (!is.na(base_cant) && base_cant != 0) round(100 * rent_canterbury / base_cant, 1) else NA_real_,
    rent_index_wellington = if (!is.na(base_wlg)  && base_wlg  != 0) round(100 * rent_wellington / base_wlg,  1) else NA_real_,
    consents_per_1000_auckland   = round(1000 * consents_auckland   / population_auckland,   2),
    consents_per_1000_canterbury = round(1000 * consents_canterbury / population_canterbury, 2),
    consents_per_1000_wellington = round(1000 * consents_wellington / population_wellington, 2)
  )

# ---- final ordering; keep OCR as exactly 2 decimals IN THE FILE ----
master_out <- master |>
  mutate(official_cash_rate = sprintf("%.2f", official_cash_rate)) |>
  select(
    date, official_cash_rate,
    rent_auckland,   rent_canterbury,   rent_wellington,
    consents_auckland, consents_canterbury, consents_wellington,
    population_auckland, population_canterbury, population_wellington,
    rent_yoy_auckland, rent_yoy_canterbury, rent_yoy_wellington,
    rent_index_auckland, rent_index_canterbury, rent_index_wellington,
    consents_per_1000_auckland, consents_per_1000_canterbury, consents_per_1000_wellington
  )

readr::write_csv(master_out, "master_quarterly_2018_2025.csv")

cat("Wrote: master_quarterly_2018_2025_fullnames.csv\n",
    "Rows: ", nrow(master_out),
    "\nDate range: ", format(min(master_out$date), "%Y-%m-%d"),
    " → ", format(max(master_out$date), "%Y-%m-%d"), "\n", sep = "")