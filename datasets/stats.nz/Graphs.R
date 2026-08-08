library(tidyverse)
library(lubridate)
library(janitor)

dir.create("figs_plain", showWarnings = FALSE, recursive = TRUE)

# ---- load ----
master_fp <- "master_quarterly_2018_2025_fullnames.csv"
stopifnot(file.exists(master_fp))

df <- readr::read_csv(master_fp, show_col_types = FALSE) |>
  clean_names() |>
  mutate(date = as_date(date))

pick_first <- function(df, cands) {
  nm_low <- tolower(names(df))
  idx <- match(tolower(cands), nm_low)
  idx <- idx[!is.na(idx)]
  if (length(idx) == 0) return(NULL)
  names(df)[idx[1]]
}

# columns (supports short/full-name variants)
ocr_col <- pick_first(df, c("official_cash_rate","ocr"))

rent_cols <- list(
  Auckland   = pick_first(df, c("rent_auckland","rent_akl")),
  Canterbury = pick_first(df, c("rent_canterbury","rent_cant")),
  Wellington = pick_first(df, c("rent_wellington","rent_wlg"))
)

yoy_cols <- list(
  Auckland   = pick_first(df, c("rent_yoy_auckland","rent_yoy_akl")),
  Canterbury = pick_first(df, c("rent_yoy_canterbury","rent_yoy_cant")),
  Wellington = pick_first(df, c("rent_yoy_wellington","rent_yoy_wlg"))
)

cons1000_cols <- list(
  Auckland   = pick_first(df, c("consents_per_1000_auckland","cons_per_1000_akl")),
  Canterbury = pick_first(df, c("consents_per_1000_canterbury","cons_per_1000_cant")),
  Wellington = pick_first(df, c("consents_per_1000_wellington","cons_per_1000_wlg"))
)

# Fallback: compute YoY if not present
yoy_calc <- function(x, k = 4) (x - dplyr::lag(x, k)) / dplyr::lag(x, k) * 100
for (nm in names(yoy_cols)) {
  if (is.null(yoy_cols[[nm]]) && !is.null(rent_cols[[nm]])) {
    new_nm <- paste0("rent_yoy_", tolower(nm))
    df[[new_nm]] <- yoy_calc(df[[rent_cols[[nm]]]])
    yoy_cols[[nm]] <- new_nm
  }
}

# Fallback: consents per 1,000 from consents & population if needed
ensure_cpk <- function(region) {
  if (is.null(cons1000_cols[[region]])) {
    c_col <- pick_first(df, paste0(c("consents_", "cons_"), tolower(region)))
    p_col <- pick_first(df, paste0(c("population_", "pop_"), tolower(region)))
    if (!is.null(c_col) && !is.null(p_col)) {
      new_nm <- paste0("consents_per_1000_", tolower(region))
      df[[new_nm]] <- 1000 * df[[c_col]] / df[[p_col]]
      cons1000_cols[[region]] <- new_nm
    }
  }
}
invisible(lapply(names(cons1000_cols), ensure_cpk))

# ---- shared theme with grid lines (plain) ----
plain_grid_theme <- function() {
  theme_minimal(base_size = 12) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background  = element_rect(fill = "white", color = NA),
      panel.grid.major = element_line(linewidth = 0.3),
      panel.grid.minor = element_blank(),
      legend.background = element_rect(fill = "white", color = NA)
    )
}

# ---- 1) Median weekly rent (plain + grid) ----
rent_long <- tibble(date = df$date)
for (nm in names(rent_cols)) rent_long[[nm]] <- df[[rent_cols[[nm]]]]
rent_long <- rent_long |> pivot_longer(-date, names_to = "region", values_to = "rent")

p1 <- ggplot(rent_long, aes(date, rent, color = region)) +
  geom_line(linewidth = 0.9) +
  labs(title = "Median Weekly Rent (Quarterly)", x = NULL, y = "NZD per week") +
  plain_grid_theme()

ggsave("figs_plain/rent_level_qtr_plain.png", p1, width = 9, height = 5, dpi = 200)

# ---- 2) Rent YoY % (plain + grid) ----
yoy_long <- tibble(date = df$date)
for (nm in names(yoy_cols)) yoy_long[[nm]] <- df[[yoy_cols[[nm]]]]
yoy_long <- yoy_long |> pivot_longer(-date, names_to = "region", values_to = "yoy")

p2 <- ggplot(yoy_long, aes(date, yoy, color = region)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_line(linewidth = 0.9) +
  labs(title = "Rent YoY Change (Quarterly)", x = NULL, y = "Percent YoY") +
  plain_grid_theme()

ggsave("figs_plain/rent_yoy_qtr_plain.png", p2, width = 9, height = 5, dpi = 200)

# ---- 3) Consents per 1,000 population (plain + grid) ----
cons_long <- tibble(date = df$date)
for (nm in names(cons1000_cols)) cons_long[[nm]] <- df[[cons1000_cols[[nm]]]]
cons_long <- cons_long |> pivot_longer(-date, names_to = "region", values_to = "per_1000")

p3 <- ggplot(cons_long, aes(date, per_1000, color = region)) +
  geom_line(linewidth = 0.9) +
  labs(title = "New Dwellings Consented per 1,000 Population",
       x = NULL, y = "Per 1,000 residents") +
  plain_grid_theme()

ggsave("figs_plain/consents_per_1000_qtr_plain.png", p3, width = 9, height = 5, dpi = 200)

# ---- 4) OCR (plain + grid) ----
if (!is.null(ocr_col)) {
  ocr_vals <- suppressWarnings(as.numeric(df[[ocr_col]]))
  p4 <- ggplot(df, aes(date, ocr_vals)) +
    geom_line(linewidth = 0.9) +
    labs(title = "Official Cash Rate (Quarterly)", x = NULL, y =  "Percent") +
    plain_grid_theme()
  ggsave("figs_plain/ocr_qtr_plain.png", p4, width = 9, height = 5, dpi = 200)
}