# Wide unemployment: date, Auckland, Christchurch, Wellington
library(readr); library(dplyr); library(tidyr)
library(janitor); library(stringr); library(lubridate)

in_dir  <- "/Volumes/arjun/Data309/DATA-309-Project/datasets/figure.nz"
csvs    <- list.files(in_dir, pattern="\\.csv$", full.names=TRUE)
stopifnot(length(csvs) >= 1)
in_file <- csvs[1]
out_wide <- file.path(in_dir, "unemployment_wide.csv")

pick_first <- function(cands, nms) { h <- intersect(cands, nms); if (length(h)) h[1] else NA_character_ }
make_date_auto <- function(df) {
  nms <- names(df)
  if ("date" %in% nms) return(ymd(df$date, quiet=TRUE))
  if ("quarter" %in% nms) {
    q  <- str_replace_all(as.character(df$quarter), "\\s+", "")
    yr <- suppressWarnings(as.integer(str_sub(q, 1, 4)))
    qn <- suppressWarnings(as.integer(str_extract(q, "(?<=Q)\\d")))
    mm <- c(`1`=1, `2`=4, `3`=7, `4`=10)[as.character(qn)]
    return(make_date(year=yr, month=mm, day=1))
  }
  if ("period" %in% nms)  return(ymd(paste0(str_replace_all(df$period, "[/]", "-"), "-01"), quiet=TRUE))
  if (all(c("year","month") %in% nms)) {
    yr <- suppressWarnings(as.integer(df$year))
    mn <- suppressWarnings(as.integer(df$month))
    if (any(is.na(mn))) mn <- match(tolower(as.character(df$month)), tolower(month.name))
    return(make_date(year=yr, month=mn, day=1))
  }
  if ("year" %in% nms) return(make_date(year=suppressWarnings(as.integer(df$year)), month=1, day=1))
  stop("No usable date/quarter/period/(year+month)/year column found.")
}
norm_region3 <- function(x) {
  s <- tolower(str_trim(as.character(x)))
  case_when(
    str_detect(s, "auck")               ~ "Auckland",
    str_detect(s, "christ|canter|chch") ~ "Christchurch",
    str_detect(s, "welling")            ~ "Wellington",
    TRUE ~ NA_character_
  )
}

raw <- read_csv(in_file, show_col_types=FALSE) |> clean_names()
nms <- names(raw)
region_col <- pick_first(c("region","region_name","location","geography",
                           "territorial_authority","ta","area","city","place"), nms)
status_col <- pick_first(c("labour_force_status","status","measure","series","indicator"), nms)  # optional
gender_col <- pick_first(c("sex","gender"), nms)                                                 # optional
value_col  <- pick_first(c("unemployment_rate","unemployment_rate_percent",
                           "rate","percent","percentage","value","values","estimate"), nms)
stopifnot(!is.na(region_col), !is.na(value_col))

date_vec <- make_date_auto(raw)
df <- tibble(
  date   = date_vec,
  region = norm_region3(raw[[region_col]]),
  status = if (!is.na(status_col)) as.character(raw[[status_col]]) else NA_character_,
  gender = if (!is.na(gender_col)) as.character(raw[[gender_col]]) else NA_character_,
  value  = readr::parse_number(as.character(raw[[value_col]]))
) |>
  filter(!is.na(date),
         date >= ymd("2015-01-01"), date <= ymd("2025-12-31"),
         region %in% c("Auckland","Christchurch","Wellington"))

# Keep only "Unemployment rate" if such a column exists
if (!all(is.na(df$status))) {
  df <- df |> filter(str_detect(str_to_lower(status), "unemployment\\s*rate"))
}
# Prefer Total/All gender if present
if (!all(is.na(df$gender))) {
  has_total <- str_detect(str_to_lower(df$gender), "total|all")
  if (any(has_total, na.rm=TRUE)) df <- df |> filter(is.na(gender) | str_detect(str_to_lower(gender), "total|all"))
}

wide <- df |>
  group_by(date, region) |>
  summarise(unemployment_rate = mean(value, na.rm=TRUE), .groups="drop") |>
  pivot_wider(names_from = region, values_from = unemployment_rate) |>
  arrange(date)

# Ensure all three columns exist (do this OUTSIDE mutate)
wanted <- c("Auckland","Christchurch","Wellington")
for (nm in wanted) if (!nm %in% names(wide)) wide[[nm]] <- NA_real_

wide <- wide |> select(date, all_of(wanted))

write_csv(wide, out_wide)
message("✅ wrote: ", out_wide)