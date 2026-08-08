# ---------------------------
# Packages
# ---------------------------
library(dplyr)
library(lme4)
library(performance)
library(emmeans)
library(lmtest)
library(sandwich)

# ---------------------------
# Data
# ---------------------------
dataset <- read.csv("datasets/master dataset/master_dataset_clean.csv",
                    stringsAsFactors = FALSE)

# Regions (collapse any U/R flavor into 4 buckets)
dataset$Region <- case_when(
  grepl("auckland",  dataset$Region_Category, ignore.case = TRUE) ~ "Auckland",
  grepl("canterbury",dataset$Region_Category, ignore.case = TRUE) ~ "Canterbury",
  grepl("wellington",dataset$Region_Category, ignore.case = TRUE) ~ "Wellington",
  TRUE ~ "Other"
) |> factor()

# Make sure key fields are numeric
num_vars <- c(
  "log_price", "Income_Level", "Unemployment_Rate",
  "Mortgage_Index", "Deposit_Index", "Rent_Index",
  "Consents_Per_1000", "Year"
)
dataset[num_vars] <- lapply(dataset[num_vars], function(x) suppressWarnings(as.numeric(x)))

# Center Year (helps collinearity)
dataset$Year_c <- dataset$Year - mean(dataset$Year, na.rm = TRUE)

# Lag supply within Region
dataset <- dataset |>
  arrange(Region, Year) |>
  group_by(Region) |>
  mutate(Consents_Per_1000_lag1 = dplyr::lag(Consents_Per_1000, 1)) |>
  ungroup()

# ---------------------------
# Affordability measures
# ---------------------------
# HAI (invert Mortgage_Index if higher = worse affordability)
dataset$HAI <- ifelse(dataset$Mortgage_Index > 0 & is.finite(1 / dataset$Mortgage_Index),
                      1 / dataset$Mortgage_Index, NA_real_)

# Price-to-income (PI) and a log proxy that behaves well in regression
dataset$PI <- exp(dataset$log_price) / dataset$Income_Level
dataset$log_PI <- with(dataset, ifelse(Income_Level > 0, log_price - log(Income_Level), NA_real_))

# COVID period flag
dataset$postCOVID <- ifelse(dataset$Year >= 2020, 1, 0)

# Optional OCR (if present)
has_OCR <- ("OCR" %in% names(dataset)) && any(is.finite(dataset$OCR))
if (has_OCR) dataset$OCR <- suppressWarnings(as.numeric(dataset$OCR))

# Urban / Rural label from Region_Category (keep “Other” as a bucket)
dataset$Urban_Rural <- case_when(
  grepl("Urban", dataset$Region_Category, ignore.case = TRUE) ~ "Urban",
  grepl("Rural", dataset$Region_Category, ignore.case = TRUE) ~ "Rural",
  TRUE ~ "Other"
) |> factor()

# ---------------------------
# PRICE models
# ---------------------------

# Mixed-effects price: random intercept by Region
m1 <- lmer(
  log_price ~ Income_Level + Unemployment_Rate + Mortgage_Index +
    Deposit_Index + Rent_Index + Year_c +
    (1 | Region),
  data = dataset
)
summary(m1)
check_collinearity(m1)

# Region FE price model
m2 <- lm(
  log_price ~ Region + Income_Level + Unemployment_Rate +
    Mortgage_Index + Deposit_Index + Rent_Index + Year_c,
  data = dataset
)
summary(m2)
check_collinearity(m2)
anova(m2)
emmeans(m2, pairwise ~ Region, adjust = "tukey")

# Urban vs Rural price model (drop “Other”)
m3 <- lm(
  log_price ~ Urban_Rural + Income_Level + Unemployment_Rate +
    Mortgage_Index + Deposit_Index + Rent_Index + Year_c,
  data = subset(dataset, Urban_Rural %in% c("Urban", "Rural"))
)
summary(m3)
check_collinearity(m3)
anova(m3)
emmeans(m3, pairwise ~ Urban_Rural, adjust = "tukey")

# ---------------------------
# AFFORDABILITY (HAI) models
# ---------------------------
aff_vars <- c("HAI","Income_Level","Unemployment_Rate",
              "Consents_Per_1000_lag1","Year_c","Region","postCOVID")
if (has_OCR) aff_vars <- c(aff_vars, "OCR")
aff_df <- dataset[complete.cases(dataset[aff_vars]), ]

# Build RHS once; include OCR only if available
rhs <- c("Income_Level","Unemployment_Rate","Consents_Per_1000_lag1","Year_c","postCOVID")
if (has_OCR) rhs <- c(rhs, "OCR")
rhs_str <- paste(rhs, collapse = " + ")

# Mixed-effects HAI (note: singular fit likely with only 4 regions)
m_aff_re <- lmer(as.formula(paste0("HAI ~ ", rhs_str, " + (1 | Region)")), data = aff_df)
summary(m_aff_re)
check_collinearity(m_aff_re)

# Region FE HAI
m_aff_fe <- lm(as.formula(paste0("HAI ~ Region + ", rhs_str)), data = aff_df)
summary(m_aff_fe)
check_collinearity(m_aff_fe)
anova(m_aff_fe)
emmeans(m_aff_fe, pairwise ~ Region, adjust = "tukey")

# Urban vs Rural HAI (drop “Other”)
aff_ur <- subset(aff_df, Urban_Rural %in% c("Urban","Rural"))
if (nrow(aff_ur) > 0) {
  m_aff_ur <- lm(as.formula(paste0("HAI ~ Urban_Rural + ", rhs_str)), data = aff_ur)
  summary(m_aff_ur)
  check_collinearity(m_aff_ur)
  emmeans(m_aff_ur, pairwise ~ Urban_Rural, adjust = "tukey")
}

# ---------------------------
# Robustness: log(Price/Income)
# ---------------------------
pi_vars <- c("log_PI","Income_Level","Unemployment_Rate",
             "Consents_Per_1000_lag1","Year_c","Region","postCOVID")
if (has_OCR) pi_vars <- c(pi_vars, "OCR")
pi_df <- dataset[complete.cases(dataset[pi_vars]), ]

m_pi_fe <- lm(as.formula(paste0("log_PI ~ Region + ", rhs_str)), data = pi_df)
summary(m_pi_fe)
check_collinearity(m_pi_fe)
emmeans(m_pi_fe, pairwise ~ Region, adjust = "tukey")

# ---------------------------
# Robust SEs (HC1) for key FE models
# ---------------------------
coeftest(m2,       vcov = vcovHC(m2,       type = "HC1"))
coeftest(m_aff_fe, vcov = vcovHC(m_aff_fe, type = "HC1"))
coeftest(m_pi_fe,  vcov = vcovHC(m_pi_fe,  type = "HC1"))