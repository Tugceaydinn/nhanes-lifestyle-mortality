
# 14_multiple_imputation.R
# Multiple imputation sensitivity analysis.
# Complete-case analysis remains the primary analysis.
# Imputed datasets will be used for sensitivity analyses.
#
# The imputation model will include:
# - Exposure and covariate variables with missing values
# - Mortality event indicator
# - Nelson-Aalen cumulative hazard
# - NHANES survey design variables and examination weights
#
# Imputation results will be pooled using Rubin's rules.

library(dplyr)
library(survival)
library(survey)
library(mice)

# 1. Prepare the eligible, non-pregnant population

mi_data <- analytic %>%
  filter(eligible) %>%
  select(
    SEQN,
    mortstat,
    permth_exm,
    SDMVPSU,
    SDMVSTRA,
    wt,
    smoking,
    alcohol,
    pa,
    bmi_cat,
    sex,
    race,
    edu,
    pir_cat,
    age
  )

# 2. Inspect missingness before imputation

mi_variables <- c(
  "smoking",
  "alcohol",
  "pa",
  "bmi_cat",
  "edu",
  "pir_cat"
)

cat("\nMI population size:\n")
print(nrow(mi_data))

cat("\nMissing values by variable:\n")
print(colSums(is.na(mi_data[mi_variables])))

cat("\nMissingness percentages:\n")
print(round(
  100 * colMeans(is.na(mi_data[mi_variables])),
  2
))

cat("\nMortality events:\n")
print(sum(mi_data$mortstat))

cat("\nUnique participants:\n")
print(n_distinct(mi_data$SEQN))

# 4. Calculate the Nelson-Aalen cumulative hazard

# Fit an intercept-only Cox model using the observed survival outcome.
# Breslow baseline cumulative hazard provides the Nelson-Aalen estimate.

na_fit <- survival::coxph(
  Surv(permth_exm, mortstat) ~ 1,
  data = mi_data,
  ties = "breslow"
)

na_curve <- survival::basehaz(
  na_fit,
  centered = FALSE
)

# Map the cumulative hazard to each participant's observed follow-up time.

na_index <- findInterval(
  mi_data$permth_exm,
  na_curve$time
)

mi_data$nelson_aalen <- ifelse(
  na_index == 0,
  0,
  na_curve$hazard[pmax(na_index, 1)]
)

# Validate the auxiliary survival variable.

cat("\nNelson-Aalen cumulative hazard summary:\n")
print(summary(mi_data$nelson_aalen))

cat("\nMissing cumulative hazard values:\n")
print(sum(is.na(mi_data$nelson_aalen)))

cat("\nZero-follow-up participant:\n")
print(
  mi_data %>%
    filter(permth_exm == 0) %>%
    select(SEQN, permth_exm, mortstat, nelson_aalen)
)

stopifnot(
  sum(is.na(mi_data$nelson_aalen)) == 0,
  all(is.finite(mi_data$nelson_aalen))
)


# 5. Prepare the multiple imputation model

# Convert to a standard data frame for mice
mi_data_mice <- as.data.frame(mi_data)

# Treat NHANES design identifiers as categorical predictors
mi_data_mice$SDMVPSU <- factor(mi_data_mice$SDMVPSU)
mi_data_mice$SDMVSTRA <- factor(mi_data_mice$SDMVSTRA)

# Define variables to be imputed
impute_variables <- c(
  "smoking",
  "alcohol",
  "pa",
  "bmi_cat",
  "edu",
  "pir_cat"
)

# Initialise imputation methods
mi_methods <- mice::make.method(mi_data_mice)
mi_methods[] <- ""

# Use multinomial logistic regression for unordered categorical variables
mi_methods[impute_variables] <- "polyreg"

# Create the predictor matrix
mi_predictors <- mice::make.predictorMatrix(mi_data_mice)

# Do not use participant ID as an imputation predictor
mi_predictors[, "SEQN"] <- 0

# Do not impute the outcome, follow-up, or fully observed predictors
mi_predictors["SEQN", ] <- 0

# Each variable must not predict its own imputation
diag(mi_predictors) <- 0

# Inspect the methods and predictor matrix
cat("\nImputation methods:\n")
print(mi_methods[impute_variables])

cat("\nPredictors for each imputed variable:\n")

for (v in impute_variables) {
  cat("\n", v, ":\n")
  print(names(which(mi_predictors[v, ] == 1)))
}
