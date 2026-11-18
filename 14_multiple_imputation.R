# 14_multiple_imputation.R
# Multiple imputation sensitivity analysis (setup and timing test).
# Complete-case analysis remains the primary analysis.
#
# The imputation model includes:
# - Exposure and covariate variables with missing values
# - Mortality event indicator
# - Nelson-Aalen cumulative hazard
# - NHANES survey design variables and examination weights
#
# Imputation results will be pooled using Rubin's rules.
# Specification is recorded in DEVIATIONS.md before imputation is run.

library(dplyr)
library(survival)
library(survey)
library(mice)

# 1. Prepare the eligible, non-pregnant population ---------------------------

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

# 2. Inspect missingness before imputation ------------------------------------

mi_variables <- c("smoking", "alcohol", "pa", "bmi_cat", "edu", "pir_cat")

cat("\nMI population size:\n")
print(nrow(mi_data))

cat("\nMissing values by variable:\n")
print(colSums(is.na(mi_data[mi_variables])))

cat("\nMissingness percentages:\n")
print(round(100 * colMeans(is.na(mi_data[mi_variables])), 2))

cat("\nMortality events:\n")
print(sum(mi_data$mortstat))

cat("\nUnique participants:\n")
print(n_distinct(mi_data$SEQN))

# 3. Calculate the Nelson-Aalen cumulative hazard -----------------------------

# Intercept-only Cox model on the observed survival outcome.
# The Breslow baseline cumulative hazard is the Nelson-Aalen estimate.

na_fit <- survival::coxph(
  Surv(permth_exm, mortstat) ~ 1,
  data = mi_data,
  ties = "breslow"
)

na_curve <- survival::basehaz(na_fit, centered = FALSE)

# Map the cumulative hazard to each participant's observed follow-up time

na_index <- findInterval(mi_data$permth_exm, na_curve$time)

mi_data$nelson_aalen <- ifelse(
  na_index == 0,
  0,
  na_curve$hazard[pmax(na_index, 1)]
)

# Validate the auxiliary survival variable

cat("\nNelson-Aalen cumulative hazard summary:\n")
print(summary(mi_data$nelson_aalen))

cat("\nMissing cumulative hazard values:\n")
print(sum(is.na(mi_data$nelson_aalen)))

cat("\nZero-follow-up participants:\n")
print(
  mi_data %>%
    filter(permth_exm == 0) %>%
    select(SEQN, permth_exm, mortstat, nelson_aalen)
)

stopifnot(
  sum(is.na(mi_data$nelson_aalen)) == 0,
  all(is.finite(mi_data$nelson_aalen))
)

# 4. Imputation settings, fixed before running (see DEVIATIONS.md) ------------

mi_m     <- 50
mi_maxit <- 10
mi_seed  <- 20261118

# 5. Prepare the multiple imputation model -------------------------------------

# Convert to a standard data frame for mice
mi_data_mice <- as.data.frame(mi_data)

# Treat NHANES design identifiers as categorical predictors
mi_data_mice$SDMVPSU  <- factor(mi_data_mice$SDMVPSU)
mi_data_mice$SDMVSTRA <- factor(mi_data_mice$SDMVSTRA)

# Variables to be imputed
impute_variables <- c("smoking", "alcohol", "pa", "bmi_cat", "edu", "pir_cat")

# Imputation methods: none by default, polytomous regression for the
# unordered/ordered categorical variables with missing values
mi_methods <- mice::make.method(mi_data_mice)
mi_methods[] <- ""
mi_methods[impute_variables] <- "polyreg"

# Predictor matrix
mi_predictors <- mice::make.predictorMatrix(mi_data_mice)

# Participant ID is not a predictor. Follow-up time enters only through the
# Nelson-Aalen estimate, so permth_exm itself is not used (near-collinear with it).
mi_predictors[, c("SEQN", "permth_exm")] <- 0

# Rows of variables that are not imputed are irrelevant, set to 0
mi_predictors["SEQN", ] <- 0

# A variable must not predict its own imputation
diag(mi_predictors) <- 0

# Inspect the methods and predictor matrix
cat("\nImputation methods:\n")
print(mi_methods[impute_variables])

cat("\nPredictors for each imputed variable:\n")
for (v in impute_variables) {
  cat("\n", v, ":\n")
  print(names(which(mi_predictors[v, ] == 1)))
}

# 6. Timing test: one imputation, one iteration --------------------------------
# The full run takes roughly (this time) x mi_m x mi_maxit.

t0 <- Sys.time()

test_imp <- mice::mice(
  mi_data_mice,
  m = 1,
  maxit = 1,
  method = mi_methods,
  predictorMatrix = mi_predictors,
  seed = mi_seed,
  printFlag = FALSE
)

cat("\nElapsed time for m = 1, maxit = 1:\n")
print(Sys.time() - t0)

cat("\nLogged events (NULL expected):\n")
print(test_imp$loggedEvents)

cat("\nRemaining missing values after one imputation (all 0 expected):\n")
print(colSums(is.na(mice::complete(test_imp)[impute_variables])))