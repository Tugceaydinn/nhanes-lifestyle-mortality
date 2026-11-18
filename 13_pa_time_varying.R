# 13_pa_time_varying.R
# Five-year time-split analysis for physical activity.
# This analysis is triggered by the physical activity PH test.
#
# Follow-up periods:
# 0 to 5 years
# After 5 years
#
# Reference category: sufficient physical activity.

library(survival)
library(survey)
library(dplyr)

# 1. Check that the objects from the previous analysis are available

stopifnot(
  exists("dat_split"),
  exists("des_split")
)

# 2. Define the five-year follow-up periods

dat_split_period <- dat_split %>%
  mutate(
    period = factor(
      ifelse(
        tstart < 60,
        "0_to_5_years",
        "after_5_years"
      ),
      levels = c("0_to_5_years", "after_5_years")
    )
  )

# 3. Update the NHANES survey design

des_split_period <- update(
  des_split,
  period = factor(
    ifelse(
      tstart < 60,
      "0_to_5_years",
      "after_5_years"
    ),
    levels = c("0_to_5_years", "after_5_years")
  )
)

# 4. Validate the period-specific dataset

cat("\nNumber of rows:\n")
print(nrow(dat_split_period))

cat("\nNumber of unique participants:\n")
print(n_distinct(dat_split_period$SEQN))

cat("\nNumber of deaths:\n")
print(sum(dat_split_period$mortstat))

cat("\nRecords by follow-up period:\n")
print(table(dat_split_period$period))

cat("\nDeaths by follow-up period:\n")
print(
  with(
    dat_split_period,
    table(period, mortstat)
  )
)

# 5. Fit the period-specific physical activity model

f_pa_period_final <- Surv(tstart, tstop, mortstat) ~
  pa +
  I((pa == "insufficient") * (period == "after_5_years")) +
  I((pa == "inactive") * (period == "after_5_years")) +
  age_s1 + age_s2 + age_s3 + age_s4 +
  sex + race + edu + pir_cat

fit_pa_period_final <- svycoxph(
  f_pa_period_final,
  design = des_split_period
)

# 6. Display model results

summary(fit_pa_period_final)


# 7. Calculate period-specific hazard ratios and 95% confidence intervals

# Extract model coefficients and their covariance matrix

b <- coef(fit_pa_period_final)
V <- vcov(fit_pa_period_final)

# Use the survey design degrees of freedom

design_df <- degf(des_split_period)
critical_value <- qt(0.975, df = design_df)

# Calculate a hazard ratio for a linear combination of coefficients

calculate_hr <- function(terms, exposure, period_label) {
  
  L <- setNames(rep(0, length(b)), names(b))
  L[terms] <- 1
  
  log_hr <- sum(L * b)
  se_log_hr <- sqrt(drop(t(L) %*% V %*% L))
  
  data.frame(
    exposure = exposure,
    period = period_label,
    HR = exp(log_hr),
    lower_95 = exp(log_hr - critical_value * se_log_hr),
    upper_95 = exp(log_hr + critical_value * se_log_hr),
    df = design_df
  )
}

# Calculate hazard ratios for both follow-up periods

pa_hr_table <- bind_rows(
  
  # First five years
  calculate_hr(
    "painsufficient",
    "Insufficient",
    "0-5 years"
  ),
  
  calculate_hr(
    "painactive",
    "Inactive",
    "0-5 years"
  ),
  
  # After five years
  calculate_hr(
    c(
      "painsufficient",
      'I((pa == "insufficient") * (period == "after_5_years"))'
    ),
    "Insufficient",
    "After 5 years"
  ),
  
  calculate_hr(
    c(
      "painactive",
      'I((pa == "inactive") * (period == "after_5_years"))'
    ),
    "Inactive",
    "After 5 years"
  )
)

# Display the results

print(pa_hr_table)


# 8. Joint Wald test for period interactions

ph_period_test <- survey::regTermTest(
  fit_pa_period_final,
  ~ I((pa == "insufficient") * (period == "after_5_years")) +
    I((pa == "inactive") * (period == "after_5_years")),
  method = "Wald"
)

print(ph_period_test)

# 9. Save period-specific hazard ratios

if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

write.csv(
  pa_hr_table,
  "results/pa_time_varying.csv",
  row.names = FALSE
)


# 10. Save the joint interaction test

pa_interaction_test <- data.frame(
  test = "Joint Wald test of physical activity period interactions",
  F_statistic = as.numeric(ph_period_test$Ftest),
  numerator_df = as.numeric(ph_period_test$df),
  denominator_df = as.numeric(ph_period_test$ddf),
  p_value = as.numeric(ph_period_test$p)
)

write.csv(
  pa_interaction_test,
  "results/pa_time_varying_interaction_test.csv",
  row.names = FALSE
)

# 11. Save the model coefficients

pa_model_coefficients <- data.frame(
  term = names(coef(fit_pa_period_final)),
  estimate = as.numeric(coef(fit_pa_period_final)),
  standard_error = sqrt(diag(vcov(fit_pa_period_final)))
)

write.csv(
  pa_model_coefficients,
  "results/pa_time_varying_model_coefficients.csv",
  row.names = FALSE
)
