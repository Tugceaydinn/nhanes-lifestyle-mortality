
# 12_ph_tests.R
# Proportional hazards checks using exposure-by-log(time) interactions
# Follow-up is split into 6-month episodes.
# The log of each episode midpoint is used as the time interaction term.
#
# Important:
# The 0.5-month shift is retained from the original analysis implementation.
# SEQN is preserved to identify repeated participant records.

library(survival)
library(survey)
library(dplyr)

# 1. Prepare the primary complete-case population

keep <- c(
  "SEQN",
  "mortstat",
  "permth_exm",
  "SDMVPSU",
  "SDMVSTRA",
  "wt",
  "smoking",
  "alcohol",
  "pa",
  "bmi_cat",
  "sex",
  "race",
  "edu",
  "pir_cat",
  paste0("age_s", 1:4)
)

dat <- analytic %>%
  filter(complete) %>%
  select(all_of(keep)) %>%
  mutate(
    tstart = 0,
    tstop = permth_exm + 0.5
  )

# 2. Split follow-up into 6-month episodes

dat_split <- survSplit(
  Surv(tstart, tstop, mortstat) ~ .,
  data = dat,
  cut = seq(6, 156, by = 6),
  episode = "ep"
)

# Calculate the log of each episode midpoint

dat_split$logt <- log(6 * dat_split$ep - 3)

# 3. Validate the split dataset

cat("\nSplit dataset validation\n")

cat("Number of rows:", nrow(dat_split), "\n")
cat("Number of unique participants:",
    length(unique(dat_split$SEQN)), "\n")
cat("Number of episodes:",
    length(unique(dat_split$ep)), "\n")
cat("Number of deaths:", sum(dat_split$mortstat), "\n")
cat("Missing log-time values:", sum(is.na(dat_split$logt)), "\n")

stopifnot(
  nrow(dat_split) == 318469,
  length(unique(dat_split$SEQN)) == 18155,
  sum(dat_split$mortstat) == 2244,
  sum(is.na(dat_split$logt)) == 0
)

# 4. Define the NHANES survey design

des_split <- svydesign(
  id = ~SDMVPSU,
  strata = ~SDMVSTRA,
  weights = ~wt,
  nest = TRUE,
  data = dat_split
)

# 5. Check that splitting follow-up reproduces the original smoking M3 estimate

f_chk <- as.formula(
  paste(
    "Surv(tstart, tstop, mortstat) ~ smoking +",
    m3_terms$smoking
  )
)

chk <- svycoxph(
  f_chk,
  design = des_split
)

original_smoking <- fit_cox(
  "smoking",
  m3_terms$smoking
)

smoking_check <- all.equal(
  unname(coef(chk)),
  unname(coef(original_smoking)),
  tolerance = 1e-4
)

cat("\nSmoking M3 coefficient reproduction check:\n")
print(smoking_check)

# 6. Smoking proportional hazards test
# Reference category: never

f_smoking_ph <- Surv(tstart, tstop, mortstat) ~
  smoking +
  I((smoking == "former") * logt) +
  I((smoking == "current") * logt) +
  age_s1 + age_s2 + age_s3 + age_s4 +
  sex + race + edu + pir_cat

fit_smoking_ph <- svycoxph(
  f_smoking_ph,
  design = des_split
)

ph_test_smoking <- regTermTest(
  fit_smoking_ph,
  ~ I((smoking == "former") * logt) +
    I((smoking == "current") * logt),
  method = "Wald"
)

# 7. Alcohol proportional hazards test
# Reference category: lt12_lifetime

f_alcohol_ph <- Surv(tstart, tstop, mortstat) ~
  alcohol +
  I((alcohol == "former") * logt) +
  I((alcohol == "current_nonheavy") * logt) +
  I((alcohol == "current_heavy") * logt) +
  age_s1 + age_s2 + age_s3 + age_s4 +
  sex + race + edu + pir_cat

fit_alcohol_ph <- svycoxph(
  f_alcohol_ph,
  design = des_split
)

ph_test_alcohol <- regTermTest(
  fit_alcohol_ph,
  ~ I((alcohol == "former") * logt) +
    I((alcohol == "current_nonheavy") * logt) +
    I((alcohol == "current_heavy") * logt),
  method = "Wald"
)

# 8. Physical activity proportional hazards test
# Reference category: sufficient

f_pa_ph <- Surv(tstart, tstop, mortstat) ~
  pa +
  I((pa == "insufficient") * logt) +
  I((pa == "inactive") * logt) +
  age_s1 + age_s2 + age_s3 + age_s4 +
  sex + race + edu + pir_cat

fit_pa_ph <- svycoxph(
  f_pa_ph,
  design = des_split
)

ph_test_pa <- regTermTest(
  fit_pa_ph,
  ~ I((pa == "insufficient") * logt) +
    I((pa == "inactive") * logt),
  method = "Wald"
)

# 9. BMI proportional hazards test
# Reference category: normal
# Adjustment set includes smoking, alcohol and physical activity.

f_bmi_ph <- Surv(tstart, tstop, mortstat) ~
  bmi_cat +
  I((bmi_cat == "underweight") * logt) +
  I((bmi_cat == "overweight") * logt) +
  I((bmi_cat == "obese") * logt) +
  age_s1 + age_s2 + age_s3 + age_s4 +
  sex + race + edu + pir_cat +
  smoking + alcohol + pa

fit_bmi_ph <- svycoxph(
  f_bmi_ph,
  design = des_split
)

ph_test_bmi <- regTermTest(
  fit_bmi_ph,
  ~ I((bmi_cat == "underweight") * logt) +
    I((bmi_cat == "overweight") * logt) +
    I((bmi_cat == "obese") * logt),
  method = "Wald"
)

# 10. Combine the PH test results

extract_ph_result <- function(test, exposure) {
  data.frame(
    exposure = exposure,
    F_statistic = as.numeric(test$Ftest),
    numerator_df = as.numeric(test$df),
    denominator_df = as.numeric(test$ddf),
    p_value = as.numeric(test$p)
  )
}

ph_table <- bind_rows(
  extract_ph_result(ph_test_smoking, "Smoking"),
  extract_ph_result(ph_test_alcohol, "Alcohol"),
  extract_ph_result(ph_test_pa, "Physical activity"),
  extract_ph_result(ph_test_bmi, "BMI")
) %>%
  mutate(
    PH_evidence = if_else(
      p_value < 0.05,
      "Evidence against PH",
      "Insufficient evidence against PH"
    )
  )

cat("\nProportional hazards test results:\n")
print(ph_table)

# 11. Save the PH test results

if (!dir.exists("results")) {
  dir.create("results", recursive = TRUE)
}

write.csv(
  ph_table,
  "results/ph_tests.csv",
  row.names = FALSE
)

cat("\nPH results saved successfully.\n")