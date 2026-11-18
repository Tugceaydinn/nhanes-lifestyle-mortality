# 10_table1.R
# Weighted descriptive table for the primary (complete-case) analysis population.
# Uses the survey design object built in 09_survey_design.R.

# Unweighted data frame, used only for counts
cc <- analytic %>% filter(complete)

# Weighted % and unweighted n for each level of a factor
tab_var <- function(var) {
  f <- as.formula(paste0("~", var))
  p <- svymean(f, des_cc, na.rm = TRUE)
  data.frame(variable     = var,
             level        = levels(cc[[var]]),
             n            = as.vector(table(cc[[var]])),
             pct_weighted = round(100 * as.vector(coef(p)), 1))
}

vars <- c("sex", "race", "edu", "pir_cat", "smoking", "pa", "alcohol", "bmi_cat")
table1 <- bind_rows(lapply(vars, tab_var))
print(table1, row.names = FALSE)

# Age: weighted mean and SE
svymean(~age, des_cc, na.rm = TRUE)

# Sample size and represented population (millions)
nrow(cc)
sum(weights(des_cc)) / 1e6

# Event counts per exposure category (sparsity check for the models, no effect estimates)
events <- bind_rows(lapply(c("smoking", "pa", "alcohol", "bmi_cat"), function(v)
  data.frame(variable = v,
             level    = levels(cc[[v]]),
             n        = as.vector(table(cc[[v]])),
             deaths   = as.vector(tapply(cc$mortstat, cc[[v]], sum)))))
print(events, row.names = FALSE)

# Save aggregate tables (no individual-level data)
dir.create("results", showWarnings = FALSE)
write.csv(table1, "results/table1.csv", row.names = FALSE)
write.csv(events, "results/event_counts.csv", row.names = FALSE)