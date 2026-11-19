# 17_sensitivity_s1_s2.R
# Pre-specified sensitivity analyses S1 and S2, plus age-specific event counts.
# S1: 24-month landmark analysis (all four exposures, M3 adjustment sets).
# S2: alternative physical activity cut-points (inactive 0, insufficient 1-299,
#     sufficient >= 300 min/week), M3 adjustment set for physical activity.
# Run in the same R session as scripts 01-11 (uses analytic, des, des_cc,
# m3_terms and exposures).

library(dplyr)
library(survival)
library(survey)

stopifnot(exists("analytic"), exists("des"), exists("des_cc"),
          exists("m3_terms"), exists("exposures"))

dir.create("results", showWarnings = FALSE)

# Helper: hazard ratios with t-based 95% CI (design degrees of freedom) 
hr_from_fit <- function(fit, design, exposure, analysis) {
  b    <- coef(fit)
  se   <- sqrt(diag(vcov(fit)))
  crit <- qt(0.975, degf(design))
  idx  <- startsWith(names(b), exposure)
  data.frame(analysis = analysis,
             exposure = exposure,
             term     = names(b)[idx],
             HR       = round(exp(b[idx]), 2),
             lower    = round(exp(b[idx] - crit * se[idx]), 2),
             upper    = round(exp(b[idx] + crit * se[idx]), 2),
             row.names = NULL)
}

# 1. S1: 24-month landmark

# Check: nobody is censored (alive) within the first 24 months (0 expected)
cat("\nAlive and censored within 24 months, complete cases (0 expected):\n")
print(sum(analytic$complete & analytic$mortstat == 0 & analytic$permth_exm <= 24))

# Landmark population: complete cases alive and under observation at 24 months.
# Taken from the design object, as pre-specified.
des_s1 <- subset(des, complete & permth_exm > 24)

cat("\nS1 participants (n) and deaths after the landmark (1883 expected):\n")
print(c(n = sum(analytic$complete & analytic$permth_exm > 24),
        deaths = sum(analytic$complete & analytic$permth_exm > 24 & analytic$mortstat == 1)))
cat("Design degrees of freedom:", degf(des_s1), "\n")

s1 <- bind_rows(lapply(exposures, function(x) {
  f   <- as.formula(paste("Surv(permth_exm - 24, mortstat) ~", x, "+", m3_terms[[x]]))
  fit <- svycoxph(f, design = des_s1)
  hr_from_fit(fit, des_s1, x, "S1 landmark 24 months")
}))

# 2. S2: alternative physical activity cut-points

cat("\nS2 categories (unweighted counts, complete cases):\n")
print(table(analytic$pa_s2[analytic$complete]))

f_s2   <- as.formula(paste("Surv(permth_exm, mortstat) ~ pa_s2 +", m3_terms$pa))
fit_s2 <- svycoxph(f_s2, design = des_cc)
s2     <- hr_from_fit(fit_s2, des_cc, "pa_s2", "S2 PA cut-points 0 / 1-299 / 300+")

# 3. Compare with the primary M3 estimates and save

primary <- read.csv("results/hr_models.csv") %>%
  filter(model == "M3 DAG-adjusted (primary)") %>%
  transmute(analysis = "M3 primary", exposure, term, HR, lower, upper)

sens <- bind_rows(primary, s1, s2)
print(sens, row.names = FALSE)
write.csv(sens, "results/sensitivity_s1_s2.csv", row.names = FALSE)

# 4. Age-specific event counts (complete cases, primary analysis) 

age_events <- analytic %>%
  filter(complete) %>%
  mutate(age_grp = cut(age, c(20, 40, 60, 80, Inf), right = FALSE,
                       labels = c("20-39", "40-59", "60-79", "80+"))) %>%
  group_by(age_grp) %>%
  summarise(n = n(), deaths = sum(mortstat), .groups = "drop")

print(age_events)
write.csv(age_events, "results/event_counts_age.csv", row.names = FALSE)