# 18_s3_chronic_disease.R
# Pre-specified sensitivity analysis S3: BMI-mortality analysis after excluding
# participants with baseline chronic disease (cardiovascular disease, cancer,
# COPD-related disease, probable chronic kidney disease).
# Definitions follow analysis_plan.md; implementation decisions are recorded in
# DEVIATIONS.md before this script is run.
# Run in the same R session as scripts 01-11 (uses analytic, m3_terms,
# read_nhanes and cycles).

library(dplyr)
library(survival)
library(survey)

stopifnot(exists("analytic"), exists("m3_terms"), exists("read_nhanes"), exists("cycles"))
dir.create("results", showWarnings = FALSE)

# 1. Read the questionnaire and laboratory files (four cycles each) ---------------

mcq    <- read_nhanes("MCQ")
biopro <- read_nhanes("BIOPRO")
albcr  <- read_nhanes("ALB_CR")

mcq_vars <- c("MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E", "MCQ160F",  # heart failure, CHD, angina, heart attack, stroke
              "MCQ160G", "MCQ160K",                                    # emphysema, chronic bronchitis
              "MCQ220")                                                # cancer or malignancy

stopifnot(all(c("SEQN", mcq_vars) %in% names(mcq)),
          all(c("SEQN", "LBXSCR") %in% names(biopro)),
          all(c("SEQN", "URXUMA", "URXUCR", "URDACT") %in% names(albcr)))

# 2. Urine albumin-creatinine ratio ----------------------------------------------------
# URDACT is not available in the 2007-2008 file. NCHS documents it as
# URDACT = URXUMA / URXUCR x 100 (mg/g). The same formula is applied to the
# 2007-2008 measurements only if it reproduces the released URDACT in the
# other cycles and the components are available in 2007-2008 (rule recorded in
# DEVIATIONS.md). Otherwise S3 is not implemented.

alb <- albcr %>%
  mutate(cycle5    = SEQN %in% analytic$SEQN[analytic$SDDSRVYR == 5],
         uacr_calc = round(100 * URXUMA / URXUCR, 2))

other  <- alb %>% filter(!cycle5, !is.na(URDACT))
max_diff <- max(abs(other$URDACT - other$uacr_calc), na.rm = TRUE)
n_c5       <- sum(alb$cycle5 & !is.na(alb$uacr_calc))
n_c5_total <- sum(alb$cycle5)

cat("\nFormula check in 2009-2014 (n =", nrow(other), "): max |URDACT - calculated| =", max_diff, "\n")
cat("2007-2008: participants with a calculated UACR:", n_c5, "of", n_c5_total, "\n")

if (!is.finite(max_diff) || max_diff > 0.02 || n_c5 < 0.5 * n_c5_total) {
  stop("The UACR formula does not reproduce URDACT in 2009-2014, or the components are ",
       "mostly missing in 2007-2008. S3 cannot be harmonised: do not implement it ",
       "(see the analysis plan) and document the limitation.")
}

alb <- alb %>% mutate(uacr = ifelse(is.na(URDACT), uacr_calc, URDACT))

# 3. Merge ---------------------------------------------------------------------------------

analytic <- analytic %>%
  select(-any_of(c(mcq_vars, "LBXSCR", "URDACT", "URXUMA", "URXUCR", "uacr"))) %>%   # lets the script be re-run
  left_join(select(mcq, SEQN, all_of(mcq_vars)), by = "SEQN") %>%
  left_join(select(biopro, SEQN, LBXSCR), by = "SEQN") %>%
  left_join(select(alb, SEQN, uacr), by = "SEQN")

stopifnot(nrow(analytic) == 39166)

# 4. Harmonisation check: percent missing by cycle (eligible adults) --------------

check_vars <- c(mcq_vars, "LBXSCR", "uacr")

miss_by_cycle <- analytic %>%
  filter(eligible) %>%
  group_by(SDDSRVYR) %>%
  summarise(across(all_of(check_vars), ~ mean(is.na(.x))), .groups = "drop")

cat("\nPercent missing by cycle (eligible adults):\n")
print(miss_by_cycle %>% mutate(across(-SDDSRVYR, ~ round(100 * .x, 1))), width = Inf)

if (any(miss_by_cycle[, -1] > 0.5)) {
  stop("A required variable is mostly missing in at least one cycle: S3 cannot be ",
       "harmonised across the four cycles. Do not use a substitute (see the analysis plan).")
}

# 5. Derive the baseline conditions --------------------------------------------------

# Three-valued status: TRUE if any item is "yes" (1); FALSE if all items are "no" (2);
# NA otherwise (refused, don't know, or not asked).
dom_status <- function(...) {
  m      <- cbind(...)
  anyyes <- rowSums(m == 1, na.rm = TRUE) > 0
  allno  <- rowSums(m == 2, na.rm = TRUE) == ncol(m)
  ifelse(anyyes, TRUE, ifelse(allno, FALSE, NA))
}

# CKD-EPI 2021 race-free creatinine equation (serum creatinine in mg/dL)
egfr_2021 <- function(scr, age, female) {
  kappa <- ifelse(female, 0.7, 0.9)
  alpha <- ifelse(female, -0.241, -0.302)
  142 * pmin(scr / kappa, 1)^alpha * pmax(scr / kappa, 1)^(-1.200) *
    0.9938^age * ifelse(female, 1.012, 1)
}

analytic <- analytic %>%
  mutate(
    cvd    = dom_status(MCQ160B, MCQ160C, MCQ160D, MCQ160E, MCQ160F),
    cancer = dom_status(MCQ220),
    copd   = dom_status(MCQ160G, MCQ160K),
    egfr   = egfr_2021(LBXSCR, age, sex == "female"),
    ckd    = case_when(egfr < 60 | uacr >= 30         ~ TRUE,
                       !is.na(egfr) & !is.na(uacr)    ~ FALSE),
    chronic = cvd | cancer | copd | ckd,    # TRUE if any condition is present
    s3_excl = chronic %in% TRUE             # known to have at least one condition
  )

elig <- analytic %>% filter(eligible)

cat("\neGFR summary (mL/min/1.73 m2):\n")
print(summary(elig$egfr))
cat("\nUACR summary (mg/g):\n")
print(summary(elig$uacr))

# Status counts by condition (unweighted, eligible adults)
conditions <- c("cvd", "cancer", "copd", "ckd", "chronic")
status_counts <- bind_rows(lapply(conditions, function(v) {
  x <- elig[[v]]
  data.frame(condition = v,
             present = sum(x %in% TRUE),
             absent  = sum(x %in% FALSE),
             unknown = sum(is.na(x)))
}))
cat("\nStatus counts (eligible adults, n = 22385):\n")
print(status_counts)
write.csv(status_counts, "results/s3_status_counts.csv", row.names = FALSE)

# 6. Survey design (built on all examined participants, as pre-specified) ----------

des <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
                 nest = TRUE, data = analytic)
des_elig <- subset(des, eligible)

# Weighted prevalence (%) among eligible adults, as a plausibility check on units
prev <- sapply(conditions, function(v) {
  f <- as.formula(paste0("~I(", v, " * 1)"))
  round(100 * as.numeric(coef(svymean(f, des_elig, na.rm = TRUE))), 1)
})
cat("\nWeighted prevalence (%) of each condition, eligible adults:\n")
print(prev)

# 7. S3 population and model ----------------------------------------------------------

des_s3 <- subset(des, complete & !s3_excl)

in_s3 <- analytic$complete & !analytic$s3_excl
cat("\nS3 population: n =", sum(in_s3), "| deaths =", sum(in_s3 & analytic$mortstat == 1), "\n")
cat("Design degrees of freedom:", degf(des_s3), "\n")

cat("\nBMI categories in the S3 population (n and deaths):\n")
print(data.frame(n      = as.vector(table(analytic$bmi_cat[in_s3])),
                 deaths = as.vector(tapply(analytic$mortstat[in_s3], analytic$bmi_cat[in_s3], sum)),
                 row.names = levels(analytic$bmi_cat)))

f_s3   <- as.formula(paste("Surv(permth_exm, mortstat) ~ bmi_cat +", m3_terms$bmi_cat))
fit_s3 <- svycoxph(f_s3, design = des_s3)

b    <- coef(fit_s3)
se   <- sqrt(diag(vcov(fit_s3)))
crit <- qt(0.975, degf(des_s3))
idx  <- startsWith(names(b), "bmi_cat")

s3 <- data.frame(analysis = "S3 excluding baseline chronic disease",
                 exposure = "bmi_cat",
                 term     = names(b)[idx],
                 HR       = round(exp(b[idx]), 2),
                 lower    = round(exp(b[idx] - crit * se[idx]), 2),
                 upper    = round(exp(b[idx] + crit * se[idx]), 2),
                 row.names = NULL)

# 8. Compare with the primary M3 and S1 estimates for BMI and save -------------------

ref <- read.csv("results/sensitivity_s1_s2.csv") %>%
  filter(exposure == "bmi_cat",
         analysis %in% c("M3 primary", "S1 landmark 24 months"))

s3_table <- bind_rows(ref, s3)
print(s3_table, row.names = FALSE)
write.csv(s3_table, "results/s3_bmi.csv", row.names = FALSE)