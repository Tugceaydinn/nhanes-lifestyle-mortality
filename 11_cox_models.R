# 11_cox_models.R
# Primary analysis as pre-specified in analysis_plan.md (plan-v1):
# M1 crude, M2 age + sex, M3 DAG-based adjustment (primary).
# Each exposure is interpreted from its own model only.

library(splines)
library(survival)
library(survey)

# 1. Natural cubic spline basis for age (4 df) 
# Knots are fixed from the primary complete-case population so that rows outside
# the analysis population (children, ineligible) cannot influence the basis.
# Their ages are clamped to the boundary; they carry zero weight anyway.
age_cc    <- analytic$age[analytic$complete]
age_bk    <- range(age_cc)
age_knots <- quantile(age_cc, c(0.25, 0.5, 0.75))
age_mat   <- matrix(ns(pmin(pmax(analytic$age, age_bk[1]), age_bk[2]),
                       knots = age_knots, Boundary.knots = age_bk),
                    nrow = nrow(analytic))
for (k in 1:4) analytic[[paste0("age_s", k)]] <- age_mat[, k]

# 2. Rebuild the design with the spline columns 
# The design is built on ALL examined participants; the analysis population is
# then taken with subset(), as pre-specified.
des    <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
                    nest = TRUE, data = analytic)
des_cc <- subset(des, complete)

# 3. Model terms (from adjustment_sets.txt) 
age_terms <- paste0("age_s", 1:4, collapse = " + ")
core      <- paste(age_terms, "+ sex + race + edu + pir_cat")

m2_terms <- paste(age_terms, "+ sex")
m3_terms <- list(
  smoking = core,
  alcohol = core,
  pa      = core,
  bmi_cat = paste(core, "+ smoking + alcohol + pa")  # BMI set also contains the other lifestyle factors
)

# 4. Helper functions 
fit_cox <- function(exposure, covars = NULL) {
  rhs <- if (is.null(covars)) exposure else paste(exposure, "+", covars)
  f   <- as.formula(paste("Surv(permth_exm, mortstat) ~", rhs))
  svycoxph(f, design = des_cc)
}

extract_hr <- function(fit, exposure, model) {
  b    <- coef(fit)
  se   <- sqrt(diag(vcov(fit)))
  crit <- qt(0.975, degf(des_cc))       # t quantile with design degrees of freedom
  idx  <- startsWith(names(b), exposure)
  data.frame(model = model, exposure = exposure, term = names(b)[idx],
             HR    = exp(b[idx]),
             lower = exp(b[idx] - crit * se[idx]),
             upper = exp(b[idx] + crit * se[idx]),
             row.names = NULL)
}

# 5. Fit M1-M3 for each exposure 
exposures <- c("smoking", "alcohol", "pa", "bmi_cat")

hr_table <- bind_rows(lapply(exposures, function(x) {
  bind_rows(
    extract_hr(fit_cox(x),                x, "M1 crude"),
    extract_hr(fit_cox(x, m2_terms),      x, "M2 age+sex"),
    extract_hr(fit_cox(x, m3_terms[[x]]), x, "M3 DAG-adjusted (primary)")
  )
}))

hr_table[, c("HR", "lower", "upper")] <- round(hr_table[, c("HR", "lower", "upper")], 2)
print(hr_table, row.names = FALSE)

write.csv(hr_table, "results/hr_models.csv", row.names = FALSE)