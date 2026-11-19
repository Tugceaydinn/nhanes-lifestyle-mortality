# 16_mi_pooled_m3.R
# MI sensitivity analysis: the primary M3 models are fitted in each of the 50
# imputed datasets and pooled with Rubin's rules (mitools::MIcombine).
# Population: eligible non-pregnant adults (n = 22,385), as specified in DEVIATIONS.md.
# Run in the same R session as scripts 11, 14 and 15 (uses their objects).

library(dplyr)
library(survival)
library(survey)
library(splines)
library(mice)
library(mitools)

stopifnot(
  exists("imp"), exists("impute_variables"), exists("m3_terms"),
  exists("exposures"), exists("age_bk"), exists("age_knots")
)

dir.create("results", showWarnings = FALSE)

# 1. Convergence plot (diagnostic only) -------------------------------------
# Chain means and SDs of the imputed values per iteration. For categorical
# variables mice may not draw useful panels; if the figure is empty, say so.
png("results/mi_convergence.png", width = 2000, height = 1400, res = 200)
try(print(plot(imp, impute_variables)))
dev.off()

# 2. The 50 completed datasets, with the same age spline basis as the primary analysis
imp_list <- mice::complete(imp, "all")

add_age_spline <- function(d) {
  s <- ns(pmin(pmax(d$age, age_bk[1]), age_bk[2]),
          knots = age_knots, Boundary.knots = age_bk)
  for (k in 1:4) d[[paste0("age_s", k)]] <- s[, k]
  d
}
imp_list <- lapply(imp_list, add_age_spline)

cat("\nNumber of datasets (50 expected):", length(imp_list), "\n")
cat("Rows per dataset (22385 expected):", nrow(imp_list[[1]]), "\n")
cat("Deaths per dataset (2879 expected):", sum(imp_list[[1]]$mortstat), "\n")

make_design <- function(d) {
  svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
            nest = TRUE, data = d)
}
df_design <- degf(make_design(imp_list[[1]]))
cat("Design degrees of freedom (64 expected):", df_design, "\n")

# 3. Fit M3 in each imputed dataset and pool -----------------------------------
fit_pool <- function(x) {
  f <- as.formula(paste("Surv(permth_exm, mortstat) ~", x, "+", m3_terms[[x]]))
  fits <- lapply(seq_along(imp_list), function(i) {
    svycoxph(f, design = make_design(imp_list[[i]]))
  })
  pooled <- MIcombine(fits, df.complete = df_design)
  
  b    <- coef(pooled)
  se   <- sqrt(diag(vcov(pooled)))
  idx  <- startsWith(names(b), x)
  crit <- qt(0.975, pooled$df[idx])
  
  data.frame(
    exposure = x,
    term     = names(b)[idx],
    HR_mi    = exp(b[idx]),
    lower_mi = exp(b[idx] - crit * se[idx]),
    upper_mi = exp(b[idx] + crit * se[idx]),
    missinfo = pooled$missinfo[idx],
    row.names = NULL
  )
}

t0 <- Sys.time()
mi_results <- bind_rows(lapply(exposures, function(x) {
  cat("Pooling:", x, "\n")
  fit_pool(x)
}))
cat("\nElapsed time:\n")
print(Sys.time() - t0)

# 4. Compare with the complete-case primary analysis ----------------------------
cc <- read.csv("results/hr_models.csv") %>%
  filter(model == "M3 DAG-adjusted (primary)") %>%
  select(exposure, term, HR_cc = HR, lower_cc = lower, upper_cc = upper)

comparison <- cc %>%
  left_join(mi_results, by = c("exposure", "term")) %>%
  mutate(across(c(HR_mi, lower_mi, upper_mi, missinfo), ~ round(.x, 2)))

print(comparison, row.names = FALSE)

write.csv(comparison, "results/mi_m3_comparison.csv", row.names = FALSE)