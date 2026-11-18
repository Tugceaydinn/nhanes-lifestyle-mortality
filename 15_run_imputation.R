# 15_run_imputation.R
# Full multiple imputation run (50 datasets, 10 iterations) as specified in DEVIATIONS.md.
# Run in the same R session, right after 14_multiple_imputation.R
# (it uses the objects created there).
# The imputations run in parallel. The result is saved to data/ (git-ignored,
# because it contains individual-level data).

library(mice)
library(parallel)

stopifnot(
  exists("mi_data_mice"), exists("mi_methods"), exists("mi_predictors"),
  exists("impute_variables"), exists("mi_m"), exists("mi_maxit"), exists("mi_seed")
)

# Number of parallel workers. It must divide mi_m (50) so that exactly 50
# datasets are produced; one logical core is left free for the system.
candidates <- c(10, 5, 2, 1)
n_core     <- max(candidates[candidates <= max(1, detectCores() - 1)])

cat("Logical cores detected:", detectCores(), "| workers used:", n_core, "\n")
cat("Expected run time (hours): about",
    round(1.54 * mi_m * mi_maxit / n_core / 60, 1), "\n")

t0 <- Sys.time()

imp <- mice::parlmice(
  mi_data_mice,
  m = mi_m,
  maxit = mi_maxit,
  method = mi_methods,
  predictorMatrix = mi_predictors,
  n.core = n_core,
  n.imp.core = mi_m / n_core,
  cluster.seed = mi_seed,
  printFlag = FALSE
)

cat("\nElapsed time:\n")
print(Sys.time() - t0)

dir.create("data", showWarnings = FALSE)
saveRDS(imp, "data/imputed_mids.rds")

# Sanity checks ------------------------------------------------------------

cat("\nNumber of imputed datasets (50 expected):\n")
print(imp$m)

cat("\nLogged events (NULL expected):\n")
print(imp$loggedEvents)

first <- mice::complete(imp, 1)

cat("\nRemaining missing values in dataset 1 (all 0 expected):\n")
print(colSums(is.na(first[impute_variables])))

cat("\nImputed alcohol categories in dataset 1:\n")
print(table(first$alcohol))