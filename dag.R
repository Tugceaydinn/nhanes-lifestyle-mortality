# DAG for the NHANES lifestyle-mortality analysis
# Written BEFORE any outcome analysis. Edit arrows only before the plan-v1 tag.

library(dagitty)

covars    <- c("Age", "Sex", "Race", "Edu", "PIR")   # PIR = income-poverty ratio
exposures <- c("Smoking", "Alcohol", "PA", "BMI")

# 1. Relationships among the covariates
edges <- c(
  "Age -> Edu",      # cohort differences in schooling
  "Age -> PIR",
  "Sex -> PIR",
  "Race -> Edu",     # race/ethnicity = marker of social stratification
  "Race -> PIR",
  "Edu -> PIR"
)

# 2. Every covariate affects every exposure and mortality
for (z in covars) {
  for (x in c(exposures, "Mortality")) {
    edges <- c(edges, paste(z, "->", x))
  }
}

# 3. Exposure-to-exposure arrows
#    No Smoking <-> Alcohol arrow (as stated in the plan).
#    Their association arises through the shared causes above.
edges <- c(edges, "Smoking -> BMI", "Alcohol -> BMI", "PA -> BMI")

# 4. Exposures affect mortality
for (x in exposures) edges <- c(edges, paste(x, "-> Mortality"))

g <- dagitty(paste0("dag { ", paste(edges, collapse = "; "), " }"))

stopifnot(isAcyclic(g))

# Layout
coordinates(g) <- list(
  x = c(Age = 0, Sex = 0, Race = 0, Edu = 1.5, PIR = 3,
        Smoking = 5, Alcohol = 5, PA = 5, BMI = 7, Mortality = 9),
  y = c(Age = 0, Sex = 1, Race = 2, Edu = 3.5, PIR = 3.5,
        Smoking = 0, Alcohol = 1.2, PA = 2.4, BMI = 1.2, Mortality = 1.2)
)

# Save the figure
png("dag.png", width = 2000, height = 1300, res = 200)
plot(g)
dev.off()

# Exposure-specific adjustment sets (total effect on mortality)
sets <- lapply(exposures, function(x)
  adjustmentSets(g, exposure = x, outcome = "Mortality", effect = "total"))
names(sets) <- exposures

print(sets)
capture.output(print(sets), file = "adjustment_sets.txt")