# Analysis plan (written before outcome analysis)

## Research question
Among US adults aged 20 and over, are smoking status, physical activity,
alcohol consumption and body mass index associated with all-cause
mortality, after accounting for confounders identified in a DAG?

## Data
NHANES 2007-08, 2009-10, 2011-12, 2013-14 with the NCHS public-use
linked mortality files (follow-up to 31 Dec 2019).

## Population
Adults aged 20+ at examination, eligible for mortality linkage.
Exclude pregnant women and participants missing the exposures or
covariates (complete-case analysis is primary).

## Exposures (definitions fixed now; variable names confirmed on Day 2)
- Smoking: never / former / current
- Physical activity: inactive (0 min/week) / insufficient (1-149) /
  sufficient (150+ min/week of moderate-equivalent activity)
- Alcohol: non-drinker / light-to-moderate / heavy
- BMI (measured): <18.5 / 18.5-24.9 (reference) / 25-29.9 / 30+

## Outcome
All-cause mortality; time = months from examination to death or
31 Dec 2019 (censored).

## Confounders
Chosen from the DAG (see dag.png): age, sex, education,
income-poverty ratio, and race/ethnicity as a marker of social
exposure. Adjustment sets are exposure-specific. Mediators (for
example hypertension, diabetes, and BMI when physical activity is the
exposure) are NOT adjusted for in the primary models.

## Survey design
Examination weights divided by the number of cycles, with strata and
PSU variables. Subset the design object, never the raw data.

## Models
M1 crude. M2 age and sex adjusted. M3 DAG-based adjustment (primary).
Survey-weighted Cox models; proportional hazards checked with
Schoenfeld residuals.

## Sensitivity analyses (only these two)
S1 exclude deaths in the first two years of follow-up.
S2 alternative physical activity cut-points.

## Missing data
Complete-case primary. Multiple imputation only if over 10% missing.

## Reporting
STROBE checklist. Any change after seeing results is recorded in
DEVIATIONS.md with the reason.
