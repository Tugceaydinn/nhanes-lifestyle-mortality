# NHANES lifestyle factors and all-cause mortality

Are smoking, physical activity, alcohol consumption and body mass index associated with all-cause mortality among US adults aged 20 and over? This project uses NHANES 2007-2014 linked to the NCHS public-use mortality files (follow-up to 31 December 2019). It is a methods-focused portfolio project: the analysis was pre-specified, and every decision made after the plan is recorded.

## Pre-specification

The analysis plan, the DAG and the exposure-specific adjustment sets were committed and tagged `plan-v1` before any outcome data were accessed. Everything that came afterwards, including operational definitions, a corrected implementation and decisions triggered by diagnostics, is documented with dates in [`DEVIATIONS.md`](DEVIATIONS.md).

## Data

- NHANES 2007-2008, 2009-2010, 2011-2012 and 2013-2014 (demographics, examination, questionnaire and laboratory files).
- NCHS 2019 Public-Use Linked Mortality Files.

The scripts download the public files themselves (to `data/raw`, which is not tracked in this repository). No individual-level data are stored here; `results/` contains aggregated tables only.

## Methods in brief

- Population: adults aged 20+ with `ELIGSTAT = 1`, not pregnant (22,385); primary analysis on complete cases (18,155 adults, 2,244 deaths, representing 183.6 million US adults).
- Survey design: examination weights (divided by the number of cycles), strata and PSUs. In the primary models, subpopulations are taken from the design object built on all examined participants; the exceptions are listed in `DEVIATIONS.md`.
- Models: survey-weighted Cox models with time since examination as time scale. M1 crude, M2 age and sex, M3 adjusted for the exposure-specific set from the DAG (primary). Age as a natural cubic spline with 4 df. Mediators are not adjusted for.
- Proportional hazards: exposure-by-log-time interactions with design-based Wald tests; a pre-specified five-year split was triggered for physical activity.
- Sensitivity analyses: S1 24-month landmark, S2 alternative physical activity cut-points, S3 exclusion of baseline chronic disease (BMI), and multiple imputation (50 datasets) for the pooled M3 models.

## Main results (primary M3 model, complete cases)

Hazard ratios with 95% confidence intervals. Each exposure is interpreted only from its own model, under the assumptions of the DAG.

| Exposure (reference) | Category | HR (95% CI) |
|---|---|---|
| Smoking (never) | Former | 1.25 (1.08-1.44) |
| | Current | 2.23 (1.94-2.57) |
| Alcohol (fewer than 12 lifetime drinks) | Former drinker | 1.40 (1.15-1.70) |
| | Current non-heavy | 0.83 (0.69-1.00) |
| | Current heavy | 1.26 (0.92-1.71) |
| Physical activity (150+ min/week) | Insufficient | 1.29 (1.10-1.52) |
| | Inactive | 1.85 (1.63-2.10) |
| BMI (18.5-24.9) | Underweight | 1.97 (1.41-2.75) |
| | Overweight | 0.82 (0.72-0.94) |
| | Obese | 0.98 (0.86-1.11) |

Sensitivity analyses (tables in `results/`):

- Multiple imputation (n = 22,385) gave similar estimates; the physical activity estimate for inactive adults was 2.03 (1.80-2.28) and for current non-heavy drinkers 0.79 (0.67-0.93).
- S1 (24-month landmark) and S2 (cut-points at 300 min/week) gave estimates close to the primary ones; the inactive group stayed at 1.83 under S2.
- S3 (BMI, n = 12,239, 608 deaths): underweight 2.49 (1.26-4.94), overweight 0.96 (0.73-1.26), obese 1.05 (0.78-1.42). The lower hazard ratio for overweight moved towards 1 with wide intervals.
- Physical activity: the proportional hazards test gave p = 0.047. Period-specific hazard ratios for inactive adults were 2.07 (0-5 years) and 1.66 (after 5 years); the interaction test was not statistically significant (p = 0.21).

## Reproducing the analysis

1. Open `nhanes-lifestyle-mortality.Rproj` in RStudio.
2. Start a fresh R session and run `source("R/00_run_all.R")`. Set `RUN_IMPUTATION <- FALSE` to reuse saved imputations.

The full run downloads the public NHANES and NCHS files (see `data/raw` for the size); the imputation step took about 1.5 hours on 10 workers. The log is saved in `results/run_log.txt` and the package versions in `results/session_info.txt`.

## Repository structure

- `analysis_plan.md`: pre-specified plan (tag `plan-v1`)
- `DEVIATIONS.md`: dated record of all decisions made after the plan
- `dag.R`, `dag.png`, `adjustment_sets.txt`: DAG and exposure-specific adjustment sets
- `R/`: analysis scripts, run in numerical order by `R/00_run_all.R`
- `results/`: aggregated result tables and figures
- `STROBE_checklist.md`: STROBE items and where they are addressed

## Limitations

- Lifestyle factors are self-reported (BMI is measured) and measured once at baseline; alcohol categories reflect average weekly intake and smoking covers cigarettes only.
- Age is top-coded at 80 years, and the public-use mortality files are perturbed by NCHS to reduce re-identification risk.
- Complete-case selection and linkage ineligibility are not corrected in the weights; multiple imputation assumes data missing at random.
- Residual confounding, reverse causation and the DAG assumptions (for example, no direct arrow between smoking and alcohol) cannot be excluded. The underweight and heavy drinking categories have few events.

## Status

The analysis is complete; a manuscript-style write-up is in preparation.
