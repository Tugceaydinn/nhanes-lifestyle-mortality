# Analysis plan (version 1, written before any outcome analysis)

Date: 17 November 2026

## Research question

Among US adults aged 20 years and older, are smoking status, physical activity, alcohol consumption and body mass index associated with all-cause mortality, after accounting for confounders identified using exposure-specific directed acyclic graphs (DAGs)?

## Data

NHANES 2007-2008, 2009-2010, 2011-2012 and 2013-2014, linked to the NCHS public-use Linked Mortality Files (follow-up through 31 December 2019).

Variable names and questionnaire routing described below follow the NHANES documentation. They will be verified against the codebooks of all four cycles before any analysis. Any discrepancy will be recorded in `DEVIATIONS.md`.

## Population

Adults aged 20 years and older at the Mobile Examination Center (MEC) examination who are eligible for public-use mortality linkage (`ELIGSTAT = 1`).

Pregnant participants will be excluded where pregnancy status can be established. Pregnancy status will not be treated as missing for participants outside the variable's eligible population.

The broad adult age range is retained to address the research question across the adult US population. Lower event counts among younger adults will be acknowledged, and age-specific event counts and uncertainty will be reported.

## Exposures

### Smoking (cigarettes only)

- Never established smoker: `SMQ020 = 2`.
- Former smoker: `SMQ020 = 1` and `SMQ040 = 3`.
- Current smoker: `SMQ020 = 1` and `SMQ040 = 1 or 2`.

Other tobacco products are not classified. Refused, unknown or genuinely missing responses are treated as missing. Skip patterns are distinguished from item non-response.

### Physical activity

Weekly moderate-equivalent minutes are calculated across work, transportation and leisure-time domains. For each domain, weekly minutes = days per week x minutes per active day.

MVPA-equivalent minutes = moderate work + moderate transportation + moderate leisure + 2 x (vigorous work + vigorous leisure).

Primary categories:
- Inactive: 0 minutes/week.
- Insufficient: 1-149 minutes/week.
- Sufficient: 150 or more minutes/week.

### Alcohol consumption

Four mutually exclusive categories:

1. Fewer than 12 lifetime drinks: `ALQ101 = 2` and `ALQ110 = 2`.
2. Former drinker: evidence of at least 12 lifetime drinks, with zero drinking days in the previous 12 months.
3. Current non-heavy drinker: current drinking below the sex-specific heavy threshold.
4. Current heavy drinker: average of 8 or more drinks/week for women, or 15 or more drinks/week for men.

At least 12 lifetime drinks is identified by `ALQ101 = 1`, or by `ALQ101 = 2` and `ALQ110 = 1`.

When `ALQ101 = 1`, `ALQ110` is structurally missing because the questionnaire routes directly to `ALQ120Q`. This is not treated as item non-response. Participants with `ALQ101 = 2` and `ALQ110 = 2` are routed out of the current-consumption questions and are classified from their lifetime responses.

For current drinkers, average weekly consumption is calculated from `ALQ120Q`, `ALQ120U` and `ALQ130`: frequency is converted to days per week and multiplied by average drinks per drinking day.

This classification reflects average weekly consumption and does not fully capture binge or daily heavy drinking.

### Body mass index

Measured BMI (`BMXBMI`): underweight below 18.5; normal weight 18.5-24.9; overweight 25.0-29.9; obesity 30.0 or above (kg/m2).

### Reference categories

- Smoking: never established smoker.
- Physical activity: sufficient (150 or more minutes/week).
- Alcohol: fewer than 12 lifetime drinks. Former drinkers remain a separate category and are not combined with this group.
- BMI: normal weight.

## Outcome and follow-up

All-cause mortality is identified by `MORTSTAT`. Follow-up time is measured in months from the MEC examination using `PERMTH_EXM`. Participants assumed alive are censored at 31 December 2019.

### Time scale

Time since MEC examination is the primary time scale. Age at examination is included as a covariate, modelled with a natural cubic spline with four degrees of freedom. This avoids relying on top-coded attained-age values for participants aged 80 years and older.

## Survey design

The analysis accounts for NHANES strata and primary sampling units. The MEC examination weight is used because the analysis includes measured BMI and examination-based variables.

Combined weight for the four cycles: `WTMEC_COMBINED = WTMEC2YR / 4`.

The survey design object is constructed before subsetting. The analytic population is selected by subsetting the design object, never by filtering the raw data before design specification.

## Confounding and DAG-based adjustment

Exposure-specific adjustment sets are derived from the DAG in `dag.R` (figure: `dag.png`), using `dagitty`, before any outcome analysis.

The DAG represents age, sex, education, income-poverty ratio (PIR) and race/ethnicity as separate nodes. Race/ethnicity is treated as a marker of social stratification and differential social exposures, not as a biological causal factor.

Smoking and alcohol are not connected by an arrow. Their association is represented through the shared causes in the DAG. Mutual adjustment between smoking and alcohol is not automatic. Mutually adjusted models are presented only as sensitivity analyses.

Mediators are not adjusted for in the primary models. In particular, BMI is not adjusted for when estimating the total association of physical activity with mortality. Hypertension and diabetes are likewise not adjusted for.

Primary adjustment sets (total effect on mortality, output of `adjustmentSets()`, saved in `adjustment_sets.txt`):

- Smoking: Age, Edu, PIR, Race, Sex
- Alcohol: Age, Edu, PIR, Race, Sex
- Physical activity: Age, Edu, PIR, Race, Sex
- BMI: Age, Alcohol, Edu, PA, PIR, Race, Sex, Smoking

Each exposure is interpreted only from its own model. Coefficients of other variables in the same model (covariates, or the other lifestyle factors in the BMI model) are not interpreted as effects.

## Models

- M1: crude survey-weighted Cox model.
- M2: age- and sex-adjusted survey-weighted Cox model.
- M3 (primary): survey-weighted Cox model with the exposure-specific DAG-based adjustment set.

### Proportional hazards

The proportional hazards assumption is assessed using exposure-by-log-follow-up-time interaction terms in survey-weighted Cox models, with design-based Wald tests from `survey::regTermTest()`. `survival::cox.zph()` is not applied directly to `survey::svycoxph()` fits.

If proportional hazards is not supported for a primary exposure, time-varying effects are examined using a prespecified five-year follow-up split, and interval-specific hazard ratios are reported instead of a single constant hazard ratio.

## Sensitivity analyses

### S1: 24-month landmark analysis

Participants who die or are censored within the first 24 months are excluded. The analysis population consists of participants alive and under observation at 24 months. Follow-up time is reset at the landmark (`LANDMARK_TIME = PERMTH_EXM - 24`). Only deaths after the landmark count as events.

### S2: alternative physical activity cut-points

Same weekly moderate-equivalent calculation, with categories: inactive 0 minutes/week; insufficient 1-299; sufficient 300 or more.

### S3: exclusion of baseline chronic disease in BMI analyses

The BMI-mortality analysis is repeated after excluding participants with any of the following at baseline:

- Cardiovascular disease: self-reported congestive heart failure, coronary heart disease, angina, heart attack or stroke.
- Cancer: self-reported physician diagnosis of cancer.
- COPD-related disease: self-reported emphysema or chronic bronchitis.
- Probable chronic kidney disease: eGFR below 60 mL/min/1.73 m2 or urine albumin-to-creatinine ratio of 30 mg/g or above.

eGFR is calculated with the 2021 race-free CKD-EPI creatinine equation:

eGFR = 142 x min(SCr/k, 1)^a x max(SCr/k, 1)^-1.200 x 0.9938^Age x 1.012 [if female]

where SCr is standardized serum creatinine (mg/dL); for females k = 0.7 and a = -0.241; for males k = 0.9 and a = -0.302.

NHANES provides a single baseline assessment, so chronicity cannot be confirmed. This variable is described as a probable CKD indicator, not a clinical diagnosis. Diabetes is not included in the exclusion definition because it may lie on the causal pathway between BMI and mortality.

Definitions are checked for harmonisation across the four cycles before S3 is implemented. If a required definition cannot be harmonised, S3 is not implemented with an improvised substitute and the limitation is documented.

## Missing data

Complete-case analysis is primary. Missingness is assessed for all exposures (smoking, physical activity, alcohol, BMI) and adjustment variables (age, sex, education, PIR, race/ethnicity). Structural missingness from skip patterns is distinguished from genuine item non-response. Missingness in PIR is examined separately.

If more than 10% of otherwise eligible participants are excluded from the complete-case analysis because of non-structural missingness in one or more key analytic variables, multiple imputation is conducted as a prespecified sensitivity analysis.

Multiple imputation: 50 imputed datasets. Imputation models include the analysis variables, relevant auxiliary predictors, the mortality event indicator and the Nelson-Aalen cumulative hazard estimate. Survey weights, strata and PSUs are included as predictors but not imputed. Mortality variables and survey design variables remain observed. Estimates are pooled using Rubin's rules, with the pooling procedure checked for compatibility with survey-design variance estimates. The imputation approach, variable-specific missingness and resulting analytic sample sizes are reported.

## Limitations acknowledged in advance

- Complete-case selection may change the population represented by the original MEC weights, with potential selection bias.
- Public-use MEC weights are not recalibrated for mortality-linkage ineligibility, with potential bias.
- Smoking and alcohol are considered through shared causes only; the absence of a direct arrow is an assumption.

## Reporting

Results are reported according to the STROBE checklist. Any deviation from this plan after examining outcome results is documented in `DEVIATIONS.md`, with the reason and date of the change.
