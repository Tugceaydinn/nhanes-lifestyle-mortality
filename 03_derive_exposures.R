analytic <- analytic %>%
  mutate(
    smoking = case_when(
      SMQ020 == 2                        ~ "never",
      SMQ020 == 1 & SMQ040 == 3          ~ "former",
      SMQ020 == 1 & SMQ040 %in% c(1, 2)  ~ "current",
      TRUE                               ~ NA_character_
    ),
    smoking = factor(smoking, levels = c("never", "former", "current")),
    bmi_cat = cut(BMXBMI, breaks = c(-Inf, 18.5, 25, 30, Inf), right = FALSE,
                  labels = c("underweight", "normal", "overweight", "obese")),
    bmi_cat = relevel(bmi_cat, ref = "normal")
  )

adults <- analytic %>% filter(RIDAGEYR >= 20)
table(adults$smoking, useNA = "ifany")
table(adults$bmi_cat, useNA = "ifany")