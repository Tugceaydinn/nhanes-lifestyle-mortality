analytic <- analytic %>%
  mutate(
    age  = RIDAGEYR,
    sex  = factor(RIAGENDR, levels = c(1, 2), labels = c("male", "female")),
    

    race = factor(RIDRETH1, levels = 1:5,
                  labels = c("mexican_american", "other_hispanic",
                             "nh_white", "nh_black", "other")),
    race = relevel(race, ref = "nh_white"),
    

    edu = case_when(DMDEDUC2 %in% 1:2 ~ "lt_high_school",
                    DMDEDUC2 == 3     ~ "high_school",
                    DMDEDUC2 == 4     ~ "some_college",
                    DMDEDUC2 == 5     ~ "college_plus"),
    edu = factor(edu, levels = c("college_plus", "some_college",
                                 "high_school", "lt_high_school")),
    

    pir_cat = case_when(INDFMPIR < 1.3  ~ "lt_1.3",
                        INDFMPIR < 3.5  ~ "1.3_to_3.49",
                        INDFMPIR >= 3.5 ~ "ge_3.5"),
    pir_cat = factor(pir_cat, levels = c("ge_3.5", "1.3_to_3.49", "lt_1.3"))
  )

adults <- analytic %>% filter(RIDAGEYR >= 20)
nrow(adults)                              

table(adults$sex, useNA = "ifany")
table(adults$race, useNA = "ifany")
table(adults$edu, useNA = "ifany")
table(adults$pir_cat, useNA = "ifany")

table(adults$RIDEXPRG, useNA = "ifany")