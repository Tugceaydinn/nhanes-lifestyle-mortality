alq <- read_nhanes("ALQ")
dim(alq)

alq_vars <- c("SEQN", "ALQ101", "ALQ110", "ALQ120Q", "ALQ120U", "ALQ130")
setdiff(alq_vars, names(alq))     

analytic <- analytic %>% left_join(select(alq, all_of(alq_vars)), by = "SEQN")
nrow(analytic)                    

adults <- analytic %>% filter(RIDAGEYR >= 20)

table(adults$ALQ101, adults$ALQ110, useNA = "ifany")


table(adults$ALQ120U, useNA = "ifany")
summary(adults$ALQ120Q)
table(adults$ALQ120Q == 0, adults$ALQ101, useNA = "ifany")


summary(adults$ALQ130)


sum(!adults$SEQN %in% alq$SEQN)

analytic <- analytic %>%
  mutate(
    
    alq120q = ifelse(ALQ120Q %in% c(777, 999), NA, ALQ120Q),
    alq130  = ifelse(ALQ130  %in% c(777, 999), NA, ALQ130),
    alq120u = ifelse(ALQ120U %in% c(1, 2, 3), ALQ120U, NA),
    
    
    drink_days_wk = case_when(
      alq120u == 1 ~ alq120q,
      alq120u == 2 ~ alq120q * 12 / 52,
      alq120u == 3 ~ alq120q / 52
    ),
    drinks_wk = drink_days_wk * alq130,
    heavy_cut = ifelse(RIAGENDR == 1, 15, 8),   
    
    
    lifetime12 = case_when(
      ALQ101 == 1                ~ TRUE,
      ALQ101 == 2 & ALQ110 == 1  ~ TRUE,
      ALQ101 == 2 & ALQ110 == 2  ~ FALSE
    ),
    
    alcohol = case_when(
      lifetime12 == FALSE                                             ~ "lt12_lifetime",
      lifetime12 == TRUE & alq120q == 0                               ~ "former",
      lifetime12 == TRUE & alq120q > 0 & drinks_wk >= heavy_cut       ~ "current_heavy",
      lifetime12 == TRUE & alq120q > 0 & drinks_wk <  heavy_cut       ~ "current_nonheavy"
    ),
    alcohol = factor(alcohol, levels = c("lt12_lifetime", "former",
                                         "current_nonheavy", "current_heavy"))
  )

adults <- analytic %>% filter(RIDAGEYR >= 20)

table(adults$alcohol, useNA = "ifany")
sum(adults$drink_days_wk > 7.5, na.rm = TRUE)   # 0 veya çok az olmalı
summary(adults$drinks_wk)