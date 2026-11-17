
analytic <- analytic %>%
  mutate(
    eligible = RIDAGEYR >= 20 & eligstat %in% 1 & (is.na(RIDEXPRG) | RIDEXPRG != 1),
    complete = eligible & !is.na(smoking) & !is.na(pa) & !is.na(alcohol) &
      !is.na(bmi_cat) & !is.na(edu) & !is.na(pir_cat)
  )
sum(analytic$eligible, na.rm = TRUE)    
sum(analytic$complete, na.rm = TRUE)    
analytic <- analytic %>%
  left_join(select(mort, SEQN, mortstat, permth_exm), by = "SEQN")
nrow(analytic)                        

elig <- analytic %>% filter(eligible)
nrow(elig)                              
sum(is.na(elig$mortstat))
sum(is.na(elig$permth_exm))
table(elig$mortstat)


summary(elig$permth_exm)
sum(elig$permth_exm == 0)


elig %>%
  group_by(SDDSRVYR) %>%
  summarise(n = n(), deaths = sum(mortstat),
            mean_months = round(mean(permth_exm)), max_months = max(permth_exm))

elig <- elig %>%
  mutate(age_grp = cut(RIDAGEYR, c(20, 40, 60, 80, Inf), right = FALSE,
                       labels = c("20-39", "40-59", "60-79", "80+")))
table(elig$age_grp, elig$mortstat)

sum(elig$mortstat == 1 & elig$permth_exm <= 24)
sum(elig$mortstat[elig$complete])
sum(elig$complete & elig$mortstat == 1 & elig$permth_exm > 24)