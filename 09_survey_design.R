library(survey)

analytic %>% group_by(SDDSRVYR) %>%
  summarise(min_strata = min(SDMVSTRA), max_strata = max(SDMVSTRA))

sum(analytic$WTMEC2YR == 0)                        

analytic <- analytic %>% mutate(wt = WTMEC2YR / 4)


des <- svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~wt,
                 nest = TRUE, data = analytic)


des_elig <- subset(des, eligible)
des_cc   <- subset(des, complete)


degf(des_elig)                                      
svytotal(~I(RIDAGEYR >= 20), des)                   
sum(weights(des_elig)) / 1e6                        