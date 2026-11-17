paq <- read_nhanes("PAQ")

paq_vars <- c("SEQN",
              "PAQ605", "PAQ610", "PAD615",   # vigorous work
              "PAQ620", "PAQ625", "PAD630",   # moderate work
              "PAQ635", "PAQ640", "PAD645",   # walk/bicycle (transport)
              "PAQ650", "PAQ655", "PAD660",   # vigorous leisure
              "PAQ665", "PAQ670", "PAD675")   # moderate leisure
setdiff(paq_vars, names(paq))     # character(0) olmalı

domain_min <- function(gate, days, mins) {
  case_when(
    gate == 2                                    ~ 0,
    gate == 1 & days %in% 1:7 & mins < 7777      ~ days * mins
  )
}

analytic <- analytic %>%
  left_join(select(paq, all_of(paq_vars)), by = "SEQN") %>%
  mutate(
    vig_work  = domain_min(PAQ605, PAQ610, PAD615),
    mod_work  = domain_min(PAQ620, PAQ625, PAD630),
    mod_trans = domain_min(PAQ635, PAQ640, PAD645),
    vig_leis  = domain_min(PAQ650, PAQ655, PAD660),
    mod_leis  = domain_min(PAQ665, PAQ670, PAD675),
    
    
    mvpa = mod_work + mod_trans + mod_leis + 2 * (vig_work + vig_leis),
    
    pa = case_when(mvpa == 0  ~ "inactive",
                   mvpa < 150 ~ "insufficient",
                   mvpa >= 150 ~ "sufficient"),
    pa = factor(pa, levels = c("sufficient", "insufficient", "inactive")),
    
    
    pa_s2 = case_when(mvpa == 0  ~ "inactive",
                      mvpa < 300 ~ "insufficient",
                      mvpa >= 300 ~ "sufficient"),
    pa_s2 = factor(pa_s2, levels = c("sufficient", "insufficient", "inactive"))
  )

nrow(analytic)    

adults <- analytic %>% filter(RIDAGEYR >= 20)
table(adults$pa, useNA = "ifany")
table(adults$pa_s2, useNA = "ifany")
quantile(adults$mvpa, c(0.5, 0.9, 0.99, 1), na.rm = TRUE)