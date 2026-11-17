library(readr)

read_mort <- function(years) {
  u <- sprintf("https://ftp.cdc.gov/pub/Health_Statistics/NCHS/datalinkage/linked_mortality/NHANES_%s_MORT_2019_PUBLIC.dat", years)
  dest <- sprintf("data/raw/NHANES_%s_MORT_2019_PUBLIC.dat", years)
  if (!file.exists(dest)) download.file(u, destfile = dest, mode = "wb")
  read_fwf(dest, col_types = "iiiiiiii",
           fwf_cols(SEQN = c(1, 6), eligstat = c(15, 15), mortstat = c(16, 16),
                    ucod_leading = c(17, 19), diabetes = c(20, 20), hyperten = c(21, 21),
                    permth_int = c(43, 45), permth_exm = c(46, 48)),
           na = c("", "."))
}

mort <- bind_rows(lapply(c("2007_2008", "2009_2010", "2011_2012", "2013_2014"), read_mort))
dim(mort)                    # 40617 
anyDuplicated(mort$SEQN)     


analytic <- analytic %>% left_join(select(mort, SEQN, eligstat), by = "SEQN")
nrow(analytic)               # 39166 

adults <- analytic %>% filter(RIDAGEYR >= 20)
table(adults$eligstat, useNA = "ifany")


flow <- adults %>% filter(eligstat == 1)
nrow(flow)
flow <- flow %>% filter(is.na(RIDEXPRG) | RIDEXPRG != 1)    # gebeler hariç
nrow(flow)

# null value
key <- c("smoking", "pa", "alcohol", "bmi_cat", "edu", "pir_cat")
colSums(is.na(flow[, key]))
round(100 * colSums(is.na(flow[, key])) / nrow(flow), 1)

cc <- complete.cases(flow[, key])
sum(cc)
round(100 * mean(!cc), 1)   