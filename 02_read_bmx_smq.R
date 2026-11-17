library(haven)
library(dplyr)

cycles <- data.frame(suffix = c("E", "F", "G", "H"),
                     year   = c(2007, 2009, 2011, 2013))

read_nhanes <- function(prefix) {
  bind_rows(lapply(seq_len(nrow(cycles)), function(i) {
    f    <- paste0(prefix, "_", cycles$suffix[i])
    dest <- sprintf("data/raw/%s.xpt", f)
    if (!file.exists(dest)) {
      u <- sprintf("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%d/DataFiles/%s.xpt",
                   cycles$year[i], f)
      download.file(u, destfile = dest, mode = "wb")
    }
    zap_formats(zap_labels(read_xpt(dest)))
  }))
}

bmx <- read_nhanes("BMX")
smq <- read_nhanes("SMQ")

dim(bmx)
dim(smq)
anyDuplicated(demo$SEQN)   
analytic <- demo %>%
  filter(RIDSTATR == 2) %>%
  left_join(select(bmx, SEQN, BMXBMI), by = "SEQN") %>%
  left_join(select(smq, SEQN, SMQ020, SMQ040), by = "SEQN")

nrow(analytic)  

summary(analytic$BMXBMI[analytic$RIDAGEYR >= 20])
table(analytic$SMQ020[analytic$RIDAGEYR >= 20], useNA = "ifany")
table(analytic$SMQ020[analytic$RIDAGEYR >= 20],
      analytic$SMQ040[analytic$RIDAGEYR >= 20], useNA = "ifany")
