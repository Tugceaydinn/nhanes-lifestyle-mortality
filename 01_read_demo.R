library(haven)
library(dplyr)

url <- "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2007/DataFiles/DEMO_E.xpt"
download.file(url, destfile = "data/raw/DEMO_E.xpt", mode = "wb")

demo_e <- read_xpt("data/raw/DEMO_E.xpt")

dim(demo_e)

table(demo_e$RIDSTATR)

vars <- c("SEQN", "RIDSTATR", "RIAGENDR", "RIDAGEYR", "RIDAGEEX", "RIDRETH1",
          "DMDEDUC2", "INDFMPIR", "RIDEXPRG", "SDMVPSU", "SDMVSTRA", "WTMEC2YR")
setdiff(vars, names(demo_e))

summary(demo_e[, vars])

cycles <- data.frame(
  file = c("DEMO_E", "DEMO_F", "DEMO_G", "DEMO_H"),
  year = c(2007, 2009, 2011, 2013)
)

for (i in seq_len(nrow(cycles))) {
  u <- sprintf("https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/%d/DataFiles/%s.xpt",
               cycles$year[i], cycles$file[i])
  download.file(u, destfile = sprintf("data/raw/%s.xpt", cycles$file[i]), mode = "wb")
}

demo <- bind_rows(lapply(cycles$file, function(f) {
  zap_formats(zap_labels(read_xpt(sprintf("data/raw/%s.xpt", f))))
}))

dim(demo)
table(demo$SDDSRVYR)
table(demo$RIDSTATR)