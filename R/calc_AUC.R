library(pROC)

source("/Users/dongmeichen/GitHub/climate-space/R/combine_CRU_Daymet.R")
ndf <- get_data()
ndf <- ndf[complete.cases(ndf),]
ndf$hosts <- ifelse(ndf$beetles==1 & ndf$hosts==0, 1, ndf$hosts)
out <- "/Users/dongmeichen/Documents/beetle/output/"
