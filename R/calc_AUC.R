library(pROC)

source("/Users/dongmeichen/GitHub/climate-space/R/combine_CRU_Daymet.R")
ndf <- get_data()
ndf <- ndf[complete.cases(ndf),]
ndf$hosts <- ifelse(ndf$beetles==1 & ndf$hosts==0, 1, ndf$hosts)
out <- "/Users/dongmeichen/Documents/beetle/output/"
setwd(out)

vargrp <- c("OctTmin", "JanTmin", "MarTmin", "Tmin", "OctMin", "JanMin", "MarMin",  
            "winterMin", "minT", "Acs", "drop5", "max.drop", "maxAugT", "AugMaxT", "AugTmax", "maxT", 
            "TMarAug", "OptTsum", "summerTmean", "AugTmean", "fallTmean", "TOctSep", "Tmean", "ddAugJul", 
            "ddAugJun", "Tvar", "PMarAug", "summerP0", "summerP1", "summerP2", "POctSep",
            "PcumOctSep", "Pmean", "PPT", "cv.gsp", "mi", "pt.coef", "vpd", "cwd", "wd")

n <- dim(ndf)[1]

ptm <- proc.time()
sink(paste0(out,"auc.txt"))
for (i in 1:length(vargrp)){
  print(auc(ndf[, "beetles"], ndf[,vargrp[i]]))
  print(i)
}  
sink()
proc.time() - ptm

