library(pROC)
library(openxlsx)

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
sink("auc.txt")
for (i in 1:length(vargrp)){
  print(auc(ndf[, "beetles"], ndf[,vargrp[i]]))
  #print(i)
}  
sink()
proc.time() - ptm

auc <- read.xlsx("/Users/dongmeichen/Documents/beetle/data/auc.xlsx")
auc$VAR <- vargrp
write.csv(paste0(out, "auc.csv"), row.names=FALSE)

univariate.plot <- function(i){
  df <- ndf[,c(vargrp[i], "hosts", "beetles")]
  df <- df[sample(nrow(df), 0.05*n),]
  df1 <- df[df$hosts==1,]
  df2 <- df[df$beetles==1,]
  df3 <- rbind(df, df1, df2)
  df3$ucs <- c(rep("Continent", dim(df)[1]), rep("Hosts", dim(df1)[1]), rep("MPB", dim(df2)[1]))
  names(df3)[which(names(df3)==vargrp[i])] <- "var"
  stripchart(var~ucs,
             data=df3,
             main=paste0(vargrp[i], ": ", round(auc$AUC[auc$VAR==vargrp[i]], 3)),
             xlab="",
             ylab="",
             cex=0.5,
             cex.main=2, cex.lab=1.5, cex.axis=1.5,
             #col=rgb(0.5, 0.5, 0.5, 0.5),
             pch=19)
  print(paste(vargrp[i], "is done!"))
}

png("bioclim_univariate_plots_auc.png", width=15, height=24, units="in", res=300)
par(mfrow=c(8,5),mar=c(3.5,3.5,3,1))
for (i in 1:length(vargrp)){
  univariate.plot(i)
}
dev.off()

mauc <- auc$VAR[auc$AUC==max(auc$AUC)]

