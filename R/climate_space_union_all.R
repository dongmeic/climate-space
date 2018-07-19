# Created by Dongmei Chen
library(scales)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/boxplot_settings.R")

years <- 1996:2015; nyr <- length(years)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

vargrp.t <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
				"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax", "drop0", "drop5")
vargrp.p <- c("PcumOctSep", "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean",
				"GSP", "POctSep", "PMarAug", "summerP0", "summerP1", "summerP2", "ddAugJul", "ddAugJun")
				
cols <- c("grey70", "#1b9e77", "#d95f02")

climate.space <- function(i){
  df.t <- read.csv(paste0(outcsvpath, vargrp.t[i], "_",years[1], "_",years[nyr], ".csv"))
  df.p <- read.csv(paste0(outcsvpath, vargrp.p[i], "_",years[1], "_",years[nyr], ".csv"))
  df <- cbind(data.frame(tmp=df.t[,1]),data.frame(pre=df.p[,1]),data.frame(prs=df.t[,2]))
  df.na <- subset(df, prs=="continent")
  plot(df.na$tmp, df.na$pre, pch=16, cex=0.1, col = alpha(cols[1], 0.2), main=paste0(vargrp.t[i],"/",vargrp.p[i]), xlab=vargrp.t[i], ylab=vargrp.p[i], cex.main=2, cex.lab=1.5, cex.axis=1.5)
  df.vgt <- subset(df, prs=="hosts")
  points(df.vgt$tmp, df.vgt$pre, pch=16, cex=0.1, col = alpha(cols[2], 0.2))
  df.btl <- subset(df, prs=="mpb")
  points(df.btl$tmp, df.btl$pre, pch=16, cex=0.1, col = alpha(cols[3], 0.2))  
}
png("cs_var_union.png", width=16, height=10, units="in", res=300)
par(mfrow=c(3,5),mar=c(5,5,3,1))
for(i in 1:length(vargrp.t)){
  climate.space(i)
  print(paste("plotting climate space with", vargrp.t[i], "and", vargrp.p[i]))
  if(i==1){
    legend('topleft', pch=16, col=cols, legend=c("Continent", "Hosts", "Beetles"), bty='n')
  }
}
dev.off()

print("all done!")