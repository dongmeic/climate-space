# Created by Dongmei Chen
library(ggplot2)
library(grid)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
years <- 1996:2015; nyr <- length(years)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/ts"
setwd(out)

vargrp1 <- c("JanTmin", "MarTmin", "TMarAug", "summerTmean", "AugTmean","OctTmin", "fallTmean",
							"Tmin", "Tmean", "TOctSep", "OptTsum","AugMaxT", "maxT", "ddAugJul", "ddAugJun",
							"OctMin", "JanMin", "MarMin", "winterMin", "minT")

vargrp2 <- c("GSP", "PMarAug", "summerP0", "Tvar",  "summerP1", "summerP2",  "Pmean",  
						 "POctSep", "PcumOctSep", "PPT", "maxAugT", "drop0", "drop5", "max.drop",
						 "cv.gsp", "wd", "vpd", "mi", "cwd", "pt.coef")
       
cols <- c("grey70", "#1b9e77", "#e41a1c")

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
bioClim <- get_data()
df <- bioClim[bioClim$beetles==1,]
df.host <- bioClim[bioClim$hosts==1,]

climate_space_time_series <- function(var1, var2){	
  png(paste0("cs_both_na_",var1,"_",var2,"_ts.png"), width=15, height=12, units="in", res=300)
  par(mfrow=c(4,5),mar=c(2,2,4,2))
  for(yr in years){
  	bioClim.ss <- subset(bioClim, year==yr)
  	df.host.ss <- subset(df.host, year==yr)
  	df.ss <- subset(df, year==yr)
  	plot(bioClim.ss[,var1], bioClim.ss[,var2], pch=19, cex=0.5, col = alpha(cols[1], 0.6), main=yr, xlab="", ylab="",
  			 xlim=range(bioClim[,var1], na.rm = T), ylim=range(bioClim[,var2], na.rm = T), cex.main=1.5, cex.lab=1.2, cex.axis=1.2)
  	points(df.host.ss[,var1], df.host.ss[,var2], pch=19, cex=0.5, col = alpha(cols[2], 0.7))
  	points(df.ss[,var1], df.ss[,var2], pch=19, cex=0.5, col = alpha(cols[3], 0.8))
  	#print(yr)			 
  }
  dev.off()
}

foreach(i=1:length(vargrp1))%dopar%{
	climate_space_time_series(vargrp1[i], vargrp2[i])
  print(paste("The time-series climate space with variables", vargrp1[i], "and", vargrp2[i], "is done!"))  
}

print("all done!")