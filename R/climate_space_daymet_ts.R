# Created by Dongmei Chen
library(ggplot2)
library(grid)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1996:2015; nyr <- length(years)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/ts"
setwd(out)

vargrp1 <- c("ddAugJul", "OptTsum", "summerP2", "drop5", "PPT", "OctMin",
						 "JanTmin", "TMarAug", "summerTmean", "AugTmean", "AugTmax",
						 "OctTmin", "fallTmean", "Tmin", "Tmean", "Acs", "Mar20", "Oct20")
				
vargrp2 <- c("maxAugT", "winterTmin", "MarTmin", "GSP", "AugMax", "Tvar",
						 "summerP1", "PMarAug", "summerP0", "POctSep", "PcumOctSep",
						 "drop0", "min20", "Jan20", "Pmean", "maxT", "minT", "JanMin")
       
cols <- c("grey70", "#1b9e77", "#e41a1c")

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
bioClim <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_r.csv")) # LDA_daymet.R
df <- bioClim[bioClim$beetles==1,]
df.host <- bioClim[bioClim$hosts==1,]

climate_space_time_series <- function(var1, var2){	
  png(paste0("cs_both_",var1,"_",var2,"_ts.png"), width=15, height=12, units="in", res=300)
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