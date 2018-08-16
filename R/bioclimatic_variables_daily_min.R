# Created by Dongmei Chen
# Writing netCDFfiles for daily bioclimatic variables (minimum temperatures)

library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/damian/getDailyStats.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)
start_year <- 1901; end_year <- 2016; years <- start_year:end_year; nt <- length(years)

print("calculating the biocliamtic variables using daily data")
ptm <- proc.time()
dim1 <- 277910; dim2 <- nt

foreach(i = 1:115)%dopar%{
	indata1 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i],".csv"))
	indata2 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i+1],".csv"))
	indata <- rbind(indata1, indata2)
	df <- data.frame(Ncs=integer(), Acs=integer(), min20=integer(), min22=integer(), min24=integer(), min26=integer(), min28=integer())
	#df <- data.frame(min30=integer(), min32=integer(), min34=integer(), min36=integer(), min38=integer(), min40=integer())
	for(j in 1:dim1){
		df.j <- indata[j,]
		for(m in 1:23){
			df.m <- rbind(df.j, indata[j+dim1*m,])
			df.j <- df.m
			#print(m)
		}
		df[j,] <- get.daily.stats(years[i], df.m$tmx, df.m$tmp, df.m$tmn)
	}
	print(paste("got data from", years[i+1]))
	write.csv(df, paste0("bioclimatic_variables_daily_min_",years[i+1],"_2.csv"), row.names = FALSE)  
}
proc.time() - ptm
print("all done!")