# Created by Dongmei Chen
# Writing netCDFfiles for daily bioclimatic variables

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
Lcs <- matrix(, nrow = dim1, ncol = dim2)
maxAugT <- matrix(, nrow = dim1, ncol = dim2)
summerT40 <- matrix(, nrow = dim1, ncol = dim2)
winterTmin <- matrix(, nrow = dim1, ncol = dim2)
Ecs <- matrix(, nrow = dim1, ncol = dim2)
Ncs <- matrix(, nrow = dim1, ncol = dim2)
Acs <- matrix(, nrow = dim1, ncol = dim2)
drop0 <- matrix(, nrow = dim1, ncol = dim2)
drop5 <- matrix(, nrow = dim1, ncol = dim2)
drop10 <- matrix(, nrow = dim1, ncol = dim2)
drop15 <- matrix(, nrow = dim1, ncol = dim2)
drop20 <- matrix(, nrow = dim1, ncol = dim2)
drop20plus <- matrix(, nrow = dim1, ncol = dim2)
max.drop <- matrix(, nrow = dim1, ncol = dim2)
ddAugJul <- matrix(, nrow = dim1, ncol = dim2)
ddAugJun <- matrix(, nrow = dim1, ncol = dim2)

foreach(i = 1:(nt-1))%dopar%{
	i <- nt-1
	indata1 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i],".csv"))
	indata2 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i+1],".csv"))
	indata <- rbind(indata1, indata2)
	df <- data.frame(Lcs=integer(), maxAugT=double(), summerT40=integer(), winterTmin=double(), Ecs=integer(),
									Ncs=integer(), Acs=integer(), drop0=numeric(), drop5=numeric(), drop10=numeric(), 
									drop15=numeric(), drop20=numeric(), drop20plus=numeric(), max.drop=double(),
									ddAugJul=double(), ddAugJun=double())
	for(j in 1:dim1){
		df.j <- indata[j,]
		for(m in 1:23){
			df.m <- rbind(df.j, indata[j+dim1*m,])
			df.j <- df.m
			#print(m)
		}
		df[j,] <- get.daily.stats(years[i], df.m$tmx, df.m$tmp, df.m$tmn)
		Lcs[j,i] <- df[j,1]
		maxAugT[j,i] <- df[j,2]
		summerT40[j,i] <- df[j,3]
		winterTmin[j,i] <- df[j,4]
		Ecs[j,i] <- df[j,5]
		Ncs[j,i] <- df[j,6]
		Acs[j,i] <- df[j,7]
		drop0[j,i] <- df[j,8]
		drop5[j,i] <- df[j,9]
		drop10[j,i] <- df[j,10]
		drop15[j,i] <- df[j,11]
		drop20[j,i] <- df[j,12]
		drop20plus[j,i] <- df[j,13]
		max.drop[j,i] <- df[j,14]
		ddAugJul[j,i] <- df[j,15]
		ddAugJun[j,i] <- df[j,16]
	}
	print(paste("got data from", years[i+1]))
	write.csv(df, paste0("bioclimatic_variables_daily_",years[i+1],".csv"), row.names = FALSE)  
}
proc.time() - ptm
print("all done!")