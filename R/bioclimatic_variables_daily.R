# Created by Dongmei Chen
# Writing daily bioclimatic variables

if(0){
	library(parallel)
	library(doParallel)
	library(foreach)
	registerDoParallel(cores=28)
}

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/getDailyStatsfromMonthly.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)
start_year <- 1901; end_year <- 1994; years <- start_year:end_year; nt <- length(years)

print("calculating the biocliamtic variables using daily data")
dim1 <- 277910; dim2 <- nt

ptm <- proc.time()
#foreach(i = 1:nt)%dopar%{
for(i in 1:nt){
	indata1 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i],".csv"))
	indata2 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i+1],".csv"))
	indata <- rbind(indata1, indata2)
	
	df <- data.frame(Lcs=double(), maxAugT=double(), summerT40=double(), winterTmin=double(), Ecs=double(), Ncs=double(), Acs=double(), drop0=double(),
									 drop5=double(), drop10=double(), drop15=double(), drop20=double(), drop20plus=double(),
									 max.drop=double(), ddAugJul=double(), ddAugJun=double(), min20=double(), min22=double(), 
									 min24=double(), min26=double(), min28=double(),min30=double(), min32=double(), 
									 min34=double(), min36=double(), min38=double(), min40=double()) 
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
	write.csv(df, paste0("daily_climate/CRU/1901/bioclimatic_variables_daily_",years[i+1],".csv"), row.names = FALSE)  
}
proc.time() - ptm
print("all done!")