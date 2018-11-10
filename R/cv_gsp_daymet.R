# Created by Dongmei Chen
# Daily bioclimatic variables from Daymet

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/getVarsfromDaymet.R")

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)
start_year <- 1996; end_year <- 2015; years <- start_year:end_year; nt <- length(years)

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	i <- as.numeric(args[1])
	print(paste('i:', i))
}

print("calculating coefficient of variance in growing season precipitation using daily data")
dim1 <- 77369; dim2 <- nt

ptm <- proc.time()
prcp.df <- read.csv(paste0(inpath, "prcp/prcp", years[i],".csv"))
df <- data.frame(cv.gsp=double())
for(j in 1:dim1){
	prcp <- as.numeric(prcp.df[j,3:367])
	if(sum(is.na(prcp))==365){
		df[j,] <- NA
	}else{
		df[j,] <- get.cv.gsp(prcp)
	}
}
print(paste("got data from", years[i]))
write.csv(df, paste0("daily_climate/Daymet/dm_cv_gsp_",years[i],".csv"), row.names = FALSE)  

proc.time() - ptm
print("all done!")