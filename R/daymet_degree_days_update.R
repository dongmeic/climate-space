# Dongmei Chen

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/damian/getStatsFromDaily.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet"
setwd(inpath)
start_year <- 1995; end_year <- 2015; years <- start_year:end_year

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	i <- as.numeric(args[1])
	print(paste('year:', years[i+1]))
}

print("calculating the biocliamtic variables using daily data")
dim <- 210748
# test: i <- 2; j <- 53600

ptm <- proc.time()
tmean.df.1 <- read.csv(paste0("datatable_na/tmean/tmean_", years[i],".csv"))
tmean.df.2 <- read.csv(paste0("datatable_na/tmean/tmean_", years[i+1],".csv"))

df <- data.frame(ddAugJul=double(), ddAugJun=double())

for(j in 1:dim){
	tmp <- c(as.numeric(tmean.df.1[j,]), as.numeric(tmean.df.2[j,]))
	if(sum(is.na(tmp))==0){
		df[j,] <- unlist(get.degree.days(tmp))
	}else{
		if(sum(is.na(tmp))!=730){
			print(paste("at the point of x(", tmean.df.1[j,1], ") and y(", tmean.df.1[j,2], ")..."))
		}	
		df[j,] <- NA
	}
}
print(paste("got data from", years[i+1]))
write.csv(df, paste0("bioclm_na/dm_bioclm_DD_",years[i+1],"_na.csv"), row.names = FALSE)  

proc.time() - ptm
print("all done!")