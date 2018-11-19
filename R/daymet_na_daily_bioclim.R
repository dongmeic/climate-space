# Created by Dongmei Chen
# Daily bioclimatic variables from Daymet

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
tmax.df.1 <- read.csv(paste0("datatable_na/tmax/tmax_", years[i],".csv"))
tmax.df.2 <- read.csv(paste0("datatable_na/tmax/tmax_", years[i+1],".csv"))
tmin.df.1 <- read.csv(paste0("datatable_na/tmin/tmin_", years[i],".csv"))
tmin.df.2 <- read.csv(paste0("datatable_na/tmin/tmin_", years[i+1],".csv"))
tmean.df.1 <- read.csv(paste0("datatable_na/tmean/tmean_", years[i],".csv"))
tmean.df.2 <- read.csv(paste0("datatable_na/tmean/tmean_", years[i+1],".csv"))
prcp.df <- read.csv(paste0("datatable_na/prcp/prcp_", years[i+1],".csv"))
df <- data.frame(Lcs=double(), maxAugT=double(), summerT40=double(), OptTsum=double(), AugMaxT=double(), maxT=double(),
								 Ecs=double(), Ncs=double(), Acs=double(), drop0=double(), drop5=double(), drop10=double(),
								 drop15=double(), drop20=double(), drop20plus=double(), max.drop=double(), ddAugJul=double(), ddAugJun=double(),
								 Oct20=double(), Oct30=double(), Oct40=double(), Jan20=double(), Jan30=double(), Jan40=double(),
								 Mar20=double(), Mar30=double(), Mar40=double(), winter20=double(), winter30=double(), winter40=double(),
								 OctMin=double(), JanMin=double(), MarMin=double(), winterMin=double(), minT=double(), cv.gsp=double())
for(j in 1:dim){
	tmx <- c(as.numeric(tmax.df.1[j,]), as.numeric(tmax.df.2[j,]))
	tmp <- c(as.numeric(tmean.df.1[j,]), as.numeric(tmean.df.2[j,]))
	tmn <- c(as.numeric(tmin.df.1[j,]), as.numeric(tmin.df.2[j,]))
	prcp <- as.numeric(prcp.df[j,])
	if(sum(is.na(tmx))==0 && sum(is.na(tmn))==0){
		df[j,] <- c(get.daily.stats(tmn, tmp, tmx), cv.gsp=get.cv.gsp(prcp))
	}else{
		if(sum(is.na(tmx))!=730){
			print(paste("at the point of x(", tmax.df.1[j,1], ") and y(", tmax.df.1[j,2], ")..."))
		}	
		df[j,] <- NA
	}
}
print(paste("got data from", years[i+1]))
write.csv(df, paste0("bioclm_na/dm_bioclm_var_",years[i+1],"_na.csv"), row.names = FALSE)  

proc.time() - ptm
print("all done!")