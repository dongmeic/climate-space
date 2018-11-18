# Created by Dongmei Chen
# Daily bioclimatic variables from Daymet

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/water_deficit.R")

inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet"
setwd(inpath)

rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")
d <- dim(rows)[1]
years <- 1996:2015

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	i <- as.numeric(args[1])
	print(paste('year:', years[i]))
}

print("calculating the biocliamtic variables using daily data")
# test: i <- 2; j <- 53600

ptm <- proc.time()
df <- data.frame(wd=double(), vpd=double(), mi=double(), cwd=double(), pt.coef=double())

for(j in 1:d){
	vpd.v <- vector()
	cwd.v <- vector()
	for(k in 0:5){
		tmp_prcp <- read.csv(paste0("evapo_na/input/",years[i]-k,"/s",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i]-k,".csv"))
		ets <- read.csv(paste0("evapo_na/output/",years[i]-k,"/ET_s",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i]-k,".csv"))
		vp <- read.csv(paste0("vpd_na/",years[i]-k,"/vp",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i]-k,".csv"))
		vpd.v[k+1] <- vpd(tmp_prcp[,2], vp$vp)
		cwd.v[k+1] <- cwd(ets[,2], ets[,3])
	}
	tmp_prcp1 <- read.csv(paste0("evapo_na/input/",years[i]-1,"/s",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i]-1,".csv"))
	tmp_prcp2 <- read.csv(paste0("evapo_na/input/",years[i],"/s",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i],".csv"))
	ets1 <- read.csv(paste0("evapo_na/output/",years[i]-1,"/ET_s",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i]-1,".csv"))
	ets2 <- read.csv(paste0("evapo_na/output/",years[i],"/ET_s",rows$rows[j],"_",rows$lat[j],"_",rows$etopo1[j],"_",years[i],".csv"))
	tmp <- c(tmp_prcp1[,2], tmp_prcp2[,2])
	prcp <- c(tmp_prcp1[,3], tmp_prcp2[,3])
	aet <- c(ets1[,3], ets2[,3])
	df[j,] <- c(wd=wd(prcp, aet, tmp), vpd=sum(vpd.v), mi=mi(prcp, aet), cwd=sum(cwd.v), pt.coef=pt.coef(ets2$equilET,ets2$actualET))	
	#print(j)
}

print(paste("got data from", years[i]))
write.csv(df, paste0("bioclm_na/dm_bioclm_var_",years[i],"_na_wd.csv"), row.names = FALSE)  

proc.time() - ptm
print("all done!")