# Created by Dongmei Chen
# generate 
library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1907:2016
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

# start_year:1901
vargrp.a <- c("JanTmin", "MarTmin", "TMarAug", "summerTmean", 
				"AugTmean", "AugTmax", "GSP", "PMarAug", "summerP0")
# start_year:1902
vargrp.b <- c("OctTmin", "fallTmean", "winterTmin", "Tmin", "Tmean", "Tvar", "TOctSep", "summerP1", "summerP2", "Pmean")
# start_year:1903
vargrp.c <- c("POctSep", "PcumOctSep")
# start_year:1907
vargrp.d <- c("PPT")
vargrp <- c(vargrp.a, vargrp.b, vargrp.c, vargrp.d)

startyrs <- c(rep(1901,9), rep(1902,10), rep(1903, 2), 1907)
# number of the start and end layer
n1 <- c(rep(7,9), rep(6,10), rep(5, 2), 1)
n2 <- c(rep(116,9), rep(115,10), rep(114, 2), 110)

get.data <- function(var, start_yr){
  ncfile <- paste0("na10km_v2_",var, "_", start_yr,".2016.3d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.yearly.table <- function(yr){
	data <- get.data(vargrp[1],startyrs[1])
	data <- data[,,n1[1]:n2[1]]
	na <- data[,,yr]
	nadf <- data.frame(var=na[!is.na(na)])
	colnames(nadf) <- vargrp[1]
	ndf <- nadf
	for(i in 2:length(vargrp)){
	  data <- get.data(vargrp[i],startyrs[i])
	  data <- data[,,n1[i]:n2[i]]
	  na <- data[,,yr]
	  nadf <- data.frame(var=na[!is.na(na)])
	  colnames(nadf) <- vargrp[i]
	  ndf <- cbind(ndf, nadf)
	}
	write.csv(ndf, paste0(outcsvpath, "bioclimatic_values_", years[yr],".csv"), row.names = FALSE)
}

foreach(i=1:length(years))%dopar%{
  get.yearly.table(i)
  print(paste("...got data for", vargrp[i]), "...")
}

print("all done")