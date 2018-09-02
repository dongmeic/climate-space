# Created by Dongmei Chen
# generate yearly tables for bioclimatic values

library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1907:2016
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(outcsvpath)

vargrp <- c("drop0", "drop5", "ddAugJul", "ddAugJun", "Acs",
						"min20", "min22", "min24", "min26", "min28", 
						"min30", "min32", "min34", "min36", "min38", "min40")

# number of the start and end layer
get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_1902.2016.3d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.yearly.table <- function(yr){
	data <- get.data(vargrp[1])
	na <- data[,,yr]
	nadf <- data.frame(var=na[!is.na(na)])
	colnames(nadf) <- vargrp[1]
	ndf <- nadf
	for(i in 2:length(vargrp)){
	  data <- get.data(vargrp[i])
	  na <- data[,,yr]
	  nadf <- data.frame(var=na[!is.na(na)])
	  colnames(nadf) <- vargrp[i]
	  ndf <- cbind(ndf, nadf)
	}
	write.csv(ndf, paste0("bioclimatic_values_", years[yr],"_daily.csv"), row.names = FALSE)
	print("done writing the data table!")
}

foreach(i=1:length(years))%dopar%{
  get.yearly.table(i)
  print(paste0("...got data for ", years[i], "..."))
}

print("all done")