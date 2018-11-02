# Created by Dongmei Chen
# generate yearly tables for na10km climatic values
# modified bioclimatic_values_time_series.R

library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1901:2016; nyr <- length(years)
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/monthly_mean"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
setwd(outcsvpath)

get.data <- function(var){
  ncfile <- paste0("na10km_v2_cru_ts4.01.",years[1],".",years[nyr],".",var,".abs4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

data <- get.data("tmp")
get.yearly.table <- function(data,yr){
  ndf <- data.frame(var=numeric())
  for(m in 1:12){
    data_slice <- data[,,m,yr]
    na.values <- data_slice[!is.na(data_slice)]
    nadf <- data.frame(var=na.values)
	  ndf <- rbind(ndf, nadf)
	  print(paste("getting values in year", years[yr], "and month", m))
  }	
  colnames(ndf) <- "tmp"
  write.csv(ndf, paste0("na10km_v2_monthly_mean_", years[yr],".csv"), row.names = FALSE)
  print("done writing the data table!")
}

foreach(i=1:nyr)%dopar%{
  get.yearly.table(data, i)
  print(paste0("...got data for year ", years[i], "..."))
}

print("all done")