# Created by Dongmei Chen
# Writing netCDFfiles for daily bioclimatic variables

library(ncdf4)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/getDailyStats.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
start_year <- 1901; end_year <- 2015; years <- start_year:end_year; nyr <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncfile <- paste0(ncpath,"na10km_v2.nc")
ncin <- nc_open(ncfile)
print(ncin)
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)

j2 <- sapply(indata$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(indata$y, function(xy) which.min(abs(y-xy)))
head(cbind(indata$x,indata$y,j2,k2))


for (i in 1:nyr){
  indata1 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i],".csv"))
  indata2 <- read.csv(paste0(inpath, "na10km_v2_climatic_values_",years[i+1],".csv"))
  indata <- rbind(indata1, indata2)
  
  
}
