# Created by Dongmei Chen
# generate yearly tables for na10km climatic values
# modified bioclimatic_values_time_series.R

library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1901:2016; nyr <- length(years)
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
setwd(outcsvpath)

na10km.df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")


vars <- c("tmp", "tmx", "tmn", "pre")

get.data <- function(var){
  ncfile <- paste0("na10km_v2_cru_ts4.01.",years[1],".",years[nyr],".",var,".abs4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.yearly.table <- function(yr){
  data <- get.data(vars[1])
  ndf <- data.frame(x=numeric(),y=numeric(),year=numeric(),month=numeric(),var=numeric())
  for(m in 1:12){
    data_slice <- data[,,m,yr]
    na.values <- data_slice[!is.na(data_slice)]
    nadf <- data.frame(x=na10km.df$x, y=na10km.df$y, year=rep(years[yr],length(na.values)),
	  					 month=rep(m,length(na.values)),var=na.values)
	ndf <- rbind(ndf, nadf)
	print(paste("getting values from",vars[1],"in year", years[yr], "and month", m))
  }	
  colnames(ndf)[5] <- vars[1]
  for(i in 2:length(vars)){
    data <- get.data(vars[i])
    nndf <- data.frame(var=numeric())
    for(m in 1:12){
      data_slice <- data[,,m,yr]
      na.values <- data_slice[!is.na(data_slice)]
      nadf <- data.frame(var=na.values)
	  nndf <- rbind(nndf, nadf)
	  print(paste("getting values from",vars[i],"in year", years[yr], "and month", m))
    }
  	colnames(nndf) <- vars[i]
  	ndf <- cbind(ndf, nndf)
  	print(paste("adding values from", vars[i]))
  }
  write.csv(ndf, paste0("na10km_v2_climatic_values_", years[yr],".csv"), row.names = FALSE)
  print("done writing the data table!")
}

foreach(i=1:length(years))%dopar%{
  get.yearly.table(i)
  print(paste0("...got data for year ", years[i], "..."))
}

print("all done")