# Created by Dongmei Chen
# generate yearly tables for SPLASH
# developed na10km_v2_climatic_values_ts.R, getPseudoDaily.R, getDailyStats.R

library(ncdf4)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/damian/getDailyStats.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/damian/getPseudoDaily.R")

years <- 1901:2016; nyr <- length(years)
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/SPLASH/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
setwd(outcsvpath)

na10km.df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
d <- dim(na10km.df)[1]

vars <- c("sun", "tmp", "pre")
varnms <- c("sf", "tair", "pn")

get.data <- function(var){
  ncfile <- paste0("na10km_v2_cru_ts4.01.",years[1],".",years[nyr],".",var,".abs4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.monthly.data <- function(var,yr){
  data <- get.data(var)
  df <- data.frame(var=numeric())
  for(m in 1:12){
    data_slice <- data[,,m,yr]
    na.values <- data_slice[!is.na(data_slice)]
    nadf <- data.frame(var=na.values)
	  df <- rbind(df, nadf)
	  #print(paste("getting values from",var,"in year", years[yr], "and month", m))
  }	
  colnames(df) <- var
  return(df)  
}

get.monthly.vector.by.cell <- function(var, yr, i){
  df <- get.monthly.data(var, yr)
  leap.year <- is.leap.year(years[yr])
  n.Feb <- ifelse(leap.year, 29, 28)
  n.days <- c(31, n.Feb, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  v <- vector()
  for(m in 1:12){
    if(var=="pre"){
      v[m] <- df[d*(m-1)+i,]/n.days[m]
    }else{
      v[m] <- df[d*(m-1)+i,]
    }
  } 
  return(v)
}

get.daily.vector.by.cell <- function(var, yr, i){
  v <- get.monthly.vector.by.cell(var, yr, i)
  leap.year <- is.leap.year(years[yr])
  n.days <- ifelse(leap.year, 366, 365)
  get.daily.from.monthly(v, n.days)
}

print("writing out data...")
for(yr in nyr:1){
  for(i in 1:d){
    col1 <- get.daily.vector.by.cell(vars[1], yr, i)/100
    col2 <- get.daily.vector.by.cell(vars[2], yr, i)
    col3 <- get.daily.vector.by.cell(vars[3], yr, i)
    df <- data.frame(cbind(col1, col2, col3))
    colnames(df) <- varnms
    outnm <- paste0("s",na10km.df$x[i], "_", na10km.df$y[i], "_", na10km.df$lat[i], "_", na10km.df$etopo1[i], "_", years[yr],".csv")
    write.csv(df, paste0(outcsvpath, outnm), row.names = FALSE)
  }
  print(paste("...got data from year", years[yr], "..."))
}

print("all done!")