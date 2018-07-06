# Created by Dongmei Chen
# generate yearly tables for the departure of bioclimatic values

library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1996:2015; nyr <- length(years)
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(outcsvpath)

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

get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_std_1996.2015.4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,paste0(var,"_std"))
  fillvalue <- ncatt_get(ncin,paste0(var,"_std"),"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.dtcol <- function(var){
  ndf <- data.frame(var=double(), prs=character(), yrs=numeric())
  data <- get.data(var)
  for (yr in 1:nyr){
    na <- data[,,1,yr]
    na <- na[!is.na(na)]
    vgt <- data[,,2,yr]
    vgt <- vgt[!is.na(vgt)]
    btl <- data[,,3,yr]
    btl <- btl[!is.na(btl)]
    valcol <- c(na, vgt, btl)
    prs <- c(rep("continent",length(na)),rep("hosts",length(vgt)),rep("mpb",length(btl)))
    yrs <- c(rep(years[yr],length(prs)))
    df <- data.frame(valcol,prs,yrs)
    ndf <- rbind(ndf, df)
  }
  colnames(ndf)[1] <- var
  write.csv(ndf, paste0(outcsvpath, var, "_std_",years[1], "_",years[nyr], ".csv"), row.names = FALSE)
}

#foreach(i = 1:length(vargrp))%dopar%{
foreach(i = 1:2)%dopar%{
  get.dtcol(vargrp[i])
  print(paste("...got bioclimatic values from", vargrp[i], "..."))
}

print("all done")