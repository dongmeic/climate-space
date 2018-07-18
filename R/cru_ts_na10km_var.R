library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

# functions
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/cru_regrid_na10km.R")

varnames <- c("pet", "sun", "vap")
varlnames <- c("potential evapotranspiration", "percent possible sunshine", "vapour pressure")
varunits <- c("mm/day", "percentage", "hPa")

foreach(i=1:length(varnames))%dopar%{
  print(paste("processing", varnames[i]))
  print(paste("getting anomalies for", varnames[i]))
  cru_ts_ltms_anomalies(varnames[i], varlnames[i], varunits[i])
  print(paste("getting interpolated anomalies for", varnames[i]))
  cru_ts_regrid_anomalies(varnames[i])
  print(paste("getting absolute values for", varnames[i]))
  cru_ts_regrid_abs(varnames[i], varlnames[i])
  print(paste("got", varnames[i], "from cru to na10km..."))
}

print("all done!")