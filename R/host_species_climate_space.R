library(ncdf4)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/data_transform.R")

library(RColorBrewer)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlstat <- read.csv(paste0(csvpath, "beetle_presence_statistics.csv"))
btlstat <- btlstat[rows$rows,]
indata <- get_data()
indata$hosts <- ifelse(indata$beetles==1 & indata$hosts==0, 1, indata$hosts)
indata$prs <- indata$beetles + indata$hosts

ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
ncfile <- paste0(ncpath, "na10km_v2_presence_pines.nc")
ncin_vgt <- nc_open(ncfile)
print(ncin_vgt)
