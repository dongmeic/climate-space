# Created by Dongmei Chen
# combine yearly tables for bioclimatic values using Daymet daily data
# run in an interactive mode

if(0){
	library(parallel)
	library(doParallel)
	library(foreach)
	registerDoParallel(cores=28)
}

years <- 1996:2015
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables"

setwd(csvpath)

combine.table <- function(yr){
	data1 <- read.csv(paste0("daily_climate/Daymet/daymet_bioclim_var_", years[yr], ".csv"))
	data2 <- read.csv(paste0("daily_climate/Daymet/dm_bioclm_var_", years[yr], ".csv"))
	data <- cbind(data1, data2)
	write.csv(data, paste0("bioclim_values_Daymet_", years[yr], ".csv"), row.names = FALSE)
}

update.table <- function(yr){
  data1 <- read.csv(paste0("bioclim_values_Daymet_", years[yr], ".csv"))
	data2 <- read.csv(paste0("daily_climate/Daymet/dm_cv_gsp_", years[yr], ".csv"))
	data <- cbind(data1, data2)
	write.csv(data, paste0("bioclim_values_Daymet_", years[yr], ".csv"), row.names = FALSE)  
}

for(i in 1:length(years)){
  #combine.table(i)
  update.table(i)
  print(paste0("...got data for ", years[i], "..."))
}

print("all done!")