# Created by Dongmei Chen
# combine yearly tables for bioclimatic values using monthly and daily data

library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1996:2015
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"

setwd(csvpath)

# first round
combine.table <- function(yr){
	data1 <- read.csv(paste0("bioclimatic_values_", years[yr], ".csv"))
	data2 <- read.csv(paste0("daily_climate/CRU/CRU_bioclim_var_", years[yr], ".csv"))
	data <- cbind(data1, data2)
	write.csv(data, paste0("bioclim_values_CRU_", years[yr], ".csv"), row.names = FALSE)
}

# second round
update.table <- function(yr){
  data <- read.csv(paste0("bioclimatic_values_", years[yr], ".csv"))
	data$ddAugJul <- data$ddAugJul.1
	data$ddAugJun <- data$ddAugJun.1
	data <- data[,1:43]
	write.csv(data, paste0("bioclim_values_CRU_", years[yr], ".csv"), row.names = FALSE)  
}

for(i in 1:length(years)){
  combine.table(i)
  #update.table(i)
  print(paste0("...got data for ", years[i], "..."))
}

print("all done!")