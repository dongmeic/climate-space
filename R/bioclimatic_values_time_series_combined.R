# Created by Dongmei Chen
# combine yearly tables for bioclimatic values using monthly and daily data

library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1907:2016
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"

vars <- c("JanTmin", "MarTmin", "TMarAug", "summerTmean", 
				"AugTmean", "AugTmax", "GSP", "PMarAug", "summerP0",
				"OctTmin", "fallTmean", "winterTmin", "Tmin", "Tmean", 
				"Tvar", "TOctSep", "summerP1", "summerP2", "Pmean",
				"POctSep", "PcumOctSep", "PPT", "ddAugJul", "ddAugJun")

setwd(csvpath)

# first round
combine.table <- function(yr){
	data1 <- read.csv(paste0("bioclimatic_values_", years[yr], ".csv"))
	data2 <- read.csv(paste0("bioclimatic_values_", years[yr], "_daily.csv"))
	data <- cbind(data1, data2)
	data <- data[,vars]
	write.csv(data, paste0("bioclimatic_values_", years[yr], ".csv"), row.names = FALSE)
}

# second round
update.table <- function(yr){
  data <- read.csv(paste0("bioclimatic_values_", years[yr], ".csv"))
	data <- data[,vars]
	write.csv(data, paste0("bioclimatic_values_", years[yr], ".csv"), row.names = FALSE)  
}

foreach(i=1:length(years))%dopar%{
  #combine.table(i)
  update.table(i)
  print(paste0("...got data for ", years[i], "..."))
}

print("all done!")