# Dongmei Chen
# run in interactive mode

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables"
setwd(inpath)

update_DD <- function(year, dataset){
	if(dataset=="CRU"){
		indata <- read.csv(paste0("daily_climate/CRU/bioclimatic_variables_daily_", year, ".csv")) 
		newindata <- read.csv(paste0("daily_climate/CRU/degree_days_CRU_", year, ".csv"))
		indata$ddAugJul <- newindata$thres5.5all
	  indata$ddAugJun <- newindata$thres5.5aug.jun
	  write.csv(indata, paste0("daily_climate/CRU/CRU_bioclim_var_", year, ".csv"), row.names = FALSE)	
	}else if(dataset=="Daymet"){
		indata <- read.csv(paste0("daily_climate/Daymet/daymet_bioclimatic_variables_", year, ".csv")) 
		newindata <- read.csv(paste0("daily_climate/Daymet/degree_days_daymet_", year, ".csv"))
		indata$ddAugJul <- newindata$thres5.5all
	  indata$ddAugJun <- newindata$thres5.5aug.jun
	  write.csv(indata, paste0("daily_climate/Daymet/daymet_bioclim_var_", year, ".csv"), row.names = FALSE)
	}
}

for(year in 1996:2015){
	update_DD(year, "CRU")
	#update_DD(year, "Daymet")
	print(year)
}

print("all done")