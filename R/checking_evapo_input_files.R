
rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")
d <- dim(rows)[1]
years <- 1991:2015; nyr <- length(years)
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/evapo_na/input"
csvpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/datatable_na/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
setwd(outcsvpath)

sink("missing_rows.txt")
source("/gpfs/projects/gavingrp/dongmeic/sdm/R/evapo_input_by_site_na.R")
tmean <- read.csv(paste0(csvpath, "tmean/tmean_", years[yr],".csv"))
prcp <- read.csv(paste0(csvpath, "prcp/prcp_", years[yr],".csv"))
for(row in rows$rows){
	i <- which(rows$rows==row)
	filename <- paste0("s",row, "_", rows$lat[i], "_", rows$etopo1[i], "_", years[yr],".csv")
	file <- file.path(years[yr],filename)
	if(file.exists(file)){
		next
	}else{		
		print(paste("missing row", row, "in the year of", years[yr]))
		evapo_input_by_site(i)
		print(paste("got data for", years[yr]))
	}
}

sink()
print("all done")