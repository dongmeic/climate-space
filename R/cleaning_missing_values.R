# remove missing values in the data table

path <- "/gpfs/projects/gavingrp/dongmeic/daymet"
setwd(path)
vars <- c('tmax', 'tmin', 'tmean', 'prcp')
years <- 1991:2015
rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	yr <- as.numeric(args[1])
	print(paste('year:', years[yr]))
}

ptm <- proc.time()
for(var in vars){
	df <- read.csv(paste0("datatable_na/",var,"/",var,"_",years[yr],".csv"))
	df.updated <- df[rows$rows,]
	write.csv(df.updated, paste0("datatable_na/",var,"/",var,"_",years[yr],".csv"), row.names = FALSE)
}

if(yr > 5){
	df <- read.csv(paste0("bioclm_na/dm_bioclm_var_",years[yr],"_na.csv"))
	df.updated <- df[rows$rows,]
	write.csv(df.updated, paste0("bioclm_na/dm_bioclm_var_",years[yr],"_na.csv"), row.names = FALSE)
}

if(yr > 4){
	df <- read.csv(paste0("datatable_na/vp/vp_",years[yr],".csv"))
	df.updated <- df[rows$rows,]
	write.csv(df.updated, paste0("datatable_na/vp/vp_",years[yr],".csv"), row.names = FALSE)	
}

print(paste("processed",years[yr],"..."))
proc.time() - ptm

print("all done!")