# get yearly daily vapor pressure by site from Daymet North America

outpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/vpd_na"
setwd(outpath)

rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")
d <- dim(rows)[1]
years <- 1991:2015; nyr <- length(years)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/datatable_na/"

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	yr <- as.numeric(args[1])
	print(paste('year:', years[yr]))
}

ifelse(!dir.exists(file.path(years[yr])), dir.create(file.path(years[yr])), FALSE)

var <- 'vp'

vp <- read.csv(paste0(csvpath, "vp/vp_", years[yr],".csv"))
get.daymet.daily.vector.by.cell <- function(var,i){
	as.numeric(vp[i,])
}

print("writing out data...")

for(i in 1:d){
	col <- get.daymet.daily.vector.by.cell(var,i)
	if(sum(is.na(col))!= 0){
		next
	}else{
		df <- data.frame(vp=col)
		outnm <- paste0("vp",rows$rows[i], "_", rows$lat[i], "_", rows$etopo1[i], "_", years[yr],".csv")
		write.csv(df, file.path(years[yr],outnm), row.names = FALSE)
	}
}
print(paste("...got data from year", years[yr], "..."))

print("all done!")