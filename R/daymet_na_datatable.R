library(raster)
library(rgdal)
library(ncdf4)

inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/ncfiles_na/"
outpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/datatable_na"
setwd(outpath)

#na10km.shp <- readOGR("/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/na10km_mask_pts.shp")
na10km.shp <- readOGR("/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/daymet_na10km.shp")
na10km <- "+proj=laea +lat_0=50 +lon_0=-100 +x_0=0 +y_0=0 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"
vars <- c('tmax', 'tmin', 'tmean', 'prcp')
years <- 1991:2015
yeardays <- 1:365

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
	df <- as.data.frame(matrix(,ncol=0,nrow=277910))
	if(var != "tmean"){
		ncfile <- paste0(inpath, years[yr], "/daymet_v3_", var, "_", years[yr], "_na.nc4")
		for (day in yeardays){
			r <- raster(ncfile, band=day)
			r1 <- aggregate(r, fact=10, fun=mean)
			r2 <- projectRaster(r1, crs = na10km)
			vals <- extract(r2, na10km.shp, df=TRUE)
			df <- cbind(df, vals[,2])
			names(df)[dim(df)[2]] <- day
		}
	}else{
		tmax.file <- paste0(inpath, years[yr], "/daymet_v3_tmax_", years[yr], "_na.nc4")
		tmin.file <- paste0(inpath, years[yr], "/daymet_v3_tmin_", years[yr], "_na.nc4")
		for (day in yeardays){
			tmax.r <- raster(tmax.file, band=day)
			tmin.r <- raster(tmin.file, band=day)
			r <- (tmax.r + tmin.r)/2
			r1 <- aggregate(r, fact=10, fun=mean)
			r2 <- projectRaster(r1, crs = na10km)
			vals <- extract(r2, na10km.shp, df=TRUE)
			df <- cbind(df, vals[,2])
			names(df)[dim(df)[2]] <- day
		}
	}
	dir.create(file.path(var), showWarnings = FALSE)
	write.csv(df, paste0(var,"/",var,"_",years[yr],".csv"), row.names = FALSE)
}
print(paste("processed",years[yr],"..."))
proc.time() - ptm
