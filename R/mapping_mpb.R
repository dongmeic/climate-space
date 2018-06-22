# Created by Dongmei Chen
# Mapping: MPB grids in North America
# There are issues in bash script running and "im.convert"

library(rgdal)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

shapepath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
mpbshapepath <- "/gpfs/projects/gavingrp/dongmeic/beetle/mpbdata/reprojected"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"

na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)

shapefile = "na10km_bb"
bb_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(bb_shp) <- na10km_projstr
shapefile = "na10km_land"
land_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(land_shp) <- na10km_projstr
shapefile = "na10km_lrglakes"
lrglakes_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(lrglakes_shp) <- na10km_projstr
shapefile = "na10km_can_prov"
can_shp <- readOGR(dsn=shapepath,layer=shapefile)
can_shp <- spTransform(can_shp, na10km_crs)
shapefile = "na10km_us_state"
us_shp <- readOGR(dsn=shapepath,layer=shapefile)
us_shp <- spTransform(us_shp, na10km_crs)


foreach(yr=1997:2016) %dopar% {
	png(file=paste(out,"map_of_mpb_", toString(yr),".png",sep=""),width=9, height=8, units="in", res=300)
	par(mar=c(0.1,0.1,2,0.1))
	plot(bb_shp, col="white", bor="black")
	title(paste("Mountain Pine Beetle in North America -", toString(yr)),line=0, cex.main=1.5)
	plot(land_shp, col="gray90", bor="black", add=TRUE)
	plot(lrglakes_shp, col="lightblue", bor="blue", add=TRUE)
	plot(can_shp, bor="black", add=TRUE)
	plot(us_shp, bor="black", add=TRUE)
	if (yr > 2000){
		shapefile <- paste0("ab_mpb_points_",toString(yr))
		ab_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
		plot(ab_shp, pch=19,cex=0.2, col="red", add=TRUE)
	}
	shapefile <- paste0("bc_mpb_points_",toString(yr))
	bc_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
	plot(bc_shp, pch=19,cex=0.2, col="red", add=TRUE)
	shapefile <- paste0("bc_mpb_poly_",toString(yr))
	bc_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
	plot(bc_shp, col="red",bord="red", add=TRUE)
	shapefile <- paste0("us_mpb_",toString(yr))
	usb_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
	plot(usb_shp,col="red",bord="red", add=TRUE)
	dev.off()
}

#im.convert(paste(out,"map_of_mpb_*.png", sep=""),output=paste(out,"maps_of_mpb.gif"))

