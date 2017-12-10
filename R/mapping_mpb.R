# Created by Dongmei Chen
# Mapping: MPB grids in North America

library(maptools)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=12)

# editted from 
shapepath <- "/home2/dongmeic/beetle/shapefiles/"
mpbshapepath <- "/home2/dongmeic/beetle/mpbdata/reprojected/"
out <- "/home2/dongmeic/beetle/output/maps/"

na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)

shapefile = "na10km_bb.shp"
bb_shp <- readShapePoly(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
shapefile = "na10km_land.shp"
land_shp <- readShapePoly(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
shapefile = "na10km_lrglakes.shp"
lrglakes_shp <- readShapePoly(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
shapefile = "na10km_can_prov.shp"
can_shp <- readShapeLines(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)
shapefile = "na10km_us_state.shp"
us_shp <- readShapeLines(paste(shapepath,shapefile,sep=""), proj4string=na10km_crs)

# foreach(yr=1996:1997) %dopar% {
#   png(file=paste(out,"map_of_mpb_", toString(yr),".png",sep=""),width=10, height=8, units="in", res=300)
#   par(mar=c(0.1,0.1,2,0.1))
#   plot(bb_shp, col="white", bor="black", main = paste("Mountain Pine Beetle in North America -", toString(yr)))
#   plot(land_shp, col="gray90", bor="black", add=TRUE)
#   plot(lrglakes_shp, col="lightblue", bor="blue", add=TRUE)
#   plot(can_shp, bor="black", add=TRUE)
#   plot(us_shp, bor="black", add=TRUE)
#   shapefile <- paste(mpbshapepath,"us_mpb_",toString(yr+1),".shp",sep="")
#   usb_shp <- readShapePoly(shapefile, proj4string=na10km_crs)
#   plot(usb_shp, col="red", bor="red",add=TRUE)
#   dev.off()
# }
# 
# foreach(yr=1998:1999) %dopar% {
#   png(file=paste(out,"map_of_mpb_", toString(yr),".png",sep=""),width=10, height=8, units="in", res=300)
#   par(mar=c(0.1,0.1,2,0.1))
#   plot(bb_shp, col="white", bor="black", main = paste("Mountain Pine Beetle in North America -", toString(yr)))
#   plot(land_shp, col="gray90", bor="black", add=TRUE)
#   plot(lrglakes_shp, col="lightblue", bor="blue", add=TRUE)
#   plot(can_shp, bor="black", add=TRUE)
#   plot(us_shp, bor="black", add=TRUE)
#   shapefile <- paste(mpbshapepath,"bc_mpb_points_",toString(yr+1),".shp",sep="")
#   bc_shp <- readShapePoints(shapefile, proj4string=na10km_crs)
#   plot(bc_shp, pch=19,cex=0.2,col="red", add=TRUE)
#   shapefile <- paste(mpbshapepath,"us_mpb_",toString(yr+1),".shp",sep="")
#   usb_shp <- readShapePoly(shapefile, proj4string=na10km_crs)
#   plot(usb_shp, col="red", bor="red",add=TRUE)
#   dev.off()
# }

foreach(yr=2000:2014) %dopar% {
	png(file=paste(out,"map_of_mpb_", toString(yr),".png",sep=""),width=9, height=8, units="in", res=300)
	par(mar=c(0.1,0.1,2,0.1))
	plot(bb_shp, col="white", bor="black")
	title(paste("Mountain Pine Beetle in North America -", toString(yr)),line=0, cex.main=1.5)
	plot(land_shp, col="gray90", bor="black", add=TRUE)
	plot(lrglakes_shp, col="lightblue", bor="blue", add=TRUE)
	plot(can_shp, bor="black", add=TRUE)
	plot(us_shp, bor="black", add=TRUE)
	shapefile <- paste(mpbshapepath,"ab_mpb_points_",toString(yr+1),".shp",sep="")
	ab_shp <- readShapePoints(shapefile, proj4string=na10km_crs)
	plot(ab_shp, pch=19,cex=0.2, col="red", add=TRUE)
	shapefile <- paste(mpbshapepath,"bc_mpb_points_",toString(yr+1),".shp",sep="")
	bc_shp <- readShapePoints(shapefile, proj4string=na10km_crs)
	plot(bc_shp, pch=19,cex=0.2, col="red", add=TRUE)
	shapefile <- paste(mpbshapepath,"us_mpb_",toString(yr+1),".shp",sep="")
	usb_shp <- readShapePoly(shapefile, proj4string=na10km_crs)
	plot(usb_shp,col="red",bord="red", add=TRUE)
	dev.off()
}

im.convert(paste(out,"map_of_mpb_*.png", sep=""),output=paste(out,"maps_of_mpb.gif"))

