# Created by Dongmei Chen
# Mapping: MPB grids in North America

library(maptools)
library(sp)
library(rgdal)
library(animation)
# library(parallel)
# library(doParallel)
# library(foreach)
# registerDoParallel(cores=28)


shapepath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/"
mpbshapepath <- "/gpfs/projects/gavingrp/dongmeic/beetle/mpbdata/reprojected"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)

na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
na10km_crs <- CRS(na10km_projstr)

shapefile = "na10km_bb"
bb_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(bb_shp) <- na10km_projstr
shapefile = "na10km_land"
land_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(land_shp) <- na10km_projstr
shapefile = "na10km_coast"
coast_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(coast_shp) <- na10km_projstr
shapefile = "na10km_lrglakes"
lrglakes_shp <- readOGR(dsn=shapepath,layer=shapefile)
proj4string(lrglakes_shp) <- na10km_projstr
shapefile = "na10km_can_prov"
can_shp <- readOGR(dsn=shapepath,layer=shapefile)
can_shp <- spTransform(can_shp, na10km_crs)
shapefile = "na10km_us_state"
us_shp <- readOGR(dsn=shapepath,layer=shapefile)
us_shp <- spTransform(us_shp, na10km_crs)
shapefile = "North_America_boundary"
na_shp <- readOGR(dsn=shapepath,layer=shapefile)
na_shp <- spTransform(na_shp, na10km_crs)

hostpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/corehost"
corehost <- readOGR(dsn=hostpath, layer="MPB_corehost_proj_disall")
corehost <- spTransform(corehost, na10km_crs)

# read a shaded relief file (from Pat Bartlein)
# http://geog.uoregon.edu/bartlein/courses/geog490/week07-RMaps2.html
datapath <- "/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/"
datafile <- "na10km_shade.csv"
shade <- read.csv(paste(datapath,datafile,sep=""))
shade$x <- shade$x*1000
shade$y <- shade$y*1000
head(shade)

coordinates(shade) <- c("x","y")
points2grid(shade)

shade_pixels <- as(shade, "SpatialPixelsDataFrame")
summary(shade_pixels)

colorfile <- "shade40_clr.csv"
shade_rgb <- read.csv(paste(datapath, colorfile, sep=""))
shade_clr <- rgb(shade_rgb)

shade_int <- as.integer(((shade$shade+1)/2)*40)+1
shade_colnum <- shade_clr[shade_int]

# pdf(file = "na_shade01b.pdf")
# plot(bb_shp, col="gray95")
# points(shade_pixels, pch=15, cex=0.09, col=shade_colnum)
# plot(can_shp, lwd=0.2, col="gray50", add=TRUE)
# plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
# plot(lrglakes_shp, lwd=0.2, bor="black", col="gray90", add=TRUE)
# plot(coast_shp, lwd=0.3, add=TRUE)
# text(-5770000, 4620000, pos=c(4), offset=0.0, cex=1.0, "NA10km_v2 -- 10m Natural Earth Outlines")
# plot(bb_shp, add=TRUE)
# dev.off()

btl_csvfile <- "na10km_presence_details_all.csv"
csvin_btl <- read.csv(paste0(csvpath, btl_csvfile))
head(csvin_btl)

# foreach(yr=1997:2016) %dopar% {
#   #pdf(file=paste0(out,"map_of_mpb_", toString(yr),".pdf"))
#   png(file=paste0(out,"map_of_mpb_", toString(yr),".png"),width=9, height=8, units="in", res=300)
#   par(mar=c(0.1,0.1,2,0.1))
#   plot(bb_shp, col="gray95")
#   points(shade_pixels, pch=15, cex=0.09, col=shade_colnum)
#   title(paste("Mountain Pine Beetles in North America -", toString(yr)),line=0, cex.main=1.5)
#   plot(can_shp, lwd=0.2, col="gray50", add=TRUE)
#   plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
#   plot(lrglakes_shp, lwd=0.2, bor="black", col="gray90", add=TRUE)
#   plot(coast_shp, lwd=0.3, add=TRUE)
#   plot(corehost,bord="lightgreen", add=TRUE)
#   if (yr > 2000){
#     shapefile <- paste0("ab_mpb_points_",toString(yr))
#     ab_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#     plot(ab_shp, pch=19,cex=0.2, col="red", add=TRUE)
#   }
#   shapefile <- paste0("bc_mpb_points_",toString(yr))
#   bc_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#   plot(bc_shp, pch=19,cex=0.2, col="red", add=TRUE)
#   shapefile <- paste0("bc_mpb_poly_",toString(yr))
#   bc_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#   bc_shp <- spTransform(bc_shp, na10km_crs)
#   plot(bc_shp, col="red",bord="red", add=TRUE)
#   shapefile <- paste0("us_mpb_",toString(yr))
#   usb_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#   plot(usb_shp,col="red",bord="red", add=TRUE)
#   plot(bb_shp, add=TRUE)
#   dev.off()
#   print(paste("mapping mpb in",yr,"done!"))
# }

# foreach(yr=1997:2016)%dopar%{
#   #pdf(file=paste0(out,"map_of_mpb_", yr,".pdf"))
#   png(file=paste0(out,"map_of_mpb_", yr,".png"),width=9, height=8, units="in", res=300)
#   par(mar=c(0.1,0.1,2,0.1))
#   plot(bb_shp, col="gray95")
#   points(shade_pixels, pch=19, cex=0.1, col=shade_colnum)
#   title(paste("Mountain Pine Beetles in North America -",yr),line=0, cex.main=1.5)
#   plot(can_shp, lwd=0.2, col="gray50", add=TRUE)
#   plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
#   plot(lrglakes_shp, lwd=0.2, bor="black", col="gray90", add=TRUE)
#   plot(coast_shp, lwd=0.3, add=TRUE)
#   plot(corehost,bord=rgb(0,0.6,0,0.3), add=TRUE)
#   df <- csvin_btl[,c("x","y",paste0("prs_", yr))]
#   coordinates(df) <- c("x","y")
#   points2grid(df)
#   btl_pixels <- as(df, "SpatialPixelsDataFrame")
#   points(btl_pixels[btl_pixels@data[,paste0("prs_", yr)]==1,], pch=19, cex=0.1, col=rgb(1,0,0,0.1))
#   plot(bb_shp, add=TRUE)
#   dev.off()
#   print(paste("mapping mpb in",yr,"done!"))
# }
# 
# im.convert(paste(out,"map_of_mpb_*.png", sep=""),output=paste(out,"maps_of_mpb.gif"))

# pdf(file=paste0(out,"map_of_mpb.pdf"))
# png(file=paste0(out,"map_of_mpb.png"),width=9, height=8, units="in", res=300)
# par(mar=c(0.1,0.1,2,0.1))
# plot(bb_shp, col="gray95")
# points(shade_pixels, pch=15, cex=0.09, col=shade_colnum)
# title(paste("Mountain Pine Beetles in North America (1997 - 2016)"),line=0, cex.main=1.5)
# plot(can_shp, lwd=0.2, col="gray50", add=TRUE)
# plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
# plot(lrglakes_shp, lwd=0.2, bor="black", col="gray90", add=TRUE)
# plot(coast_shp, lwd=0.3, add=TRUE)
# plot(corehost,bord="lightgreen", add=TRUE)
# foreach(yr=1997:2016)%dopar%{
#   if (yr > 2000){
#     shapefile <- paste0("ab_mpb_points_",toString(yr))
#     ab_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#     plot(ab_shp, pch=19,cex=0.2, col="red", add=TRUE)
#   }
#   shapefile <- paste0("bc_mpb_points_",toString(yr))
#   bc_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#   plot(bc_shp, pch=19,cex=0.2, col="red", add=TRUE)
#   shapefile <- paste0("bc_mpb_poly_",toString(yr))
#   bc_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#   plot(bc_shp, col="red",bord="red", add=TRUE)
#   shapefile <- paste0("us_mpb_",toString(yr))
#   usb_shp <- readOGR(dsn=mpbshapepath, layer=shapefile)
#   plot(usb_shp,col="red",bord="red", add=TRUE)
#   print(paste("mapping mpb in",yr))
# }
# plot(bb_shp, add=TRUE)
# dev.off()

#pdf(file=paste0(out,"map_of_mpb.pdf"))
png(file=paste0(out,"map_of_mpb.png"),width=9, height=8, units="in", res=300)
par(mar=c(0,0,0,0))
plot(bb_shp, col="gray95")
points(shade_pixels, pch=19, cex=0.1, col=shade_colnum)
#title(paste("Mountain pine beetle presence in North America (1997 - 2016)"),line=0, cex.main=1.5)
plot(can_shp, lwd=0.2, col="gray50", add=TRUE)
plot(us_shp, lwd=0.2, col="gray50", add=TRUE)
plot(lrglakes_shp, lwd=0.2, bor="black", col="lightblue", add=TRUE)
plot(na_shp, add=TRUE)
#plot(coast_shp, lwd=0.3, add=TRUE)
plot(corehost,bord=rgb(0,0.6,0,1), add=TRUE)
df <- csvin_btl[,c("x","y","allyears")]
coordinates(df) <- c("x","y")
points2grid(df)
btl_pixels <- as(df, "SpatialPixelsDataFrame")
points(btl_pixels[btl_pixels$allyears==1,], pch=19, cex=0.1, col=rgb(0.8,0,0,0.2))
plot(bb_shp, add=TRUE)
legend(-5000000, -2200000, bty="n", pch=0, cex=1.2, legend="Study area")
legend(-5000000, -2600000, bty="n", pch=15, col="lightblue", cex=1.2, legend="Large lakes")
legend(-5000000, -3000000, bty="n", pch=15, col=rgb(0.8,0,0,0.8), cex=1.2, legend="MPB outbreak range")
legend(-5000000, -3400000, bty="n", pch=0, col=rgb(0,0.6,0,1), cex=1.2, legend="Core host range")
dev.off()
