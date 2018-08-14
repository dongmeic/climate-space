# Created by Dongmei Chen

library(ncdf4)
library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(latticeExtra)
library(gridExtra)
library(RColorBrewer)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/plotlist.R")

years <- 1996:2015; nyr <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)

vargrp <- c("min30", "min32", "min34", "min36", "min38", "min40")
#vargrp <- c("min20", "min22", "min24", "min26", "min28")

ncin <- nc_open("/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/na10km_v2.nc")
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
grid <- expand.grid(x=x, y=y)

shppath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
canada.prov <- readOGR(dsn = shppath, layer = "na10km_can_prov")
us.states <- readOGR(dsn = shppath, layer = "na10km_us_state")
crs <- proj4string(us.states)
lrglakes <- readOGR(dsn = shppath, layer = "na10km_lrglakes")
proj4string(lrglakes) <- crs

cutpts <- c(0,10,20,30,40,50,60,70,80,90,100)

get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")

              
foreach(i=1:length(vargrp)) %dopar% {
  var_4d <- get.data(vargrp[i])
  plotclm <- function(yr){
	  var_4d_slice <- var_4d[,,1,yr]
		p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
			col.regions=brewer.pal(10,"RdBu"),
			par.settings = list(axis.line = list(col = "transparent")), 
			scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr]), cex=1.5),
			xlab="",ylab="")
	  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
  }
  plots <- lapply(1:20, function(i) plotclm(i))
  png(paste0("bioclimatic_maps_",vargrp[i],".png"), width=20, height=12, units="in", res=300)
  par(mfrow=c(4,5), xpd=FALSE, mar=rep(0.5,4))
  print.plotlist(plots, layout=matrix(1:20, ncol=5))
  dev.off()
}

for(i in 1:length(vargrp)){
  var_4d <- get.data(vargrp[i])
  for(j in 1:length(years)){
    var_4d_slice <- var_4d[,,1,j]
		p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
			col.regions=brewer.pal(10,"RdBu"),
			par.settings = list(axis.line = list(col = "transparent")), 
			scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[j]), cex=1.5),
			xlab="",ylab="")
    p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
    df <- btlprs[,c("x","y",paste0("prs_",(years[j]+1)))]
    coordinates(df) <- c("x","y")
    points2grid(df)
    btl_pixels <- as(df, "SpatialPixelsDataFrame")
    names(btl_pixels) <- "btlprs"
    p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
    png(paste0("bioclimatic_map_",vargrp[i],"_",years[j],".png"), width=9, height=8, units="in", res=300)
    print(p)
    dev.off()
    print(paste("mapping", vargrp[i], "in", years[j], "is done!"))
  }
}

foreach(i=1:length(vargrp)) %dopar% {
  im.convert(paste0("bioclimatic_map_",vargrp[i],"_*.png"),output=paste0("bioclimatic_map_",vargrp[i],".gif"))
}

print("all done!")