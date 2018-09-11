# Created by Dongmei Chen
library(ncdf4)
library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(latticeExtra)
library(gridExtra)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/plotlist.R")
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
ncfile <- paste0(ncpath, "na10km_v2_presence_beetle_statistics.nc")
varnms <- c("maxprs", "maxabs", "prsyr", "absyr", "meanprs", "ngbyrs")
ncin <- nc_open(ncfile)
print(ncin)
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
grid <- expand.grid(x=x, y=y)

shppath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
canada.prov <- readOGR(dsn = shppath, layer = "na10km_can_prov")
us.states <- readOGR(dsn = shppath, layer = "na10km_us_state")
crs <- proj4string(us.states)
lrglakes <- readOGR(dsn = shppath, layer = "na10km_lrglakes")
proj4string(lrglakes) <- crs

hostpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/corehost"
corehost <- readOGR(dsn=hostpath, layer="MPB_corehost_proj_disall")
corehost <- spTransform(corehost, crs)
labels <- c("Maximum continuous presence", "Maximum continuous absence", 
						"Presence year after absence", "Absence year after presence", 
						"Mean of presence length", "Neighboring presence")
plots <- list()
#for(i in 1:length(varnms)){
for(i in c(1,2,6)){
  var <- ncvar_get(ncin,varnms[i])
  if(i==1 | i==2){
    cutpts <- seq(1,20,by=2)
    p <- levelplot(var ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,2000000),
               par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), 
               margin=F, at=cutpts, cuts=10, pretty=T, col.regions=rev(brewer.pal(10,"RdBu")), 
               main=list(label=labels[i], cex=0.6), xlab="",ylab="", aspect="iso")
  #}else if(i==3 | i==4){
  # cutpts <- c(1999,2001,2003,2005,2006,2007,2009,2011,2013,2015,2016)
  #  p <- levelplot(var ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,2000000),
  #             par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), 
  #             margin=F, cuts=11, pretty=T,col.regions=brewer.pal(11,"Spectral"),
  #             main=list(label=varnms[i], cex=1.5), xlab="", ylab="", aspect="iso")
  }else if(i==6){
    cutpts <- seq(0,180,by=20)
    p <- levelplot(var ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,2000000),
               par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), 
               margin=F, at=cutpts, cuts=10, pretty=T, col.regions=rev(brewer.pal(10,"RdBu")), 
               main=list(label=labels[i], cex=0.6), xlab="",ylab="", aspect="iso")
  }
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  #p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,1,0,0.3)))
  plots[[i]] <- p
  #png(paste0("beetle_presence_stat_", varnms[i],".png"), width=5, height=7, units="in", res=300)
  #print(p)
  #dev.off()
  print(varnms[i])
}

#png("composite_beetle_presence_stat.png", width=11, height=9, units="in", res=300)
png("beetle_presence_stats.png", width=6, height=3, units="in", res=300)
par(mfrow=c(1,3), xpd=FALSE, mar=rep(0.5,4))
print.plotlist(plots, layout=matrix(1:3, ncol=3))
dev.off()

print("all done!")