# Created by Dongmei Chen
library(ncdf4)
library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(latticeExtra)
library(gridExtra)
library(animation)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/plotlist.R")
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
#btl_ncfile <- paste0(ncpath, "na10km_v2_mpb_presence.nc")
btl_ncfile <- paste0(ncpath, "na10km_v2_mpb_presence_fishnet.nc")
ncin_btl <- nc_open(btl_ncfile)
print(ncin_btl)
btl <- ncvar_get(ncin_btl,"mpb_prs")
x <- ncvar_get(ncin_btl, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_btl, varid="y"); ny <- length(y)
nc_close(ncin_btl)

shppath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
canada.prov <- readOGR(dsn = shppath, layer = "na10km_can_prov")
us.states <- readOGR(dsn = shppath, layer = "na10km_us_state")
crs <- proj4string(us.states)
lrglakes <- readOGR(dsn = shppath, layer = "na10km_lrglakes")
proj4string(lrglakes) <- crs

hostpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/corehost"
corehost <- readOGR(dsn=hostpath, layer="MPB_corehost_proj_disall")
corehost <- spTransform(corehost, crs)

myColors <- c('grey', 'red')
myKey <- list(text=list(lab=c("North America","Beetle affected"), cex=c(1.2,1.2)), 
              rectangles=list(col = myColors), space="inside", width = 0.5, columns=1)
years <- 1997:2016

grid <- expand.grid(x=x, y=y)
for(i in 1:length(years)){
  btl_slice <- btl[,,i]
  p <- levelplot(btl_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,2000000),
                 par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F, 
                 col.regions=myColors, main=list(label=paste0("Beetle presence in ", toString(years[i])), cex=1.5), 
                 xlab="",ylab="", colorkey = FALSE, key=myKey)
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,1,0,0.5)))
  png(paste0(out,"beetle_presence_", years[i],".png"), width=4, height=8, units="in", res=300)
  print(p)
  dev.off()
  print(years[i])
}

im.convert("beetle_presence_*.png",output="beetle_presence.gif")

plotbtl <- function(i){
  btl_slice <- btl[,,i]
  p <- levelplot(btl_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
                 par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F, 
                 col.regions=myColors, main=list(label=toString(years[i]), cex=1.2), 
                 xlab="",ylab="", colorkey = FALSE, aspect="iso")
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  #p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,1,0,0.5)))
  print(p)
}

plots <- lapply(1:20, function(i) plotbtl(i))

png("composite_beetle_presence.png", width=8, height=10, units="in", res=300)
par(mfrow=c(4,5), xpd=FALSE, mar=rep(0.5,4))
print.plotlist(plots, layout=matrix(1:20, ncol=5, byrow=T))
dev.off()

print("all done!")