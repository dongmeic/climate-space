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

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/plotlist.R")
years <- 1998:2016; nyr <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
#ncfile <- "time_series_presence_statistics.nc"
ncfile <- "time_series_alteration_statistics.nc"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)

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

hostpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles/corehost"
corehost <- readOGR(dsn=hostpath, layer="MPB_corehost_proj_disall")
corehost <- spTransform(corehost, crs)

# yearly transitions (0-stay the same as 0, 1-stay the same as 1; 2-from 1 to 0; 3-from 0 to 1)
#myColors <- c('#377eb8', '#e41a1c', '#4daf4a', '#984ea3')
myColors <- c('#abd9e9', '#d7191c', '#2c7bb6', '#fdae61')
myKey <- list(text=list(lab=c("00","01","10","11"), cex=c(1.2,1.2)), 
              rectangles=list(col = myColors), space="inside", width = 0.3, columns=1)

ncin <- nc_open(paste0(ncpath, ncfile))
var <- "prs_alt"
var3d <- ncvar_get(ncin,var)
fillvalue <- ncatt_get(ncin,var,"_FillValue")$value
var3d[var3d==fillvalue] <- NA
# mask out the locations with all zeros over years
m <- apply(var3d, c(1,2), sum)
m[m==0] <- NA
var3d[is.na(m)] <- NA

for(i in 1:length(years)){
  alt_slice <- var3d[,,i]
  p <- levelplot(alt_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
                 par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F, 
                 col.regions=myColors, main=list(label=paste0("Alteration in ", toString(years[i])), cex=1.5), 
                 xlab="", ylab="", colorkey = FALSE, key=myKey)
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,0.3,0,0.5)))
  png(paste0(out,"presence_alteration_", years[i],".png"), width=4, height=8, units="in", res=300)
  print(p)
  dev.off()
  print(years[i])
}

im.convert("presence_alteration_*.png",output="presence_alteration_ts.gif")

plotalt <- function(i){
  alt_slice <- var3d[,,i]
  p <- levelplot(alt_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
                 par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F, 
                 col.regions=myColors, main=list(label=toString(years[i]), cex=1.0), 
                 xlab="",ylab="", colorkey = FALSE, aspect="iso")
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  #p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,0.3,0,0.5)))
  print(p)
}

plots <- lapply(1:nyr, function(i) plotalt(i))
empty.plot <- function(){
	#plot(0,type='n',axes=FALSE,ann=FALSE)
	openplotmat()
  legend('center', fill=myColors, legend=c("From absence to absence", "From absence to presence",
                                           "From presence to absence", "From presence to presence"), cex = 1.5, bty='n')
}
plots[[20]] <- xyplot(1:10~1:10, par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE),
											xlab="",ylab="",col="white", key=list(corner=c(0.5,0.5), columns=1, rectangles=list(col = myColors), between=1,
  										text=list(lab=c("From absence to absence", "From absence to presence", "From presence to absence", "From presence to presence"),cex=0.4)))

png("composite_presence_alteration.png", width=8, height=10, units="in", res=300)
par(mfrow=c(4,5), xpd=FALSE, mar=rep(0.5,4))
print.plotlist(plots, layout=matrix(1:20, ncol=5, byrow=T))
dev.off()

print("all done!")