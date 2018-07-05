# Created by Dongmei Chen
# modified from bioclimatic_variables_maps.R

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

years <- 1996:2015; nyr <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)

vargrp <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
			"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax", 
			 "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean", 
			 "GSP", "POctSep", "PMarAug")

print.plotlist<-function(xx, layout=matrix(1:length(xx), nrow=1), more=F) {
  lty<-NULL
  if ( is.matrix(layout) ) {
    lyt <- layout
    col.widths <- rep.int(1, ncol(lyt))
    row.heights <- rep.int(1, nrow(lyt))
  } else if ( is.list(layout) ) {
    stopifnot(class(layout[[1]]) == "matrix")
    lyt <- layout[[1]]
    col.widths <- if (!is.null(layout$widths)) layout$widths else rep.int(1, ncol(lyt))
    row.heights <- if (!is.null(layout$heights)) layout$heights else rep.int(1, nrow(lyt))
  }
  stopifnot(length(col.widths)==ncol(lty))
  stopifnot(length(row.heights)==nrow(lty))
  maxi <- max(lyt)
  col.pts <- cumsum(c(0, col.widths))/sum(col.widths)
  row.pts <- rev(cumsum(c(0, rev(row.heights)))/sum(row.heights))
  for(i in 1:length(xx)) {
    j <-((i-1)%%maxi)+1
    wch <- which(lyt==j, arr.ind=T)
    stopifnot(nrow(wch)>0)
    pos <- apply(wch,2,range)
    ps <- c(col.pts[pos[1,2]], row.pts[pos[2,1]+1], col.pts[pos[2,2]+1],row.pts[pos[1,1]])
    print(
      xx[[i]], 
      position = ps,
      #split=c(rev(which(lyt==j, arr.ind=T)),rev(dim(lyt))),
      more=ifelse(j != maxi & i<length(xx), T, more)
    )
  }
  invisible(F)
}

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

cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)

get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_std_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,paste0(var,"_std"))
  fillvalue <- ncatt_get(ncin,paste0(var,"_std"),"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")

#foreach(i=1:length(vargrp)) %dopar% {
foreach(i=c(4,5)) %dopar% {
    var_4d <- get.data(vargrp[i])
    plotclm <- function(yr){
      var_4d_slice <- var_4d[,,1,yr]
	  if(i<=13){
		 p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr]), cex=1.5),
			  xlab="",ylab="")
	  }else{
		 p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
			  col.regions=brewer.pal(10,"RdYlGn"),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr]), cex=1.5),
			  xlab="",ylab="")
	  }
      p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
      p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
      p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
      print(p)
    }
    plots <- lapply(1:20, function(i) plotclm(i))
    png(paste0("bioclimatic_departure_maps_",vargrp[i],".png"), width=20, height=12, units="in", res=300)
    par(mfrow=c(4,5), xpd=FALSE, mar=rep(0.5,4))
    print.plotlist(plots, layout=matrix(1:20, ncol=5))
    dev.off()
  }

#for(i in 1:length(vargrp)){
for(i in c(4,5)){
  var_4d <- get.data(vargrp[i])
  for(j in 1:length(years)){
    var_4d_slice <- var_4d[,,1,j]
    if(i<=13){
	  p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[j]), cex=1.5),
			  xlab="",ylab="")
	}else{
	  p <- levelplot(var_4d_slice ~ x * y, data=grid, at=d, cuts=11, pretty=T, 
			  col.regions=brewer.pal(10,"RdYlGn"),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[j]), cex=1.5),
			  xlab="",ylab="")
	}
    p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
    df <- btlprs[,c("x","y",paste0("prs_",(years[j]+1)))]
    coordinates(df) <- c("x","y")
    points2grid(df)
    btl_pixels <- as(df, "SpatialPixelsDataFrame")
    names(btl_pixels) <- "btlprs"
    if(i<=13){
      p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
    }else{
      p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='blue', alpha=0.4))
    }
    png(paste0("bioclimatic_departure_map_",vargrp[i],"_",years[j],".png"), width=9, height=8, units="in", res=300)
    print(p)
    dev.off()
    print(paste("mapping", vargrp[i], "in", years[j], "is done!"))
  }
}

#foreach(i=1:length(vargrp)) %dopar% {
foreach(i=c(4,5)) %dopar% {
  im.convert(paste0("bioclimatic_departure_map_",vargrp[i],"_*.png"),output=paste0("bioclimatic_departure_map_",vargrp[i],".gif"))
}

print("all done!")