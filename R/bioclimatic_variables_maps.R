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

years <- 1997:2016
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

#OctTmin
cutpts <- cbind(c(-45,-25,-15,-5,0,5,10,15,20,25,30),
#fallTmean
c(-45,-25,-15,-5,0,5,10,15,20,25,30),
#winterTmin
c(-50,-40,-30,-20,-15,-10,-5,0,5,15,30),
#JanTmin
c(-40,-30,-20,-10,-5,0,5,10,15,25,30),
#MarTmin
c(-40,-30,-20,-15,-10,-5,0,5,10,20,30),
#Tmin
c(-50,-30,-15,-10,-5,0,5,10,15,25,35),
#Tmean
c(-35,-10,-5,0,5,10,15,20,25,30,35),
#Tvar
c(0,2,4,6,7,8,9,10,12,14,16),
#TOctSep
c(-35,-25,-15,-5,0,5,10,15,20,25,30),
#TMarAug
c(-25,-10,-5,0,5,10,12,15,20,25,30),
#summerTmean
c(-20,-10,-5,0,5,10,15,20,25,30,35),
#AugTmean
c(-20,-10,-5,0,5,10,15,20,25,30,35),
#AugTmax
c(-20,-10,0,5,10,15,20,25,30,35,45),
#PcumOctSep
c(0,100,300,500,800,1000,1200,1500,2000,6000,10000),
#summerP0
c(0,30,50,80,100,150,250,500,1000,2000,3000),
#summerP1
c(0,30,50,80,100,150,250,500,1000,2000,3000),
#summerP2
c(0,50,100,200,300,500,600,800,1000,2000,3500),
#PPT
c(0,500,1000,1500,2000,2500,3000,4000,6000,8000,30000),
#Pmean 
c(0,10,20,30,40,50,80,100,200,400,800),
#GSP
c(0,25,50,100,150,200,250,300,500,1000,2000),
#POctSep
c(0,100,200,300,500,800,1000,1500,2000,4000,8000),
#PMarAug
c(0,50,100,200,300,400,600,800,1000,2000,5000))

get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_1997.2016.4d.nc")
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
	  if(i<=13){
		 p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr]), cex=1.5),
			  xlab="",ylab="")
	  }else{
		 p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			  col.regions=brewer.pal(10,"RdYlGn"),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr]), cex=1.5),
			  xlab="",ylab="")
	  }
	  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
	  # df <- btlprs[,c("x","y",paste0("prs_",years[yr]))]
	  # coordinates(df) <- c("x","y")
	  # points2grid(df)
	  # btl_pixels <- as(df, "SpatialPixelsDataFrame")
	  # names(btl_pixels) <- "btlprs"	  
	  # p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='gray', alpha=0.5))
	  #png(paste0("climate_map_", years[yr],".png"), width=9, height=8, units="in", res=300)
	  print(p)
	  #dev.off()
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
    if(i<=13){
	  p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[j]), cex=1.5),
			  xlab="",ylab="")
	}else{
	  p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			  col.regions=brewer.pal(10,"RdYlGn"),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[j]), cex=1.5),
			  xlab="",ylab="")
	}
    p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
    df <- btlprs[,c("x","y",paste0("prs_",years[j]))]
    coordinates(df) <- c("x","y")
    points2grid(df)
    btl_pixels <- as(df, "SpatialPixelsDataFrame")
    names(btl_pixels) <- "btlprs"
    if(i<=13){
      p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
    }else{
      p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='blue', alpha=0.4))
    }
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