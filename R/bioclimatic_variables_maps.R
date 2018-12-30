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

#source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/plotlist.R")
years <- 1996:2015; nyr <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
setwd(out)

vargrp <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
			"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax", "PcumOctSep", 
			"summerP0", "summerP1", "summerP2", "PPT", "Pmean", "GSP", "POctSep", "PMarAug")

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
c(-55,-40,-30,-20,-15,-10,-5,0,5,15,30),
#JanTmin
c(-55,-30,-20,-10,-5,0,5,10,15,25,30),
#MarTmin
c(-55,-30,-20,-15,-10,-5,0,5,10,20,30),
#Tmin
c(-50,-30,-15,-10,-5,0,5,10,15,25,35),
#Tmean
c(-35,-10,-5,0,5,10,15,20,25,30,35),
#Tvar
c(0,2,4,6,7,8,10,12,14,16,19),
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
  ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

#btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence_updated.csv")
pos <- cbind(c(1,1),c(2,1),c(3,1),c(4,1),c(5,1),
						 c(1,2),c(2,2),c(3,2),c(4,2),c(5,2),
						 c(1,3),c(2,3),c(3,3),c(4,3),c(5,3),
						 c(1,4),c(2,4),c(3,4),c(4,4),c(5,4)) 

for (i in 1:length(vargrp)){
  var_4d <- get.data(vargrp[i])
  png(paste0("bioclimatic_maps_",vargrp[i],".png"), width=12, height=12, units="in", res=300)
  plot.new()
  par(mfrow=c(5,4), xpd=FALSE, mar=rep(0.5,4))
  yr = 1
	var_4d_slice <- var_4d[,,1,yr]	
	if(i<=13){
		p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			col.regions=rev(brewer.pal(10,"RdBu")),xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")), aspect="iso", 
			scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr])),
			xlab="",ylab="")
	}else{
		p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			col.regions=brewer.pal(10,"RdYlGn"),xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")), aspect="iso", 
			scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr])),
			xlab="",ylab="")
	}
	p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
	df <- btlprs[,c("x","y",paste0("prs_",(years[yr]+1)))]
	coordinates(df) <- c("x","y")
	points2grid(df)
	btl_pixels <- as(df, "SpatialPixelsDataFrame")
	names(btl_pixels) <- "btlprs"
	if(i<=13){
		p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
	}else{
		p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='blue', alpha=0.4))
	}
	print(p,split=c(pos[,yr][1], pos[,yr][2], 5, 4))
  for(yr in 2:20){
	  var_4d_slice <- var_4d[,,1,yr]
	  if(i<=13){
		 p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")),xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), aspect="iso", 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr])),
			  xlab="",ylab="")
	  }else{
		 p <- levelplot(var_4d_slice ~ x * y, data=grid, at=cutpts[,i], cuts=11, pretty=T, 
			  col.regions=brewer.pal(10,"RdYlGn"),xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), aspect="iso", 
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(vargrp[i],years[yr])),
			  xlab="",ylab="")
	  }
	  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
		df <- btlprs[,c("x","y",paste0("prs_",(years[yr]+1)))]
		coordinates(df) <- c("x","y")
		points2grid(df)
		btl_pixels <- as(df, "SpatialPixelsDataFrame")
		names(btl_pixels) <- "btlprs"
		if(i<=13){
			p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
		}else{
			p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='blue', alpha=0.4))
		}
	  print(p,split=c(pos[,yr][1], pos[,yr][2], 5, 4), newpage=FALSE)
  }
  dev.off()
  print(vargrp[i])
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