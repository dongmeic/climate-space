library(ncdf4)
library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(latticeExtra)
library(gridExtra)
library(RColorBrewer)
library(animation)

years <- 1996:2015; nyr <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/daily/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/daymet/"
setwd(out)

drops <- c("drop5", "drop10", "drop15", "drop20", "drop20plus", "max.drop")
mins <- c("min20", "min22", "min24", "min26", "min28", "min30", "min32", "min34", "min36", "min38", "min40")
varnms <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", drops, "ddAugJul", "ddAugJun", mins)

ncin <- nc_open("/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/na10km_v2.nc")
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
yr <- ncvar_get(ncin, varid="yr"); ny <- length(yr)
grid <- expand.grid(x=x, yr=yr)

shppath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
canada.prov <- readOGR(dsn = shppath, layer = "na10km_can_prov")
us.states <- readOGR(dsn = shppath, layer = "na10km_us_state")
crs <- proj4string(us.states)
lrglakes <- readOGR(dsn = shppath, layer = "na10km_lrglakes")
proj4string(lrglakes) <- crs

#Lcs, Ecs, Ncs 
cutpts <- data.frame(maxAugT=c(0,5,7,9,11,13,17,21,25,29,31),
winterTmin=c(-50,-40,-30,-20,-15,-10,-5,0,5,15,30),
Acs=c(0,2,4,6,8,10,15,20,40,60,90),
drop0=c(0,10,20,30,40,45,50,60,70,80,90),
drop5=c(0,10,20,30,40,45,50,60,70,80,90),
drop10=c(0,10,20,30,40,45,50,60,70,80,90),
drop15=c(0,10,20,30,40,45,50,60,70,80,90),
drop20=c(0,10,20,30,40,45,50,60,70,80,90),
drop20plus=c(0,10,20,30,40,45,50,60,70,80,90),
max.drop=c(1,5,8,10,12,15,18,20,30,40,50),
ddAugJul=c(0,1000,2000,3000,4000,5000,6000,8000,9000,10000,11000),
ddAugJun=c(0,1000,2000,3000,4000,5000,6000,7000,8000,9000,10000),
summerT40=c(0,5,10,15,25,35,45,55,65,75,95),
min20 = c(0,10,20,30,40,50,60,70,80,90,100),
min22 = c(0,10,20,30,40,50,60,70,80,90,100),
min24 = c(0,10,20,30,40,50,60,70,80,90,100),
min26 = c(0,10,20,30,40,50,60,70,80,90,100),
min28 = c(0,10,20,30,40,50,60,70,80,90,100),
min30 = c(0,10,20,30,40,50,60,70,80,90,100),
min32 = c(0,10,20,30,40,50,60,70,80,90,100),
min34 = c(0,10,20,30,40,50,60,70,80,90,100),
min36 <- c(0,10,20,30,40,50,60,70,80,90,100),
min38 <- c(0,10,20,30,40,50,60,70,80,90,100),
min40 <- c(0,10,20,30,40,50,60,70,80,90,100))

get.data <- function(var){
  ncfile <- paste0("na10km_v2_daymet_", var, "_1996.2015.3d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")

myColors <- c('grey', 'red')
myKey <- list(text=list(lab=c("0","1"), cex=c(1.2,1.2)), 
              rectangles=list(col = myColors), space="inside", width = 0.5, columns=1)
myColors2 <- c('grey', 'red', 'darkred')
myKey2 <- list(text=list(lab=c("0","1","2"), cex=c(1.2,1.2)), 
              rectangles=list(col = myColors2), space="inside", width = 0.5, columns=1)

pos <- cbind(c(1,1),c(2,1),c(3,1),c(4,1),c(5,1),
						 c(1,2),c(2,2),c(3,2),c(4,2),c(5,2),
						 c(1,3),c(2,3),c(3,3),c(4,3),c(5,3),
						 c(1,4),c(2,4),c(3,4),c(4,4),c(5,4))
						 				 
# check the codes before running
				              
for(var in varnms){
  var_3d <- get.data(var)
  png(paste0("daymet_maps_",var,".png"), width=8, height=10, units="in", res=300)
  plot.new()
  par(mfrow=c(5,4), xpd=FALSE, mar=rep(0.5,4))
  yr = 1
  var_3d_slice <- var_3d[,,yr]
  if(var=="Lcs" | var =="Ecs"){
		p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
					par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors,
					scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.2),
					xlab="", ylab="", colorkey = FALSE, aspect="iso")
	}else if(var == "Ncs"){
		p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors2,
			scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.2),
			xlab="",ylab="", colorkey = FALSE, aspect="iso")
	}else if(var %in% c("Acs", drops)){
		p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
			col.regions=brewer.pal(10,"RdBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
			scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.0),
			xlab="",ylab="", aspect="iso")
	}else{
		p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
			col.regions=rev(brewer.pal(10,"RdBu")), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
			scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.2),
			xlab="",ylab="", aspect="iso")
	}
	p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))
	df <- btlprs[,c("x","y",paste0("prs_",(years[yr]+1)))]
	coordinates(df) <- c("x","y")
	points2grid(df)
	btl_pixels <- as(df, "SpatialPixelsDataFrame")
	names(btl_pixels) <- "btlprs"
	p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
  print(p,split=c(pos[,yr][1], pos[,yr][2], 5, 4))
  for(yr in 2:20){
	  var_3d_slice <- var_3d[,,yr]
	  if(var=="Lcs" | var =="Ecs"){
		  p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors,
			  scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.2),
			  xlab="",ylab="", colorkey = FALSE, aspect="iso")
		}else if(var == "Ncs"){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors2,
			  scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.2),
			  xlab="",ylab="", colorkey = FALSE, aspect="iso")
		}else if(var %in% c("Acs", drops)){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
				col.regions=brewer.pal(10,"RdBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
				par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
				scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.0),
				xlab="",ylab="", aspect="iso")
	  }else{
	    p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), colorkey = FALSE,
			  scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.2),
			  xlab="",ylab="", aspect="iso")
	  }
	  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))
	  df <- btlprs[,c("x","y",paste0("prs_",(years[yr]+1)))]
    coordinates(df) <- c("x","y")
    points2grid(df)
    btl_pixels <- as(df, "SpatialPixelsDataFrame")
    names(btl_pixels) <- "btlprs"
    p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
    print(p,split=c(pos[,yr][1], pos[,yr][2], 5, 4), newpage=FALSE) 
  }
  dev.off()
  print(var)
}

# plot a legend separately
if(0){
	var_3d_slice <- var_3d[,,yr]
	png(paste0("daymet_legend_",var,".png"), width=8, height=7.5, units="in", res=300)
	p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
					col.regions=brewer.pal(10,"RdBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
					par.settings = list(axis.line = list(col = "transparent")), colorkey=list(space="bottom", height=2, width=2),
					scales = list(draw = FALSE), margin=F, main=list(label=years[yr], cex=1.0),
					xlab="",ylab="", aspect="iso")
	print(p)
	dev.off()
}

for(var in varnms){
  var_3d <- get.data(var)
  for(yr in years){
    var_3d_slice <- var_3d[,,which(years==yr)]
    if(var=="Lcs" | var =="Ecs"){
		  p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000), 
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors,
			  scales = list(draw = FALSE), margin=F, main=list(label=yr, cex=1.5),
			  xlab="",ylab="", colorkey = FALSE, key=myKey, aspect="iso")
		}else if(var == "Ncs"){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors2,
			  scales = list(draw = FALSE), margin=F, main=list(label=yr, cex=1.5),
			  xlab="",ylab="", colorkey = FALSE, key=myKey2, aspect="iso")
		}else if(var %in% c("Acs", drops)){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
			  col.regions=brewer.pal(10,"RdBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=yr, cex=1.5),
			  xlab="",ylab="", aspect="iso")
	  }else{
	    p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=11, pretty=T, 
			  col.regions=rev(brewer.pal(10,"RdBu")), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=yr, cex=1.5),
			  xlab="",ylab="", aspect="iso")
	}
    p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
    p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))	  
    df <- btlprs[,c("x","y",paste0("prs_",(yr+1)))]
    coordinates(df) <- c("x","y")
    points2grid(df)
    btl_pixels <- as(df, "SpatialPixelsDataFrame")
    names(btl_pixels) <- "btlprs"
    p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='green', alpha=0.4))
    png(paste0("daymet_map_",var,"_",yr,".png"), width=6, height=8, units="in", res=300)
    print(p)
    dev.off()
    print(paste("mapping", var, "in", yr, "is done!"))
  }
}

foreach(var=varnms) %dopar% {
  im.convert(paste0("daymet_map_",var,"_*.png"),output=paste0("daymet_map_",var,".gif"))
}

print("all done!")