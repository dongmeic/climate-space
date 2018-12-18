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
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/daily/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/daymet/"
setwd(out)

drops <- c("drop5", "drop10", "drop15", "drop20", "drop20plus","max.drop")
mindays <- c("Oct20","Oct30","Oct40","Jan20","Jan30","Jan40","Mar20","Mar30",
					"Mar40","winter20","winter30","winter40")
mins <- c("OctMin","JanMin","MarMin","winterMin","minT")
wd <- c("wd", "cwd", "vpd", "mi", "pt.coef", "cv.gsp")
maxs <- c("OptTsum", "AugMaxT", "maxT")
varnms <- c("Lcs", "maxAugT", "summerT40", "Ecs", "Ncs", "Acs", "drop0", drops, 
						"ddAugJul", "ddAugJun", mindays, mins, maxs, wd)

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

#Lcs, Ecs, Ncs 
cutpts <- data.frame(maxAugT=c(0,5,8,11,13,17,21,25,29,31),
Acs=c(0,2,4,6,8,10,20,40,60,90),
drop0=c(30,35,40,45,48,52,55,60,65,70),
drop5=c(5,10,20,25,30,35,40,45,50,60),
drop10=c(0,2,4,6,8,10,13,16,20,25),
drop15=c(0,1,2,3,4,5,6,7,10,15),
drop20=c(0,1,2,3,4,5,6,7,9,10),
drop20plus=c(0,1,2,3,4,5,6,7,10,12),
max.drop=c(1,5,8,10,12,15,20,30,40,50),
ddAugJul=c(10,1000,2000,3000,4000,5000,6000,8000,10000,11000),
ddAugJun=c(5,1000,2000,3000,4000,5000,6000,7000,8500,10000),
summerT40=c(0,5,10,15,25,35,45,60,75,95),
Oct20=c(0,2,4,6,8,10,15,20,25,30),
Oct30=c(0,2,4,6,8,10,15,18,23,28),
Oct40=c(0,2,4,6,8,10,12,14,16,18),
OctMin=c(-50,-25,-15,-10,-5,0,5,10,20,43),
Jan20=c(0,2,4,6,8,10,15,20,25,30),
Jan30=c(0,2,4,6,8,10,15,20,25,30),
Jan40=c(0,2,4,6,8,10,15,20,25,28),
JanMin=c(-50,-25,-15,-10,-5,0,5,10,20,35),
Mar20=c(0,2,4,6,8,10,15,20,25,30),
Mar30=c(0,2,4,6,8,10,15,20,25,30),
Mar40=c(0,2,4,6,8,10,15,20,25,28),
MarMin=c(-50,-25,-15,-10,-5,0,5,10,20,36),
winter20=c(0,10,20,30,40,50,60,70,80,90),
winter30=c(0,10,15,20,25,30,40,50,60,80),
winter40=c(0,2,5,10,15,20,25,35,55,75),
winterMin=c(-50,-35,-25,-20,-15,-10,-5,0,5,21),
minT=c(-50,-35,-25,-20,-15,-10,-5,0,5,21),
wd=c(-500, -100, 0, 500, 1000, 2000, 5000, 10000, 20000, 45000), 
cwd=c(0,100,200,300,400,500,600,800,1000,1200), 
vpd=c(0,50000,80000,120000,180000,200000,300000,400000,600000,800000), 
mi=c(0,1,2,3,4,6,8,10,14,20), 
pt.coef=c(0,0.1,0.3,0.5,0.6,0.7,0.8,1.0,1.1,1.2), 
cv.gsp=c(0,1,2,3,4,5,6,7,8,10),
OptTsum=c(0,2,10,20,30,40,60,70,80,95), 
AugMaxT=c(-25,0,10,20,25,30,35,40,45,50), 
maxT=c(0,5,10,15,20,25,30,35,40,50))

get.data <- function(var){
  ncfile <- paste0("na10km_v2_daymet_na_", var, "_1996.2015.3d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")

myColors <- c('white', 'lightblue')
myKey <- list(text=list(lab=c("0","1"), cex=c(1.2,1.2)), 
              rectangles=list(col = myColors), space="bottom", width = 0.5, columns=2)
myColors2 <- c('white', 'lightblue', 'blue')
myKey2 <- list(text=list(lab=c("0","1","2"), cex=c(1.2,1.2)), 
              rectangles=list(col = myColors2), space="bottom", width = 0.5, columns=2)

pos <- cbind(c(1,1),c(2,1),c(3,1),c(4,1),c(5,1),
						 c(1,2),c(2,2),c(3,2),c(4,2),c(5,2),
						 c(1,3),c(2,3),c(3,3),c(4,3),c(5,3),
						 c(1,4),c(2,4),c(3,4),c(4,4),c(5,4))
						 				 
# check the codes before running, particularly colors		              
for(var in varnms){
  var_3d <- get.data(var)
  png(paste0("daymet_maps_",var,".png"), width=10, height=12, units="in", res=300)
  plot.new()
  par(mfrow=c(5,4), xpd=FALSE, mar=rep(0.5,4))
  yr = 1
  var_3d_slice <- var_3d[,,yr]
  if(var=="Lcs" | var =="Ecs"){
		p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
					par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors,
					scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
					xlab="", ylab="", colorkey = FALSE, key = myKey, aspect="iso")
	}else if(var == "Ncs"){
		p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors2,
			scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
			xlab="", ylab="", colorkey = FALSE, key = myKey2, aspect="iso")
	}else if(var %in% c("Acs", drops, mindays)){
		p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
			col.regions=brewer.pal(9,"GnBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")),colorkey = TRUE,
			scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
			xlab="", ylab="", aspect="iso")
	}else{
		p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
			col.regions=rev(brewer.pal(9,"GnBu")), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			par.settings = list(axis.line = list(col = "transparent")),colorkey = TRUE,
			scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
			xlab="", ylab="", aspect="iso")
	}
	p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))
	df <- btlprs[,c("x","y",paste0("prs_",(years[yr]+1)))]
	coordinates(df) <- c("x","y")
	points2grid(df)
	btl_pixels <- as(df, "SpatialPixelsDataFrame")
	names(btl_pixels) <- "btlprs"
	p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='#e41a1c', alpha=0.4))
  print(p,split=c(pos[,yr][1], pos[,yr][2], 5, 4))
  for(yr in 2:20){
	  var_3d_slice <- var_3d[,,yr]
	  if(var=="Lcs" | var =="Ecs"){
		  p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors,
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
			  xlab="", ylab="", colorkey = FALSE, aspect="iso")
		}else if(var == "Ncs"){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors2,
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
			  xlab="", ylab="", colorkey = FALSE, aspect="iso")
		}else if(var %in% c("Acs", drops, mindays)){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
				col.regions=brewer.pal(9,"GnBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
				par.settings = list(axis.line = list(col = "transparent")), colorkey = TRUE,
				scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
				xlab="", ylab="", aspect="iso")
	  }else{
	    p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
			  col.regions=rev(brewer.pal(9,"GnBu")), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), colorkey = TRUE,
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
			  xlab="", ylab="", aspect="iso")
	  }
	  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))
	  df <- btlprs[,c("x","y",paste0("prs_",(years[yr]+1)))]
    coordinates(df) <- c("x","y")
    points2grid(df)
    btl_pixels <- as(df, "SpatialPixelsDataFrame")
    names(btl_pixels) <- "btlprs"
    p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='#e41a1c', alpha=0.4))
    print(p,split=c(pos[,yr][1], pos[,yr][2], 5, 4), newpage=FALSE) 
  }
  dev.off()
  print(var)
}

# plot a legend separately
if(0){
	var <- "Acs"; yr <- 1
	var_3d <- get.data(var)
	var_3d_slice <- var_3d[,,yr]
	png(paste0("daymet_legend_",var,".png"), width=8, height=7.5, units="in", res=300)
	p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
					col.regions=brewer.pal(9,"GnBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
					par.settings = list(axis.line = list(col = "transparent")), colorkey=list(space="bottom", height=2, width=2),
					scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr])),
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
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr]), cex=1.5),
			  xlab="",ylab="", colorkey = FALSE, key=myKey, aspect="iso")
		}else if(var == "Ncs"){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), col.regions=myColors2,
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr]), cex=1.5),
			  xlab="",ylab="", colorkey = FALSE, key=myKey2, aspect="iso")
		}else if(var %in% c("Acs", drops, mindays)){
			p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
			  col.regions=brewer.pal(9,"GnBu"), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), colorkey = TRUE,
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr]), cex=1.5),
			  xlab="",ylab="", aspect="iso")
	  }else{
	    p <- levelplot(var_3d_slice ~ x * y, data=grid, at=cutpts[,var], cuts=10, pretty=T, 
			  col.regions=rev(brewer.pal(9,"GnBu")), xlim=c(-2050000,20000), ylim=c(-2000000,1600000),
			  par.settings = list(axis.line = list(col = "transparent")), colorkey = TRUE,
			  scales = list(draw = FALSE), margin=F, main=list(label=paste(var,years[yr]), cex=1.5),
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
    p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='#e41a1c', alpha=0.4))
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