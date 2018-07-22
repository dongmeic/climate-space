# Created by Dongmei Chen
# generate data for beetle neighboring resources
# adapted from beetle_presence_statistics.R

library(ncdf4)
library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(latticeExtra)
library(gridExtra)
library(RColorBrewer)
library(animation)

years <- 1997:2016; nyr <- length(years)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(csvpath)
btlprs <- read.csv("beetle_presence.csv")
btlprs$key <- seq(1,dim(btlprs)[1])
target_columns <- colnames(btlprs[,grepl("prs_", colnames(btlprs))])
btlprs$sumprs <- rowSums(btlprs[,target_columns])
btlprs$sumprs[btlprs$sumprs==0] <- NA
prs.df <- btlprs[!is.na(btlprs$sumprs),]
# make a bounding box
Xmin <- min(prs.df$x) - 10000;
Xmax <- max(prs.df$x) + 10000;
Ymin <- min(prs.df$y) - 10000;
Ymax <- max(prs.df$y) + 10000;
loc <- read.csv("location.csv")
# select the target data
btlprs.df <- cbind(loc[,c("x", "y")],btlprs[,c(target_columns,"key")])
target.df <- subset(btlprs.df, x >= Xmin & x <= Xmax & y >= Ymin & y <= Ymax)
rest.df <- subset(btlprs.df, !(key %in% target.df$key))
nobs <- dim(target.df)[1]

print("calculating...")
for(k in 1:(nyr-1)){
  for (n in 1:nobs){
    get.neighbor.sum <- function(df,n){
			x <- df$x[n]; y <- df$y[n]
			total <- 0
			for(i in c(-10000, 0, 10000)){
				for(j in c(-10000, 0, 10000)){
					cell.value <- df[df$x == x + i & df$y == y + j, paste0("prs_",years[k])]
					if (length(cell.value) && !is.na(cell.value)) {
						total <- total + cell.value
					}
				}
			}
			ifelse(length(total), total, 0)
	  }
    target.df$sum9[n] <- get.neighbor.sum(target.df,n)
    #print(n)
  }
  rest.df$sum9 <- rep(0,dim(rest.df)[1])
  df <- rbind(target.df, rest.df)
  df <- df[order(df$key),]
  btlprs.df$sum9 <- df$sum9
  colnames(btlprs.df)[which(colnames(btlprs.df)=="sum9")] <- paste0("sum9_",years[k])
  print(paste("got", years[k]))
}
target_columns <- colnames(btlprs.df[,grepl("sum9_", colnames(btlprs.df))])
ndf <- btlprs.df[,c("x","y",target_columns)]
write.csv(ndf, paste0(csvpath, "ts_presence_sum9.csv"), row.names=FALSE)
print("finished CSV writing")

# open points netCDF file to get dimensions, etc.
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncin <- nc_open(paste0(ncpath,"na10km_v2.nc"))
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
lon <- ncvar_get(ncin, varid="lon")
lat <- ncvar_get(ncin, varid="lat")
nc_close(ncin)

# time
year <- 1998:2016
nt <- length(year)
tunits <- "year"

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
tdim <- ncdim_def("year",units=tunits,longname="year",as.integer(year))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- "lambert_azimuthal_equal_area"
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")
dlname <- "year"
time_def <- ncvar_def(dlname,tunits,list(tdim),NULL,dlname,prec="integer")

ncfname <- paste(ncpath,"prs/ts_presence_sum9.nc", sep="")
csvfile <- "ts_presence_sum9.csv"
dname <- "sum9"
dlname <- "Summary of the nearest 8 grid cells and the center grid cell"
dunits <- ""

# read and reshape
print("read time series beetle presence statistics")
indata <- read.csv(paste(csvpath, csvfile, sep=""))
str(indata)
print("done!")

print("set j and k index")
j2 <- sapply(indata$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(indata$y, function(xy) which.min(abs(y-xy)))
head(cbind(indata$x,indata$y,j2,k2))
print("done!")

print("create temporary array")
nobs <- dim(indata)[1]
m <- rep(1:nt,each=nobs)
temp_array <- array(fillvalue, dim=c(nx,ny,nt))
temp_array[cbind(j2,k2,m)] <- as.matrix(indata[1:nobs,3:dim(indata)[2]])
print("done!")

# create netCDF file and put data
print("define the netCDF file")
var_def <- ncvar_def(dname,dunits,list(xdim,ydim,tdim),fillvalue,dlname,prec="float")
ncout <- nc_create(ncfname,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
print("done!")

# put additional attributes into dimension and data variables
print("writing output")
ncatt_put(ncout,"x","axis","X")
ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
ncatt_put(ncout,"x","grid_spacing","10000 m")
ncatt_put(ncout,"x","_CoordinateAxisType","GeoX")
ncatt_put(ncout,"y","axis","Y")
ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
ncatt_put(ncout,"y","grid_spacing","10000 m")
ncatt_put(ncout,"y","_CoordinateAxisType","GeoY")

ncatt_put(ncout,projname,"name",projname)
ncatt_put(ncout,projname,"long_name",projname)
ncatt_put(ncout,projname,"grid_mapping_name",projname)
ncatt_put(ncout,projname,"longitude_of_projection_origin",-100.0)
ncatt_put(ncout,projname,"latitude_of_projection_origin",50.0)
ncatt_put(ncout,projname,"_CoordinateTransformType","Projection")
ncatt_put(ncout,projname,"_CoordinateAxisTypes","GeoX GeoY")
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
ncatt_put(ncout,projname,"CRS.PROJ.4",na10km_projstr)

# put variables
ncvar_put(ncout,lon_def,lon)
ncvar_put(ncout,lat_def,lat)
ncvar_put(ncout,time_def,year)
ncvar_put(ncout,var_def,temp_array)

# add global attributes
ncatt_put(ncout,0,"title","beetle presence statistics onto the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by beetle_statistics_sum9.R")
history <- paste("D.Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1998-2016")
nc_close(ncout)

print("mapping...")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/plotlist.R")
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"

setwd(out)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
ncfile <- paste0(ncpath, "ts_presence_sum9.nc")
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

var <- "sum9"
var3d <- ncvar_get(ncin,var)
fillvalue <- ncatt_get(ncin,var,"_FillValue")$value
var3d[var3d==fillvalue] <- NA
# mask out the locations with all zeros over years
m <- apply(var3d, c(1,2), sum)
m[m==0] <- NA
var3d[is.na(m)] <- NA
cutpts <- seq(1,9,by=1)
for(i in 1:(nyr-1)){
  sum9slice <- var3d[,,i]
  p <- levelplot(sum9slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,2000000),
               par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), 
               margin=F, at=cutpts, cuts=9, pretty=T, col.regions=rev(brewer.pal(8,"RdBu")), 
               main=list(label=years[i], cex=1.5), xlab="",ylab="", aspect="iso")
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,0.3,0,0.5)))
  png(paste0("presence_sum9_", years[i],".png"), width=4, height=8, units="in", res=300)
  print(p)
  dev.off()
  print(years[i])
}

im.convert("presence_sum9_*.png",output="presence_sum9_ts.gif")

plotalt <- function(i){
  sum9slice <- var3d[,,i]
  p <- levelplot(sum9slice ~ x * y, data=grid, xlim=c(-2050000,20000), ylim=c(-2000000,2000000),
                 par.settings = list(axis.line = list(col = "transparent")), scales = list(draw = FALSE), margin=F, 
                 col.regions=rev(brewer.pal(8,"RdBu")), main=list(label=years[i], cex=1.5), 
                 xlab="",ylab="", colorkey = FALSE)
  p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray'))
  p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='lightblue'))
  p <- p + latticeExtra::layer(sp.polygons(corehost, lwd=0.8, col=rgb(0,0.3,0,0.5)))
  print(p)
}

plots <- lapply(1:(nyr-1), function(i) plotalt(i))

png("composite_presence_sum9.png", width=8, height=12, units="in", res=300)
par(mfrow=c(4,5), xpd=FALSE, mar=rep(0.5,4))
print.plotlist(plots, layout=matrix(1:20, ncol=5))
dev.off()

print("all done!")
