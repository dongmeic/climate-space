library(ncdf4)
library(lattice)
library(rgdal)
library(raster)
library(rasterVis)
library(latticeExtra)
library(gridExtra)
library(RColorBrewer)

# input shapefiles
print("converting shapefile to NetCDF ...")
shppath <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
setwd("/gpfs/projects/gavingrp/dongmeic/beetle/output/maps")
age.layer <- "stand_age"
age.std.layer <- "stand_age_std"
density.layer <- "tree_density"

age.shp <- readOGR(dsn = shppath, layer = age.layer, stringsAsFactors=FALSE)
age.df <- age.shp@data
age.std.shp <- readOGR(dsn = shppath, layer = age.std.layer, stringsAsFactors=FALSE)
age.std.df <- age.std.shp@data
density.shp <- readOGR(dsn = shppath, layer = density.layer, stringsAsFactors=FALSE)
density.df <- density.shp@data

# combine input as a dataframe
indata <- cbind(age.df$RASTERVALU, age.std.df$RASTERVALU, density.df$RASTERVALU)
colnames(indata) <- c("age","age_std","density")

# open points netCDF file to get dimensions, etc.
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncin <- nc_open(paste(ncpath,"na10km_v2.nc",sep=""))
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
lon <- ncvar_get(ncin, varid="lon")
lat <- ncvar_get(ncin, varid="lat")
nc_close(ncin)

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- "lambert_azimuthal_equal_area"
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

vardefs <- data.frame(data_name=c("age","std","dens"), units=c("year","year","tree"),
                      long_name=c("stand age", "standard deviation of stand age", "tree density"),
                      stringsAsFactors = FALSE)

# presence variables
ncfname <- paste0(ncpath, "prs/na10km_v2_stand_age_density.nc")
nvars <- 3

print("set j and k index")
na10km <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
j2 <- sapply(na10km$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(na10km$y, function(xy) which.min(abs(y-xy)))
head(cbind(na10km$x,na10km$y,j2,k2))

nobs <- dim(indata)[1]
m <- rep(1:nvars,each=nobs)
temp_array <- array(fillvalue, dim=c(nx,ny,nvars))
temp_array[cbind(j2,k2,m)] <- as.matrix(indata[1:nobs,1:3])
temp_array[temp_array==0] <- fillvalue
temp_array[temp_array==-9999] <- fillvalue

# create netCDF file and put data

ncout <- nc_create(ncfname,list(lon_def,lat_def,proj_def),force_v4=TRUE, verbose=FALSE)

# put additional attributes into dimension and data variables
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

# add global attributes
ncatt_put(ncout,0,"title","stand age and density on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
history <- paste("D.Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
#ncatt_put(ncout,0,"Conventions","CF-1_6")

# add variables
for (i in 1:nvars){
  var_def <- ncvar_def(vardefs$data_name[i],vardefs$units[i],list(xdim,ydim),fillvalue,vardefs$long_name[i],prec="float")  
  ncout <- ncvar_add(ncout, var_def)
  ncvar_put(ncout, var_def, temp_array[,,i])
}

# close the file, writing data to disk
nc_close(ncout)

print("got the ncfile ...")

print("making maps ...")
canada.prov <- readOGR(dsn = shppath, layer = "na10km_can_prov")
us.states <- readOGR(dsn = shppath, layer = "na10km_us_state")
crs <- proj4string(us.states)
lrglakes <- readOGR(dsn = shppath, layer = "na10km_lrglakes")
proj4string(lrglakes) <- crs
coast <- readOGR(dsn = shppath, layer = "na10km_coast")
proj4string(coast) <- crs

cutpts <- cbind(c(20,50,60,70,80,90,100,120,150,300),
c(0,10,15,20,25,30,40,50,60,80),
c(1000,5000,10000,20000,30000,40000,60000,100000,150000,200000))

# read all years beetle presence data
#ncfname <- paste0(ncpath, "prs/na10km_v2_presence_pines.nc")
#ncin <- nc_open(ncfname)
#btl_slice <- ncvar_get(ncin, "nabtlprsayr")

btlprs <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
fignames <- c("na10km_stand_age","na10km_stand_age_std","na10km_tree_density")
grid <- expand.grid(x=x, y=y)
for(i in 1:nvars){
  var_slice <- temp_array[,,i]
	p <- levelplot(var_slice ~ x * y, data=grid, at=cutpts[,i], cuts=10, pretty=T, 
			  col.regions=brewer.pal(9,"Greens"),
			  par.settings = list(axis.line = list(col = "transparent")), 
			  scales = list(draw = FALSE), margin=F, main=list(label=vardefs$long_name[i], cex=1.5),
			  xlab="",ylab="")
	p <- p + latticeExtra::layer(sp.polygons(canada.prov, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(us.states, lwd=0.8, col='dimgray', alpha=0.3))
	p <- p + latticeExtra::layer(sp.polygons(coast, lwd=0.8, col='dimgray', alpha=0.5))
	p <- p + latticeExtra::layer(sp.polygons(lrglakes, lwd=0.8, col='dimgray', fill='lightblue', alpha=0.3))
	
	df <- btlprs[,c("x","y","allyears")]
	coordinates(df) <- c("x","y")
	points2grid(df)
	btl_pixels <- as(df, "SpatialPixelsDataFrame")
	names(btl_pixels) <- "btlprs"
	p <- p + latticeExtra::layer(sp.points(btl_pixels[btl_pixels$btlprs==1,], pch=19, cex=0.05, col='red', alpha=0.3))
  png(paste0(fignames[i],".png"), width=9, height=8, units="in", res=300)
  print(p)
  dev.off()
}

print("all done!")