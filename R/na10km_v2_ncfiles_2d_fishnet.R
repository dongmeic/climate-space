# Created by Dongmei Chen
# Date: 2018/12/22
# Update beetle and corehost presence/absence netCDF file

library(ncdf4)
library(rgdal)

path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"

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

# presence variables
ncfname <- paste0(ncpath, "prs/na10km_v2_presence_2d.nc")
btl_fishnet <- readOGR(dsn=path, layer="na_fishnet_presence_beetle_cohosts", stringsAsFactors = FALSE)

# read and reshape
na10km <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
indata <- cbind(na10km,btl_fishnet@data[,26:27])
str(indata)

j2 <- sapply(indata$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(indata$y, function(xy) which.min(abs(y-xy)))
head(cbind(indata$x,indata$y,j2,k2))

nobs <- dim(indata)[1]
nvars <- 2
m <- rep(1:nvars,each=nobs)
temp_array <- array(fillvalue, dim=c(nx,ny,nvars))
temp_array[cbind(j2,k2,m)] <- as.matrix(indata[1:nobs,8:9])
vardefs <- data.frame(data_name=c('nabtlprsayr', 'navgtprs'), units=c('binary', 'binary'), 
											long_name=c('north_america_moutain_pine_beetle_presence_all_years',
																	'north_america_vegetation_presence'), stringsAsFactors=FALSE)
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
ncatt_put(ncout,0,"title","vegetation presence on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
history <- paste("D.Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
#ncatt_put(ncout,0,"Conventions","CF-1_6")

# add variables
for (i in 1:nvars){
  var_def <- ncvar_def(vardefs$data_name[i],vardefs$units[i],list(xdim,ydim),fillvalue,vardefs$long_name[i],prec="float")  
  ncout <- ncvar_add(ncout, var_def)
  ncvar_put(ncout, var_def, temp_array[,,i])
  if (i==1){
    ncatt_put(ncout,vardefs$data_name[i],"base_period","1997-2016")
  }
}


# close the file, writing data to disk
nc_close(ncout)
print("all done")