# Created by Dongmei Chen
# To generate absolute values for climatic variables

library(ncdf4)
library(lattice)
library(RColorBrewer)

# open points netCDF file to get dimensions, etc.
tspath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
ltmpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ltm/"
ncinfile <- "na10km_v2_cru_ts4.01.1901.2016.pre.anm3d.nc"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
ncin <- nc_open(paste(tspath,ncinfile,sep=""))
print(ncin)

# get data
dname <- "pre_anm"
var3d <- ncvar_get(ncin,dname)
dim(var3d)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dunits <- ncatt_get(ncin,dname,"units")
dlongname <- "Precipitation"
print (fillvalue)

# get dimension variables and attributes
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
x_long_name <- ncatt_get(ncin, "x", "long_name")$value
x_axis <- ncatt_get(ncin, "x", "axis")$value
x_standard_name <- ncatt_get(ncin, "x", "standard_name")$value
x_grid_spacing <- ncatt_get(ncin, "x", "grid_spacing")$value
x_CoordinatAxisType <- ncatt_get(ncin, "x", "CoordinateAxisType")$value

y <- ncvar_get(ncin, varid="y"); ny <- length(y)
y_long_name <- ncatt_get(ncin, "x", "long_name")$value
y_axis <- ncatt_get(ncin, "x", "axis")$value
y_standard_name <- ncatt_get(ncin, "x", "standard_name")$value
y_grid_spacing <- ncatt_get(ncin, "x", "grid_spacing")$value
y_CoordinatAxisType <- ncatt_get(ncin, "x", "CoordinateAxisType")$value

time <- ncvar_get(ncin, varid="time"); nt <- length(time)
tunits <- ncatt_get(ncin,"time","units")

# get longitude and latitude and attributes
lon <- ncvar_get(ncin,"lon"); 
lon_units <- ncatt_get(ncin, "lon", "units")$value
lat <- ncvar_get(ncin,"lat"); 
lat_units <- ncatt_get(ncin, "lat", "units")$value

# get CRS attributes
crs_units <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "units")$value
crs_name <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "name")$value
crs_long_name <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "long_name")$value
crs_grid_mapping_name <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "grid_mapping_name")$value
crs_longitude_of_projection_origin <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "longitude_of_projection_origin")$value
crs_latitude_of_projection_origin <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "latitude_of_projection_origin")$value
crs_earth_shape <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "earth_shape")$value
crs_CoordinateTransformType <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "_CoordinateTransformType")$value
crs_CoordinateAxisTypes <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "_CoordinateAxisTypes")$value
crs_CRS.PROJ.4 <- ncatt_get(ncin, "lambert_azimuthal_equal_area", "CRS.PROJ.4")$value

# close the input file
nc_close(ncin)

# calculate abosulte values

# quick map to check data
n <- nt
var_slice_3d <- var3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(file=paste(out,"na10km_v2_cru_4.01.2016.anm.pre3d.png",sep=""))
levelplot(var_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# replace netCDF _FillValues with R NA's
var3d[var3d==fillvalue$value] <- NA

# reshape to 4d
nm <- 12 # number of months in year
nyr <- nt/12 # number of years in CRU data set
var4d <- array(var3d, dim=c(nx,ny,nm,nyr))
dim(var4d)

# quick maps to check data
m <- 12; n <- nyr-2
var_slice_4d <- var4d[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(file=paste(out,"na10km_v2_cru_4.01.2014.anm.pre4d.png",sep=""))
levelplot(var_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# read long-term mean data
ltmnc <- "na10km_v2_pre.nc"
ltmncfile <- paste(ltmpath,ltmnc,sep="")
ltmncin <- nc_open(ltmncfile)
print(ltmncin)

ltm <- ncvar_get(ltmncin,"pre")
dim(ltm)

# quick maps to check long-term means
m <- 12
var_slice_3d <- ltm[,,m]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(file=paste(out,"na10km_ltm.pre3d.png",sep=""))
levelplot(var_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

ptm <- proc.time()
for (n in 1:nyr) {
  for (m in 1:nm) {
    var3d[,,((n-1)*nm+m)] <- var3d[,,((n-1)*nm+m)] + ltm[,,m]
    var4d[,,m,n] <- var4d[,,m,n] + ltm[,,m]
  }
}
proc.time() - ptm

# quick maps to check data
n <- nt
var_slice_3d <- var3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(file=paste(out,"na10km_v2_cru_4.01.2016.abs.pre3d.png",sep=""))
levelplot(var_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
m <- 6; n <- nyr
var_slice_4d <- var4d[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(file=paste(out,"na10km_v2_cru_4.01.2016.abs.pre4d.png",sep=""))
levelplot(var_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# recode fillvalues
fillvalue <- 1e32
var3d[is.na(var3d)] <- fillvalue
var4d[is.na(var4d)] <- fillvalue

# replace precipitation anomalies (below 0) with 0
var3d[var3d<0] <- 0
var4d[var4d<0] <- 0

# write out absolute values -- 3d array (nx, ny, nt)

ptm <- proc.time() # timer
absfile <- "na10km_v2_cru_ts4.01.1901.2016.pre.abs3d.nc"
abs_ncfile <- paste(tspath,absfile,sep="")

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
tdim <- ncdim_def("time", units=tunits$value, longname="time", as.double(time))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- crs_name
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

# create netCDF file and put data
dname <- "pre"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname,prec="double")
ncout <- nc_create(abs_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
#nc_close(ncout)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"x","axis",x_axis)
ncatt_put(ncout,"x","standard_name",x_standard_name)
ncatt_put(ncout,"x","grid_spacing",x_grid_spacing)
ncatt_put(ncout,"x","_CoordinateAxisType",x_CoordinatAxisType)
ncatt_put(ncout,"y","axis",y_axis)
ncatt_put(ncout,"y","standard_name",y_standard_name)
ncatt_put(ncout,"y","grid_spacing",y_grid_spacing)
ncatt_put(ncout,"y","_CoordinateAxisType",y_CoordinatAxisType)
ncatt_put(ncout,"time","axis","T")
ncatt_put(ncout,"time","calendar","standard")

ncatt_put(ncout,crs_name,"name",crs_name)
ncatt_put(ncout,crs_name,"long_name",crs_long_name)
ncatt_put(ncout,crs_name,"grid_mapping_name",crs_grid_mapping_name)
ncatt_put(ncout,crs_name,"longitude_of_projection_origin",crs_longitude_of_projection_origin)
ncatt_put(ncout,crs_name,"latitude_of_projection_origin",crs_latitude_of_projection_origin)
ncatt_put(ncout,crs_name,"_CoordinateTransformType",crs_CoordinateTransformType)
ncatt_put(ncout,crs_name,"_CoordinateAxisTypes",crs_CoordinateAxisTypes)
ncatt_put(ncout,crs_name,"CRS.PROJ.4",crs_CRS.PROJ.4)

# put variables
ncvar_put(ncout,lon_def,lon)
ncvar_put(ncout,lat_def,lat)
ncvar_put(ncout,tdim,time)
ncvar_put(ncout,var_def,var3d)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_abs.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm

# to save memory
remove(var3d)

# write 4d data

ptm <- proc.time() # timer
absfile <- "na10km_v2_cru_ts4.01.1901.2016.pre.abs4d.nc"
abs_ncfile <- paste(tspath,absfile,sep="")

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
year <- seq(1901,2016, by=1)
yeardim <- ncdim_def("year","year",as.integer(year))
month <- seq(1,12, by=1)
monthdim <- ncdim_def("month","month",as.integer(month))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- crs_name
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

# create netCDF file and put data
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,monthdim,yeardim),fillvalue,dlongname,prec="double")
ncout <- nc_create(abs_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
#nc_close(ncout)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"x","axis",x_axis)
ncatt_put(ncout,"x","standard_name",x_standard_name)
ncatt_put(ncout,"x","grid_spacing",x_grid_spacing)
ncatt_put(ncout,"x","_CoordinateAxisType",x_CoordinatAxisType)
ncatt_put(ncout,"y","axis",y_axis)
ncatt_put(ncout,"y","standard_name",y_standard_name)
ncatt_put(ncout,"y","grid_spacing",y_grid_spacing)
ncatt_put(ncout,"y","_CoordinateAxisType",y_CoordinatAxisType)

ncatt_put(ncout,crs_name,"name",crs_name)
ncatt_put(ncout,crs_name,"long_name",crs_long_name)
ncatt_put(ncout,crs_name,"grid_mapping_name",crs_grid_mapping_name)
ncatt_put(ncout,crs_name,"longitude_of_projection_origin",crs_longitude_of_projection_origin)
ncatt_put(ncout,crs_name,"latitude_of_projection_origin",crs_latitude_of_projection_origin)
ncatt_put(ncout,crs_name,"_CoordinateTransformType",crs_CoordinateTransformType)
ncatt_put(ncout,crs_name,"_CoordinateAxisTypes",crs_CoordinateAxisTypes)
ncatt_put(ncout,crs_name,"CRS.PROJ.4",crs_CRS.PROJ.4)

# put variables
ncvar_put(ncout,lon_def,lon)
ncvar_put(ncout,lat_def,lat)
ncvar_put(ncout,var_def,var4d)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_abs.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm