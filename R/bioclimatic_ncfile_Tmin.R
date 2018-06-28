# Created by Dongmei Chen
# To generate NetCDF files for the bioclimatic variables - Tmin, fallTmean

library(ncdf4)
library(abind)
library(lattice)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

path <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/daily/20180625/"

# variable
# Tmin - mean of monthly average of minimum temperature from November in the prior year through March in the year of the outbreak
# fallTmean - mean of monthly average of daily mean temperature from September through November in the year prior to the outbreak

print("read minimum temperature netCDF file")
ncfile <- "na10km_v2_cru_ts4.01.1901.2016.tmn.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "tmn"
tmn <- ncvar_get(ncin,dname)
dim(tmn)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")

print("read mean temperature netCDF file")
ncfile <- "na10km_v2_cru_ts4.01.1901.2016.tmp.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "tmp"
tmp <- ncvar_get(ncin,dname)
dim(tmp)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")

# get common variables and attributes
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

nc_close(ncin)
tmn[tmn==fillvalue$value] <- NA
tmp[tmp==fillvalue$value] <- NA

# make a missing data mask
print("make a landmask...")
landmask <- array(1, dim=c(nx,ny))
# # use last month of data to set data flag
for (j in 1:nx) {
  for (k in 1:ny) {
    if (is.na(tmp[j,k,1,1])) landmask[j,k]=NA
  }
  #print(j);print(k)
}

start_year = 1901; end_year = 2016;
nt <- end_year - start_year
Tmin_3d <- array(NA, dim=c(nx, ny, nt))
fallTmean_3d <- array(NA, dim=c(nx, ny, nt))

print("calculate long term means")
ptm <- proc.time()
for (k in 1:nt){
	Tmin_3d[,,k] <- apply(abind(tmn[,,11:12,k],tmn[,,1:3,(k+1)],along=3),c(1,2),mean)
	fallTmean_3d[,,k] <- apply(abind(tmp[,,9:11,k],along=3),c(1,2),mean)
	print(k)
}
proc.time() - ptm

var_all_3d <- abind(fallTmean_3d, Tmin_3d, along=3)
# write 3d data in a loop
print("start to write 3d data")
dnames <- c("fallTmean", "Tmin")
dlongnames <- c("Mean of monthly average of daily mean temperature from September to November",
				"Mean of monthly average of minimum temperature from November to March")
dunits <- c("°C", "°C", "°C", "°C")
d1 <- dim(var_all_3d)[1];d2 <- dim(var_all_3d)[2]; d3 <- (dim(var_all_3d)[3])/(length(dnames))
start_year <- 1902; end_year <- 2016
nyr <- length(start_year:end_year)
ptm <- proc.time()
foreach(i=1:length(dnames)) %dopar%{

	filenm <- paste0("na10km_v2_",dnames[i],"_",start_year,".",end_year,".3d.nc")
	ncfile <- paste0(path,"var/",filenm)

	# define dimensions
	xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
	ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
	year <- seq(start_year,end_year, by=1)
	yeardim <- ncdim_def("year","year",as.integer(year))

	# define common variables
	fillvalue <- 1e32
	dlname <- "Longitude of cell center"
	lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
	dlname <- "Latitude of cell center"
	lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
	projname <- crs_name
	proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

	# create netCDF file and put data
	var_def <- ncvar_def(dnames[i],dunits[i],list(xdim,ydim,yeardim),fillvalue,dlongnames[i],prec="double")
	ncout <- nc_create(ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)

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
	var3d <- array(var_all_3d[,,(1+nyr*(i-1)):(nyr*i)],dim=c(d1,d2,d3))
	ncvar_put(ncout,lon_def,lon)
	ncvar_put(ncout,lat_def,lat)
	ncvar_put(ncout,var_def,var3d)

	# add global attributes
	ncatt_put(ncout,0,"title","CRU TS 4.01 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tmin_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1902-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm
print("all done!")