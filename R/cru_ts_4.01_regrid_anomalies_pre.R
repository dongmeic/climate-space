# Created by Pat Bartlein
# Modified by Dongmei Chen

# Read CRU TS 4.01 anomalies

library(ncdf4)
library(lattice)
library(RColorBrewer)
library(fields)
library(geosphere)

# open CRU netCDF file to get dimensions, etc.
crupath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/derived/"
crufile <- "cru_ts4.01.1901.2016.pre.anm3d.nc"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
cru_ncfile <- paste(crupath,crufile,sep="")
ncin <- nc_open(cru_ncfile)
print(ncin)

# get dimension variables
crulon <- ncvar_get(ncin, varid="lon"); ncrulon <- length(crulon)
crulat <- ncvar_get(ncin, varid="lat"); ncrulat <- length(crulat)
time <- ncvar_get(ncin, varid="time"); nt <- length(time)
tunits <- ncatt_get(ncin,"time","units")
print(c(ncrulon, ncrulat, nt))
print(tunits)

# get data
dname <- "pre_anm"
var3d <- ncvar_get(ncin,dname)
dim(var3d)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
dunits <- ncatt_get(ncin,dname,"units")
dlongname <- ncatt_get(ncin,dname,"long_name")
print (fillvalue)

# close the CRU input file
nc_close(ncin)

# open na10km_v2 points netCDF file to get dimensions, etc.
napath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ltm/"
nafile <- "na10km_v2.nc"
na_ncfile <- paste(napath,nafile,sep="")
ncin <- nc_open(na_ncfile)
print(ncin)

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
lon <- ncvar_get(ncin,"lon")
lon_units <- ncatt_get(ncin, "lon", "units")$value
lat <- ncvar_get(ncin,"lat")
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

# close the na10km_v2 file
nc_close(ncin)

# read the na10km_v2 grid "target values"
targpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/"
targfile <- "na10km_v2.csv"
na10km_v2 <- read.csv(paste(targpath, targfile, sep=""))
head(na10km_v2)
ntarg <- dim(na10km_v2)[1]
ntarg

# begin interpolation

# quick map to check data
n <- 1368
var_slice_3d <- var3d[,,n]
grid <- expand.grid(x=crulon, y=crulat)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
levelplot(var_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# define arrrays to hold interpolated anomaly values
interp_mat <- array(NA, dim=c(nx, ny))
interp_anm <- array(NA, dim=c(nx, ny, nt))
interp_var <- rep(NA, ntarg)

# get array subscripts for each target point
j <- sapply(na10km_v2$x, function(c) which.min(abs(x-c)))
k <- sapply(na10km_v2$y, function(c) which.min(abs(y-c)))
head(cbind(j,k,na10km_v2$x,na10km_v2$y))


# bilinear interpolation for most points:
# loop over times
ptm <- proc.time() # timer
for (n in 1:nt) {
#n <- 1  

  print(n)
  # bilinear interpolation from fields package
  control_dat <- list(x=crulon, y=crulat, z=var3d[,,n])
  interp_var <- interp.surface(control_dat, cbind(na10km_v2$lon,na10km_v2$lat))

  # head(cbind(na10km_v2$x, na10km_v2$y, na10km_v2$lon, na10km_v2$lat, interp_var))
  # sum(is.na(interp_var))
  
  # put interpolated values into array
  # head(cbind(j,k))
  interp_mat[cbind(j,k)] <- interp_var[1:ntarg]
  
  interp_anm[,,n] <- interp_mat
}
proc.time() - ptm

# quick map to check data
n <- 1380
test_slice1 <- interp_anm[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(paste(out,"cru_ts4.01.pre.interp.2016.12.3d.png", sep=""))
levelplot(test_slice1 ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()
sum(is.na(test_slice1))

# nearest-neighbor assignment for missing bilinear-interpolation values
# use last time-slice

# find locations of non-missing CRU points
cru_grid <- expand.grid(clon=crulon, clat=crulat)
cru_nonmiss <- data.frame(cbind(cru_grid,as.vector(var3d[,,nt])))
names(cru_nonmiss) <- c("clon", "clat", "cru")
sum(is.na(cru_nonmiss))
cru_nonmiss <- cru_nonmiss[(!is.na(cru_nonmiss$cru)),]
sum(is.na(cru_nonmiss))

miss_id <- seq(1:length(interp_var))[is.na(interp_var)]
head(miss_id)
nmiss <- length(miss_id)
dist <- rep(1,nmiss)

# find closest nonmissing CRU point to each missing target point
ptm <- proc.time() # timer
for (i in 1:nmiss) {
  print(i)
  id <- miss_id[i]
  
  # subscripts of missing target point
  j2 <- j[id]; k2 <- k[id]
  
  # find closest nonmissing CRU point
  dist <- distCosine(cbind(na10km_v2$lon[id],na10km_v2$lat[id]), 
                     cbind(cru_nonmiss$clon, cru_nonmiss$clat), r=6378137)
  closest_cru_id <- which.min(dist)
  
  # subscripts of closest CRU point
  j3 <- which.min(abs(crulon-cru_nonmiss$clon[closest_cru_id]))
  k3 <- which.min(abs(crulat-cru_nonmiss$clat[closest_cru_id]))
  # c(j2,k2,j3,k3)
  
  # copy values
  interp_anm[j2,k2,] <- var3d[j3,k3,]
}
proc.time() - ptm

# # quick map to check data
n <- 1380
test_slice2 <- interp_anm[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(paste(out,"cru_ts4.01.pre.nonmiss.2016.12.3d.png", sep=""))
levelplot(test_slice2 ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()
sum(is.na(test_slice2))

# points that were filled by nearest-neighbor interpolation
test_slice3 <- test_slice2
test_slice3[!is.na(test_slice1)] <- NA
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
png(paste(out,"cru_ts4.01.pre.near.2016.12.3d.png", sep=""))
levelplot(test_slice3 ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()
sum(is.na(test_slice3))

# replace R NA's with fillvalues
# fillvalue
fillvalue <- 1e32
interp_anm[is.na(interp_anm)] <- fillvalue

# write out interpolated anomalies -- 3d array (nx, ny, nt)

ptm <- proc.time() # timer
natspath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
interpfile <- "na10km_v2_cru_ts4.01.1901.2016.pre.anm3d.nc"
interp_ncfile <- paste(natspath,interpfile,sep="")

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
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname$value,prec="double")
ncout <- nc_create(interp_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
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
ncvar_put(ncout,var_def,interp_anm)

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 anomalies interpolated onto the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_anomalies.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm

# reshape 3d array to 4d
nm <- 12 # number of months in year
nyr <- nt/12 # number of years in CRU data set
interp_anm_4d <- array(interp_anm, dim=c(nx,ny,nm,nyr))
dim(interp_anm_4d)

# quick map to check reshaping
m <- 12; n <- nyr
var_slice_4d <- interp_anm_4d[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-1000,-500,-200,0,50,100,200,500,1000,2000,3000)
levelplot(var_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))

# to save memory
remove(interp_anm)

# write 4d data

ptm <- proc.time() # timer
interpfile <- "na10km_v2_cru_ts4.01.1901.2016.pre.anm4d.nc"
interp_ncfile <- paste(natspath,interpfile,sep="")

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
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,monthdim,yeardim),fillvalue,dlongname$value,prec="double")
ncout <- nc_create(interp_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
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
ncvar_put(ncout,var_def,interp_anm_4d)

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 anomalies interpolated onto the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_anomalies.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm
