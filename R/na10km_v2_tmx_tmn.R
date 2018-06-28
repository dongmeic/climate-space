# Created by Dongmei Chen
# To generate long-term means tmx and tmn on the na10km grid

library(ncdf4)
library(lattice)
library(RColorBrewer)

# open points netCDF file to get dimensions, etc.
path <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ltm/"
ncinfile_tmp <- "na10km_v2_tmp.nc"
ncinfile_dtr <- "na10km_v2_dtr.nc"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
ncin_tmp <- nc_open(paste(path,ncinfile_tmp,sep=""))
ncin_dtr <- nc_open(paste(path,ncinfile_dtr,sep=""))
print(ncin_tmp)
print(ncin_dtr)

# get data
dname <- "tmp"
tmpltm <- ncvar_get(ncin_tmp,dname)
dname <- "dtr"
dtrltm <- ncvar_get(ncin_dtr,dname)
dim(tmpltm)
dim(dtrltm)
fillvalue <- ncatt_get(ncin_dtr,dname,"_FillValue")
dunits <- ncatt_get(ncin_dtr,dname,"units")
print (fillvalue)

# get dimension variables and attributes
x <- ncvar_get(ncin_tmp, varid="x"); nx <- length(x)
x_long_name <- ncatt_get(ncin_tmp, "x", "long_name")$value
x_axis <- ncatt_get(ncin_tmp, "x", "axis")$value
x_standard_name <- ncatt_get(ncin_tmp, "x", "standard_name")$value
x_grid_spacing <- ncatt_get(ncin_tmp, "x", "grid_spacing")$value
x_CoordinatAxisType <- ncatt_get(ncin_tmp, "x", "CoordinateAxisType")$value

y <- ncvar_get(ncin_tmp, varid="y"); ny <- length(y)
y_long_name <- ncatt_get(ncin_tmp, "x", "long_name")$value
y_axis <- ncatt_get(ncin_tmp, "x", "axis")$value
y_standard_name <- ncatt_get(ncin_tmp, "x", "standard_name")$value
y_grid_spacing <- ncatt_get(ncin_tmp, "x", "grid_spacing")$value
y_CoordinatAxisType <- ncatt_get(ncin_tmp, "x", "CoordinateAxisType")$value

time <- ncvar_get(ncin_tmp,varid="time"); nt <- length(time)
tunits <- ncatt_get(ncin_tmp,"time","units")

# get longitude and latitude and attributes
lon <- ncvar_get(ncin_tmp,"lon"); nlon <- length(lon)
lon_units <- ncatt_get(ncin_tmp, "lon", "units")$value
lat <- ncvar_get(ncin_tmp,"lat"); nlat <- length(lat)
lat_units <- ncatt_get(ncin_tmp, "lat", "units")$value

# get CRS attributes
crs_units <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "units")$value
crs_name <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "name")$value
crs_long_name <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "long_name")$value
crs_grid_mapping_name <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "grid_mapping_name")$value
crs_longitude_of_projection_origin <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "longitude_of_projection_origin")$value
crs_latitude_of_projection_origin <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "latitude_of_projection_origin")$value
crs_earth_shape <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "earth_shape")$value
crs_CoordinateTransformType <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "_CoordinateTransformType")$value
crs_CoordinateAxisTypes <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "_CoordinateAxisTypes")$value
crs_CRS.PROJ.4 <- ncatt_get(ncin_tmp, "lambert_azimuthal_equal_area", "CRS.PROJ.4")$value

# close the input file
nc_close(ncin_tmp)
nc_close(ncin_dtr)

# calculate abosulte values

# quick map to check data
m <- 12
tmp_slice_ltm <- tmpltm[,,m]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(file=paste(out,"na10km_ltm_tmp_12.png",sep=""))
levelplot(tmp_slice_ltm ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

m <- 12
dtr_slice_ltm <- dtrltm[,,m]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(file=paste(out,"na10km_ltm_dtr_12.png",sep=""))
levelplot(dtr_slice_ltm ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# replace netCDF _FillValues with R NA's
tmpltm[tmpltm==fillvalue$value] <- NA
dtrltm[dtrltm==fillvalue$value] <- NA

# define monthly average daily minimum and maximum temperature
# monthly average daily minimum temperature
tmnltm <- array(NA,c(nx,ny,nt))
# monthly average daily maximum temperature
tmxltm <- array(NA,c(nx,ny,nt))

# make a missing data mask
landmask <- array(1, dim=c(nx,ny))
# use last month of data to set data flag
for (j in 1:nx) {
  for (k in 1:ny) {
    if (is.na(tmpltm[j,k,nt]) | is.na(dtrltm[j,k,nt])) landmask[j,k]=NA
  }
}
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,5,10,15,20,25,30,35,40,45,50)
png(file=paste(out,"landmask.png",sep=""))
levelplot(landmask ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

ptm <- proc.time()
for (j in 1:nx) {
  for (k in 1:ny) {
    if (!is.na(landmask[j,k])) {
      for (t in 1:nt) {
		tmnltm[j,k,t] <- tmpltm[j,k,t] - dtrltm[j,k,t]/2
		tmxltm[j,k,t] <- tmpltm[j,k,t] + dtrltm[j,k,t]/2
      }
    }
  }
}
proc.time() - ptm

# quick maps to check data
n <- nt
tmn_slice_ltm <- tmnltm[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_ltm_tmn_12.png",sep=""))
levelplot(tmn_slice_ltm ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

n <- nt
tmx_slice_ltm <- tmxltm[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_ltm_tmx_12.png",sep=""))
levelplot(tmx_slice_ltm ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# recode fillvalues
fillvalue <- 1e32
tmnltm[is.na(tmnltm)] <- fillvalue
tmxltm[is.na(tmxltm)] <- fillvalue

# write out absolute values -- ltm array (nx, ny, nt)
# time -- values calculated by cf_time_subs.f90 -- 1961-1990
nbnds <- 2
timeltm <-  c(27773.5, 27803.5, 27833.5, 27864, 27894.5, 27925, 27955.5, 27986.5,
  28017, 28047.5, 28078, 28108.5)
climatology_bounds <- array( dim=c(nbnds,12))
climatology_bounds[1,] <- c( 22280, 22311, 22339, 22370, 22400, 22431,22461,
  22492, 22523, 22553, 22584, 22614)
climatology_bounds[2,] <- c(32902, 32930, 32961, 32991, 33022, 33052, 33083,
  33114, 33144, 33175, 33205, 33236) 
climatology_bounds
bounds <- c(1,2)
tunits <- "days since 1900-01-01 00:00:00.0 -0:00"

ptm <- proc.time() # timer
ncfile <- "na10km_v2_tmn.nc"
tmn_ncfile <- paste(path,ncfile,sep="")

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
tdim <- ncdim_def("time",units=tunits,longname="time",as.double(timeltm))
bdim <- ncdim_def("nbnds",units="1",longname=NULL,as.double(bounds))
dlname <- "climatology_bounds"
bnds_def <- ncvar_def("climatology_bounds",tunits,list(bdim,tdim),NULL,dlname,prec="double")

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- crs_name
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

# create netCDF file and put data
dname <- "tmn"
dlongname <- "near-surface temperature minimum"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname,prec="double")
ncout <- nc_create(tmn_ncfile,list(lon_def,lat_def,bnds_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"time","axis","T")
ncatt_put(ncout,"time","calendar","standard")
ncatt_put(ncout,"time","bounds","climatology_bounds")
ncatt_put(ncout,"time","climatology","climatology_bounds")

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
ncvar_put(ncout,tdim,time)
ncvar_put(ncout,bnds_def,climatology_bounds)
ncvar_put(ncout,var_def,tmnltm)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by na10km_v2_tmx_tmn.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm

ptm <- proc.time() # timer
ncfile <- "na10km_v2_tmx.nc"
tmx_ncfile <- paste(path,ncfile,sep="")

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
tdim <- ncdim_def("time", units=tunits, longname="time", as.double(time))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- crs_name
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

# create netCDF file and put data
dname <- "tmx"
dlongname <- "near-surface temperature maximum"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname,prec="double")
ncout <- nc_create(tmx_ncfile,list(lon_def,lat_def,bnds_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
# nc_close(ncout)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"time","axis","T")
ncatt_put(ncout,"time","calendar","standard")
ncatt_put(ncout,"time","bounds","climatology_bounds")
ncatt_put(ncout,"time","climatology","climatology_bounds")

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
ncvar_put(ncout,tdim,time)
ncvar_put(ncout,bnds_def,climatology_bounds)
ncvar_put(ncout,var_def,tmxltm)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by na10km_v2_tmx_tmn.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm