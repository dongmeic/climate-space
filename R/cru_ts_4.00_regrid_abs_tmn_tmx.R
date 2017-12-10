# Created by Dongmei Chen
# To generate absolute values for climatic variables

library(ncdf4)
library(lattice)
library(RColorBrewer)

# define time parameters
start_year = 1999; end_year = 2014; start_time = 1189; time_length = 192
#start_year = 1961; end_year = 1990; start_time = 721; end_time = 1080; time_length = 30*12

# open points netCDF file to get dimensions, etc.
path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
ncinfile_tmp <- paste0("na10km_v2_cru_ts4.00.",start_year,".",end_year,".tmp.abs3d.nc")
ncinfile_dtr <- paste0("na10km_v2_cru_ts4.00.",start_year,".",end_year,".dtr.abs3d.nc")
out <- "/home2/dongmeic/beetle/output/maps/"
#ncinfile_tmp <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs3d.nc"
#ncinfile_dtr <- "na10km_v2_cru_ts4.00.1901.2015.dtr.abs3d.nc"
ncin_tmp <- nc_open(paste(path,ncinfile_tmp,sep=""))
ncin_dtr <- nc_open(paste(path,ncinfile_dtr,sep=""))
print(ncin_tmp)
print(ncin_dtr)

# get data
dname <- "tmp"
tmp3d <- ncvar_get(ncin_tmp,dname)
#tmp3d <- ncvar_get(ncin_tmp,dname,start=c(x=1,y=1,time=start_time),count=c(x=1078, y=900, time=time_length))
dname <- "dtr"
dtr3d <- ncvar_get(ncin_dtr,dname)
#dtr3d <- ncvar_get(ncin_dtr,dname,start=c(x=1,y=1,time=start_time),count=c(x=1078, y=900, time=time_length))
dim(tmp3d)
dim(dtr3d)
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
#time <- ncvar_get(ncin_tmp,varid="time",start=start_time,count=time_length); nt <- length(time)
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
n <- time_length
tmp_slice_3d <- tmp3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.",end_year,".tmp.png",sep=""))
levelplot(tmp_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

n <- time_length
dtr_slice_3d <- dtr3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.",end_year,".dtr.png",sep=""))
levelplot(dtr_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# replace netCDF _FillValues with R NA's
tmp3d[tmp3d==fillvalue$value] <- NA
dtr3d[dtr3d==fillvalue$value] <- NA

nm <- 12 # number of months in year
nyr <- nt/12 # number of years in CRU data set
# define monthly average daily minimum and maximum temperature
#monthly average daily minimum temperature
tmn3d <- array(NA,c(nx,ny,nt))
#monthly average daily maximum temperature
tmx3d <- array(NA,c(nx,ny,nt))

# make a missing data mask
landmask <- array(1, dim=c(nx,ny))
# use last month of data to set data flag
for (j in 1:nx) {
  for (k in 1:ny) {
    if (is.na(tmp3d[j,k,nt]) | is.na(dtr3d[j,k,nt])) landmask[j,k]=NA
  }
}
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,5,10,15,20,25,30,35,40,45,50)
levelplot(landmask ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))


ptm <- proc.time()

for (j in 1:nx) {
  for (k in 1:ny) {
    if (!is.na(landmask[j,k])) {
      for (n in 1:nyr) {
        for (m in 1:nm) {
          tmn3d[j,k,((n-1)*nm+m)] <- tmp3d[j,k,((n-1)*nm+m)] - dtr3d[j,k,((n-1)*nm+m)]/2
          tmx3d[j,k,((n-1)*nm+m)] <- tmp3d[j,k,((n-1)*nm+m)] + dtr3d[j,k,((n-1)*nm+m)]/2
        }
      }
    }
  }
}

proc.time() - ptm

# quick maps to check data
n <- time_length
tmn_slice_3d <- tmn3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.",end_year,".tmn.3d.png",sep=""))
levelplot(tmn_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()


n <- time_length
tmx_slice_3d <- tmx3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.",end_year,".tmx.3d.png",sep=""))
levelplot(tmx_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# reshape to 4d
tmn4d <- array(tmn3d,c(nx,ny,nm,nyr))
tmx4d <- array(tmx3d,c(nx,ny,nm,nyr))

# quick maps to check data
m <- 6; n <- nyr
tmn_slice_4d <- tmn4d[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.",end_year,".tmn.4d.png",sep=""))
levelplot(tmn_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

m <- 6; n <- nyr
tmx_slice_4d <- tmx4d[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.",end_year,".tmx.4d.png",sep=""))
levelplot(tmx_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# recode fillvalues
fillvalue <- 1e32
tmn3d[is.na(tmn3d)] <- fillvalue
tmn4d[is.na(tmn4d)] <- fillvalue

tmx3d[is.na(tmx3d)] <- fillvalue
tmx4d[is.na(tmx4d)] <- fillvalue

# write out absolute values -- 3d array (nx, ny, nt)

ptm <- proc.time() # timer
ncfile <- paste0("na10km_v2_cru_ts4.00.",start_year,".",end_year,".tmn.3d.nc")
tmn_ncfile <- paste(path,ncfile,sep="")

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
dname <- "tmn"
dlongname <- "near-surface temperature minimum"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname,prec="double")
ncout <- nc_create(tmn_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
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
ncvar_put(ncout,var_def,tmn3d)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.00_regrid_abs.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm

# to save memory
remove(tmn3d)

# write out absolute values -- 3d array (nx, ny, nt)

ptm <- proc.time() # timer
ncfile <- paste0("na10km_v2_cru_ts4.00.",start_year,".",end_year,".tmx.3d.nc")
tmx_ncfile <- paste(path,ncfile,sep="")

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
dname <- "tmx"
dlongname <- "near-surface temperature maximum"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname,prec="double")
ncout <- nc_create(tmx_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
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
ncvar_put(ncout,var_def,tmx3d)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.00_regrid_abs.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm

# to save memory
remove(tmx3d)


# write 4d data

ptm <- proc.time() # timer
ncfile <- paste0("na10km_v2_cru_ts4.00.",start_year,".",end_year,".tmn.4d.nc")
tmn_ncfile <- paste(path,ncfile,sep="")

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
year <- seq(start_year,end_year, by=1)
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
dname <- "tmn"
dlongname <- "near-surface temperature minimum"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,monthdim,yeardim),fillvalue,dlongname,prec="double")
ncout <- nc_create(tmn_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
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
ncvar_put(ncout,var_def,tmn4d)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.00_regrid_abs.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm

remove(tmn4d)

ptm <- proc.time() # timer
ncfile <- paste0("na10km_v2_cru_ts4.00.",start_year,".",end_year,".tmx.4d.nc")
tmx_ncfile <- paste(path,ncfile,sep="")

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
year <- seq(start_year,end_year, by=1)
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
dname <- "tmx"
dlongname <- "near-surface temperature maximum"
var_def <- ncvar_def(dname,dunits$value,list(xdim,ydim,monthdim,yeardim),fillvalue,dlongname,prec="double")
ncout <- nc_create(tmx_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
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
ncvar_put(ncout,var_def,tmx4d)

# add global attributes
ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by cru_ts_4.00_regrid_abs.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
proc.time() - ptm
