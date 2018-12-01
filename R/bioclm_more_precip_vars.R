# cancelled - unclear definition in the variables

library(ncdf4)
library(abind)
library(lattice)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
prefile <- "ts/na10km_v2_cru_ts4.01.1996.2015.pre.abs4d.nc"
tmpfile <- "ts/na10km_v2_cru_ts4.01.1996.2015.tmp.abs4d.nc"
ltmfile <- "ltm/na10km_v2_pre.nc"

ncin <- nc_open(paste0(ncpath, prefile))
print(ncin)
ncin_tmp <- nc_open(paste0(ncpath, tmpfile))
print(ncin_tmp)
ncin_ltm <- nc_open(paste0(ncpath, ltmfile))
print(ncin_ltm)

dname <- "pre"
pre <- ncvar_get(ncin,dname)
dim(pre)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
ltm <- ncvar_get(ncin_ltm,dname)
dim(ltm)

dname <- "tmp"
tmp <- ncvar_get(ncin_tmp,dname)
dim(tmp)

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
nc_close(ncin_tmp)
nc_close(ncin_ltm)

pre[pre==fillvalue$value] <- NA
tmp[tmp==fillvalue$value] <- NA
ltm[pre==fillvalue$value] <- NA

nt <- dim(pre)[4]
gsp.diff_3d <- array(NA, dim=c(nx, ny, nt))
aridity_3d <- array(NA, dim=c(nx, ny, nt))
summerPvar_3d <- array(NA, dim=c(nx, ny, nt))

pre_ltm <- apply(abind(ltm[,,4:6], along=3), c(1,2),sum)

for (k in 1:nt){
	gsp <- apply(abind(pre[,,4:6,k], along=3),c(1,2),sum)
	gsp.diff_3d[,,k] <- gsp - pre_ltm
	summerPvar_3d[,,k] <- apply(abind(pre[,,6:8,k],along=3),c(1,2),var)
	summerTmean <- apply(abind(tmp[,,6:8,k],along=3),c(1,2),mean)
	summerPmean <- apply(abind(pre[,,6:8,k],along=3),c(1,2),mean)
	aridity_3d[,,k] <- summerPmean/summerTmean
	print(k)
}

# quick maps to check data
n <- 1
aridity_3d_slice <- aridity_3d[,,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,1,2,3,4,5,6,7,8,9,10)
options(bitmapType='cairo')
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/daily/20181129/"
png(file=paste(out,"na10km_v2_aridity_test.png",sep=""))
levelplot(aridity_3d_slice ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

var_all_3d <- abind(gsp.diff_3d, summerPvar_3d, aridity_3d, along=3)
# write 3d data in a loop
dnames <- c("gsp.diff", "summerPvar", "aridity")
dlongnames <- c("Difference between total precipitation from April to June and long-term average",
				"Variability in summer precipitation",
				"Ratio of mean summer precipitation to mean summer temperature")
dunits <- c("mm", "mm", "")
d1 <- dim(var_all_3d)[1];d2 <- dim(var_all_3d)[2]; d3 <- (dim(var_all_3d)[3])/(length(dnames))
start_year <- 1996; end_year <- 2015
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
	ncatt_put(ncout,0,"source","generated by bioclm_more_precip_vars.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm