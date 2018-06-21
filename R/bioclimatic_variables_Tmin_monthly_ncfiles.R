# Created by Dongmei Chen
# To generate NetCDF files for bioclimatic variables - "cold mortality" (monthly)

print("load libraries...")
library(ncdf4)
library(abind)
library(lattice)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

path <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/20180610/"

# variables (6)
# winterTmin - minimum monthly average of daily minimum temperature during winter (from December in the prior year through February in the year of the outbreak)
# Tmin - mean of monthly average of minimum temperature from November in the prior year through March in the year of the outbreak
# OctTmin - monthly average daily minimum temperature in October in the year prior to the outbreak
# fallTmean - mean of monthly average of daily mean temperature from September through November in the year prior to the outbreak
# JanTmin - monthly average daily minimum temperature in January in the year of the outbreak
# MarTmin - monthly average daily minimum temperature in March in the year of the outbreak

print("read minimum temperature netCDF file")
ncfile <- "na10km_v2_cru_ts4.01.1901.2016.tmn.4d.nc"
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

start_year = 1901; end_year = 2016; first_year = 1997
years = first_year:end_year; nyr <- length(years)
# get long-term means
# define array for long-term means
nt <- end_year - start_year
winterTmin_3d <- array(NA, dim=c(nx, ny, nt))
Tmin_3d <- array(NA, dim=c(nx, ny, nt))
fallTmean_3d <- array(NA, dim=c(nx, ny, (nt+1)))
OctTmin_3d <- array(NA, dim=c(nx, ny, (nt+1)))
JanTmin_3d <- array(NA, dim=c(nx, ny, (nt+1)))
MarTmin_3d <- array(NA, dim=c(nx, ny, (nt+1)))

print("calculate long term means")
ptm <- proc.time()
for (k in 1:nt){
	winterTmin_3d[,,k] <- apply(abind(tmn[,,12,k],tmn[,,1:2,(k+1)],along=3),c(1,2),min)
	Tmin_3d[,,k] <- apply(abind(tmn[,,11:12,k],tmn[,,1:3,(k+1)],along=3),c(1,2),mean)
	print(k)
}
for (k in 1:(nt+1)){
	fallTmean_3d[,,k] <- apply(abind(tmp[,,9:11,k],along=3),c(1,2),mean)
	OctTmin_3d[,,k] <- tmn[,,10,k]
	JanTmin_3d[,,k] <- tmn[,,1,k]
	MarTmin_3d[,,k] <- tmn[,,3,k]
	print(k)
}
proc.time() - ptm

print("calculate standard deviations")
winterTmin_ltm <- apply(winterTmin_3d, c(1,2), mean, na.rm=TRUE)
winterTmin_std <- apply(winterTmin_3d, c(1,2), sd, na.rm=TRUE)
Tmin_ltm <- apply(Tmin_3d, c(1,2), mean, na.rm=TRUE)
Tmin_std <- apply(Tmin_3d, c(1,2), sd, na.rm=TRUE)
fallTmean_ltm <- apply(fallTmean_3d, c(1,2), mean, na.rm=TRUE)
fallTmean_std <- apply(fallTmean_3d, c(1,2), sd, na.rm=TRUE)
OctTmin_ltm <- apply(OctTmin_3d, c(1,2), mean, na.rm=TRUE)
OctTmin_std <- apply(OctTmin_3d, c(1,2), sd, na.rm=TRUE)
JanTmin_ltm <- apply(JanTmin_3d, c(1,2), mean, na.rm=TRUE)
JanTmin_std <- apply(JanTmin_3d, c(1,2), sd, na.rm=TRUE)
MarTmin_ltm <- apply(MarTmin_3d, c(1,2), mean, na.rm=TRUE)
MarTmin_std <- apply(MarTmin_3d, c(1,2), sd, na.rm=TRUE)

# read vegetation and bettle presence data
prs_path <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste(prs_path,vgt_ncfile,sep=""))
print(ncin_vgt)
vgt <- ncvar_get(ncin_vgt,"navgtprs")
btl_ncfile <- "na10km_v2_mpb_presence.nc"
ncin_btl <- nc_open(paste(prs_path,btl_ncfile, sep=""))
print(ncin_btl)
btl <- ncvar_get(ncin_btl,"mpb_prs")

print("get 3d array for climatic variables")
ptm <- proc.time()
# winterTmin, OctTmin, Tmin: year from first_year to end_year-1
# JanTmin, MarTmin: year from first_year to end_year

print("minimum winter temperatures...")
for (yr in (nt-nyr+1):nt){
	winterTmin_slice <- apply(abind(tmn[,,12,yr],tmn[,,1:2,(yr+1)],along=3), c(1,2), min)
	winterTmin_std_slice <- (winterTmin_slice - winterTmin_ltm)/winterTmin_std
	OctTmin_slice <- tmn[,,10,yr]
	OctTmin_std_slice <- (OctTmin_slice - OctTmin_ltm)/OctTmin_std
	fallTmean_slice <- apply(abind(tmp[,,9:11,yr],along=3), c(1,2), mean)
	fallTmean_std_slice <- (fallTmean_slice - fallTmean_ltm)/fallTmean_std
	Tmin_slice <- apply(abind(tmn[,,11:12,yr],tmn[,,1:3,(yr+1)],along=3), c(1,2), mean)
	Tmin_std_slice <- (Tmin_slice - Tmin_ltm)/Tmin_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	winterTmin_vgt <- winterTmin_slice * vgt
	OctTmin_vgt <- OctTmin_slice * vgt
	fallTmean_vgt <- fallTmean_slice * vgt
	Tmin_vgt <- Tmin_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	winterTmin_btl <- winterTmin_slice * btl_slice
	OctTmin_btl <- OctTmin_slice * btl_slice
	fallTmean_btl <- fallTmean_slice * btl_slice
	Tmin_btl <- Tmin_slice * btl_slice
	
	# get standard deviation
	winterTmin_std_vgt <- winterTmin_std_slice * vgt
	OctTmin_std_vgt <- OctTmin_std_slice * vgt
	fallTmean_std_vgt <- fallTmean_std_slice * vgt
	Tmin_std_vgt <- Tmin_std_slice * vgt
	
	winterTmin_std_btl <- winterTmin_std_slice * btl_slice
	OctTmin_std_btl <- OctTmin_std_slice * btl_slice
	fallTmean_std_btl <- fallTmean_std_slice * btl_slice
	Tmin_std_btl <- Tmin_std_slice * btl_slice
	
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		winterTmin <- abind(winterTmin_slice, winterTmin_vgt, winterTmin_btl, along=3)
		OctTmin <- abind(OctTmin_slice, OctTmin_vgt, OctTmin_btl, along=3)
		fallTmean <- abind(fallTmean_slice, fallTmean_vgt, fallTmean_btl, along=3)
		Tmin <- abind(Tmin_slice, Tmin_vgt, Tmin_btl, along=3)
		
		winterTmin_std_all <- abind(winterTmin_std_slice, winterTmin_std_vgt, winterTmin_std_btl, along=3)
		OctTmin_std_all <- abind(OctTmin_std_slice, OctTmin_std_vgt, OctTmin_std_btl, along=3)
		fallTmean_std_all <- abind(fallTmean_std_slice, fallTmean_std_vgt, fallTmean_std_btl, along=3)
		Tmin_std_all <- abind(Tmin_std_slice, Tmin_std_vgt, Tmin_std_btl, along=3)
	}else{
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		winterTmin <- abind(winterTmin, winterTmin_slice, winterTmin_vgt, winterTmin_btl, along=3)
		OctTmin <- abind(OctTmin, OctTmin_slice, OctTmin_vgt, OctTmin_btl, along=3)
		fallTmean <- abind(fallTmean, fallTmean_slice, fallTmean_vgt, fallTmean_btl, along=3)
		Tmin <- abind(Tmin, Tmin_slice, Tmin_vgt, Tmin_btl, along=3)
		
		winterTmin_std_all <- abind(winterTmin_std_all, winterTmin_std_slice, winterTmin_std_vgt, winterTmin_std_btl, along=3)
		OctTmin_std_all <- abind(OctTmin_std_all, OctTmin_std_slice, OctTmin_std_vgt, OctTmin_std_btl, along=3)
		fallTmean_std_all <- abind(fallTmean_std_all, fallTmean_std_slice, fallTmean_std_vgt, fallTmean_std_btl, along=3)
		Tmin_std_all <- abind(Tmin_std_all, Tmin_std_slice, Tmin_std_vgt, Tmin_std_btl, along=3)
	}
	print(paste0(years[btlyr], " is done!"))
}
proc.time() - ptm

print("reshape 3d to 4d")
# reshape 3d array to 4d
nv <- 3 # three variables: land, tree, beetle
winterTmin_4d <- array(winterTmin, dim=c(nx,ny,nv,nyr)) 
OctTmin_4d <- array(OctTmin, dim=c(nx,ny,nv,nyr))
fallTmean_4d <- array(fallTmean, dim=c(nx,ny,nv,nyr)) 
Tmin_4d <- array(Tmin, dim=c(nx,ny,nv,nyr)) 

winterTmin_std_4d <- array(winterTmin_std_all, dim=c(nx,ny,nv,nyr)) 
OctTmin_std_4d <- array(OctTmin_std_all, dim=c(nx,ny,nv,nyr))
fallTmean_std_4d <- array(fallTmean_std_all, dim=c(nx,ny,nv,nyr))
Tmin_std_4d <- array(Tmin_std_all, dim=c(nx,ny,nv,nyr)) 

print("quick maps...")
# quick maps to check data
n <- nyr
winterTmin_slice_3d <- winterTmin_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-60,-40,-20,-10,0,5,10,15,25,30,35)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_winterTmin_tree_",end_year,".png",sep=""))
levelplot(winterTmin_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
Tmin_std_slice_4d <- Tmin_std_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_Tmin_std_beetle_",end_year,".png",sep=""))
levelplot(Tmin_std_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# combine all 4d for a loop
var_all_4d <- abind(winterTmin_4d, OctTmin_4d, fallTmean_4d, Tmin_4d, winterTmin_std_4d, OctTmin_std_4d, fallTmean_std_4d, Tmin_std_4d, along=4)
# write 4d data in a loop
print("start to write 4d data")

dnames <- c("winterTmin", "OctTmin", "fallTmean", "Tmin", "winterTmin_std", "OctTmin_std", "fallTmean_std", "Tmin_std")
dlongnames <- c("Minimum monthly average of daily minimum temperature during winter",
				"Monthly average daily minimum temperature in October",
				"Mean of monthly average of daily mean temperature from September to November",
				"Mean of monthly average of minimum temperature from November to March",
				"Departure from the long-term (1901-2016) mean of minimum monthly average of daily minimum temperature during winter",
				"Departure from the long-term (1901-2016) mean of monthly average daily minimum temperature in October",
				"Departure from the long-term (1901-2016) mean of mean of monthly average of daily mean temperature from September to November",
				"Departure from the long-term (1901-2016) mean of mean of monthly average of minimum temperature from November to March")
dunits <- c("°C", "°C", "°C", "°C", "", "", "", "")

d1 <- dim(var_all_4d)[1];d2 <- dim(var_all_4d)[2];d3 <- dim(var_all_4d)[3];d4 <- (dim(var_all_4d)[4])/(length(dnames))
start_year <- 1997; end_year <- 2016
nyr <- length(start_year:end_year)
ptm <- proc.time() # timer
foreach(i=1:length(dnames)) %dopar%{

	filenm <- paste0("na10km_v2_",dnames[i],"_",start_year,".",end_year,".4d.nc")
	ncfile <- paste0(path,"var/",filenm)

	# define dimensions
	xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
	ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
	year <- seq(start_year,end_year, by=1)
	yeardim <- ncdim_def("year","year",as.integer(year))
	vars <- seq(1,3, by=1)
	vardim <- ncdim_def("variable","variable",as.integer(vars))

	# define common variables
	fillvalue <- 1e32
	dlname <- "Longitude of cell center"
	lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
	dlname <- "Latitude of cell center"
	lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
	projname <- crs_name
	proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

	# create netCDF file and put data
	var_def <- ncvar_def(dnames[i],dunits[i],list(xdim,ydim,vardim,yeardim),fillvalue,dlongnames[i],prec="double")
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
	var4d <- array(var_all_4d[,,,(1+nyr*(i-1)):(nyr*i)],dim=c(d1,d2,d3,d4))
	ncvar_put(ncout,lon_def,lon)
	ncvar_put(ncout,lat_def,lat)
	ncvar_put(ncout,var_def,var4d)

	# add global attributes
	ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tmin_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1997-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

var_all_3d <- abind(winterTmin_3d, OctTmin_3d, fallTmean_3d, Tmin_3d, along=3)
# write 3d data in a loop
print("start to write 3d data")
dnames <- c("winterTmin", "OctTmin", "fallTmean", "Tmin")
dlongnames <- c("Minimum monthly average of daily minimum temperature during winter",
				"Monthly average daily minimum temperature in October",
				"Mean of monthly average of daily mean temperature from September to November",
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
	ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
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

# repeat the same process as above for the rest two bioclimatic variables
start_year = 1901; end_year = 2016; first_year = 1997
years = first_year:end_year; nyr <- length(years)
for (yr in (nt-nyr+2):(nt+1)){

	JanTmin_slice <- tmn[,,1,yr]
	JanTmin_std_slice <- (JanTmin_slice - JanTmin_ltm)/JanTmin_std
	MarTmin_slice <- tmn[,,3,yr]
	MarTmin_std_slice <- (MarTmin_slice - MarTmin_ltm)/MarTmin_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	JanTmin_vgt <- JanTmin_slice * vgt
	MarTmin_vgt <- MarTmin_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr + 1)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	JanTmin_btl <- JanTmin_slice * btl_slice
	MarTmin_btl <- MarTmin_slice * btl_slice
	
	# get standard deviation
	JanTmin_std_vgt <- JanTmin_std_slice * vgt
	MarTmin_std_vgt <- MarTmin_std_slice * vgt
	
	JanTmin_std_btl <- JanTmin_std_slice * btl_slice
	MarTmin_std_btl <- MarTmin_std_slice * btl_slice
	
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		JanTmin <- abind(JanTmin_slice, JanTmin_vgt, JanTmin_btl, along=3)
		MarTmin <- abind(MarTmin_slice, MarTmin_vgt, MarTmin_btl, along=3)
		
		JanTmin_std_all <- abind(JanTmin_std_slice, JanTmin_std_vgt, JanTmin_std_btl, along=3)
		MarTmin_std_all <- abind(MarTmin_std_slice, MarTmin_std_vgt, MarTmin_std_btl, along=3)
	}else{
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		JanTmin <- abind(JanTmin, JanTmin_slice, JanTmin_vgt, JanTmin_btl, along=3)
		MarTmin <- abind(MarTmin, MarTmin_slice, MarTmin_vgt, MarTmin_btl, along=3)
		
		JanTmin_std_all <- abind(JanTmin_std, JanTmin_std_slice, JanTmin_std_vgt, JanTmin_std_btl, along=3)
		MarTmin_std_all <- abind(MarTmin_std, MarTmin_std_slice, MarTmin_std_vgt, MarTmin_std_btl, along=3)
	}
	print(paste0(years[btlyr], " is done!"))
}
proc.time() - ptm

print("reshape 3d to 4d")
# reshape 3d array to 4d
nv <- 3 # three variables: land, tree, beetle
JanTmin_4d <- array(JanTmin, dim=c(nx,ny,nv,nyr)) 
MarTmin_4d <- array(MarTmin, dim=c(nx,ny,nv,nyr)) 

JanTmin_std_4d <- array(JanTmin_std_all, dim=c(nx,ny,nv,nyr)) 
MarTmin_std_4d <- array(MarTmin_std_all, dim=c(nx,ny,nv,nyr)) 

print("quick maps...")
# quick maps to check data
n <- nyr
JanTmin_slice_3d <- JanTmin_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-60,-40,-20,-10,0,5,10,15,25,30,35)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_JanTmin_tree_",end_year,".png",sep=""))
levelplot(JanTmin_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
MarTmin_std_slice_4d <- MarTmin_std_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_MarTmin_std_beetle_",end_year,".png",sep=""))
levelplot(MarTmin_std_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# combine all 4d for a loop
var_all_4d <- abind(JanTmin_4d, MarTmin_4d, JanTmin_std_4d, MarTmin_std_4d, along=4)
# write 4d data in a loop
print("start to write 4d data")

dnames <- c("JanTmin", "MarTmin", "JanTmin_std", "MarTmin_std")
dlongnames <- c("Monthly average daily minimum temperature in January",
				"Monthly average daily minimum temperature in March",
				"Departure from the long-term (1901-2016) mean of monthly average daily minimum temperature in January",
				"Departure from the long-term (1901-2016) mean of monthly average daily minimum temperature in March")
dunits <- c("°C", "°C", "", "")

d1 <- dim(var_all_4d)[1];d2 <- dim(var_all_4d)[2];d3 <- dim(var_all_4d)[3];d4 <- (dim(var_all_4d)[4])/(length(dnames))
start_year <- 1997; end_year <- 2016
nyr <- length(start_year:end_year)
ptm <- proc.time() # timer
foreach(i=1:length(dnames)) %dopar%{

	filenm <- paste0("na10km_v2_",dnames[i],"_",start_year,".",end_year,".4d.nc")
	ncfile <- paste0(path,"var/",filenm)

	# define dimensions
	xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
	ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
	year <- seq(start_year,end_year, by=1)
	yeardim <- ncdim_def("year","year",as.integer(year))
	vars <- seq(1,3, by=1)
	vardim <- ncdim_def("variable","variable",as.integer(vars))

	# define common variables
	fillvalue <- 1e32
	dlname <- "Longitude of cell center"
	lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
	dlname <- "Latitude of cell center"
	lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
	projname <- crs_name
	proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

	# create netCDF file and put data
	var_def <- ncvar_def(dnames[i],dunits[i],list(xdim,ydim,vardim,yeardim),fillvalue,dlongnames[i],prec="double")
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
	var4d <- array(var_all_4d[,,,(1+nyr*(i-1)):(nyr*i)],dim=c(d1,d2,d3,d4))
	ncvar_put(ncout,lon_def,lon)
	ncvar_put(ncout,lat_def,lat)
	ncvar_put(ncout,var_def,var4d)

	# add global attributes
	ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tmin_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1997-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

var_all_3d <- abind(JanTmin_3d, MarTmin_3d, along=3)
# write 3d data in a loop
print("start to write 3d data")
dnames <- c("JanTmin", "MarTmin")
dlongnames <- c("Monthly average daily minimum temperature in January",
				"Monthly average daily minimum temperature in March")
dunits <- c("°C","°C")
d1 <- dim(var_all_3d)[1];d2 <- dim(var_all_3d)[2]; d3 <- (dim(var_all_3d)[3])/(length(dnames))
start_year <- 1901; end_year <- 2016
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
	ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tmin_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1901-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm
print("all done!")