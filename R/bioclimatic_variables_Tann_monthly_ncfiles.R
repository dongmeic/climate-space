# Created by Dongmei Chen
# To generate NetCDF files for bioclimatic variables - "adaptive seasonality" (monthly)

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
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/daily/20180625/"

# variables (6)
# Tmean - mean of monthly average of daily mean temperature from August in the prior year through July in the year of the outbreak
# Tvar - standard deviation of monthly average of daily mean temperature from August in the prior year through July in the year of the outbreak
# TOctSep - mean of monthly average of daily mean temperature from October in the prior year through September in the year of the outbreak
# TMarAug - mean of monthly average of daily mean temperature from March through August in the year of the outbreak
# summerTmean - mean of monthly average of daily mean temperature from June through August in the year of the outbreak
# AugTmean - monthly average of daily mean temperature in August in the year of the outbreak
# AugTmax - monthly average of daily maximum temperature in August in the year of the outbreak

print("read mean temperature netCDF file")
ncfile <- "na10km_v2_cru_ts4.01.1901.2016.tmp.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "tmp"
tmp <- ncvar_get(ncin,dname)

ncfile <- "na10km_v2_cru_ts4.01.1901.2016.tmx.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "tmx"
tmx <- ncvar_get(ncin,dname)

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
Tmean_3d <- array(NA, dim=c(nx, ny, nt))
Tvar_3d <- array(NA, dim=c(nx, ny, nt))
TOctSep_3d <- array(NA, dim=c(nx, ny, nt))
TMarAug_3d <- array(NA, dim=c(nx, ny, (nt+1)))
summerTmean_3d <- array(NA, dim=c(nx, ny, (nt+1)))
AugTmean_3d <- array(NA, dim=c(nx, ny, (nt+1)))
AugTmax_3d <- array(NA, dim=c(nx, ny, (nt+1)))

print("calculate long term means")
ptm <- proc.time()
for (k in 1:nt){
	Tmean_3d[,,k] <- apply(abind(tmp[,,8:12,k],tmp[,,1:7,(k+1)],along=3),c(1,2),mean)
	Tvar_3d[,,k] <- apply(abind(tmp[,,8:12,k],tmp[,,1:7,(k+1)],along=3),c(1,2),sd)
	TOctSep_3d[,,k] <- apply(abind(tmp[,,10:12,k],tmp[,,1:9,(k+1)],along=3),c(1,2),mean)
	print(k)
}
for (k in 1:(nt+1)){
	TMarAug_3d[,,k] <- apply(abind(tmp[,,3:8,k],along=3),c(1,2),mean)
	summerTmean_3d[,,k] <- apply(abind(tmp[,,6:8,k],along=3),c(1,2),mean)
	AugTmean_3d[,,k] <- tmp[,,8,k]
	AugTmax_3d[,,k] <- tmx[,,8,k]
	print(k)
}
proc.time() - ptm

print("calculate standard deviations")
Tmean_ltm <- apply(Tmean_3d, c(1,2), mean, na.rm=TRUE)
Tmean_std <- apply(Tmean_3d, c(1,2), sd, na.rm=TRUE)
Tvar_ltm <- apply(Tvar_3d, c(1,2), mean, na.rm=TRUE)
Tvar_std <- apply(Tvar_3d, c(1,2), sd, na.rm=TRUE)
TOctSep_ltm <- apply(TOctSep_3d, c(1,2), mean, na.rm=TRUE)
TOctSep_std <- apply(TOctSep_3d, c(1,2), sd, na.rm=TRUE)
TMarAug_ltm <- apply(TMarAug_3d, c(1,2), mean, na.rm=TRUE)
TMarAug_std <- apply(TMarAug_3d, c(1,2), sd, na.rm=TRUE)
summerTmean_ltm <- apply(summerTmean_3d, c(1,2), mean, na.rm=TRUE)
summerTmean_std <- apply(summerTmean_3d, c(1,2), sd, na.rm=TRUE)
AugTmean_ltm <- apply(AugTmean_3d, c(1,2), mean, na.rm=TRUE)
AugTmean_std <- apply(AugTmean_3d, c(1,2), sd, na.rm=TRUE)
AugTmax_ltm <- apply(AugTmax_3d, c(1,2), mean, na.rm=TRUE)
AugTmax_std <- apply(AugTmax_3d, c(1,2), sd, na.rm=TRUE)

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

for (yr in (nt-nyr+1):nt){
	Tmean_slice <- apply(abind(tmp[,,8:12,yr],tmp[,,1:7,(yr+1)],along=3), c(1,2), mean)
	Tmean_std_slice <- (Tmean_slice - Tmean_ltm)/Tmean_std
	Tvar_slice <- apply(abind(tmp[,,8:12,yr],tmp[,,1:7,(yr+1)],along=3),c(1,2),sd)
	Tvar_std_slice <- (Tvar_slice - Tvar_ltm)/Tvar_std
	TOctSep_slice <- apply(abind(tmp[,,10:12,yr],tmp[,,1:9,(yr+1)],along=3),c(1,2),mean)
	TOctSep_std_slice <- (TOctSep_slice - TOctSep_ltm)/TOctSep_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	Tmean_vgt <- Tmean_slice * vgt
	Tvar_vgt <- Tvar_slice * vgt
	TOctSep_vgt <- TOctSep_slice * vgt
	
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	Tmean_btl <- Tmean_slice * btl_slice
	Tvar_btl <- Tvar_slice * btl_slice
	TOctSep_btl <- TOctSep_slice * btl_slice
	
	# get standard deviation
	Tmean_std_vgt <- Tmean_std_slice * vgt
	Tvar_std_vgt <- Tvar_std_slice * vgt
	TOctSep_std_vgt <- TOctSep_std_slice * vgt
	
	Tmean_std_btl <- Tmean_std_slice * btl_slice
	Tvar_std_btl <- Tvar_std_slice * btl_slice
	TOctSep_std_btl <- TOctSep_std_slice * btl_slice
	
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		Tmean <- abind(Tmean_slice, Tmean_vgt, Tmean_btl, along=3)
		Tvar <- abind(Tvar_slice, Tvar_vgt, Tvar_btl, along=3)
		TOctSep <- abind(TOctSep_slice, TOctSep_vgt, TOctSep_btl, along=3)
		
		Tmean_std_all <- abind(Tmean_std_slice, Tmean_std_vgt, Tmean_std_btl, along=3)
		Tvar_std_all <- abind(Tvar_std_slice, Tvar_std_vgt, Tvar_std_btl, along=3)
		TOctSep_std_all <- abind(TOctSep_std_slice, TOctSep_std_vgt, TOctSep_std_btl, along=3)
	}else{
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		Tmean <- abind(Tmean, Tmean_slice, Tmean_vgt, Tmean_btl, along=3)
		Tvar <- abind(Tvar, Tvar_slice, Tvar_vgt, Tvar_btl, along=3)
		TOctSep <- abind(TOctSep, TOctSep_slice, TOctSep_vgt, TOctSep_btl, along=3)
		
		Tmean_std_all <- abind(Tmean_std_all, Tmean_std_slice, Tmean_std_vgt, Tmean_std_btl, along=3)
		Tvar_std_all <- abind(Tvar_std_all, Tvar_std_slice, Tvar_std_vgt, Tvar_std_btl, along=3)
		TOctSep_std_all <- abind(TOctSep_std_all, TOctSep_std_slice, TOctSep_std_vgt, TOctSep_std_btl, along=3)
	}
	print(paste0(years[btlyr], " is done!"))
}
proc.time() - ptm

print("reshape 3d to 4d")
# reshape 3d array to 4d
nv <- 3 # three variables: land, tree, beetle
Tmean_4d <- array(Tmean, dim=c(nx,ny,nv,nyr)) 
Tvar_4d <- array(Tvar, dim=c(nx,ny,nv,nyr))
TOctSep_4d <- array(TOctSep, dim=c(nx,ny,nv,nyr)) 

Tmean_std_4d <- array(Tmean_std_all, dim=c(nx,ny,nv,nyr)) 
Tvar_std_4d <- array(Tvar_std_all, dim=c(nx,ny,nv,nyr))
TOctSep_std_4d <- array(TOctSep_std_all, dim=c(nx,ny,nv,nyr))

print("quick maps...")
# quick maps to check data
n <- nyr
Tmean_slice_3d <- Tmean_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-60,-40,-20,-10,0,5,10,15,25,30,35)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_Tmean_tree_",end_year,".png",sep=""))
levelplot(Tmean_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
Tvar_std_slice_4d <- Tvar_std_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_Tvar_std_beetle_",end_year,".png",sep=""))
levelplot(Tvar_std_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# combine all 4d for a loop
var_all_4d <- abind(Tmean_4d, Tvar_4d, TOctSep_4d, Tmean_std_4d, Tvar_std_4d, TOctSep_std_4d, along=4)
# write 4d data in a loop
print("start to write 4d data")

dnames <- c("Tmean", "Tvar", "TOctSep", "Tmean_std", "Tvar_std", "TOctSep_std")
dlongnames <- c("Mean of monthly average of daily mean temperature from August to July",
				"Standard deviation of monthly average of daily mean temperature from August to July",
				"Mean of monthly average of daily mean temperature from October to September in current water year",
				"Departure from the long-term (1901-2016) mean of mean of monthly average of daily mean temperature from August to July",
				"Departure from the long-term (1901-2016) mean of standard deviation of monthly average of daily mean temperature from August to July",
				"Departure from the long-term (1901-2016) mean of mean of monthly average of daily mean temperature from October to September in current water year")
dunits <- c("°C", "°C", "°C", "", "", "")

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
	ncatt_put(ncout,0,"title","CRU TS 4.01 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tann_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1997-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

var_all_3d <- abind(Tmean_3d, Tvar_3d, TOctSep_3d, along=3)
# write 3d data in a loop
print("start to write 3d data")
dnames <- c("Tmean", "Tvar", "TOctSep")
dlongnames <- c("Mean of monthly average of daily mean temperature from August to July",
				"Standard deviation of monthly average of daily mean temperature from August to July",
				"Mean of monthly average of daily mean temperature from October to September in current water year")
dunits <- c("°C", "°C", "°C")
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
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tann_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1902-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

# repeat the same process as above for the rest four bioclimatic variables
start_year = 1901; end_year = 2016; first_year = 1997
years = first_year:end_year; nyr <- length(years)
for (yr in (nt-nyr+2):(nt+1)){

	TMarAug_slice <- apply(abind(tmp[,,3:8,yr],along=3),c(1,2),mean)
	TMarAug_std_slice <- (TMarAug_slice - TMarAug_ltm)/TMarAug_std
	summerTmean_slice <- apply(abind(tmp[,,6:8,yr],along=3),c(1,2),mean)
	summerTmean_std_slice <- (summerTmean_slice - summerTmean_ltm)/summerTmean_std
	AugTmean_slice <- tmp[,,8,yr]
	AugTmean_std_slice <- (AugTmean_slice - AugTmean_ltm)/AugTmean_std
	AugTmax_slice <- tmx[,,8,yr]
	AugTmax_std_slice <- (AugTmax_slice - AugTmax_ltm)/AugTmax_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	TMarAug_vgt <- TMarAug_slice * vgt
	summerTmean_vgt <- summerTmean_slice * vgt
	AugTmean_vgt <- AugTmean_slice * vgt
	AugTmax_vgt <- AugTmax_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr + 1)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	TMarAug_btl <- TMarAug_slice * btl_slice
	summerTmean_btl <- summerTmean_slice * btl_slice
	AugTmean_btl <- AugTmean_slice * btl_slice
	AugTmax_btl <- AugTmax_slice * btl_slice
	
	# get standard deviation
	TMarAug_std_vgt <- TMarAug_std_slice * vgt
	summerTmean_std_vgt <- summerTmean_std_slice * vgt
	AugTmean_std_vgt <- AugTmean_std_slice * vgt
	AugTmax_std_vgt <- AugTmax_std_slice * vgt
	
	TMarAug_std_btl <- TMarAug_std_slice * btl_slice
	summerTmean_std_btl <- summerTmean_std_slice * btl_slice
	AugTmean_std_btl <- AugTmean_std_slice * btl_slice
	AugTmax_std_btl <- AugTmax_std_slice * btl_slice
	
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		TMarAug <- abind(TMarAug_slice, TMarAug_vgt, TMarAug_btl, along=3)
		summerTmean <- abind(summerTmean_slice, summerTmean_vgt, summerTmean_btl, along=3)
		AugTmean <- abind(AugTmean_slice, AugTmean_vgt, AugTmean_btl, along=3)
		AugTmax <- abind(AugTmax_slice, AugTmax_vgt, AugTmax_btl, along=3)
		
		TMarAug_std_all <- abind(TMarAug_std_slice, TMarAug_std_vgt, TMarAug_std_btl, along=3)
		summerTmean_std_all <- abind(summerTmean_std_slice, summerTmean_std_vgt, summerTmean_std_btl, along=3)
		AugTmean_std_all <- abind(AugTmean_std_slice, AugTmean_std_vgt, AugTmean_std_btl, along=3)
		AugTmax_std_all <- abind(AugTmax_std_slice, AugTmax_std_vgt, AugTmax_std_btl, along=3)
	}else{
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		TMarAug <- abind(TMarAug, TMarAug_slice, TMarAug_vgt, TMarAug_btl, along=3)
		summerTmean <- abind(summerTmean, summerTmean_slice, summerTmean_vgt, summerTmean_btl, along=3)
		AugTmean <- abind(AugTmean, AugTmean_slice, AugTmean_vgt, AugTmean_btl, along=3)
		AugTmax <- abind(AugTmax, AugTmax_slice, AugTmax_vgt, AugTmax_btl, along=3)
		
		TMarAug_std_all <- abind(TMarAug_std_all, TMarAug_std_slice, TMarAug_std_vgt, TMarAug_std_btl, along=3)
		summerTmean_std_all <- abind(summerTmean_std_all, summerTmean_std_slice, summerTmean_std_vgt, summerTmean_std_btl, along=3)
		AugTmean_std_all <- abind(AugTmean_std_all, AugTmean_std_slice, AugTmean_std_vgt, AugTmean_std_btl, along=3)
		AugTmax_std_all <- abind(AugTmax_std_all, AugTmax_std_slice, AugTmax_std_vgt, AugTmax_std_btl, along=3)
	}
	print(paste0(years[btlyr], " is done!"))
}
proc.time() - ptm

print("reshape 3d to 4d")
# reshape 3d array to 4d
nv <- 3 # three variables: land, tree, beetle
TMarAug_4d <- array(TMarAug, dim=c(nx,ny,nv,nyr)) 
summerTmean_4d <- array(summerTmean, dim=c(nx,ny,nv,nyr)) 
AugTmean_4d <- array(AugTmean, dim=c(nx,ny,nv,nyr))
AugTmax_4d <- array(AugTmax, dim=c(nx,ny,nv,nyr))

TMarAug_std_4d <- array(TMarAug_std_all, dim=c(nx,ny,nv,nyr)) 
summerTmean_std_4d <- array(summerTmean_std_all, dim=c(nx,ny,nv,nyr)) 
AugTmean_std_4d <- array(AugTmean_std_all, dim=c(nx,ny,nv,nyr))
AugTmax_std_4d <- array(AugTmax_std_all, dim=c(nx,ny,nv,nyr)) 

print("quick maps...")
# quick maps to check data
n <- nyr
TMarAug_slice_3d <- TMarAug_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-20,-10,-5,0,5,10,15,20,25,30,35)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_TMarAug_tree_",end_year,".png",sep=""))
levelplot(TMarAug_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
summerTmean_std_slice_4d <- summerTmean_std_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_summerTmean_std_beetle_",end_year,".png",sep=""))
levelplot(summerTmean_std_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# combine all 4d for a loop
var_all_4d <- abind(TMarAug_4d, summerTmean_4d, AugTmean_4d, AugTmax_4d, TMarAug_std_4d, summerTmean_std_4d, AugTmean_std_4d, AugTmax_std_4d, along=4)
# write 4d data in a loop
print("start to write 4d data")

dnames <- c("TMarAug", "summerTmean", "AugTmean", "AugTmax", "TMarAug_std", "summerTmean_std", "AugTmean_std", "AugTmax_std")
dlongnames <- c("Mean of monthly average of daily mean temperature from March to August",
				"Mean of monthly average of daily mean temperature from June to August in current year",
				"Monthly average of daily mean temperature in August",
				"Monthly average of daily maximum temperature in August",
				"Departure from the long-term (1901-2016) mean of mean of monthly average of daily mean temperature from March to August",
				"Departure from the long-term (1901-2016) mean of mean of monthly average of daily mean temperature from June to August in current year",
				"Departure from the long-term (1901-2016) mean of monthly average of daily mean temperature in August",
				"Departure from the long-term (1901-2016) mean of monthly average of daily maximum temperature in August")
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
	ncatt_put(ncout,0,"title","CRU TS 4.01 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tann_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1997-2016")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

var_all_3d <- abind(TMarAug_3d, summerTmean_3d, AugTmean_3d, AugTmax_3d, along=3)
# write 3d data in a loop
print("start to write 3d data")
dnames <- c("TMarAug", "summerTmean", "AugTmean", "AugTmax")
dlongnames <- c("Mean of monthly average of daily mean temperature from March to August",
				"Mean of monthly average of daily mean temperature from June to August in current year",
				"Monthly average of daily mean temperature in August",
				"Monthly average of daily maximum temperature in August")
dunits <- c("°C","°C","°C","°C")
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
	ncatt_put(ncout,0,"title","CRU TS 4.01 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Tann_monthly_ncfiles.R")
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