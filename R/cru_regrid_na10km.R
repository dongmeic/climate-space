library(ncdf4)
library(fields)
library(geosphere)
library(lattice)
library(RColorBrewer)

cru_ts_ltms_anomalies <- function(varname, varlname, varunit){
	# path
	ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/source/"
	ncoutpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/derived/"
	print("open points netCDF file to get dimensions, etc.")
	ncinfile <- paste0("cru_ts4.01.1901.2016.",varname,".dat.nc")
	ncin <- nc_open(paste0(ncpath,ncinfile))

	print("get dimension variables")
	lon <- ncvar_get(ncin, varid="lon"); nlon <- length(lon)
	lat <- ncvar_get(ncin, varid="lat"); nlat <- length(lat)
	time <- ncvar_get(ncin, varid="time"); nt <- length(time)
	tunits <- ncatt_get(ncin,"time","units")

	var3d <- ncvar_get(ncin,varname)
	fillvalue <- ncatt_get(ncin,varname,"_FillValue")$value

	print("close the input file")
	nc_close(ncin)

	print("replace netCDF _FillValues with R NA's")
	var3d[var3d==fillvalue] <- NA
	
	# quick maps to check data
	n <- 1344
	var_slice_3d <- var3d[,,n]
	grid <- expand.grid(lon=lon, lat=lat)
	# double check the cut points
	cutpts <- c(0,10,20,30,40,50,60,70,80,90,100)
	png(paste0(out,"cru_ts4.01.",varname,".2012.12.3d.png"), width=10, height=8, units="in", res=300)
	levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
		col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()
	
	print("reshape 3d array to 4d")
	nm <- 12 # number of months in year
	ny <- nt/12 # number of years in CRU data set
	var4d <- array(var3d, dim=c(nlon,nlat,nm,ny))

	# quick maps to check data
	m <- 6; n <- ny-2
	var_slice_4d <- var4d[,,m,n]
	grid <- expand.grid(lon=lon, lat=lat)
	cutpts <- c(0,10,20,30,40,50,60,70,80,90,100)
	png(paste0(out,"cru_ts4.01.",varname,".2014.06.4d.png"), width=10, height=8, units="in", res=300)
	levelplot(var_slice_4d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
		col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()

	print("make a missing data mask")
	landmask <- array(1, dim=c(nlon,nlat))
	# use last month of data to set data flag
	for (j in 1:nlon) {
		for (k in 1:nlat) {
			if (is.na(var3d[j,k,nt])) landmask[j,k]=NA
		}
	}

	print("get long-term means")
	begyr <- 1901; endyr <- 2016 # year range of CRU data 
	firstyr <- 1961; lastyear <- 1990 # year range for long-term means (e.g. 1961-1990)

	print("subscripts of first and last year")
	n1 <- firstyr-begyr+1; n2 <- lastyear-begyr+1

	print("define array for long-term means")
	ltm <- array(NA, dim=c(nlon,nlat,nm))
	# long-term means
	for (j in 1:nlon) {
		for (k in 1:nlat) {
			if (!is.na(landmask[j,k])) {
				for (m in 1:nm) {
					ltm[j,k,m] <- mean(var4d[j,k,m,n1:n2], na.rm=TRUE)
				}
			}
		}
	}
	
	# quick maps to check long-term means
	m <- 12
	var_slice_3d <- ltm[,,m]
	grid <- expand.grid(lon=lon, lat=lat)
	cutpts <- c(0,10,20,30,40,50,60,70,80,90,100)
	png(paste0(out,"cru_ts4.01.",varname,".ltm.12.3d.png"), width=10, height=8, units="in", res=300)
	levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
		col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()

  ltm[is.na(ltm)] <- fillvalue
	print("write out the long-term means")

	print("time -- values calculated by cf_time_subs.f90 -- 1961-1990")
	nbnds <- 2
	timeltm <-  c(27773.5, 27803.5, 27833.5, 27864, 27894.5, 27925, 27955.5, 27986.5,
		28017, 28047.5, 28078, 28108.5)
	climatology_bounds <- array( dim=c(nbnds,nm))
	climatology_bounds[1,] <- c( 22280, 22311, 22339, 22370, 22400, 22431,22461,
		22492, 22523, 22553, 22584, 22614)
	climatology_bounds[2,] <- c(32902, 32930, 32961, 32991, 33022, 33052, 33083,
		33114, 33144, 33175, 33205, 33236) 
	climatology_bounds
	bounds <- c(1,2)
	tunits <- "days since 1900-01-01 00:00:00.0 -0:00"

	print("define dimensions")
	londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
	latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
	tdim <- ncdim_def("time",units=tunits,longname="time",as.double(timeltm))
	bdim <- ncdim_def("nbnds",units="1",longname=NULL,as.double(bounds))
	dlname <- "climatology_bounds"
	bnds_def <- ncvar_def("climatology_bounds",tunits,list(bdim,tdim),NULL,dlname,prec="double")

	print("define variable")
	var_def <- ncvar_def(paste0(varname, "_ltm"),varunit,list(londim,latdim,tdim),fillvalue,varlname,prec="single")

	print("create netCDF file and put array")
	ncfname <- paste0(ncoutpath,"cru_ts4.01.1961-1990.",varname,".ltm.nc")
	ncout <- nc_create(ncfname,list(bnds_def,var_def),force_v4=TRUE, verbose=FALSE)

	print("put additional attributes into dimension and data variables")
	ncatt_put(ncout,"time","axis","T")
	ncatt_put(ncout,"time","calendar","standard")
	ncatt_put(ncout,"time","bounds","climatology_bounds")
	ncatt_put(ncout,"time","climatology","climatology_bounds")

	print("put variables")
	ncvar_put(ncout,bnds_def,climatology_bounds)
	ncvar_put(ncout,var_def,ltm)

	print("add global attributes")
	ncatt_put(ncout,0,"title","CRU TS 4.01 1961-1990 long-term means")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source",ncinfile)
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1961-1990")

	print("close the file, writing data to disk")
	nc_close(ncout)

	print("get anomalies (both 3d and 4d arrays)")
  ltm[ltm==fillvalue] <- NA
	for (j in 1:nlon) {
		for (k in 1:nlat) {
			if (!is.na(landmask[j,k])) {
				for (n in 1:ny) {
					for (m in 1:nm) {
						var3d[j,k,((n-1)*nm+m)] <- var3d[j,k,((n-1)*nm+m)] - ltm[j,k,m]
						var4d[j,k,m,n] <- var4d[j,k,m,n] - ltm[j,k,m]
					}
				}
			}
		}
	}

	# quick maps to check anomalies
	n <- 1380
	var_slice_3d <- var3d[,,n]
	grid <- expand.grid(lon=lon, lat=lat)
	cutpts <- c(-60,-20,-15,-10,-5,0,5,10,15,20,60)
	png(paste0(out,"cru_ts4.01.",varname,".anm.2015.12.3d.png"), width=10, height=8, units="in", res=300)
	levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
		col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()

	# quick maps to check anomalies
	m <- 6; n <- ny
	var_slice_4d <- var4d[,,m,n]
	grid <- expand.grid(lon=lon, lat=lat)
	cutpts <- c(-60,-20,-15,-10,-5,0,5,10,15,20,60)
	png(paste0(out,"cru_ts4.01.",varname,".anm.2016.06.4d.png"), width=10, height=8, units="in", res=300)
	levelplot(var_slice_4d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
		col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()

	var3d[is.na(var3d)] <- fillvalue
	var4d[is.na(var4d)] <- fillvalue

	print("write out the 3d data")

	print("define dimensions")
	londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
	latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
	timedim <- ncdim_def("time",tunits,as.double(time))

	print("define variable")
	fillvalue <- 1e32
	dlname <- paste(varlname,"anomalies")
	var_def <- ncvar_def(paste0(varname,"_anm"),varunit,list(londim,latdim,timedim),fillvalue,dlname,prec="single")

	print("create netCDF file and put arrays")
	ncfname <- paste0(ncoutpath,"cru_ts4.01.1901.2016.",varname,".anm3d.nc")
	ncout <- nc_create(ncfname,var_def,force_v4=T)

	print("put variables")
	ncvar_put(ncout,var_def,var3d)

	print("put additional attributes into dimension and data variables")
	ncatt_put(ncout,"time","axis","T")
	ncatt_put(ncout,"time","calendar","standard")
	ncatt_put(ncout,paste0(varname,"_anm"),"comment","anomalies, differences from 1961-1990 base period long-term means")

	print("add global attributes")
	ncatt_put(ncout,0,"title",paste("CRU TS 4.01",varlname,"anomalies"))
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source",ncinfile)
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"anomaly_base_period","1961-1990")

	print("close the file, writing data to disk")
	nc_close(ncout)

	print("write out the 4d data")

	print("define dimensions")
	londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
	latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
	year <- seq(1901,2016, by=1)
	yeardim <- ncdim_def("year","year",as.integer(year))
	month <- seq(1,12, by=1)
	monthdim <- ncdim_def("month","month",as.integer(month))

	print("define variables")
	fillvalue <- 1e32
	dlname <- paste(varlname,"anomalies")
	var.def <- ncvar_def(paste0(varname,"_anm"),varunit,list(londim,latdim,monthdim,yeardim),fillvalue,dlname,prec="single")

	print("create netCDF file and put arrays")
	ncfname <- paste0(ncoutpath,"cru_ts4.01.1901.2016.",varname,".anm4d.nc")
	ncout <- nc_create(ncfname,var.def,force_v4=T)

	print("put variables")
	ncvar_put(ncout,var.def,var4d)

	print("put additional attributes into dimension and data variables")
	ncatt_put(ncout,paste0(varname,"_anm"),"comment","anomalies, differences from 1961-1990 base period long-term means")

	print("add global attributes")
	ncatt_put(ncout,0,"title",paste("CRU TS 4.01",varlname,"anomalies"))
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source",ncinfile)
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"anomaly_base_period","1961-1990")

	print("close the file, writing data to disk")
	nc_close(ncout)
}

cru_ts_regrid_anomalies <- function(varname){
	print("open CRU netCDF file to get dimensions, etc.")
	crupath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/derived/"
	crufile <- paste0("cru_ts4.01.1901.2016.",varname,".anm3d.nc")
	cru_ncfile <- paste(crupath,crufile,sep="")
	ncin <- nc_open(cru_ncfile)

	print("get dimension variables")
	crulon <- ncvar_get(ncin, varid="lon"); ncrulon <- length(crulon)
	crulat <- ncvar_get(ncin, varid="lat"); ncrulat <- length(crulat)
	time <- ncvar_get(ncin, varid="time"); nt <- length(time)
	tunits <- ncatt_get(ncin,"time","units")

	print("get data")
	dname <- paste0(varname,"_anm")
	var3d <- ncvar_get(ncin,dname)
	dim(var3d)
	fillvalue <- ncatt_get(ncin,dname,"_FillValue")
	dunits <- ncatt_get(ncin,dname,"units")
	dlongname <- ncatt_get(ncin,dname,"long_name")

	print("close the CRU input file")
	nc_close(ncin)

	print("open na10km_v2 points netCDF file to get dimensions, etc.")
	napath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ltm/"
	nafile <- "na10km_v2.nc"
	na_ncfile <- paste(napath,nafile,sep="")
	ncin <- nc_open(na_ncfile)

	print("get dimension variables and attributes")
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

	print("get longitude and latitude and attributes")
	lon <- ncvar_get(ncin,"lon")
	lon_units <- ncatt_get(ncin, "lon", "units")$value
	lat <- ncvar_get(ncin,"lat")
	lat_units <- ncatt_get(ncin, "lat", "units")$value

	print("get CRS attributes")
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

	print("close the na10km_v2 file")
	nc_close(ncin)

	print("read the na10km_v2 grid target values")
	targpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/"
	targfile <- "na10km_v2.csv"
	na10km_v2 <- read.csv(paste(targpath, targfile, sep=""))
	head(na10km_v2)
	ntarg <- dim(na10km_v2)[1]

	print("begin interpolation")
	print("define arrrays to hold interpolated anomaly values")
	interp_mat <- array(NA, dim=c(nx, ny))
	interp_anm <- array(NA, dim=c(nx, ny, nt))
	interp_var <- rep(NA, ntarg)

	print("get array subscripts for each target point")
	j <- sapply(na10km_v2$x, function(c) which.min(abs(x-c)))
	k <- sapply(na10km_v2$y, function(c) which.min(abs(y-c)))

	print("bilinear interpolation for most points:")
	print("loop over times")
	for (n in 1:nt) {
		# print(n)
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

	# quick map to check data
	n <- 1380
	test_slice1 <- interp_anm[,,n]
	grid <- expand.grid(x=x, y=y)
	cutpts <- c(-60,-20,-15,-10,-5,0,5,10,15,20,60)
	png(paste0(out,"cru_ts4.01.",varname,".interp.2016.12.3d.png"), width=9, height=8, units="in", res=300)
	levelplot(test_slice1 ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
		col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()
	
	sum(is.na(test_slice1))
	print("nearest-neighbor assignment for missing bilinear-interpolation values")
	# use last time-slice

	print("find locations of non-missing CRU points")
	cru_grid <- expand.grid(clon=crulon, clat=crulat)
	cru_nonmiss <- data.frame(cbind(cru_grid,as.vector(var3d[,,nt])))
	names(cru_nonmiss) <- c("clon", "clat", "cru")
	sum(is.na(cru_nonmiss))
	cru_nonmiss <- cru_nonmiss[(!is.na(cru_nonmiss$cru)),]
	sum(is.na(cru_nonmiss))

	miss_id <- seq(1:length(interp_var))[is.na(interp_var)]
	nmiss <- length(miss_id)
	dist <- rep(1,nmiss)

	print("find closest nonmissing CRU point to each missing target point")
	ptm <- proc.time()
	for (i in 1:nmiss) {
		# print(i)
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
	cutpts <- c(-60,-20,-15,-10,-5,0,5,10,15,20,60)
	png(paste0(out,"cru_ts4.01.",varname,".nonmiss.2016.12.3d.png"), width=9, height=8, units="in", res=300)
	levelplot(test_slice2 ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
						col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()
	sum(is.na(test_slice2))

	# points that were filled by nearest-neighbor interpolation
	test_slice3 <- test_slice2
	test_slice3[!is.na(test_slice1)] <- NA
	grid <- expand.grid(x=x, y=y)
	cutpts <- c(-60,-20,-15,-10,-5,0,5,10,15,20,60)
	png(paste0(out,"cru_ts4.01.",varname,".near.2016.12.3d.png"), width=9, height=8, units="in", res=300)
	levelplot(test_slice3 ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
						col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()
	sum(is.na(test_slice3))

	print("replace R NA's with fillvalues")
	# fillvalue
	fillvalue <- 1e32
	interp_anm[is.na(interp_anm)] <- fillvalue

	print("write out interpolated anomalies -- 3d array (nx, ny, nt)")

	ptm <- proc.time()
	natspath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
	interpfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.",varname,".anm3d.nc")
	interp_ncfile <- paste0(natspath,interpfile)

	print("define dimensions")
	xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
	ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
	tdim <- ncdim_def("time", units=tunits$value, longname="time", as.double(time))

	print("define common variables")
	dlname <- "Longitude of cell center"
	lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
	dlname <- "Latitude of cell center"
	lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
	projname <- crs_name
	proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

	print("create netCDF file and put data")
	var_def <- ncvar_def(paste0(varname,"_anm"),dunits$value,list(xdim,ydim,tdim),fillvalue,dlongname$value,prec="double")
	ncout <- nc_create(interp_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)

	print("put additional attributes into dimension and data variables")
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

	print("put variables")
	ncvar_put(ncout,lon_def,lon)
	ncvar_put(ncout,lat_def,lat)
	ncvar_put(ncout,tdim,time)
	ncvar_put(ncout,var_def,interp_anm)

	print("add global attributes")
	ncatt_put(ncout,0,"title","CRU TS 4.01 anomalies interpolated onto the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_anomalies.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1961-1990")

	print("close the file, writing data to disk")
	nc_close(ncout)
	proc.time() - ptm

	print("reshape 3d array to 4d")
	nm <- 12 # number of months in year
	nyr <- nt/12 # number of years in CRU data set
	interp_anm_4d <- array(interp_anm, dim=c(nx,ny,nm,nyr))
	dim(interp_anm_4d)

	# to save memory
	remove(interp_anm)

	print("write 4d data")
	ptm <- proc.time()
	interpfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.",varname,".anm4d.nc")
	interp_ncfile <- paste0(natspath,interpfile)

	print("define dimensions")
	xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
	ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
	year <- seq(1901,2016, by=1)
	yeardim <- ncdim_def("year","year",as.integer(year))
	month <- seq(1,12, by=1)
	monthdim <- ncdim_def("month","month",as.integer(month))

	print("define common variables")
	dlname <- "Longitude of cell center"
	lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
	dlname <- "Latitude of cell center"
	lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
	projname <- crs_name
	proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

	print("create netCDF file and put data")
	var_def <- ncvar_def(paste0(varname,"_anm"),dunits$value,list(xdim,ydim,monthdim,yeardim),fillvalue,dlongname$value,prec="double")
	ncout <- nc_create(interp_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)

  print("put additional attributes into dimension and data variables")
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

	print("put variables")
	ncvar_put(ncout,lon_def,lon)
	ncvar_put(ncout,lat_def,lat)
	ncvar_put(ncout,var_def,interp_anm_4d)

	print("add global attributes")
	ncatt_put(ncout,0,"title","CRU TS 4.01 anomalies interpolated onto the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_anomalies.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1961-1990")

	print("close the file, writing data to disk")
	nc_close(ncout)
	proc.time() - ptm
}

cru_ts_regrid_abs <- function(varname, varlname){
	# define time parameters
	start_year = 1996; end_year = 2015; start_time = 1141; time_length = 240

  print("open points netCDF file to get dimensions, etc.")
	tspath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
	ltmpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ltm/"
	ncinfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.",varname,".anm3d.nc")
	ncin <- nc_open(paste0(tspath,ncinfile))

	print("get data")
	dname <- paste0(varname,"_anm")
	var3d <- ncvar_get(ncin,dname,start=c(x=1,y=1,time=start_time),count=c(x=1078,y=900,time=time_length))
	var3d.lt <- ncvar_get(ncin,dname)
	fillvalue <- ncatt_get(ncin,dname,"_FillValue")$value
	dunits <- ncatt_get(ncin,dname,"units")

	print("get dimension variables and attributes")
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

	time <- ncvar_get(ncin, varid="time", start=start_time, count=time_length); nt <- length(time)
	time.lt <- ncvar_get(ncin, varid="time"); nt.lt <- length(time.lt)
	tunits <- ncatt_get(ncin,"time","units")

	print("get longitude and latitude and attributes")
	lon <- ncvar_get(ncin,"lon"); 
	lon_units <- ncatt_get(ncin, "lon", "units")$value
	lat <- ncvar_get(ncin,"lat"); 
	lat_units <- ncatt_get(ncin, "lat", "units")$value

	print("get CRS attributes")
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

	print("close the input file")
	nc_close(ncin)

	print("calculate absolute values")

	print("replace netCDF _FillValues with R NA's")
	var3d[var3d==fillvalue] <- NA
  var3d.lt[var3d.lt==fillvalue] <- NA
  
	print("reshape to 4d")
	nm <- 12 # number of months in year
	nyr <- nt/12 # number of years in CRU data set
	nyr.lt <- nt.lt/12
	var4d <- array(var3d, dim=c(nx,ny,nm,nyr))
	var4d.lt <- array(var3d.lt, dim=c(nx,ny,nm,nyr.lt))

	print("read long-term mean data")
	ltmnc <- paste0("na10km_v2_",varname,".nc")
	ltmncfile <- paste(ltmpath,ltmnc,sep="")
	ltmncin <- nc_open(ltmncfile)

	ltm <- ncvar_get(ltmncin,varname)

  print("get absolute values")
	ptm <- proc.time()
	for (n in 1:nyr) {
		for (m in 1:nm) {
			var3d[,,((n-1)*nm+m)] <- var3d[,,((n-1)*nm+m)] + ltm[,,m]
			var4d[,,m,n] <- var4d[,,m,n] + ltm[,,m]
		}
	}
	proc.time() - ptm
	
	print("get absolute values for long-term")
	ptm <- proc.time()
	for (n in 1:nyr.lt) {
		for (m in 1:nm) {
			var3d.lt[,,((n-1)*nm+m)] <- var3d.lt[,,((n-1)*nm+m)] + ltm[,,m]
			var4d.lt[,,m,n] <- var4d.lt[,,m,n] + ltm[,,m]
		}
	}
	proc.time() - ptm

	# quick maps to check data
	n <- time_length
	var_slice_3d <- var3d[,,n]
	grid <- expand.grid(x=x, y=y)
	cutpts <- c(0,10,20,30,40,50,60,70,80,90,100)
	png(file=paste0(out,"na10km_v2_cru_4.01.",end_year,".abs.",varname,"3d.png"), width=9, height=8, units="in", res=300)
	levelplot(var_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
						col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()

	# quick maps to check data
	m <- 6; n <- nyr
	var_slice_4d <- var4d[,,m,n]
	grid <- expand.grid(x=x, y=y)
	cutpts <- c(0,10,20,30,40,50,60,70,80,90,100)
	png(file=paste0(out,"na10km_v2_cru_4.01.",end_year,".abs",varname,"4d.png"), width=9, height=8, units="in", res=300)
	levelplot(var_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
						col.regions=(rev(brewer.pal(10,"RdBu"))))
	dev.off()

	print("recode fillvalues")
	fillvalue <- 1e32
	var3d[is.na(var3d)] <- fillvalue
	var4d[is.na(var4d)] <- fillvalue
	var3d.lt[is.na(var3d.lt)] <- fillvalue
	var4d.lt[is.na(var4d.lt)] <- fillvalue

	print("replace precipitation anomalies (below 0) with 0")
	var3d[var3d<0] <- 0
	var4d[var4d<0] <- 0
	var3d.lt[var3d.lt<0] <- 0
	var4d.lt[var4d.lt<0] <- 0

	print("write out absolute values")
	ptm <- proc.time() # timer
	outncdf <- c("short-term 3d", "long-term 3d", "short-term 4d", "long-term 4d")
  for(i in 1:4){
    print(paste("writing", outncdf[i]))
    if(i==1){
      absfile <- paste0("na10km_v2_cru_ts4.01.",start_year,".",end_year,".",varname,".abs3d.nc")
    }else if(i==2){
      absfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.",varname,".abs3d.nc")
    }else if(i==3){
      absfile <- paste0("na10km_v2_cru_ts4.01.",start_year,".",end_year,".",varname,".abs4d.nc")
    }else if(i==4){
      absfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.",varname,".abs4d.nc")
    }
    
		abs_ncfile <- paste0(tspath,absfile)

		# define dimensions
		xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
		ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
		tdim <- ncdim_def("time", units=tunits$value, longname="time", as.double(time))
		tdim.lt <- ncdim_def("time", units=tunits$value, longname="time", as.double(time.lt))
    year <- seq(start_year,end_year, by=1)
	  yeardim <- ncdim_def("year","year",as.integer(year))
	  year.lt <- seq(1901,2016, by=1)
	  yeardim.lt <- ncdim_def("year","year",as.integer(year.lt))
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
		if(i==1){
      var_def <- ncvar_def(varname,dunits$value,list(xdim,ydim,tdim),fillvalue,varlname,prec="double")
    }else if(i==2){
      var_def <- ncvar_def(varname,dunits$value,list(xdim,ydim,tdim.lt),fillvalue,varlname,prec="double")
    }else if(i==3){
      var_def <- ncvar_def(varname,dunits$value,list(xdim,ydim,monthdim,yeardim),fillvalue,varlname,prec="double")
    }else if(i==4){
      var_def <- ncvar_def(varname,dunits$value,list(xdim,ydim,monthdim,yeardim.lt),fillvalue,varlname,prec="double")
    }

		ncout <- nc_create(abs_ncfile,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)

		print("put additional attributes into dimension and data variables")
		ncatt_put(ncout,"x","axis",x_axis)
		ncatt_put(ncout,"x","standard_name",x_standard_name)
		ncatt_put(ncout,"x","grid_spacing",x_grid_spacing)
		ncatt_put(ncout,"x","_CoordinateAxisType",x_CoordinatAxisType)
		ncatt_put(ncout,"y","axis",y_axis)
		ncatt_put(ncout,"y","standard_name",y_standard_name)
		ncatt_put(ncout,"y","grid_spacing",y_grid_spacing)
		ncatt_put(ncout,"y","_CoordinateAxisType",y_CoordinatAxisType)
		if (i ==1 | i ==2){
		  ncatt_put(ncout,"time","axis","T")
		  ncatt_put(ncout,"time","calendar","standard")		
		}
		ncatt_put(ncout,crs_name,"name",crs_name)
		ncatt_put(ncout,crs_name,"long_name",crs_long_name)
		ncatt_put(ncout,crs_name,"grid_mapping_name",crs_grid_mapping_name)
		ncatt_put(ncout,crs_name,"longitude_of_projection_origin",crs_longitude_of_projection_origin)
		ncatt_put(ncout,crs_name,"latitude_of_projection_origin",crs_latitude_of_projection_origin)
		ncatt_put(ncout,crs_name,"_CoordinateTransformType",crs_CoordinateTransformType)
		ncatt_put(ncout,crs_name,"_CoordinateAxisTypes",crs_CoordinateAxisTypes)
		ncatt_put(ncout,crs_name,"CRS.PROJ.4",crs_CRS.PROJ.4)

		print("put variables")
		ncvar_put(ncout,lon_def,lon)
		ncvar_put(ncout,lat_def,lat)
		if(i==1){
      ncvar_put(ncout,tdim,time)
      ncvar_put(ncout,var_def,var3d)
	    remove(var3d)
    }else if(i==2){
      ncvar_put(ncout,tdim.lt,time.lt)
      ncvar_put(ncout,var_def,var3d.lt)
	    remove(var3d.lt)
    }else if(i==3){
      ncvar_put(ncout,var_def,var4d)
	    remove(var4d)
    }else if(i==4){
      ncvar_put(ncout,var_def,var4d.lt)
	    remove(var4d.lt)
    }

		# add global attributes
		ncatt_put(ncout,0,"title","CRU TS 4.01 absolute values on the na10km_v2 10-km Grid")
		ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
		ncatt_put(ncout,0,"source","generated by cru_ts_4.01_regrid_abs.R")
		history <- paste("D. Chen", date(), sep=", ")
		ncatt_put(ncout,0,"history",history)
		if(i==1 | i==3){
      ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
    }else if(i==2 | i==4){
      ncatt_put(ncout,0,"base_period","1901-2016")
    }	
		# close the file, writing data to disk
		nc_close(ncout)
		print(paste("finished writing", outncdf[i]))
		proc.time() - ptm 
  }
}