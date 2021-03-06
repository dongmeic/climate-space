library(ncdf4)
library(abind)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/min/"; setwd(inpath)
start_year <- 1902; end_year <- 2016; years <- start_year:end_year; nt <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncfile <- paste0(ncpath,"na10km_v2.nc")
ncin <- nc_open(ncfile)
print(ncin)
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)

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
tunits <- "year"

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
tdim <- ncdim_def("year",units=tunits,longname="year",as.integer(years))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- "lambert_azimuthal_equal_area"
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")
dlname <- "year"
time_def <- ncvar_def(dlname,tunits,list(tdim),NULL,dlname,prec="integer")

print("set j and k index")
na10km <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
j2 <- sapply(na10km$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(na10km$y, function(xy) which.min(abs(y-xy)))
head(cbind(na10km$x,na10km$y,j2,k2))
print("done!")

varnms <- c("Ncs", "Acs", "min20", "min22", "min24", "min26", "min28")
nvar <- length(varnms)
varlnms <- c("total number of cold snaps occurring through out the winter",
             "average duration of a cold snap during winter",
						 "number of days with minimum temperatures at or below -20 °C during winter",
             "number of days with minimum temperatures at or below -22 °C during winter",
             "number of days with minimum temperatures at or below -24 °C during winter",
             "number of days with minimum temperatures at or below -26 °C during winter",
             "number of days with minimum temperatures at or below -28 °C during winter")
dunits <- c("one", "one", "day", "day", "day", "day", "day")
            
dim1 <- 277910; dim2 <- nt

print("writing 3D netCDF files")
ptm <- proc.time()
foreach(k = 1:nvar)%dopar%{
  indata <- read.csv(paste0("bioclimatic_variables_daily_min_",years[1],"_2.csv"))
  df <- data.frame(indata[,varnms[k]])
  colnames(df) <- years[1]
  for(i in 2:nt){
    indata <- read.csv(paste0("bioclimatic_variables_daily_min_",years[i],"_2.csv"))
    ndf <- data.frame(indata[,varnms[k]])
    colnames(ndf) <- years[i]
    df <- cbind(df,ndf)
    print(paste("got data for", varnms[k], years[i]))
  }
  write.csv(df, paste0("bioclimatic_variables_daily_",varnms[k],".csv"), row.names = FALSE)
  m <- rep(1:nt,each=dim1)
  temp_array <- array(fillvalue, dim=c(nx,ny,nt))
  temp_array[cbind(j2,k2,m)] <- as.matrix(df)
  ncfname <- paste0(ncpath,"ts/var/na10km_v2_",varnms[k],"_1902.2016.3d.nc")
  dname <- varnms[k]
  dlname <- varlnms[k]
  dunit <- dunits[k]
  var_def <- ncvar_def(dname,dunit,list(xdim,ydim,tdim),fillvalue,dlname,prec="float")
  ncout <- nc_create(ncfname,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE,verbose=FALSE)

  print(paste("writing output for", dname))
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
  ncvar_put(ncout,time_def,years)
  ncvar_put(ncout,var_def,temp_array)

  # add global attributes
  ncatt_put(ncout,0,"title","CRU TS 4.01 interpolated values on the na10km_v2 10-km Grid")
  ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
  ncatt_put(ncout,0,"source","generated by bioclimatic_daily_min_ncfiles.R")
  history <- paste("D.Chen", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)
  ncatt_put(ncout,0,"base_period","1902-2016")
  nc_close(ncout)
}
proc.time() - ptm
print("done!")

print("getting 4D data")
first_year <- 1996; last_year <- 2015; years <- first_year:last_year; nyr <- length(years)
start_time = 1996-1902+1; time_length = 20

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

nv <- 3 # three variables: land, tree, beetle
foreach(k = 1:nvar)%dopar%{
  dname <- varnms[k]
  print(paste("running for", dname))
	ncinfile <- paste0(ncpath,"ts/var/na10km_v2_",dname,"_1902.2016.3d.nc")
	ncin <- nc_open(ncinfile)
	var3d <- ncvar_get(ncin,dname)
	var3d[var3d==fillvalue] <- NA 
	var_ltm <- apply(var3d, c(1,2), mean, na.rm=TRUE)
	var_std <- apply(var3d, c(1,2), sd, na.rm=TRUE)
	var3d <- ncvar_get(ncin,dname,start=c(x=1,y=1,time=start_time),count=c(x=1078,y=900,time=time_length))
	var3d[var3d==fillvalue] <- NA 
	for (i in 1:nyr){
		var_slice <- var3d[,,i]
		var_std_slice <- (var_slice - var_ltm)/var_std

		# get climate data with the presence of vegetation
		vgt[vgt==0] <- NA
		var_vgt <- var_slice * vgt

		# get climate data with the presence of mpb
		btl_slice <- btl[,,i]
		btl_slice[btl_slice==0] <- NA
		var_btl <- var_slice * btl_slice

		# get standard deviation
		var_std_vgt <- var_std_slice * vgt
		var_std_btl <- var_std_slice * btl_slice

		if(i == 1){
		print(paste("start to reshape 2d to 3d in year", years[i]))
		var <- abind(var_slice, var_vgt, var_btl, along=3)
		var_std_all <- abind(var_std_slice, var_std_vgt, var_std_btl, along=3)
		}else{
		var <- abind(var, var_slice, var_vgt, var_btl, along=3)
		var_std_all <- abind(var_std_all, var_std_slice, var_std_vgt, var_std_btl, along=3)
		}
		print(paste(years[i], "is done!"))
	}
	print("reshape 3d to 4d")
	var_4d <- array(var, dim=c(nx,ny,nv,nyr))
	var_std_4d <- array(var_std_all, dim=c(nx,ny,nv,nyr))
	for(j in 1:2){
		if(j==1){
			filenm <- paste0("na10km_v2_",dname,"_",first_year,".",last_year,".4d.nc")
		}else{
			filenm <- paste0("na10km_v2_",dname,"_std_",first_year,".",last_year,".4d.nc")
		}
		ncfile <- paste0(ncpath,"ts/var/",filenm)
		# define dimensions
		xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
		ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
		year <- seq(first_year,last_year, by=1)
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
		if(j==1){
			var_def <- ncvar_def(dname,dunits[k],list(xdim,ydim,vardim,yeardim),fillvalue,varlnms[k],prec="double")
		}else{
			dlname <- paste("departure from long-term mean in the",varlnms[k])
			var_def <- ncvar_def(paste0(dname,"_std"),"",list(xdim,ydim,vardim,yeardim),fillvalue,dlname,prec="double")
		}
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
		ncvar_put(ncout,lon_def,lon)
		ncvar_put(ncout,lat_def,lat)
		if(j==1){
			ncvar_put(ncout,var_def,var_4d)
		}else{
			ncvar_put(ncout,var_def,var_std_4d)
		}
		# add global attributes
		ncatt_put(ncout,0,"title","CRU TS 4.01 interpolated values on the na10km_v2 10-km Grid")
		ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
		ncatt_put(ncout,0,"source","generated by bioclimatic_daily_min_ncfiles.R")
		history <- paste("D. Chen", date(), sep=", ")
		ncatt_put(ncout,0,"history",history)
		ncatt_put(ncout,0,"base_period",paste0(first_year,"-",last_year))
		# close the file, writing data to disk
		nc_close(ncout)
  }
	print(paste("writing netCDF file for", dname, "is done!"))  
}

print("all done!")