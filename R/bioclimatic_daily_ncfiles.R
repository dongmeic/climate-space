library(ncdf4)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

print("select variables...")
vars <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", "drop5", "ddAugJul", "ddAugJun")
nvar <- length(vars)
print("done!")

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
start_year <- 1901; end_year <- 2016; years <- start_year:end_year; nt <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncfile <- paste0(ncpath,"na10km_v2.nc")
ncin <- nc_open(ncfile)
print(ncin)
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
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

varnms <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", "drop5",
           "drop10", "drop15", "drop20", "drop20plus", "max.drop", "ddAugJul", "ddAugJun")
nvar <- length(varnms)
varlnms <- c("late cold snap occurring between March through mid-April",
             "the frequency of maximum daily temperatures greater than 18.3 °C during August",
             "number of days with maximum temperatures higher than 40°C during summer",
             "minimum daily minimum temperature during winter",
             "early cold snap occurring mid-October through November",
             "total number of cold snaps occurring through out the winter",
             "average duration of a cold snap during winter",
             "number of days of positive temperature changes on any two consecutive days",
             "number of days when a 0-5 °C drop on any two consecutive days during winter",
             "number of days when a 5-10 °C drop on any two consecutive days during winter",
             "number of days when a 10-15 °C drop on any two consecutive days during winter",
             "number of days when a 15-20 °C drop on any two consecutive days during winter",
             "number of days when a >20 °C drop on any two consecutive days during winter",
             "the largest drop in daily average temperature during winter",
             "accumulated degree days above 5.5 °C from August to July",
             "accumulated degree days above 5.5 °C from August to June")
dunits <- c("binary", "day", "day", "°C", "binary", "one", "one", "day", "day", "day", "day", 
            "day", "day", "°C", "°C", "°C")
            
dim1 <- 277910; dim2 <- nt

print("writing 3D netCDF files")
ptm <- proc.time()
foreach(k = 1:nvar)%dopar%{
  mat <- matrix(, nrow = dim1, ncol = dim2)
  for(i=1:nt){
    
  }
  m <- rep(1:nt,each=dim1)
  temp_array <- array(fillvalue, dim=c(nx,ny,nt))
  temp_array[cbind(j2,k2,m)] <- matrix.all[1:dim1,1+(nt*(k-1)):(nt*k)]
  ncfname <- paste0(ncpath,"ts/var/na10km_v2_",varnms[k],"_1902.2016.3d.nc")
  dname <- varnms[k]
  dlname <- varlnms[k]
  dunit <- dunits[k]
  var_def <- ncvar_def(dname,dunit,list(xdim,ydim,tdim),fillvalue,dlname,prec="float")
  ncout <- nc_create(ncfname,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
  
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
  ncvar_put(ncout,time_def,year)
  ncvar_put(ncout,var_def,temp_array)

  # add global attributes
  ncatt_put(ncout,0,"title","CRU TS 4.01 interpolated values on the na10km_v2 10-km Grid")
  ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
  ncatt_put(ncout,0,"source","generated by bioclimatic_daily_ncfiles.R")
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
foreach(j = 1:nvar)%dopar%{
  dname <- varnms[j]
  print(paste("running for", dname))
  if(j != 1 & j != 5){
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
	for(k in 1:2){
	  if(k==1){
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
	  if(k==1){
	    var_def <- ncvar_def(dname,dunits[j],list(xdim,ydim,vardim,yeardim),fillvalue,varlnms[j],prec="double")
	  }else{
	    dlname <- paste("departure from long-term mean in the",varlnms[j])
	    var_def <- ncvar_def(dname,dunits[j],list(xdim,ydim,vardim,yeardim),fillvalue,dlname,prec="double")
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
	  if(k==1){
	    ncvar_put(ncout,var_def,var4d)
	  }else{
	    ncvar_put(ncout,var_def,var_std_4d)
	  }
	  # add global attributes
	  ncatt_put(ncout,0,"title","CRU TS 4.01 interpolated values on the na10km_v2 10-km Grid")
	  ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	  ncatt_put(ncout,0,"source","generated by bioclimatic_daily_ncfiles.R")
	  history <- paste("D. Chen", date(), sep=", ")
	  ncatt_put(ncout,0,"history",history)
	  ncatt_put(ncout,0,"base_period",paste0(first_year,"-",last_year))
	  # close the file, writing data to disk
	  nc_close(ncout)
	}
  }else{
    ncinfile <- paste0(ncpath,"ts/var/na10km_v2_",dname,"_1902.2016.3d.nc")
	ncin <- nc_open(ncinfile)
	var3d <- ncvar_get(ncin,dname,start=c(x=1,y=1,time=start_time),count=c(x=1078,y=900,time=time_length))
	var3d[var3d==fillvalue] <- NA 
	for (i in 1:nyr){
	  var_slice <- var3d[,,i]

	  # get climate data with the presence of vegetation
	  vgt[vgt==0] <- NA
	  var_vgt <- var_slice * vgt

	  # get climate data with the presence of mpb
	  btl_slice <- btl[,,i]
	  btl_slice[btl_slice==0] <- NA
	  var_btl <- var_slice * btl_slice

	  if(i == 1){
		print(paste("start to reshape 2d to 3d in year", years[i]))
		var <- abind(var_slice, var_vgt, var_btl, along=3)
	  }else{
		var <- abind(var, var_slice, var_vgt, var_btl, along=3)
	  }
	  print(paste(years[i], "is done!"))
	}
	print("reshape 3d to 4d")
	var_4d <- array(var, dim=c(nx,ny,nv,nyr))
	filenm <- paste0("na10km_v2_",dname,"_",first_year,".",last_year,".4d.nc")
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
	var_def <- ncvar_def(dname,dunits[j],list(xdim,ydim,vardim,yeardim),fillvalue,varlnms[j],prec="double")
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
	ncvar_put(ncout,var_def,var4d)

	# add global attributes
	ncatt_put(ncout,0,"title","CRU TS 4.01 interpolated values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by bioclimatic_daily_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period",paste0(first_year,"-",last_year))
	# close the file, writing data to disk
	nc_close(ncout)
  }
  print(paste("writing netCDF file for", dname, "is done!"))  
}

print("all done!")