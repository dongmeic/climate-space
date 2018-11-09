# get the 4D netCDF files for the updated degree day variables
# run in an interactive mode

library(ncdf4)
library(abind)

start_year <- 1996; end_year <- 2015; years <- start_year:end_year; nt <- length(years)
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncfile <- paste0(ncpath,"na10km_v2.nc")
ncin <- nc_open(ncfile)
print(ncin)

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

varnms <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", "drop5", "ddAugJul", "ddAugJun")
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
             "accumulated degree days above 5.5 °C from August to July",
             "accumulated degree days above 5.5 °C from August to June")
dunits <- c("binary", "day", "day", "°C", "binary", "one", "one", "day", "day", "°C", "°C")
            
dim1 <- 277910; dim2 <- nt

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

k <- 10
#k <- 11
dname <- varnms[k]
print(paste("running for", dname))

ncinfile <- paste0(ncpath,"ts/var/daily/na10km_v2_cru_",dname,"_1996.2015.3d.nc")
ncin <- nc_open(ncinfile)
var3d <- ncvar_get(ncin,dname)
var3d[var3d==fillvalue] <- NA 

for (i in 1:nt){
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
var_4d <- array(var, dim=c(nx,ny,nv,nt))

filenm <- paste0("na10km_v2_",dname,"_1996.2015.4d.nc")
ncfile <- paste0(ncpath,"ts/var/",filenm)
# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
year <- seq(1996,2015, by=1)
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
var_def <- ncvar_def(dname,dunits[k],list(xdim,ydim,vardim,yeardim),fillvalue,varlnms[k],prec="double")
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
ncvar_put(ncout,var_def,var_4d)

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 interpolated values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by bioclimatic_daily_ncfiles.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period",paste0(1996,"-",2015))
# close the file, writing data to disk
nc_close(ncout)

print(paste("writing netCDF file for", dname, "is done!"))  

