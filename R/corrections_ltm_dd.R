# run bioclimatic_daily_ncfiles.R
# time_series_boxplot.R
library(ncdf4)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/daily_climate/CRU"; setwd(inpath)
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

varnms <- c("ddAugJul", "ddAugJun")
nvar <- length(varnms)
varlnms <- c("accumulated degree days above 5.5 °C from August to July",
             "accumulated degree days above 5.5 °C from August to June")
dunits <- c("DD", "DD")
            
dim1 <- 277910; dim2 <- nt

# k <- 1; k <- 2
indata <- read.csv(paste0("degree_days_CRU_",years[1],".csv"))
# "thres5.5all", "thres5.5aug.jun"
#df <- data.frame(indata[,"thres5.5all"])
df <- data.frame(indata[,"thres5.5aug.jun"])
colnames(df) <- years[1]
for(i in 2:nt){
	indata <- read.csv(paste0("degree_days_CRU_",years[i],".csv"))
	#ndf <- data.frame(indata[,"thres5.5all"])
	ndf <- data.frame(indata[,"thres5.5aug.jun"])
	colnames(ndf) <- years[i]
	df <- cbind(df,ndf)
	print(paste("got data for", varnms[k], years[i]))
}
write.csv(df, paste0("bioclim_var_daily_",varnms[k],"_1996.csv"), row.names = FALSE)

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
ncatt_put(ncout,0,"source","generated by bioclimatic_daily_ncfiles.R")
history <- paste("D.Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1996-2015")
nc_close(ncout)