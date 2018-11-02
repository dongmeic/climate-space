library(ncdf4)
library(abind)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/daily_climate/CRU/"; setwd(inpath)
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

varnms <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", "drop5", "drop10", "drop15",
					 "drop20", "drop20plus", "max.drop", "ddAugJul", "ddAugJun", "min20", "min22", "min24", "min26", "min28", 
					 "min30", "min32", "min34", "min36", "min38", "min40")

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
             "number of days when a > 20 °C drop on any two consecutive days during winter",
             "the largest temperature drop on any two consecutive days during winter",
             "accumulated degree days above 5.5 °C from August to July",
             "accumulated degree days above 5.5 °C from August to June",
             "number of days with minimum temperatures at or below -20 °C during winter",
             "number of days with minimum temperatures at or below -22 °C during winter",
             "number of days with minimum temperatures at or below -24 °C during winter",
             "number of days with minimum temperatures at or below -26 °C during winter",
             "number of days with minimum temperatures at or below -28 °C during winter",
             "number of days with minimum temperatures at or below -30 °C during winter",
             "number of days with minimum temperatures at or below -32 °C during winter",
             "number of days with minimum temperatures at or below -34 °C during winter",
             "number of days with minimum temperatures at or below -36 °C during winter",
             "number of days with minimum temperatures at or below -38 °C during winter",
             "number of days with minimum temperatures at or below -40 °C during winter")
             
dunits <- c("binary", rep("day", 2), "°C", "binary", rep("one", 2), rep("day", 2), rep("°C", 9), rep("day", 11))
            
dim1 <- 277910; dim2 <- nt

print("writing 3D netCDF files")
ptm <- proc.time()
foreach(k = 1:nvar)%dopar%{
  indata <- read.csv(paste0("bioclimatic_variables_daily_",years[1],".csv"))
  df <- data.frame(indata[,varnms[k]])
  colnames(df) <- years[1]
  for(i in 2:nt){
    indata <- read.csv(paste0("bioclimatic_variables_daily_",years[i],".csv"))
    ndf <- data.frame(indata[,varnms[k]])
    colnames(ndf) <- years[i]
    df <- cbind(df,ndf)
    print(paste("got data for", varnms[k], years[i]))
  }
  write.csv(df, paste0("bioclimatic_variables_daily_",varnms[k],".csv"), row.names = FALSE)
  m <- rep(1:nt,each=dim1)
  temp_array <- array(fillvalue, dim=c(nx,ny,nt))
  temp_array[cbind(j2,k2,m)] <- as.matrix(df)
  ncfname <- paste0(ncpath,"ts/var/daily/na10km_v2_cru_",varnms[k],"_1996.2015.3d.nc")
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
  ncatt_put(ncout,0,"source","generated by bioclimatic_daily_ncfiles_1996.R")
  history <- paste("D.Chen", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)
  ncatt_put(ncout,0,"base_period","1996-2015")
  nc_close(ncout)
}
proc.time() - ptm
print("all done!")