# Created by Dongmei Chen

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/damian/getPercentSunshine.R")
library(ncdf4)

# open points netCDF file to get dimensions, etc.
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/source/"
ncoutpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/derived/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
ncinfile <- "cru_ts4.01.1901.2016.cld.dat.nc"
ncin <- nc_open(paste0(ncpath,ncinfile))

# get dimension variables
lon <- ncvar_get(ncin, varid="lon"); nlon <- length(lon)
lat <- ncvar_get(ncin, varid="lat"); nlat <- length(lat)
time <- ncvar_get(ncin, varid="time"); nt <- length(time)
tunits <- ncatt_get(ncin,"time","units")$value

# get data
dname <- "cld"
var3d <- ncvar_get(ncin,dname)
dims <- dim(var3d)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")$value

# close the input file
nc_close(ncin)

# replace netCDF _FillValues with R NA's
var3d[var3d==fillvalue] <- NA

var3d <- unlist(lapply(var3d, get.percent.sunshine))
dim(var3d) <- dims

var3d[is.na(var3d)] <- fillvalue

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- ncdim_def("time",tunits,as.double(time))

# define variable
dlname <- "percent possible sunshine"
var_def <- ncvar_def("sun","percentage",list(londim,latdim,timedim),fillvalue,dlname,prec="single")

# create netCDF file and put arrays
ncfname <- paste0(ncoutpath,"cru_ts4.01.1901.2016.sun.dat.nc")
ncout <- nc_create(ncfname,var_def,force_v4=T)

# put variables
ncvar_put(ncout,var_def,var3d)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"time","axis","T")
ncatt_put(ncout,"time","calendar","standard")

# add global attributes
ncatt_put(ncout,0,"title","CRU TS4.01 Percent Possible Sunshine")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source",ncinfile)
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1901-2016")

# close the file, writing data to disk
nc_close(ncout)

print("all done!")