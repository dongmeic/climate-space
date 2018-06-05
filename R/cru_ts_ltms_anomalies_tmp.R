# Created by Pat Bartlein
# Modified by Dongmei Chen

# Read CRU TS 4.01 climate data

library(ncdf4)
library(lattice)
library(RColorBrewer)

# open points netCDF file to get dimensions, etc.
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/source/"
ncoutpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/derived/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
ncinfile <- "cru_ts4.01.1901.2016.tmp.dat.nc"
ncin <- nc_open(paste(ncpath,ncinfile,sep=""))
print(ncin)

# get dimension variables
lon <- ncvar_get(ncin, varid="lon"); nlon <- length(lon)
lat <- ncvar_get(ncin, varid="lat"); nlat <- length(lat)
time <- ncvar_get(ncin, varid="time"); nt <- length(time)
tunits <- ncatt_get(ncin,"time","units")
print(c(nlon, nlat, nt))
print(tunits)

# get data
dname <- "tmp"
var3d <- ncvar_get(ncin,dname)
dim(var3d)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
print (fillvalue)

# close the input file
nc_close(ncin)

# replace netCDF _FillValues with R NA's
var3d[var3d==fillvalue$value] <- NA

# quick maps to check data
n <- 1344
var_slice_3d <- var3d[,,n]
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(paste(out,"cru_ts4.01.tmp.2012.12.3d.png", sep=""))
levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# reshape 3d array to 4d
nm <- 12 # number of months in year
ny <- nt/12 # number of years in CRU data set
var4d <- array(var3d, dim=c(nlon,nlat,nm,ny))
dim(var4d)

# quick maps to check data
m <- 6; n <- ny-2
var_slice_4d <- var4d[,,m,n]
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(paste(out,"cru_ts4.01.tmp.2014.06.4d.png", sep=""))
levelplot(var_slice_4d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# make a missing data mask
landmask <- array(1, dim=c(nlon,nlat))
# use last month of data to set data flag
for (j in 1:nlon) {
  for (k in 1:nlat) {
    if (is.na(var3d[j,k,nt])) landmask[j,k]=NA
  }
}
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(paste(out,"landmask_tmp.png", sep=""))
levelplot(landmask ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# get long-term means
begyr <- 1901; endyr <- 2016 # year range of CRU data 
firstyr <- 1961; lastyear <- 1990 # year range for long-term means (e.g. 1961-1990)

# subscripts of first and last year
n1 <- firstyr-begyr+1; n2 <- lastyear-begyr+1

# define array for long-term means
ltm <- array(NA, dim=c(nlon,nlat,nm))
dim(ltm)
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
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(paste(out,"cru_ts4.01.tmp.ltm.12.3d.png", sep=""))
levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# fillvalue
fillvalue <- 1e32
ltm[is.na(ltm)] <- fillvalue


# write out the long-term means

# time -- values calculated by cf_time_subs.f90 -- 1961-1990
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

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
tdim <- ncdim_def("time",units=tunits,longname="time",as.double(timeltm))
bdim <- ncdim_def("nbnds",units="1",longname=NULL,as.double(bounds))
dlname <- "climatology_bounds"
bnds_def <- ncvar_def("climatology_bounds",tunits,list(bdim,tdim),NULL,dlname,prec="double")

# define variable
dlname <- "2m air temperature"
var_def <- ncvar_def("tmp_ltm","deg_C",list(londim,latdim,tdim),fillvalue,dlname,prec="single")

# create netCDF file and put array
ncfname <- paste(ncoutpath,"cru_ts4.01.1961-1990.tmp.ltm.nc",sep="")
ncout <- nc_create(ncfname,list(bnds_def,var_def),force_v4=TRUE, verbose=FALSE)
#nc_close(ncout)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"time","axis","T")
ncatt_put(ncout,"time","calendar","standard")
ncatt_put(ncout,"time","bounds","climatology_bounds")
ncatt_put(ncout,"time","climatology","climatology_bounds")

# put variables
ncvar_put(ncout,bnds_def,climatology_bounds)
ncvar_put(ncout,var_def,ltm)

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 1961-1990 long-term means")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source",ncinfile)
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)

# get anomalies (both 3d and 4d arrays)

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
cutpts <- c(-10,-5,-2,-1,-.5,0,.5,1,2,5,10)
png(paste(out,"cru_ts4.01.tmp.anm.2015.12.3d.png", sep=""))
levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check anomalies
m <- 6; n <- ny
var_slice_4d <- var4d[,,m,n]
grid <- expand.grid(lon=lon, lat=lat)
cutpts <- c(-10,-5,-2,-1,-.5,0,.5,1,2,5,10)
png(paste(out,"cru_ts4.01.tmp.anm.2016.06.4d.png", sep=""))
levelplot(var_slice_4d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
  col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# recode fillvalues
fillvalue <- 1e32
var3d[is.na(var3d)] <- fillvalue
var4d[is.na(var4d)] <- fillvalue

# write out the 3d data

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
timedim <- ncdim_def("time",tunits,as.double(time))

# define variable
fillvalue <- 1e32
dlname <- "2m air temperature anomalies"
tmp_def <- ncvar_def("tmp_anm","deg_C",list(londim,latdim,timedim),fillvalue,dlname,prec="single")

# create netCDF file and put arrays
ncfname <- paste(ncoutpath,"cru_ts4.01.1901.2016.tmp.anm3d.nc",sep="")
ncout <- nc_create(ncfname,tmp_def,force_v4=T)

# put variables
ncvar_put(ncout,tmp_def,var3d)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"time","axis","T")
ncatt_put(ncout,"time","calendar","standard")
ncatt_put(ncout,"tmp_anm","comment","anomalies, differences from 1961-1990 base period long-term means")

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 2m air temperature anomalies")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source",ncinfile)
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"anomaly_base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)


# write out the 4d data

# define dimensions
londim <- ncdim_def("lon","degrees_east",as.double(lon)) 
latdim <- ncdim_def("lat","degrees_north",as.double(lat)) 
year <- seq(1901,2016, by=1)
yeardim <- ncdim_def("year","year",as.integer(year))
month <- seq(1,12, by=1)
monthdim <- ncdim_def("month","month",as.integer(month))

# define variables
fillvalue <- 1e32
dlname <- "2m air temperature anomalies"
tmp.def <- ncvar_def("tmp_anm","deg_C",list(londim,latdim,monthdim,yeardim),fillvalue,dlname,prec="single")

# create netCDF file and put arrays
ncfname <- paste(ncoutpath,"cru_ts4.01.1901.2016.tmp.anm4d.nc",sep="")
ncout <- nc_create(ncfname,tmp.def,force_v4=T)

# put variables
ncvar_put(ncout,tmp.def,var4d)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"tmp_anm","comment","anomalies, differences from 1961-1990 base period long-term means")

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 2m air temperature anomalies")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source",ncinfile)
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"anomaly_base_period","1961-1990")
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)