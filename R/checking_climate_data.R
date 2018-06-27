# Created by Dongmei Chen
# Examine climate data
library(ncdf4)
library(lattice)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/maps/"
ncfolder <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/cru_ts4.01/source/"
dname <- "tmn"
ncfile <- paste0("cru_ts4.01.1901.2016.", dname, ".dat.nc")
ncin <- nc_open(paste0(ncfolder, ncfile))
var3d <- ncvar_get(ncin, dname)
lon <- ncvar_get(ncin, varid="lon"); nlon <- length(lon)
lat <- ncvar_get(ncin, varid="lat"); nlat <- length(lat)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
nc_close(ncin)
var3d[var3d==fillvalue$value] <- NA

years <- c(1901,1901,1902,1902,1902,1902,1902,1903,1903,1903,1903,1903,1904,1904,1904)
months <- c(11,12,1,2,3,11,12,1,2,3,11,12,1,2,3)
seqs <- c(11:15,23:27,35:39)
print(paste("mapping", dname))
foreach(i=1:length(seqs))%dopar%{
	var_slice_3d <- var3d[,,seqs[i]]
	grid <- expand.grid(lon=lon, lat=lat)
	png(paste0(out,"cru_ts4.01.", dname, ".", years[i],".", months[i],".png"), width=12, height=9, units="in", res=300)
	cutpts <- c(-50,-35,-25,-15,-5,0,5,15,25,35,50)
	print(levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
	  col.regions=(rev(brewer.pal(10,"RdBu")))))
	dev.off()
}

dname <- "tmp"
ncfile <- paste0("cru_ts4.01.1901.2016.", dname, ".dat.nc")
ncin <- nc_open(paste0(ncfolder, ncfile))
var3d <- ncvar_get(ncin, dname)
nc_close(ncin)
var3d[var3d==fillvalue$value] <- NA

print(paste("mapping", dname)) 
foreach(i=1:length(seqs))%dopar%{
	var_slice_3d <- var3d[,,seqs[i]]
	grid <- expand.grid(lon=lon, lat=lat)
	png(paste0(out,"cru_ts4.01.", dname, ".", years[i],".", months[i],".png"), width=12, height=9, units="in", res=300)
	cutpts <- c(-50,-35,-25,-15,-5,0,5,15,25,35,50)
	print(levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
	  col.regions=(rev(brewer.pal(10,"RdBu")))))
	dev.off()
}

dname <- "tmx"
ncfile <- paste0("cru_ts4.01.1901.2016.", dname, ".dat.nc")
ncin <- nc_open(paste0(ncfolder, ncfile))
var3d <- ncvar_get(ncin, dname)
nc_close(ncin)
var3d[var3d==fillvalue$value] <- NA

print(paste("mapping", dname)) 
foreach(i=1:length(seqs))%dopar%{
	var_slice_3d <- var3d[,,seqs[i]]
	grid <- expand.grid(lon=lon, lat=lat)
	png(paste0(out,"cru_ts4.01.", dname, ".", years[i],".", months[i],".png"), width=12, height=9, units="in", res=300)
	cutpts <- c(-50,-35,-25,-15,-5,0,5,15,25,35,50)
	print(levelplot(var_slice_3d ~ lon * lat, data=grid, at=cutpts, cuts=11, pretty=T, 
	  col.regions=(rev(brewer.pal(10,"RdBu")))))
	dev.off()
}

print("all done!")