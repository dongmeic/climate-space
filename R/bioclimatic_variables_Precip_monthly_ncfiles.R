# Created by Dongmei Chen
# To generate NetCDF files for bioclimatic variables - "tree resistance" (monthly)

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
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/daily/20180628/"

# variables (9)
# GSP - sum of precipitation from April through June in the year of the outbreak
# Pmean - mean of monthly precipitation from August in the prior year through July in the year of the outbreak 
# summerP0 - sum of precipitation from June through August in the year of the outbreak
# summerP1 - sum of precipitation from June through August in the year prior to the outbreak
# summerP2 - cumulative precipitation from June through August in the two years preceding the outbreak
# wateryearP - precipitation from October two years prior to the outbreak through September in the year prior to the outbreak
# wateryearPcum - cumulative precipitation from October two years prior to the outbreak through September in the year of the outbreak 
# PMarAug - sum of precipitation from March through August in the year of the outbreak 
# PPT - cumulative monthly October-August precipitation in current and previous 5 years 
#		(from October through August in the five years preceding the outbreak and the year of the outbreak)

print("read precipitation netCDF files")
ncfile <- "na10km_v2_cru_ts4.01.1901.2016.pre.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "pre"
pre <- ncvar_get(ncin,dname)
dim(pre)
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
pre[pre==fillvalue$value] <- NA

start_year = 1901; end_year = 2016; first_year = 1996; last_year = 2015
years = first_year:last_year; nyr <- length(years)
# get long-term means
# define array for long-term means
nt <- end_year - start_year
GSP_3d <- array(NA, dim=c(nx, ny, (nt+1)))
Pmean_3d <- array(NA, dim=c(nx, ny, nt))
summerP0_3d <- array(NA, dim=c(nx, ny, (nt+1)))
summerP1_3d <- array(NA, dim=c(nx, ny, nt))
summerP2_3d <- array(NA, dim=c(nx, ny, nt))
POctSep_3d <- array(NA, dim=c(nx, ny, (nt-1)))
PcumOctSep_3d <- array(NA, dim=c(nx, ny, (nt-1)))
PMarAug_3d <- array(NA, dim=c(nx, ny, (nt+1)))
PPT_3d <- array(NA, dim=c(nx, ny, (nt-5)))

print("calculate long term means")
ptm <- proc.time()
for (k in 1:(nt-5)){
	PPT_3d[,,k] <- apply(abind(pre[,,10:12,k:(k+5)],pre[,,1:8,(k+1):(k+6)],along=3),c(1,2),sum)
	print(k)
}

for (k in 1:(nt-1)){
	POctSep_3d[,,k] <- apply(abind(pre[,,10:12,k],pre[,,1:9,(k+1)],along=3),c(1,2),sum)
	PcumOctSep_3d[,,k] <- apply(abind(pre[,,10:12,k:(k+1)],pre[,,1:9,(k+1):(k+2)],along=3),c(1,2),sum)
	print(k)
}

for (k in 1:nt){
	Pmean_3d[,,k] <- apply(abind(pre[,,8:12,k],pre[,,1:7,(k+1)],along=3),c(1,2),mean)
	summerP1_3d[,,k] <- apply(abind(pre[,,6:8,k],along=3),c(1,2),sum)
	summerP2_3d[,,k] <- apply(abind(pre[,,6:8,k],pre[,,6:8,(k+1)],along=3),c(1,2),sum)
	print(k)
}

for (k in 1:(nt+1)){
	GSP_3d[,,k] <- apply(abind(pre[,,4:6,k],along=3),c(1,2),sum)
	summerP0_3d[,,k] <- apply(abind(pre[,,6:8,k],along=3),c(1,2),sum)
	PMarAug_3d[,,k] <- apply(abind(pre[,,3:8,k],along=3),c(1,2),sum)
	print(k)
}
proc.time() - ptm

print("calculate standard deviations")
GSP_ltm <- apply(GSP_3d, c(1,2), mean, na.rm=TRUE)
GSP_std <- apply(GSP_3d, c(1,2), sd, na.rm=TRUE)
PPT_ltm <- apply(PPT_3d, c(1,2), mean, na.rm=TRUE)
PPT_std <- apply(PPT_3d, c(1,2), sd, na.rm=TRUE)
Pmean_ltm <- apply(Pmean_3d, c(1,2), mean, na.rm=TRUE)
Pmean_std <- apply(Pmean_3d, c(1,2), sd, na.rm=TRUE)
PcumOctSep_ltm <- apply(PcumOctSep_3d, c(1,2), mean, na.rm=TRUE)
PcumOctSep_std <- apply(PcumOctSep_3d, c(1,2), sd, na.rm=TRUE)
POctSep_ltm <- apply(POctSep_3d, c(1,2), mean, na.rm=TRUE)
POctSep_std <- apply(POctSep_3d, c(1,2), sd, na.rm=TRUE)
PMarAug_ltm <- apply(PMarAug_3d, c(1,2), mean, na.rm=TRUE)
PMarAug_std <- apply(PMarAug_3d, c(1,2), sd, na.rm=TRUE)
summerP0_ltm <- apply(summerP0_3d, c(1,2), mean, na.rm=TRUE)
summerP0_std <- apply(summerP0_3d, c(1,2), sd, na.rm=TRUE)
summerP1_ltm <- apply(summerP1_3d, c(1,2), mean, na.rm=TRUE)
summerP1_std <- apply(summerP1_3d, c(1,2), sd, na.rm=TRUE)
summerP2_ltm <- apply(summerP2_3d, c(1,2), mean, na.rm=TRUE)
summerP2_std <- apply(summerP2_3d, c(1,2), sd, na.rm=TRUE)

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
for (yr in (nt-nyr-5):(nt-6)){
	PPT_slice <- apply(abind(pre[,,10:12,yr:(yr+5)],pre[,,1:8,(yr+1):(yr+6)],along=3),c(1,2),sum)
	PPT_std_slice <- (PPT_slice - PPT_ltm)/PPT_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	PPT_vgt <- PPT_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr - 6)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	PPT_btl <- PPT_slice * btl_slice
	
	# get standard deviation
	PPT_std_vgt <- PPT_std_slice * vgt
	PPT_std_btl <- PPT_std_slice * btl_slice	
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		PPT <- abind(PPT_slice, PPT_vgt, PPT_btl, along=3)
		PPT_std_all <- abind(PPT_std_slice, PPT_std_vgt, PPT_std_btl, along=3)
	}else{
		PPT <- abind(PPT, PPT_slice, PPT_vgt, PPT_btl, along=3)
		PPT_std_all <- abind(PPT_std_all, PPT_std_slice, PPT_std_vgt, PPT_std_btl, along=3)
		}
	print(paste0(years[btlyr], " is done!"))
}

for (yr in (nt-nyr-1):(nt-2)){
	POctSep_slice <- apply(abind(pre[,,10:12,yr],pre[,,1:9,(yr+1)],along=3),c(1,2),sum)
	POctSep_std_slice <- (POctSep_slice - POctSep_ltm)/POctSep_std
	PcumOctSep_slice <- apply(abind(pre[,,10:12,yr:(yr+1)],pre[,,1:9,(yr+1):(yr+2)],along=3),c(1,2),sum)
	PcumOctSep_std_slice <- (PcumOctSep_slice - PcumOctSep_ltm)/PcumOctSep_std
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	POctSep_vgt <- POctSep_slice * vgt
	PcumOctSep_vgt <- PcumOctSep_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr - 2)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	POctSep_btl <- POctSep_slice * btl_slice
	PcumOctSep_btl <- PcumOctSep_slice * btl_slice
	
	# get standard deviation
	POctSep_std_vgt <- POctSep_std_slice * vgt
	PcumOctSep_std_vgt <- PcumOctSep_std_slice * vgt
	POctSep_std_btl <- POctSep_std_slice * btl_slice
	PcumOctSep_std_btl <- PcumOctSep_std_slice * btl_slice
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		POctSep <- abind(POctSep_slice, POctSep_vgt, POctSep_btl, along=3)
		POctSep_std_all <- abind(POctSep_std_slice, POctSep_std_vgt, POctSep_std_btl, along=3)
		PcumOctSep <- abind(PcumOctSep_slice, PcumOctSep_vgt, PcumOctSep_btl, along=3)
		PcumOctSep_std_all <- abind(PcumOctSep_std_slice, PcumOctSep_std_vgt, PcumOctSep_std_btl, along=3)
	}else{
		POctSep <- abind(POctSep, POctSep_slice, POctSep_vgt, POctSep_btl, along=3)
		POctSep_std_all <- abind(POctSep_std_all, POctSep_std_slice, POctSep_std_vgt, POctSep_std_btl, along=3)
		PcumOctSep <- abind(PcumOctSep, PcumOctSep_slice, PcumOctSep_vgt, PcumOctSep_btl, along=3)
		PcumOctSep_std_all <- abind(PcumOctSep_std_all, PcumOctSep_std_slice, PcumOctSep_std_vgt, PcumOctSep_std_btl, along=3)
		}
	print(paste0(years[btlyr], " is done!"))
}

for (yr in (nt-nyr):(nt-1)){	
	Pmean_slice <- apply(abind(pre[,,8:12,yr],pre[,,1:7,(yr+1)],along=3), c(1,2), mean)
	Pmean_std_slice <- (Pmean_slice - Pmean_ltm)/Pmean_std
	summerP1_slice <- apply(abind(pre[,,6:8,yr], along=3),c(1,2),sum)
	summerP1_std_slice <- (summerP1_slice - summerP1_ltm)/summerP1_std
	summerP2_slice <- apply(abind(pre[,,6:8,yr],pre[,,6:8,(yr+1)],along=3),c(1,2),sum)
	summerP2_std_slice <- (summerP1_slice - summerP1_ltm)/summerP1_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	Pmean_vgt <- Pmean_slice * vgt
	summerP1_vgt <- summerP1_slice * vgt
	summerP2_vgt <- summerP2_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr -1)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	Pmean_btl <- Pmean_slice * btl_slice
	summerP1_btl <- summerP1_slice * btl_slice
	summerP2_btl <- summerP2_slice * btl_slice
	
	# get standard deviation
	Pmean_std_vgt <- Pmean_std_slice * vgt
	summerP1_std_vgt <- summerP1_std_slice * vgt
	summerP2_std_vgt <- summerP2_std_slice * vgt
	
	Pmean_std_btl <- Pmean_std_slice * btl_slice
	summerP1_std_btl <- summerP1_std_slice * btl_slice
	summerP2_std_btl <- summerP2_std_slice * btl_slice
	
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		Pmean <- abind(Pmean_slice, Pmean_vgt, Pmean_btl, along=3)
		summerP1 <- abind(summerP1_slice, summerP1_vgt, summerP1_btl, along=3)
		summerP2 <- abind(summerP2_slice, summerP2_vgt, summerP2_btl, along=3)
		
		Pmean_std_all <- abind(Pmean_std_slice, Pmean_std_vgt, Pmean_std_btl, along=3)
		summerP1_std_all <- abind(summerP1_std_slice, summerP1_std_vgt, summerP1_std_btl, along=3)
		summerP2_std_all <- abind(summerP2_std_slice, summerP2_std_vgt, summerP2_std_btl, along=3)
	}else{
		Pmean <- abind(Pmean, Pmean_slice, Pmean_vgt, Pmean_btl, along=3)
		summerP1 <- abind(summerP1, summerP1_slice, summerP1_vgt, summerP1_btl, along=3)
		summerP2 <- abind(summerP2, summerP2_slice, summerP2_vgt, summerP2_btl, along=3)
		
		Pmean_std_all <- abind(Pmean_std_all, Pmean_std_slice, Pmean_std_vgt, Pmean_std_btl, along=3)
		summerP1_std_all <- abind(summerP1_std_all, summerP1_std_slice, summerP1_std_vgt, summerP1_std_btl, along=3)
		summerP2_std_all <- abind(summerP2_std_all, summerP2_std_slice, summerP2_std_vgt, summerP2_std_btl, along=3)
	}
	print(paste0(years[btlyr], " is done!"))
}

for (yr in (nt-nyr+1):nt){
	GSP_slice <- apply(abind(pre[,,4:6,yr],along=3),c(1,2),sum)
	GSP_std_slice <- (GSP_slice - GSP_ltm)/GSP_std
	summerP0_slice <- apply(abind(pre[,,6:8,yr],along=3),c(1,2),sum)
	summerP0_std_slice <- (summerP0_slice - summerP0_ltm)/summerP0_std
	PMarAug_slice <- apply(abind(pre[,,3:8,yr],along=3),c(1,2),sum)
	PMarAug_std_slice <- (PMarAug_slice - PMarAug_ltm)/PMarAug_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	GSP_vgt <- GSP_slice * vgt
	summerP0_vgt <- summerP0_slice * vgt
	PMarAug_vgt <- PMarAug_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr - (nt - nyr)
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	GSP_btl <- GSP_slice * btl_slice
	summerP0_btl <- summerP0_slice * btl_slice
	PMarAug_btl <- PMarAug_slice * btl_slice
	
	# get standard deviation
	GSP_std_vgt <- GSP_std_slice * vgt
	summerP0_std_vgt <- summerP0_std_slice * vgt
	PMarAug_std_vgt <- PMarAug_std_slice * vgt
	
	GSP_std_btl <- GSP_std_slice * btl_slice
	summerP0_std_btl <- summerP0_std_slice * btl_slice
	PMarAug_std_btl <- PMarAug_std_slice * btl_slice

	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		GSP <- abind(GSP_slice, GSP_vgt, GSP_btl, along=3)
		summerP0 <- abind(summerP0_slice, summerP0_vgt, summerP0_btl, along=3)
		PMarAug <- abind(PMarAug_slice, PMarAug_vgt, PMarAug_btl, along=3)
		
		GSP_std_all <- abind(GSP_std_slice, GSP_std_vgt, GSP_std_btl, along=3)
		summerP0_std_all <- abind(summerP0_std_slice, summerP0_std_vgt, summerP0_std_btl, along=3)
		PMarAug_std_all <- abind(PMarAug_std_slice, PMarAug_std_vgt, PMarAug_std_btl, along=3)
	}else{
		GSP <- abind(GSP, GSP_slice, GSP_vgt, GSP_btl, along=3)
		summerP0 <- abind(summerP0, summerP0_slice, summerP0_vgt, summerP0_btl, along=3)
		PMarAug <- abind(PMarAug, PMarAug_slice, PMarAug_vgt, PMarAug_btl, along=3)
		
		GSP_std_all <- abind(GSP_std_all, GSP_std_slice, GSP_std_vgt, GSP_std_btl, along=3)
		summerP0_std_all <- abind(summerP0_std_all, summerP0_std_slice, summerP0_std_vgt, summerP0_std_btl, along=3)
		PMarAug_std_all <- abind(PMarAug_std_all, PMarAug_std_slice, PMarAug_std_vgt, PMarAug_std_btl, along=3)
	}
	print(paste0(years[btlyr], " is done!"))
}
proc.time() - ptm

print("reshape 3d to 4d")
# reshape 3d array to 4d
nv <- 3 # three variables: land, tree, beetle
Pmean_4d <- array(Pmean, dim=c(nx,ny,nv,nyr)) 
GSP_4d <- array(GSP, dim=c(nx,ny,nv,nyr))
POctSep_4d <- array(POctSep, dim=c(nx,ny,nv,nyr))
PPT_4d <- array(PPT, dim=c(nx,ny,nv,nyr)) 
PMarAug_4d <- array(PMarAug, dim=c(nx,ny,nv,nyr))
PcumOctSep_4d <- array(PcumOctSep, dim=c(nx,ny,nv,nyr))
summerP0_4d <- array(summerP0, dim=c(nx,ny,nv,nyr)) 
summerP1_4d <- array(summerP1, dim=c(nx,ny,nv,nyr))
summerP2_4d <- array(summerP2, dim=c(nx,ny,nv,nyr)) 

Pmean_std_4d <- array(Pmean_std_all, dim=c(nx,ny,nv,nyr)) 
GSP_std_4d <- array(GSP_std_all, dim=c(nx,ny,nv,nyr))
POctSep_std_4d <- array(POctSep_std_all, dim=c(nx,ny,nv,nyr))
PPT_std_4d <- array(PPT_std_all, dim=c(nx,ny,nv,nyr)) 
PMarAug_std_4d <- array(PMarAug_std_all, dim=c(nx,ny,nv,nyr))
PcumOctSep_std_4d <- array(PcumOctSep_std_all, dim=c(nx,ny,nv,nyr))
summerP0_std_4d <- array(summerP0_std_all, dim=c(nx,ny,nv,nyr)) 
summerP1_std_4d <- array(summerP1_std_all, dim=c(nx,ny,nv,nyr))
summerP2_std_4d <- array(summerP2_std_all, dim=c(nx,ny,nv,nyr))

print("quick maps...")
# quick maps to check data
n <- nyr
Pmean_slice_3d <- Pmean_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,10,20,30,40,50,80,100,200,400,800)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_Pmean_tree_",last_year,".png",sep=""))
levelplot(Pmean_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
summerP0_std_slice_4d <- summerP0_std_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_summerP0_std_beetle_",last_year,".png",sep=""))
levelplot(summerP0_std_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# combine all 4d for a loop
var_all_4d <- abind(Pmean_4d, GSP_4d, POctSep_4d, 
					PPT_4d, PMarAug_4d, PcumOctSep_4d,
					summerP0_4d, summerP1_4d, summerP2_4d,
					Pmean_std_4d, GSP_std_4d, POctSep_std_4d,
					PPT_std_4d, PMarAug_std_4d, PcumOctSep_std_4d,
					summerP0_std_4d, summerP1_std_4d, summerP2_std_4d, along=4)
# write 4d data in a loop
print("start to write 4d data")

dnames <- c("Pmean", "GSP", "POctSep", "PPT", "PMarAug", "PcumOctSep", "summerP0", "summerP1", "summerP2", 
			"Pmean_std", "GSP_std", "POctSep_std", "PPT_std", "PMarAug_std", "PcumOctSep_std", 
			"summerP0_std", "summerP1_std", "summerP2_std")
dlongnames <- c("Mean of monthly precipitation from August to July",
				"Sum of precipitation from April to June",
				"Precipitation from October and September in previous year",
				"Cumulative monthly October-August precipitation in current and previous 5 years",
				"Sum of precipitation from March to August",
				"Cumulative precipitation from October to September in current and previous year",
				"Sum of precipitation from June to August",
				"Sum of precipitation from June to August in previous year",
				"Cumulative precipitation from June to August in current and previous year",
				"Departure from the long-term (1901-2016) mean of mean of monthly precipitation from August to July",
				"Departure from the long-term (1901-2016) mean of sum of precipitation from April to June",
				"Departure from the long-term (1901-2016) mean of precipitation from October and September in previous year",
				"Departure from the long-term (1901-2016) mean of cumulative monthly October-August precipitation in current and previous 5 years",
				"Departure from the long-term (1901-2016) mean of sum of precipitation from March to August",
				"Departure from the long-term (1901-2016) mean of cumulative precipitation from October to September in current and previous year",				
				"Departure from the long-term (1901-2016) mean of sum of precipitation from June to August",
				"Departure from the long-term (1901-2016) mean of sum of precipitation from June to August in previous year",
				"Departure from the long-term (1901-2016) mean of cumulative precipitation from June to August in current and previous year")
dunits <- c("mm", "mm", "mm", "mm", "mm", "mm", "mm", "mm", "mm", "", "", "", "", "", "", "", "", "")

d1 <- dim(var_all_4d)[1];d2 <- dim(var_all_4d)[2];d3 <- dim(var_all_4d)[3];d4 <- (dim(var_all_4d)[4])/(length(dnames))
start_year <- 1996; end_year <- 2015
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
	ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

print("start to write 3d data")
# repeat the process four times
var_all_3d <- abind(GSP_3d, PMarAug_3d, summerP0_3d, along=3)
# write 3d data in a loop
dnames <- c("GSP", "PMarAug", "summerP0")
dlongnames <- c("Sum of precipitation from April to June",
				"Sum of precipitation from March to August",
				"Sum of precipitation from June to August")
dunits <- c("mm", "mm", "mm")
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
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Precip_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

var_all_3d <- abind(Pmean_3d, summerP1_3d, summerP2_3d, along=3)
# write 3d data in a loop
dnames <- c("Pmean", "summerP1", "summerP2")
dlongnames <- c("Mean of monthly precipitation from August to July",
				"Sum of precipitation from June to August in previous year",
				"Cumulative precipitation from June to August in current and previous year")
dunits <- c("mm", "mm", "mm")
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
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Precip_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

var_all_3d <- abind(POctSep_3d, PcumOctSep_3d, along=3)
# write 3d data in a loop
dnames <- c("POctSep", "PcumOctSep")
dlongnames <- c("Precipitation from October and September in previous year",
				"Cumulative precipitation from October to September in current and previous year")
dunits <- c("mm", "mm")
d1 <- dim(var_all_3d)[1];d2 <- dim(var_all_3d)[2]; d3 <- (dim(var_all_3d)[3])/(length(dnames))
start_year <- 1903; end_year <- 2016
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
	ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Precip_monthly_ncfiles.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

# write 3d data for PPT
dnames <- "PPT"
dlongnames <- "Cumulative monthly October-August precipitation in current and previous 5 years"
dunits <- "mm"
d1 <- dim(PPT_3d)[1];d2 <- dim(PPT_3d)[2]; d3 <- dim(PPT_3d)[3]
start_year <- 1907; end_year <- 2016
nyr <- length(start_year:end_year)
ptm <- proc.time()
filenm <- paste0("na10km_v2_",dnames,"_",start_year,".",end_year,".3d.nc")
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
var_def <- ncvar_def(dnames,dunits,list(xdim,ydim,yeardim),fillvalue,dlongnames,prec="double")
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
var3d <- array(PPT_3d,dim=c(d1,d2,d3))
ncvar_put(ncout,lon_def,lon)
ncvar_put(ncout,lat_def,lat)
ncvar_put(ncout,var_def,var3d)

# add global attributes
ncatt_put(ncout,0,"title","CRU TS 4.01 absolute values on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by bioclimatic_variables_Precip_monthly_ncfiles.R")
history <- paste("D. Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period",paste0(start_year,"-",end_year))
ncatt_put(ncout,0,"Conventions","CF-1_6")

# close the file, writing data to disk
nc_close(ncout)
print(paste0("writing netCDF file for ", dnames, " is done!"))
proc.time() - ptm
print("all done!")