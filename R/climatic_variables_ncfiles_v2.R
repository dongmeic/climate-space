# Created by Dongmei Chen
# To generate NetCDF files for critical climatic variables

print("load libraries...")
library(ncdf4)
library(abind)
library(lattice)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

path <- "/projects/bonelab/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/projects/bonelab/dongmeic/beetle/output/20180207/"

# variables
# 1: annual mean monthly average of daily mean temperature in the past water year - mat
# 2: mean of monthly average of daily mean temperature from April to August - mtaa
# 3: monthly average of daily mean temperature in August - mta
# 4: minimum of monthly average of daily minimum temperature between Dec and Feb - ntw
# 5: monthly average of daily minimum temperature in October - nto
# 6: monthly average of daily minimum temperature in January - ntj
# 7: monthly average of daily minimum temperature in March - ntm
# 8: monthly average of daily maximum temperature in August - xta
# 9: mean annual precipitation - map; use precipitation data from January to December in the past water year
# 10: cumulative precipitation from June to August in the current and previous year - cpja; use precipitation data in the previous and current year
# 11: precipitation from June to August the previous year - pja;
# 12: cumulative precipitation from October to September in the current and previous year - cpos;
# 13: precipitation from October to September the previous year - pos;
# 14: growing season precipitation the current year - gsp;
# 15: variability of growing season precipitation - vgp

# define time parameters
# near surface air temperature - tmp (1 & 2)
print("read temperature netCDF file")
ncfile <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "tmp"
tmp <- ncvar_get(ncin,dname)
dim(tmp)
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
tmp[tmp==fillvalue$value] <- NA

# make a missing data mask
print("make a landmask...")
landmask <- array(1, dim=c(nx,ny))
# # use last month of data to set data flag
for (j in 1:nx) {
  for (k in 1:ny) {
    if (is.na(tmp[j,k,1,1])) landmask[j,k]=NA
  }
  #print(j);print(k)
}

# near surface air temperature minimum - tmn
print("read temperature minimum netCDF file")
ncfile_tmn <- "na10km_v2_cru_ts4.00.1901.2015.tmn.4d.nc"
ncin_tmn <- nc_open(paste(path, ncfile_tmn, sep=""))
print(ncin_tmn)
dname <- "tmn"
tmn <- ncvar_get(ncin_tmn,dname)
dim(tmn)
nc_close(ncin_tmn)
tmn[tmn==fillvalue$value] <- NA

# near surface air temperature maximum - tmx
print("read temperature maximum netCDF file")
ncfile_tmx <- "na10km_v2_cru_ts4.00.1901.2015.tmx.4d.nc"
ncin_tmx <- nc_open(paste(path, ncfile_tmx, sep=""))
print(ncin_tmx)
dname <- "tmx"
tmx <- ncvar_get(ncin_tmx,dname)
dim(tmx)
nc_close(ncin_tmx)
tmx[tmx==fillvalue$value] <- NA

print("read precipitation netCDF file")
ncfile_pre <- "na10km_v2_cru_ts4.00.1901.2015.pre.abs4d.nc"
ncin_pre <- nc_open(paste(path, ncfile_pre, sep=""))
print(ncin_pre)
dname <- "pre"
pre <- ncvar_get(ncin_pre,dname)
dim(pre)
nc_close(ncin_pre)
pre[pre==fillvalue$value] <- NA

years = 2000:2014; nyr <- length(years)
# get long-term means
# define array for long-term means
nt <- 2015 - 1901
mat_3d <- array(NA, dim=c(nx,ny,nt))
mtaa_3d <- array(NA, dim=c(nx,ny,(nt+1)))
mta_3d <- array(NA, dim=c(nx,ny,(nt+1)))
ntw_3d <- array(NA, dim=c(nx,ny,nt))
nto_3d <- array(NA, dim=c(nx,ny,(nt+1)))
ntj_3d <- array(NA, dim=c(nx,ny,(nt+1)))
ntm_3d <- array(NA, dim=c(nx,ny,(nt+1)))
xta_3d <- array(NA, dim=c(nx,ny,(nt+1)))
map_3d <- array(NA, dim=c(nx,ny,nt))
cpja_3d <- array(NA, dim=c(nx,ny,nt))
pja_3d <- array(NA, dim=c(nx,ny,(nt+1)))
cpos_3d <- array(NA, dim=c(nx,ny,(nt-1)))
pos_3d <- array(NA, dim=c(nx,ny,(nt+1)))
gsp_3d <- array(NA, dim=c(nx,ny,(nt+1)))
vgp_3d <- array(NA, dim=c(nx,ny,(nt+1)))

# long-term means
print("calculate long term means")
ptm <- proc.time()
for (k in 1:(nt+1)){
	if (k > 1 && k < (nt+1)){
		cpos_3d[,,(k-1)] <- apply(abind(pre[,,10:12,(k-1):k],pre[,,1:9,k:(k+1)],along=3),c(1,2),sum)
	} 
	if (k > 1){
		mat_3d[,,(k-1)] <- apply(abind(tmp[,,10:12,(k-1)],tmp[,,1:9,k],along=3),c(1,2),mean)
		map_3d[,,(k-1)] <- apply(abind(pre[,,10:12,(k-1)],pre[,,1:9,k],along=3),c(1,2),mean)
		cpja_3d[,,(k-1)] <- apply(abind(pre[,,6:8,(k-1):k],along=3),c(1,2),sum)
		pos_3d[,,(k-1)] <- apply(abind(pre[,,10:12,(k-1)],pre[,,1:9,k],along=3),c(1,2),sum)		
	}
	if (k < (nt+1)){
		ntw_3d[,,k] <- apply(abind(tmn[,,12,k],tmn[,,1:2,(k+1)],along=3),c(1,2),min)
	}
	mtaa_3d[,,k] <- apply(abind(tmp[,,4:8,k], along=3),c(1,2),mean)
	mta_3d[,,k] <- tmp[,,8,k]
	nto_3d[,,k] <- tmn[,,10,k]
	ntj_3d[,,k] <- tmn[,,1,k]
	ntm_3d[,,k] <- tmn[,,3,k]
	xta_3d[,,k] <- tmx[,,8,k]
	pja_3d[,,k] <- apply(abind(pre[,,6:8,k],along=3),c(1,2),sum)
	gsp_3d[,,k] <- apply(abind(pre[,,4:6,k],along=3),c(1,2),sum)
	vgp_3d[,,k] <- apply(abind(pre[,,4:6,k],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	print(k)
}
proc.time() - ptm

print("calculate standard deviations")
mat_ltm <- apply(mat_3d,c(1,2),mean,na.rm=TRUE)
mat_std <- apply(mat_3d,c(1,2),sd,na.rm=TRUE)
mtaa_ltm <- apply(mtaa_3d,c(1,2),mean,na.rm=TRUE)
mtaa_std <- apply(mtaa_3d,c(1,2),sd,na.rm=TRUE)
mta_ltm <- apply(mta_3d,c(1,2),mean,na.rm=TRUE)
mta_std <- apply(mta_3d,c(1,2),sd,na.rm=TRUE)
ntw_ltm <- apply(ntw_3d,c(1,2),mean,na.rm=TRUE)
ntw_std <- apply(ntw_3d,c(1,2),sd,na.rm=TRUE)
nto_ltm <- apply(nto_3d,c(1,2),mean,na.rm=TRUE)
nto_std <- apply(nto_3d,c(1,2),sd,na.rm=TRUE)
ntj_ltm <- apply(ntj_3d,c(1,2),mean,na.rm=TRUE)
ntj_std <- apply(ntj_3d,c(1,2),sd,na.rm=TRUE)
ntm_ltm <- apply(ntm_3d,c(1,2),mean,na.rm=TRUE)
ntm_std <- apply(ntm_3d,c(1,2),sd,na.rm=TRUE)
xta_ltm <- apply(xta_3d,c(1,2),mean,na.rm=TRUE)
xta_std <- apply(xta_3d,c(1,2),sd,na.rm=TRUE)
map_ltm <- apply(map_3d,c(1,2),mean,na.rm=TRUE)
map_std <- apply(map_3d,c(1,2),sd,na.rm=TRUE)
cpja_ltm <- apply(cpja_3d,c(1,2),mean,na.rm=TRUE)
cpja_std <- apply(cpja_3d,c(1,2),sd,na.rm=TRUE)
cpos_ltm <- apply(cpos_3d,c(1,2),mean,na.rm=TRUE)
cpos_std <- apply(cpos_3d,c(1,2),sd,na.rm=TRUE)
pja_ltm <- apply(pja_3d,c(1,2),mean,na.rm=TRUE)
pja_std <- apply(pja_3d,c(1,2),sd,na.rm=TRUE)
pos_ltm <- apply(pos_3d,c(1,2),mean,na.rm=TRUE)
pos_std <- apply(pos_3d,c(1,2),sd,na.rm=TRUE)
gsp_ltm <- apply(gsp_3d,c(1,2),mean,na.rm=TRUE)
gsp_std <- apply(gsp_3d,c(1,2),sd,na.rm=TRUE)
vgp_ltm <- apply(vgp_3d,c(1,2),mean,na.rm=TRUE)
vgp_std <- apply(vgp_3d,c(1,2),sd,na.rm=TRUE)

# read vegetation and bettle presence data
prs_path <- "/projects/bonelab/dongmeic/beetle/ncfiles/na10km_v2/prs/"
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
for (yr in 98:112){
	# 1. annual mean temperature
	mat_slice <- apply(abind(tmp[,,10:12,(yr+1)],tmp[,,1:9,(yr+2)],along=3),c(1,2),mean)
	matstd_slice <- (mat_slice - mat_ltm)/mat_std
	
	# 2. mean temperature between April and August (in the current year)
	mtaa_slice <- apply(abind(tmp[,,4:8,(yr+2)],along=3),c(1,2),mean)
	mtaastd_slice <- (mtaa_slice - mtaa_ltm)/mtaa_std

	# 3. current year August mean temperature
	mta_slice <- tmp[,,8,(yr+2)]
	mtastd_slice <- (mta_slice - mta_ltm)/mta_std

	# winter as December, January, February
	winter <- abind(tmn[,,12,(yr+1)],tmn[,,1:2,(yr+2)],along=3)
	# 4. winter monthly average daily minimum
	ntw_slice <- apply(winter,c(1,2),min)
	ntwstd_slice <- (ntw_slice - ntw_ltm)/ntw_std

	# 5. October monthly average daily minimum
	nto_slice <- tmn[,,10,(yr+1)]
	ntostd_slice <- (nto_slice - nto_ltm)/nto_std

	# 6. January monthly average daily minimum
	ntj_slice <- tmn[,,1,(yr+2)]
	ntjstd_slice <- (ntj_slice - ntj_ltm)/ntj_std

	# 7. March monthly average daily minimum
	ntm_slice <- tmn[,,3,(yr+2)]
	ntmstd_slice <- (ntm_slice - ntm_ltm)/ntm_std

	# 8. August monthly average daily maximum (in the current year)
	xta_slice <- tmx[,,8,(yr+2)]
	xtastd_slice <- (xta_slice - xta_ltm)/xta_std

	# 9. mean annual precipitation
	map_slice <- apply(abind(pre[,,10:12,(yr+1)],pre[,,1:9,(yr+2)],along=3),c(1,2),mean)
	mapstd_slice <- (map_slice - map_ltm)/map_std

	# 10. cumulative precipitation from June to August in the current and previous years
	cpja_slice <- apply(abind(pre[,,6:8,(yr+1):(yr+2)],along=3),c(1,2),sum)
	cpjastd_slice <- (cpja_slice - cpja_ltm)/cpja_std

	# 11. precipitation from June to August in the previous year
	pja_slice <- apply(abind(pre[,,6:8,(yr+1)],along=3),c(1,2),sum)
	pjastd_slice <- (pja_slice - pja_ltm)/pja_std

	# 12. cumulative precipitation from October to September in the current and previous years
	cpos_slice <- apply(abind(pre[,,10:12,yr:(yr+1)],pre[,,1:9,(yr+1):(yr+2)],along=3),c(1,2),sum)
	cposstd_slice <- (cpos_slice - cpos_ltm)/cpos_std

	# 13. precipitation from October to September in the previous year
	pos_slice <- apply(abind(pre[,,10:12,yr],pre[,,1:9,(yr+1)],along=3),c(1,2),sum)
	posstd_slice <- (pos_slice - pos_ltm)/pos_std

	# 14. total growing season precipitation
	gsp_slice <- apply(pre[,,4:6,(yr+2)],c(1,2),sum)
	gspstd_slice <- (gsp_slice - gsp_ltm)/gsp_std

	# 15. variability of growing season precipitation
	vgp_slice <- apply(abind(pre[,,4:6,(yr+2)],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	vgpstd_slice <- (vgp_slice - vgp_ltm)/vgp_std
	
	# get climate data with the presence of vegetation
	vgt[vgt==0] <- NA
	vgt_mat <- mat_slice * vgt
	vgt_mtaa <- mtaa_slice * vgt
	vgt_mta <- mta_slice * vgt
	vgt_ntw <- ntw_slice * vgt
	vgt_nto <- nto_slice * vgt
	vgt_ntj <- ntj_slice * vgt
	vgt_ntm <- ntm_slice * vgt
	vgt_xta <- xta_slice * vgt
	vgt_map <- map_slice * vgt
	vgt_cpja <- cpja_slice * vgt
	vgt_pja <- pja_slice * vgt
	vgt_cpos <- cpos_slice * vgt
	vgt_pos <- pos_slice * vgt	
	vgt_gsp <- gsp_slice * vgt
	vgt_vgp <- vgp_slice * vgt
	
	# get climate data with the presence of all mpb
	btlyr <- yr-97
	btl_slice <- btl[,,btlyr]
	btl_slice[btl_slice==0] <- NA
	btl_mat <- mat_slice * btl_slice
	btl_mtaa <- mtaa_slice * btl_slice
	btl_mta <- mta_slice * btl_slice
	btl_ntw <- ntw_slice * btl_slice
	btl_nto <- nto_slice * btl_slice
	btl_ntj <- ntj_slice * btl_slice
	btl_ntm <- ntm_slice * btl_slice
	btl_xta <- xta_slice * btl_slice
	btl_map <- map_slice * btl_slice
	btl_cpja <- cpja_slice * btl_slice
	btl_pja <- pja_slice * btl_slice
	btl_cpos <- cpos_slice * btl_slice
	btl_pos <- pos_slice * btl_slice	
	btl_gsp <- gsp_slice * btl_slice
	btl_vgp <- vgp_slice * btl_slice
	
	# get standard deviation
	vgt_matstd <- matstd_slice * vgt
	vgt_mtaastd <- mtaastd_slice * vgt
	vgt_mtastd <- mtastd_slice * vgt
	vgt_ntwstd <- ntwstd_slice * vgt
	vgt_ntostd <- ntostd_slice * vgt
	vgt_ntjstd <- ntjstd_slice * vgt
	vgt_ntmstd <- ntmstd_slice * vgt
	vgt_xtastd <- xtastd_slice * vgt
	vgt_mapstd <- mapstd_slice * vgt
	vgt_cpjastd <- cpjastd_slice * vgt
	vgt_pjastd <- pjastd_slice * vgt
	vgt_cposstd <- cposstd_slice * vgt
	vgt_posstd <- posstd_slice * vgt
	vgt_gspstd <- gspstd_slice * vgt
	vgt_vgpstd <- vgpstd_slice * vgt
	
	btl_matstd <- matstd_slice * btl_slice
	btl_mtaastd <- mtaastd_slice * btl_slice
	btl_mtastd <- mtastd_slice * btl_slice
	btl_ntwstd <- ntwstd_slice * btl_slice
	btl_ntostd <- ntostd_slice * btl_slice
	btl_ntjstd <- ntjstd_slice * btl_slice
	btl_ntmstd <- ntmstd_slice * btl_slice
	btl_xtastd <- xtastd_slice * btl_slice
	btl_mapstd <- mapstd_slice * btl_slice
	btl_cpjastd <- cpjastd_slice * btl_slice
	btl_pjastd <- pjastd_slice * btl_slice
	btl_cposstd <- cposstd_slice * btl_slice
	btl_posstd <- posstd_slice * btl_slice
	btl_gspstd <- gspstd_slice * btl_slice
	btl_vgpstd <- vgpstd_slice * btl_slice
	
	# get array data
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		mat <- abind(mat_slice, vgt_mat, btl_mat, along=3) 
		mtaa <- abind(mtaa_slice, vgt_mtaa, btl_mtaa, along=3)
		mta <- abind(mta_slice, vgt_mta, btl_mta, along=3)
		ntw <- abind(ntw_slice, vgt_ntw, btl_ntw, along=3)
		nto <- abind(nto_slice, vgt_nto, btl_nto, along=3)
		ntj <- abind(ntj_slice, vgt_ntj, btl_ntj, along=3)
		ntm <- abind(ntm_slice, vgt_ntm, btl_ntm, along=3) 
		xta <- abind(xta_slice, vgt_xta, btl_xta, along=3)	
		matstd <- abind(matstd_slice, vgt_matstd, btl_matstd, along=3)
		mtaastd <- abind(mtaastd_slice, vgt_mtaastd, btl_mtaastd, along=3)
		mtastd <- abind(mtastd_slice, vgt_mtastd, btl_mtastd, along=3)
		ntwstd <- abind(ntwstd_slice, vgt_ntwstd, btl_ntwstd, along=3)
		ntostd <- abind(ntostd_slice, vgt_ntostd, btl_ntostd, along=3)
		ntjstd <- abind(ntjstd_slice, vgt_ntjstd, btl_ntjstd, along=3)
		ntmstd <- abind(ntmstd_slice, vgt_ntmstd, btl_ntmstd, along=3)
		xtastd <- abind(xtastd_slice, vgt_xtastd, btl_xtastd, along=3)
		map <- abind(map_slice, vgt_map, btl_map, along=3)
		cpja <- abind(cpja_slice, vgt_cpja, btl_cpja, along=3)
		pja <- abind(pja_slice, vgt_pja, btl_pja, along=3)
		cpos <- abind(cpos_slice, vgt_cpos, btl_cpos, along=3)
		pos <- abind(pos_slice, vgt_pos, btl_pos, along=3)
		gsp <- abind(gsp_slice, vgt_gsp, btl_gsp, along=3)
		vgp <- abind(vgp_slice, vgt_vgp, btl_vgp, along=3)
		mapstd <- abind(mapstd_slice, vgt_mapstd, btl_mapstd, along=3)
		cpjastd <- abind(cpjastd_slice, vgt_cpjastd, btl_cpjastd, along=3)
		pjastd <- abind(pjastd_slice, vgt_pjastd, btl_pjastd, along=3)
		cposstd <- abind(cposstd_slice, vgt_cposstd, btl_cposstd, along=3)
		posstd <- abind(posstd_slice, vgt_posstd, btl_posstd, along=3)
		gspstd <- abind(gspstd_slice, vgt_gspstd, btl_gspstd, along=3)
		vgpstd <- abind(vgpstd_slice, vgt_vgpstd, btl_vgpstd, along=3)
					
	} else{
		print(paste0("start to reshape 2d to 3d in year ", years[btlyr]))
		mat <- abind(mat, mat_slice, vgt_mat, btl_mat, along=3) 
		mtaa <- abind(mtaa, mtaa_slice, vgt_mtaa, btl_mtaa, along=3)
		mta <- abind(mta, mta_slice, vgt_mta, btl_mta, along=3)
		ntw <- abind(ntw, ntw_slice, vgt_ntw, btl_ntw, along=3)
		nto <- abind(nto, nto_slice, vgt_nto, btl_nto, along=3)
		ntj <- abind(ntj, ntj_slice, vgt_ntj, btl_ntj, along=3)
		ntm <- abind(ntm, ntm_slice, vgt_ntm, btl_ntm, along=3) 
		xta <- abind(xta, xta_slice, vgt_xta, btl_xta, along=3)	
		matstd <- abind(matstd, matstd_slice, vgt_matstd, btl_matstd, along=3)
		mtaastd <- abind(mtaastd, mtaastd_slice, vgt_mtaastd, btl_mtaastd, along=3)
		mtastd <- abind(mtastd, mtastd_slice, vgt_mtastd, btl_mtastd, along=3)
		ntwstd <- abind(ntwstd, ntwstd_slice, vgt_ntwstd, btl_ntwstd, along=3)
		ntostd <- abind(ntostd, ntostd_slice, vgt_ntostd, btl_ntostd, along=3)
		ntjstd <- abind(ntjstd, ntjstd_slice, vgt_ntjstd, btl_ntjstd, along=3)
		ntmstd <- abind(ntmstd, ntmstd_slice, vgt_ntmstd, btl_ntmstd, along=3)
		xtastd <- abind(xtastd, xtastd_slice, vgt_xtastd, btl_xtastd, along=3)
		map <- abind(map, map_slice, vgt_map, btl_map, along=3)
		cpja <- abind(cpja, cpja_slice, vgt_cpja, btl_cpja, along=3)
		pja <- abind(pja, pja_slice, vgt_pja, btl_pja, along=3)
		cpos <- abind(cpos, cpos_slice, vgt_cpos, btl_cpos, along=3)
		pos <- abind(pos, pos_slice, vgt_pos, btl_pos, along=3)
		gsp <- abind(gsp, gsp_slice, vgt_gsp, btl_gsp, along=3)
		vgp <- abind(vgp, vgp_slice, vgt_vgp, btl_vgp, along=3)
		mapstd <- abind(mapstd, mapstd_slice, vgt_mapstd, btl_mapstd, along=3)
		cpjastd <- abind(cpjastd, cpjastd_slice, vgt_cpjastd, btl_cpjastd, along=3)
		pjastd <- abind(pjastd, pjastd_slice, vgt_pjastd, btl_pjastd, along=3)
		cposstd <- abind(cposstd, cposstd_slice, vgt_cposstd, btl_cposstd, along=3)
		posstd <- abind(posstd, posstd_slice, vgt_posstd, btl_posstd, along=3)
		gspstd <- abind(gspstd, gspstd_slice, vgt_gspstd, btl_gspstd, along=3)
		vgpstd <- abind(vgpstd, vgpstd_slice, vgt_vgpstd, btl_vgpstd, along=3)
		
	}
	print(paste0(years[btlyr], " is done!"))
}
proc.time() - ptm

print("reshape 3d to 4d")
# reshape 3d array to 4d
nv <- 3 # three variables: land, tree, beetle
mat_4d <- array(mat, dim=c(nx,ny,nv,nyr)) 
mtaa_4d <- array(mtaa, dim=c(nx,ny,nv,nyr)) 
mta_4d <- array(mta, dim=c(nx,ny,nv,nyr)) 
ntw_4d <- array(ntw, dim=c(nx,ny,nv,nyr)) 
nto_4d <- array(nto, dim=c(nx,ny,nv,nyr)) 
ntj_4d <- array(ntj, dim=c(nx,ny,nv,nyr)) 
ntm_4d <- array(ntm, dim=c(nx,ny,nv,nyr)) 
xta_4d <- array(xta, dim=c(nx,ny,nv,nyr))
matstd_4d <- array(matstd, dim=c(nx,ny,nv,nyr)) 
mtaastd_4d <- array(mtaastd, dim=c(nx,ny,nv,nyr)) 
mtastd_4d <- array(mtastd, dim=c(nx,ny,nv,nyr)) 
ntwstd_4d <- array(ntwstd, dim=c(nx,ny,nv,nyr)) 
ntostd_4d <- array(ntostd, dim=c(nx,ny,nv,nyr)) 
ntjstd_4d <- array(ntjstd, dim=c(nx,ny,nv,nyr)) 
ntmstd_4d <- array(ntmstd, dim=c(nx,ny,nv,nyr)) 
xtastd_4d <- array(xtastd, dim=c(nx,ny,nv,nyr)) 
map_4d <- array(map, dim=c(nx,ny,nv,nyr)) 
cpja_4d <- array(cpja, dim=c(nx,ny,nv,nyr))
pja_4d <- array(pja, dim=c(nx,ny,nv,nyr)) 
cpos_4d <- array(cpos, dim=c(nx,ny,nv,nyr)) 
pos_4d <- array(pos, dim=c(nx,ny,nv,nyr)) 
gsp_4d <- array(gsp, dim=c(nx,ny,nv,nyr)) 
vgp_4d <- array(vgp, dim=c(nx,ny,nv,nyr)) 
mapstd_4d <- array(mapstd, dim=c(nx,ny,nv,nyr)) 
cpjastd_4d <- array(cpjastd, dim=c(nx,ny,nv,nyr)) 
pjastd_4d <- array(pjastd, dim=c(nx,ny,nv,nyr)) 
cposstd_4d <- array(cposstd, dim=c(nx,ny,nv,nyr)) 
posstd_4d <- array(posstd, dim=c(nx,ny,nv,nyr)) 
gspstd_4d <- array(gspstd, dim=c(nx,ny,nv,nyr)) 
vgpstd_4d <- array(vgpstd, dim=c(nx,ny,nv,nyr))

print("quick maps...")
start_year = 2000; end_year = 2014;
# quick maps to check data
n <- 15
mat_slice_3d <- mat_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_mat_tree_",end_year,".png",sep=""))
levelplot(mat_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
gspstd_slice_4d <- gspstd_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_gspstd_beetle_",end_year,".png",sep=""))
levelplot(gspstd_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

print("reshape 4d to 2d")
# write to csvfiles (data example) - reshape 4d array to 2d, and then to vector
mat_m <- apply(mat_4d[,,3,], c(1,2), mean, na.rm=TRUE)
mtaa_m <- apply(mtaa_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
mta_m <- apply(mta_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
ntw_m <- apply(ntw_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
nto_m <- apply(nto_4d[,,3,], c(1,2), mean, na.rm=TRUE)
ntj_m <- apply(ntj_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
ntm_m <- apply(ntm_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
xta_m <- apply(xta_4d[,,3,], c(1,2), mean)
matstd_m <- apply(matstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
mtaastd_m <- apply(mtaastd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
mtastd_m <- apply(mtastd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
ntwstd_m <- apply(ntwstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
ntostd_m <- apply(ntostd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
ntjstd_m <- apply(ntjstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
ntmstd_m <- apply(ntmstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
xtastd_m <- apply(xtastd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
map_m <- apply(map_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
cpja_m <- apply(cpja_4d[,,3,], c(1,2), mean, na.rm=TRUE)
pja_m <- apply(pja_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
cpos_m <- apply(cpos_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
pos_m <- apply(pos_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
gsp_m <- apply(gsp_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
vgp_m <- apply(vgp_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
mapstd_m <- apply(mapstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
cpjastd_m <- apply(cpjastd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
pjastd_m <- apply(pjastd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
cposstd_m <- apply(cposstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
posstd_m <- apply(posstd_4d[,,3,], c(1,2), mean, na.rm=TRUE)
gspstd_m <- apply(gspstd_4d[,,3,], c(1,2), mean, na.rm=TRUE) 
vgpstd_m <- apply(vgpstd_4d[,,3,], c(1,2), mean, na.rm=TRUE)
print("reshape 4d to 2d DONE!")

print("mask 2d in land")
mat_m <- mat_m[!is.na(landmask)] 
mtaa_m <- mtaa_m[!is.na(landmask)] 
mta_m <- mta_m[!is.na(landmask)] 
ntw_m <- ntw_m[!is.na(landmask)] 
nto_m <- nto_m[!is.na(landmask)]
ntj_m <- ntj_m[!is.na(landmask)] 
ntm_m <- ntm_m[!is.na(landmask)] 
xta_m <- xta_m[!is.na(landmask)]
matstd_m <- matstd_m[!is.na(landmask)] 
mtaastd_m <- mtaastd_m[!is.na(landmask)] 
mtastd_m <- mtastd_m[!is.na(landmask)] 
ntwstd_m <- ntwstd_m[!is.na(landmask)] 
ntostd_m <- ntostd_m[!is.na(landmask)] 
ntjstd_m <- ntjstd_m[!is.na(landmask)] 
ntmstd_m <- ntmstd_m[!is.na(landmask)] 
xtastd_m <- xtastd_m[!is.na(landmask)] 
map_m <- map_m[!is.na(landmask)] 
cpja_m <- cpja_m[!is.na(landmask)]
pja_m <- pja_m[!is.na(landmask)] 
cpos_m <- cpos_m[!is.na(landmask)] 
pos_m <- pos_m[!is.na(landmask)] 
gsp_m <- gsp_m[!is.na(landmask)] 
vgp_m <- vgp_m[!is.na(landmask)] 
mapstd_m <- mapstd_m[!is.na(landmask)] 
cpjastd_m <- cpjastd_m[!is.na(landmask)] 
pjastd_m <- pjastd_m[!is.na(landmask)] 
cposstd_m <- cposstd_m[!is.na(landmask)] 
posstd_m <- posstd_m[!is.na(landmask)]
gspstd_m <- gspstd_m[!is.na(landmask)] 
vgpstd_m <- vgpstd_m[!is.na(landmask)]
print("mask 2d in land DONE")

csvpath <- "/projects/bonelab/dongmeic/beetle/csvfiles/"
na10km_2d <- read.csv(paste0(csvpath, "na10km_v2.csv"))

if (all(sapply(list(c(mat_m), c(mtaa_m), c(mta_m), c(ntw_m),c(nto_m),c(ntj_m),c(ntm_m),c(xta_m),
c(matstd_m),c(mtaastd_m),c(mtastd_m),c(ntwstd_m),c(ntostd_m),c(ntjstd_m),c(ntmstd_m),
c(xtastd_m), c(map_m), c(cpja_m), c(pja_m),c(cpos_m),c(pos_m),c(gsp_m),c(vgp_m),c(mapstd_m),
c(cpjastd_m),c(pjastd_m),c(cposstd_m),c(posstd_m),c(gspstd_m),c(vgpstd_m)), function(x) identical(length(x), dim(na10km_2d)[1])))){
	
	na10km_2d_n <- cbind(na10km_2d, c(mat_m), c(mtaa_m), c(mta_m), c(ntw_m),c(nto_m),c(ntj_m),c(ntm_m),c(xta_m),
	c(matstd_m),c(mtaastd_m),c(mtastd_m),c(ntwstd_m),c(ntostd_m),c(ntjstd_m),c(ntmstd_m),
	c(xtastd_m), c(map_m), c(cpja_m), c(pja_m),c(cpos_m),c(pos_m),c(gsp_m),c(vgp_m),c(mapstd_m),
	c(cpjastd_m),c(pjastd_m),c(cposstd_m),c(posstd_m),c(gspstd_m),c(vgpstd_m))
	print("write a csvfile...")
	write.csv(na10km_2d_n, paste0(csvpath,"climatic_variables_beetle_v2.csv"), row.names=FALSE)
	
}else{

	sapply(list(c(mat_m), c(mtaa_m), c(mta_m), c(ntw_m),c(nto_m),c(ntj_m),c(ntm_m),c(xta_m),
             c(matstd_m),c(mtaastd_m),c(mtastd_m),c(ntwstd_m),c(ntostd_m),c(ntjstd_m),c(ntmstd_m),
             c(xtastd_m), c(map_m), c(cpja_m), c(pja_m),c(cpos_m),c(pos_m),c(gsp_m),c(vgp_m),c(mapstd_m),
             c(cpjastd_m),c(pjastd_m),c(cposstd_m),c(posstd_m),c(gspstd_m),c(vgpstd_m)), function(x) identical(length(x), dim(na10km_2d)[1]))
             
}

print("recode fillvalue")
# recode fillvalues
fillvalue <- 1e32
mat_4d[is.na(mat_4d)] <- fillvalue
mtaa_4d[is.na(mtaa_4d)] <- fillvalue 
mta_4d[is.na(mta_4d)] <- fillvalue 
ntw_4d[is.na(ntw_4d)] <- fillvalue 
nto_4d[is.na(nto_4d)] <- fillvalue 
ntj_4d[is.na(ntj_4d)] <- fillvalue 
ntm_4d[is.na(ntm_4d)] <- fillvalue 
xta_4d[is.na(xta_4d)] <- fillvalue 
matstd_4d[is.na(matstd_4d)] <- fillvalue 
mtaastd_4d[is.na(mtaastd_4d)] <- fillvalue 
mtastd_4d[is.na(mtastd_4d)] <- fillvalue 
ntwstd_4d[is.na(ntwstd_4d)] <- fillvalue 
ntostd_4d[is.na(ntostd_4d)] <- fillvalue 
ntjstd_4d[is.na(ntjstd_4d)] <- fillvalue 
ntmstd_4d[is.na(ntmstd_4d)] <- fillvalue 
xtastd_4d[is.na(xtastd_4d)] <- fillvalue 
map_4d[is.na(map_4d)] <- fillvalue 
cpja_4d[is.na(cpja_4d)] <- fillvalue 
pja_4d[is.na(pja_4d)] <- fillvalue 
cpos_4d[is.na(cpos_4d)] <- fillvalue 
pos_4d[is.na(pos_4d)] <- fillvalue 
gsp_4d[is.na(gsp_4d)] <- fillvalue 
vgp_4d[is.na(vgp_4d)] <- fillvalue 
mapstd_4d[is.na(mapstd_4d)] <- fillvalue 
cpjastd_4d[is.na(cpjastd_4d)] <- fillvalue 
pjastd_4d[is.na(pjastd_4d)] <- fillvalue 
cposstd_4d[is.na(cposstd_4d)] <- fillvalue 
posstd_4d[is.na(posstd_4d)] <- fillvalue 
gspstd_4d[is.na(gspstd_4d)] <- fillvalue 
vgpstd_4d[is.na(vgpstd_4d)] <- fillvalue

# combine all 4d for a loop
var_all_4d <- abind(mat_4d,mtaa_4d,mta_4d,ntw_4d,nto_4d,ntj_4d,ntm_4d,xta_4d,matstd_4d,mtaastd_4d,mtastd_4d,ntwstd_4d,ntostd_4d,
ntjstd_4d,ntmstd_4d,xtastd_4d,map_4d,cpja_4d,pja_4d,cpos_4d,pos_4d,gsp_4d,vgp_4d,mapstd_4d,cpjastd_4d,pjastd_4d,
cposstd_4d,posstd_4d,gspstd_4d,vgpstd_4d, along=4)

# write 4d data in a loop
print("start to write 4d data")

dnames <- c("mat", "mtaa", "mta", "ntw", "nto", "ntj", "ntm", "xta","matstd", "mtaastd", "mtastd", 
"ntwstd", "ntostd", "ntjstd", "ntmstd", "xtastd", "map", "cpja", "pja", "cpos", "pos", 
"gsp", "vgp", "mapstd", "cpjastd", "pjastd", "cposstd", "posstd", "gspstd", "vgpstd") 

dlongnames <- c("Annual mean monthly average of daily mean temperature in the past water year - mat",
  "Mean of monthly average of daily mean temperature from April to August - mtaa",
  "Monthly average of daily mean temperature in August - mta",
  "Minimum of monthly average of daily minimum temperature between Dec and Feb - ntw",
  "Monthly average of daily minimum temperature in October - nto",
  "Monthly average of daily minimum temperature in January - ntj",
  "Monthly average of daily minimum temperature in March - ntm",
  "Monthly average of daily maximum temperature in August - xta",
  "Departure from the long-term (1901-2015) mean of annual mean monthly average of daily mean temperature in the past water year - matstd",
  "Departure from the long-term (1901-2015) mean of mean of monthly average of daily mean temperature from April to August - mtaastd",
  "Departure from the long-term (1901-2015) mean of monthly average of daily mean temperature in August - mtastd",
  "Departure from the long-term (1901-2015) mean of minimum of monthly average of daily minimum temperature between Dec and Feb - ntwstd",
  "Departure from the long-term (1901-2015) mean of monthly average of daily minimum temperature in October - ntostd",
  "Departure from the long-term (1901-2015) mean of monthly average of daily minimum temperature in January - ntjstd",
  "Departure from the long-term (1901-2015) mean of monthly average of daily minimum temperature in March - ntmstd",
  "Departure from the long-term (1901-2015) mean of monthly average of daily maximum temperature in August - xtastd",  
  "Mean annual precipitation in the past water year - map",
  "Cumulative precipitation from June to August the current and previous year - cpja",
  "Precipitation from June to August the previous year - pja",
  "Cumulative precipitation from October to September the current and previous year - cpos",
  "Precipitation from October to September the previous year - pos",
  "Growing season precipitation the current year - gsp",
  "Variability of growing season precipitation - vgp",
  "Departure from the long-term (1901-2015) mean of mean annual precipitation in the past water year - mapstd",
  "Departure from the long-term (1901-2015) mean of cumulative precipitation from June to August the current and previous year - cpjastd",
  "Departure from the long-term (1901-2015) mean of precipitation from June to August the previous year - pjastd",
  "Departure from the long-term (1901-2015) mean of cumulative precipitation from October to September the current and previous year - cposstd",
  "Departure from the long-term (1901-2015) mean of precipitation from October to September the previous year - posstd",
  "Departure from the long-term (1901-2015) mean of growing season precipitation the current year - gspstd",
  "Departure from the long-term (1901-2015) mean of variability of growing season precipitation - vgpstd")

dunits <- c("°C","°C","°C","°C","°C","°C","°C","°C","","","","","","","","","mm","mm","mm","mm","mm","mm","","","","","","","","")

d1 <- dim(var_all_4d)[1];d2 <- dim(var_all_4d)[2];d3 <- dim(var_all_4d)[3];d4 <- (dim(var_all_4d)[4])/(length(dnames))
ptm <- proc.time() # timer
foreach(i=1:30) %dopar%{

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
	ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by climatic_variables_ncfiles_v2.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","2000-2014")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

# combine all 3d for a loop
var_all_3d <- abind(mat_3d[,,2:113],mtaa_3d[,,3:114],mta_3d[,,3:114],ntw_3d[,,2:113],nto_3d[,,2:113],ntj_3d[,,3:114],ntm_3d[,,3:114],xta_3d[,,3:114],
map_3d[,,2:113],cpja_3d[,,2:113],pja_3d[,,3:114],cpos_3d[,,1:112],pos_3d[,,2:113],gsp_3d[,,3:114],vgp_3d[,,3:114],along=3)

start_year = 1903; end_year = 2014;
nyr <- end_year - start_year + 1
# write 3d data in a loop
print("start to write 3d data")
dnames <- c("mat", "mtaa", "mta", "ntw", "nto", "ntj", "ntm", "xta", "map", "cpja", "pja", "cpos", "pos", "gsp", "vgp") 

dlongnames <- c("Annual mean monthly average of daily mean temperature in the past water year - mat",
  "Mean of monthly average of daily mean temperature from April to August - mtaa",
  "Monthly average of daily mean temperature in August - mta",
  "Minimum of monthly average of daily minimum temperature between Dec and Feb - ntw",
  "Monthly average of daily minimum temperature in October - nto",
  "Monthly average of daily minimum temperature in January - ntj",
  "Monthly average of daily minimum temperature in March - ntm",
  "Monthly average of daily maximum temperature in August - xta",
  "Mean annual precipitation in the past water year - map",
  "Cumulative precipitation from June to August the current and previous year - cpja",
  "Precipitation from June to August the previous year - pja",
  "Cumulative precipitation from October to September the current and previous year - cpos",
  "Precipitation from October to September the previous year - pos",
  "Growing season precipitation the current year - gsp",
  "Variability of growing season precipitation - vgp")

dunits <- c("°C","°C","°C","°C","°C","°C","°C","°C","mm","mm","mm","mm","mm","mm","")

d1 <- dim(var_all_3d)[1];d2 <- dim(var_all_3d)[2]; d3 <- (dim(var_all_3d)[3])/(length(dnames))
ptm <- proc.time() # timer
foreach(i=1:15) %dopar%{

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
	ncatt_put(ncout,0,"title","CRU CL 2.0 absolute values on the na10km_v2 10-km Grid")
	ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
	ncatt_put(ncout,0,"source","generated by climatic_variables_ncfiles_v2.R")
	history <- paste("D. Chen", date(), sep=", ")
	ncatt_put(ncout,0,"history",history)
	ncatt_put(ncout,0,"base_period","1903-2014")
	ncatt_put(ncout,0,"Conventions","CF-1_6")

	# close the file, writing data to disk
	nc_close(ncout)
	print(paste0("writing netCDF file for ", dnames[i], " is done!"))
}
proc.time() - ptm

print("all done!")