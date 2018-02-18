# Created by Dongmei Chen
# To generate yearly climatic data with coordinates
# Based on climatic_variable_ncfiles.R

print("load libraries...")
library(ncdf4)
library(abind)
library(lattice)
library(RColorBrewer)

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
# 10: cumulative precipitation from June to August in current and previous year - cpja; use precipitation data in the previous and current year
# 11: precipitation from June to August in previous year - pja;
# 12: cumulative precipitation from October to September in current and previous year - cpos;
# 13: precipitation from October to September in previous year - pos;
# 14: growing season precipitation in current year - gsp;
# 15: variability of growing season precipitation - vgp

# define time parameters
start_year = 1903; end_year = 2014;

# near surface air temperature - tmp (1 & 2)
print("read temperature netCDF file")
ncfile <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs4d.nc"
ncin <- nc_open(paste(path, ncfile, sep=""))
print(ncin)
dname <- "tmp"
tmp <- ncvar_get(ncin,dname)
dim(tmp)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")

nc_close(ncin)
tmp[tmp==fillvalue$value] <- NA

# near surface air temperature minimum - tmn
print("read temperature minimum netCDF file")
ncfile_tmn <- "na10km_v2_cru_ts4.00.1901.2015.tmn.4d.nc"
ncin_tmn <- nc_open(paste(path, ncfile_tmn, sep=""))
print(ncin_tmn)
dname <- "tmn"
tmn <- ncvar_get(ncin_tmn,dname)
dim(tmn)

# get dimension variables and attributes
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
# get longitude and latitude and attributes
lon <- ncvar_get(ncin,"lon")
lat <- ncvar_get(ncin,"lat")

nc_close(ncin_tmn)
tmn[tmn==fillvalue$value] <- NA

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

years = 1903:2014; nyr <- length(years)
# get long-term means
# define array for long-term means
nt <- 2015 - 1901
mat <- array(NA, dim=c(nx,ny,nt))
mtaa <- array(NA, dim=c(nx,ny,(nt+1)))
mta <- array(NA, dim=c(nx,ny,(nt+1)))
ntw <- array(NA, dim=c(nx,ny,nt))
nto <- array(NA, dim=c(nx,ny,(nt+1)))
ntj <- array(NA, dim=c(nx,ny,(nt+1)))
ntm <- array(NA, dim=c(nx,ny,(nt+1)))
xta <- array(NA, dim=c(nx,ny,(nt+1)))
map <- array(NA, dim=c(nx,ny,nt))
cpja <- array(NA, dim=c(nx,ny,nt))
pja <- array(NA, dim=c(nx,ny,(nt+1)))
cpos <- array(NA, dim=c(nx,ny,(nt-1)))
pos <- array(NA, dim=c(nx,ny,(nt+1)))
gsp <- array(NA, dim=c(nx,ny,(nt+1)))
vgp <- array(NA, dim=c(nx,ny,(nt+1)))

# long-term means
print("calculate long term means")
ptm <- proc.time()
for (k in 1:(nt+1)){
	if (k > 1 && k < (nt+1)){
		cpos[,,(k-1)] <- apply(abind(pre[,,10:12,(k-1):k],pre[,,1:9,k:(k+1)],along=3),c(1,2),sum)
	} 
	if (k > 1){
		mat[,,(k-1)] <- apply(abind(tmp[,,10:12,(k-1)],tmp[,,1:9,k],along=3),c(1,2),mean)
		map[,,(k-1)] <- apply(abind(pre[,,10:12,(k-1)],pre[,,1:9,k],along=3),c(1,2),mean)
		cpja[,,(k-1)] <- apply(abind(pre[,,6:8,(k-1):k],along=3),c(1,2),sum)
		pos[,,(k-1)] <- apply(abind(pre[,,10:12,(k-1)],pre[,,1:9,k],along=3),c(1,2),sum)		
	}
	if (k < (nt+1)){
		ntw[,,k] <- apply(abind(tmn[,,12,k],tmn[,,1:2,(k+1)],along=3),c(1,2),min)
	}
	mtaa[,,k] <- apply(abind(tmp[,,4:8,k], along=3),c(1,2),mean)
	mta[,,k] <- tmp[,,8,k]
	nto[,,k] <- tmn[,,10,k]
	ntj[,,k] <- tmn[,,1,k]
	ntm[,,k] <- tmn[,,3,k]
	xta[,,k] <- tmx[,,8,k]
	pja[,,k] <- apply(abind(pre[,,6:8,k],along=3),c(1,2),sum)
	gsp[,,k] <- apply(abind(pre[,,4:6,k],along=3),c(1,2),sum)
	vgp[,,k] <- apply(abind(pre[,,4:6,k],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	print(k)
}
proc.time() - ptm

print("calculate standard deviations")
mat_ltm <- apply(mat,c(1,2),mean,na.rm=TRUE)
mat_std <- apply(mat,c(1,2),sd,na.rm=TRUE)
mtaa_ltm <- apply(mtaa,c(1,2),mean,na.rm=TRUE)
mtaa_std <- apply(mtaa,c(1,2),sd,na.rm=TRUE)
mta_ltm <- apply(mta,c(1,2),mean,na.rm=TRUE)
mta_std <- apply(mta,c(1,2),sd,na.rm=TRUE)
ntw_ltm <- apply(ntw,c(1,2),mean,na.rm=TRUE)
ntw_std <- apply(ntw,c(1,2),sd,na.rm=TRUE)
nto_ltm <- apply(nto,c(1,2),mean,na.rm=TRUE)
nto_std <- apply(nto,c(1,2),sd,na.rm=TRUE)
ntj_ltm <- apply(ntj,c(1,2),mean,na.rm=TRUE)
ntj_std <- apply(ntj,c(1,2),sd,na.rm=TRUE)
ntm_ltm <- apply(ntm,c(1,2),mean,na.rm=TRUE)
ntm_std <- apply(ntm,c(1,2),sd,na.rm=TRUE)
xta_ltm <- apply(xta,c(1,2),mean,na.rm=TRUE)
xta_std <- apply(xta,c(1,2),sd,na.rm=TRUE)
map_ltm <- apply(map,c(1,2),mean,na.rm=TRUE)
map_std <- apply(map,c(1,2),sd,na.rm=TRUE)
cpja_ltm <- apply(cpja,c(1,2),mean,na.rm=TRUE)
cpja_std <- apply(cpja,c(1,2),sd,na.rm=TRUE)
cpos_ltm <- apply(cpos,c(1,2),mean,na.rm=TRUE)
cpos_std <- apply(cpos,c(1,2),sd,na.rm=TRUE)
pja_ltm <- apply(pja,c(1,2),mean,na.rm=TRUE)
pja_std <- apply(pja,c(1,2),sd,na.rm=TRUE)
pos_ltm <- apply(pos,c(1,2),mean,na.rm=TRUE)
pos_std <- apply(pos,c(1,2),sd,na.rm=TRUE)
gsp_ltm <- apply(gsp,c(1,2),mean,na.rm=TRUE)
gsp_std <- apply(gsp,c(1,2),sd,na.rm=TRUE)
vgp_ltm <- apply(vgp,c(1,2),mean,na.rm=TRUE)
vgp_std <- apply(vgp,c(1,2),sd,na.rm=TRUE)

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
	ntwstd_slice <- (ntw_slice - ntw_ltm)/ntm_std

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
	
	# mask the points in land
	mat_slice_msk <- mat_slice[!is.na(landmask)]
	mtaa_slice_msk <- mtaa_slice[!is.na(landmask)] 
	mta_slice_msk <- mta_slice[!is.na(landmask)]
	ntw_slice_msk <- ntw_slice[!is.na(landmask)] 
	nto_slice_msk <- nto_slice[!is.na(landmask)] 
	ntj_slice_msk <- ntj_slice[!is.na(landmask)] 
	ntm_slice_msk <- ntm_slice[!is.na(landmask)] 
	xta_slice_msk <- xta_slice[!is.na(landmask)] 
	map_slice_msk <- map_slice[!is.na(landmask)] 
	cpja_slice_msk <- cpja_slice[!is.na(landmask)] 
	pja_slice_msk <- pja_slice[!is.na(landmask)]  
	cpos_slice_msk <- cpos_slice[!is.na(landmask)] 
	pos_slice_msk <- pos_slice[!is.na(landmask)] 
	gsp_slice_msk <- gsp_slice[!is.na(landmask)] 
	vgp_slice_msk <- vgp_slice[!is.na(landmask)] 
	
	matstd_slice_msk <- matstd_slice[!is.na(landmask)] 
	mtaastd_slice_msk <- mtaastd_slice[!is.na(landmask)] 
	mtastd_slice_msk <- mtastd_slice[!is.na(landmask)]
	ntwstd_slice_msk <- ntwstd_slice[!is.na(landmask)] 
	ntostd_slice_msk <- ntostd_slice[!is.na(landmask)] 
	ntjstd_slice_msk <- ntjstd_slice[!is.na(landmask)] 
	ntmstd_slice_msk <- ntmstd_slice[!is.na(landmask)] 
	xtastd_slice_msk <- xtastd_slice[!is.na(landmask)]
	mapstd_slice_msk <- mapstd_slice[!is.na(landmask)] 
	cpjastd_slice_msk <- cpjastd_slice[!is.na(landmask)] 
	pjastd_slice_msk <- pjastd_slice[!is.na(landmask)] 
	cposstd_slice_msk <- cposstd_slice[!is.na(landmask)]  
	posstd_slice_msk <- posstd_slice[!is.na(landmask)]
	gspstd_slice_msk <- gspstd_slice[!is.na(landmask)] 
	vgpstd_slice_msk <- vgpstd_slice[!is.na(landmask)] 
	
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
	
	vgt_mat_msk <- vgt_mat[!is.na(landmask)] 
	vgt_mtaa_msk <- vgt_mtaa[!is.na(landmask)]
	vgt_mta_msk <- vgt_mta[!is.na(landmask)] 
	vgt_ntw_msk <- vgt_ntw[!is.na(landmask)] 
	vgt_nto_msk <- vgt_nto[!is.na(landmask)] 
	vgt_ntj_msk <- vgt_ntj[!is.na(landmask)] 
	vgt_ntm_msk <- vgt_ntm[!is.na(landmask)] 
	vgt_xta_msk <- vgt_xta[!is.na(landmask)] 
	vgt_map_msk <- vgt_map[!is.na(landmask)] 
	vgt_cpja_msk <- vgt_cpja[!is.na(landmask)] 
	vgt_pja_msk <- vgt_pja[!is.na(landmask)] 
	vgt_cpos_msk <- vgt_cpos[!is.na(landmask)] 
	vgt_pos_msk <- vgt_pos[!is.na(landmask)] 	
	vgt_gsp_msk <- vgt_gsp[!is.na(landmask)] 
	vgt_vgp_msk <- vgt_vgp[!is.na(landmask)] 
	
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
	
	btl_mat_msk <- btl_mat[!is.na(landmask)]
	btl_mtaa_msk <- btl_mtaa[!is.na(landmask)]
	btl_mta_msk <- btl_mta[!is.na(landmask)]
	btl_ntw_msk <- btl_ntw[!is.na(landmask)]
	btl_nto_msk <- btl_nto[!is.na(landmask)]
	btl_ntj_msk <- btl_ntj[!is.na(landmask)]
	btl_ntm_msk <- btl_ntm[!is.na(landmask)]
	btl_xta_msk <- btl_xta[!is.na(landmask)]
	btl_map_msk <- btl_map[!is.na(landmask)]
	btl_cpja_msk <- btl_cpja[!is.na(landmask)]
	btl_pja_msk <- btl_pja[!is.na(landmask)]
	btl_cpos_msk <- btl_cpos[!is.na(landmask)]
	btl_pos_msk <- btl_pos[!is.na(landmask)]	
	btl_gsp_msk <- btl_gsp[!is.na(landmask)]
	btl_vgp_msk <- btl_vgp[!is.na(landmask)]

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
	
	vgt_matstd_msk <- vgt_matstd[!is.na(landmask)]
	vgt_mtaastd_msk <- vgt_mtaastd[!is.na(landmask)]
	vgt_mtastd_msk <- vgt_mtastd[!is.na(landmask)]
	vgt_ntwstd_msk <- vgt_ntwstd[!is.na(landmask)]
	vgt_ntostd_msk <- vgt_ntostd[!is.na(landmask)]
	vgt_ntjstd_msk <- vgt_ntjstd[!is.na(landmask)]
	vgt_ntmstd_msk <- vgt_ntmstd[!is.na(landmask)]
	vgt_xtastd_msk <- vgt_xtastd[!is.na(landmask)]
	vgt_mapstd_msk <- vgt_mapstd[!is.na(landmask)]
	vgt_cpjastd_msk <- vgt_cpjastd[!is.na(landmask)]
	vgt_pjastd_msk <- vgt_pjastd[!is.na(landmask)]
	vgt_cposstd_msk <- vgt_cposstd[!is.na(landmask)]
	vgt_posstd_msk <- vgt_posstd[!is.na(landmask)]
	vgt_gspstd_msk <- vgt_gspstd[!is.na(landmask)]
	vgt_vgpstd_msk <- vgt_vgpstd[!is.na(landmask)]
	
	btl_matstd_msk <- btl_matstd[!is.na(landmask)] 
	btl_mtaastd_msk <- btl_mtaastd[!is.na(landmask)] 
	btl_mtastd_msk <- btl_mtastd[!is.na(landmask)] 
	btl_ntwstd_msk <- btl_ntwstd[!is.na(landmask)] 
	btl_ntostd_msk <- btl_ntostd[!is.na(landmask)] 
	btl_ntjstd_msk <- btl_ntjstd[!is.na(landmask)] 
	btl_ntmstd_msk <- btl_ntmstd[!is.na(landmask)] 
	btl_xtastd_msk <- btl_xtastd[!is.na(landmask)] 
	btl_mapstd_msk <- btl_mapstd[!is.na(landmask)] 
	btl_cpjastd_msk <- btl_cpjastd[!is.na(landmask)] 
	btl_pjastd_msk <- btl_pjastd[!is.na(landmask)] 
	btl_cposstd_msk <- btl_cposstd[!is.na(landmask)] 
	btl_posstd_msk <- btl_posstd[!is.na(landmask)] 
	btl_gspstd_msk <- btl_gspstd[!is.na(landmask)] 
	btl_vgpstd_msk <- btl_vgpstd[!is.na(landmask)] 
	
	# get array data
	if (btlyr == 1){
		print(paste0("start to reshape 2d to 3d in year ", years[yr]))
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
		# reshape to 2d
		print(paste0("start to reshape 2d to 1d in year ", years[yr]))
		mat_msk <- cbind(mat_slice_msk, vgt_mat_msk, btl_mat_msk) 
		mtaa_msk <- cbind(mtaa_slice_msk, vgt_mtaa_msk, btl_mtaa_msk)
		mta_msk <- cbind(mta_slice_msk, vgt_mta_msk, btl_mta_msk)
		ntw_msk <- cbind(ntw_slice_msk, vgt_ntw_msk, btl_ntw_msk)
		nto_msk <- cbind(nto_slice_msk, vgt_nto_msk, btl_nto_msk)
		ntj_msk <- cbind(ntj_slice_msk, vgt_ntj_msk, btl_ntj_msk)
		ntm_msk <- cbind(ntm_slice_msk, vgt_ntm_msk, btl_ntm_msk) 
		xta_msk <- cbind(xta_slice_msk, vgt_xta_msk, btl_xta_msk)	
		matstd_msk <- cbind(matstd_slice_msk, vgt_matstd_msk, btl_matstd_msk)
		mtaastd_msk <- cbind(mtaastd_slice_msk, vgt_mtaastd_msk, btl_mtaastd_msk)
		mtastd_msk <- cbind(mtastd_slice_msk, vgt_mtastd_msk, btl_mtastd_msk)
		ntwstd_msk <- cbind(ntwstd_slice_msk, vgt_ntwstd_msk, btl_ntwstd_msk)
		ntostd_msk <- cbind(ntostd_slice_msk, vgt_ntostd_msk, btl_ntostd_msk)
		ntjstd_msk <- cbind(ntjstd_slice_msk, vgt_ntjstd_msk, btl_ntjstd_msk)
		ntmstd_msk <- cbind(ntmstd_slice_msk, vgt_ntmstd_msk, btl_ntmstd_msk)
		xtastd_msk <- cbind(xtastd_slice_msk, vgt_xtastd_msk, btl_xtastd_msk)
		map_msk <- cbind(map_slice_msk, vgt_map_msk, btl_map_msk)
		cpja_msk <- cbind(cpja_slice_msk, vgt_cpja_msk, btl_cpja_msk)
		pja_msk <- cbind(pja_slice_msk, vgt_pja_msk, btl_pja_msk)
		cpos_msk <- cbind(cpos_slice_msk, vgt_cpos_msk, btl_cpos_msk)
		pos_msk <- cbind(pos_slice_msk, vgt_pos_msk, btl_pos_msk)
		gsp_msk <- cbind(gsp_slice_msk, vgt_gsp_msk, btl_gsp_msk)
		vgp_msk <- cbind(vgp_slice_msk, vgt_vgp_msk, btl_vgp_msk)
		mapstd_msk <- cbind(mapstd_slice_msk, vgt_mapstd_msk, btl_mapstd_msk)
		cpjastd_msk <- cbind(cpjastd_slice_msk, vgt_cpjastd_msk, btl_cpjastd_msk)
		pjastd_msk <- cbind(pjastd_slice_msk, vgt_pjastd_msk, btl_pjastd_msk)
		cposstd_msk <- cbind(cposstd_slice_msk, vgt_cposstd_msk, btl_cposstd_msk)
		posstd_msk <- cbind(posstd_slice_msk, vgt_posstd_msk, btl_posstd_msk)
		gspstd_msk <- cbind(gspstd_slice_msk, vgt_gspstd_msk, btl_gspstd_msk)
		vgpstd_msk <- cbind(vgpstd_slice_msk, vgt_vgpstd_msk, btl_vgpstd_msk)
		
	} else{
		print(paste0("start to reshape 2d to 3d in year ", years[yr]))	
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
		
		print(paste0("start to reshape 2d to 1d in year ", years[yr]))
		mat_msk <- cbind(mat_msk, mat_slice_msk, vgt_mat_msk, btl_mat_msk) 
		mtaa_msk <- cbind(mtaa_msk, mtaa_slice_msk, vgt_mtaa_msk, btl_mtaa_msk)
		mta_msk <- cbind(mta_msk, mta_slice_msk, vgt_mta_msk, btl_mta_msk)
		ntw_msk <- cbind(ntw_msk, ntw_slice_msk, vgt_ntw_msk, btl_ntw_msk)
		nto_msk <- cbind(nto_msk, nto_slice_msk, vgt_nto_msk, btl_nto_msk)
		ntj_msk <- cbind(ntj_msk, ntj_slice_msk, vgt_ntj_msk, btl_ntj_msk)
		ntm_msk <- cbind(ntm_msk, ntm_slice_msk, vgt_ntm_msk, btl_ntm_msk) 
		xta_msk <- cbind(xta_msk, xta_slice_msk, vgt_xta_msk, btl_xta_msk)	
		matstd_msk <- cbind(matstd_msk, matstd_slice_msk, vgt_matstd_msk, btl_matstd_msk)
		mtaastd_msk <- cbind(mtaastd_msk, mtaastd_slice_msk, vgt_mtaastd_msk, btl_mtaastd_msk)
		mtastd_msk <- cbind(mtastd_msk, mtastd_slice_msk, vgt_mtastd_msk, btl_mtastd_msk)
		ntwstd_msk <- cbind(ntwstd_msk, ntwstd_slice_msk, vgt_ntwstd_msk, btl_ntwstd_msk)
		ntostd_msk <- cbind(ntostd_msk, ntostd_slice_msk, vgt_ntostd_msk, btl_ntostd_msk)
		ntjstd_msk <- cbind(ntjstd_msk, ntjstd_slice_msk, vgt_ntjstd_msk, btl_ntjstd_msk)
		ntmstd_msk <- cbind(ntmstd_msk, ntmstd_slice_msk, vgt_ntmstd_msk, btl_ntmstd_msk)
		xtastd_msk <- cbind(xtastd_msk, xtastd_slice_msk, vgt_xtastd_msk, btl_xtastd_msk)
		map_msk <- cbind(map_msk, map_slice_msk, vgt_map_msk, btl_map_msk)
		cpja_msk <- cbind(cpja_msk, cpja_slice_msk, vgt_cpja_msk, btl_cpja_msk)
		pja_msk <- cbind(pja_msk, pja_slice_msk, vgt_pja_msk, btl_pja_msk)
		cpos_msk <- cbind(cpos_msk, cpos_slice_msk, vgt_cpos_msk, btl_cpos_msk)
		pos_msk <- cbind(pos_msk, pos_slice_msk, vgt_pos_msk, btl_pos_msk)
		gsp_msk <- cbind(gsp_msk, gsp_slice_msk, vgt_gsp_msk, btl_gsp_msk)
		vgp_msk <- cbind(vgp_msk, vgp_slice_msk, vgt_vgp_msk, btl_vgp_msk)
		mapstd_msk <- cbind(mapstd_msk, mapstd_slice_msk, vgt_mapstd_msk, btl_mapstd_msk)
		cpjastd_msk <- cbind(cpjastd_msk, cpjastd_slice_msk, vgt_cpjastd_msk, btl_cpjastd_msk)
		pjastd_msk <- cbind(pjastd_msk, pjastd_slice_msk, vgt_pjastd_msk, btl_pjastd_msk)
		cposstd_msk <- cbind(cposstd_msk, cposstd_slice_msk, vgt_cposstd_msk, btl_cposstd_msk)
		posstd_msk <- cbind(posstd_msk, posstd_slice_msk, vgt_posstd_msk, btl_posstd_msk)
		gspstd_msk <- cbind(gspstd_msk, gspstd_slice_msk, vgt_gspstd_msk, btl_gspstd_msk)
		vgpstd_msk <- cbind(vgpstd_msk, vgpstd_slice_msk, vgt_vgpstd_msk, btl_vgpstd_msk)
		
	}	
	print(paste0(years[yr], " is done!"))
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
# quick maps to check data
n <- 15
ntm_slice_3d <- ntm_4d[,,2,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_ntm_tree_",end_year,".png",sep=""))
levelplot(ntm_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

# quick maps to check data
mapstd_slice_4d <- mapstd_4d[,,3,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-5,-4,-3,-2,-1,0,1,2,3,4,5)
options(bitmapType='cairo')
png(file=paste(out,"na10km_v2_mapstd_beetle_",end_year,".png",sep=""))
levelplot(mapstd_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

csvpath <- "/projects/bonelab/dongmeic/beetle/csvfiles/"
na10km_2d <- read.csv(paste0(csvpath, "na10km_v2.csv"))

if (all(sapply(list(mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
matstd_msk, mtaastd_msk, mtastd_msk, ntwstd_msk, ntostd_msk, ntjstd_msk, ntmstd_msk,
xtastd_msk, map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk, mapstd_msk,
cpjastd_msk, pjastd_msk, cposstd_msk, posstd_msk, gspstd_msk, vgpstd_msk), function(x) identical(dim(x)[1],dim(na10km_2d)[1])))){
	
	na10km_2d_n1 <- cbind(na10km_2d, mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
	map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk)
	
	na10km_2d_n2 <- cbind(na10km_2d, matstd_msk, mtaastd_msk, mtastd_msk, ntwstd_msk, ntostd_msk, ntjstd_msk, ntmstd_msk,
	xtastd_msk, mapstd_msk, cpjastd_msk, pjastd_msk, cposstd_msk, posstd_msk, gspstd_msk, vgpstd_msk)
 
	print("write csvfiles...")
	write.csv(na10km_2d_n1, paste0(csvpath,"climatic_variables_longlat_var_v2.csv"))
	write.csv(na10km_2d_n2, paste0(csvpath,"climatic_variables_longlat_std_v2.csv"))
	
} else{
	
	sapply(list(mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
	matstd_msk, mtaastd_msk, mtastd_msk, ntwstd_msk, ntostd_msk, ntjstd_msk, ntmstd_msk,
	xtastd_msk, map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk, mapstd_msk,
	cpjastd_msk, pjastd_msk, cposstd_msk, posstd_msk, gspstd_msk, vgpstd_msk), function(x) identical(dim(x)[1],dim(na10km_2d)[1]))

}

# print("try again...")
# print("convert the 3d array to 2d for climatic variables")
ptm <- proc.time()
for (yr in 1:112){
	# 1. annual mean temperature
	mat_slice <- apply(abind(tmp[,,10:12,(yr+1)],tmp[,,1:9,(yr+2)],along=3),c(1,2),mean)
	# matstd_slice <- (mat_slice - mat_ltm)/mat_std
	
	# 2. mean temperature between April and August (in the current year)
	mtaa_slice <- apply(abind(tmp[,,4:8,(yr+2)],along=3),c(1,2),mean)
	# mtaastd_slice <- (mtaa_slice - mtaa_ltm)/mtaa_std

	# 3. current year August mean temperature
	mta_slice <- tmp[,,8,(yr+2)]
	# mtastd_slice <- (mta_slice - mta_ltm)/mta_std

	# winter as December, January, February
	winter <- abind(tmn[,,12,(yr+1)],tmn[,,1:2,(yr+2)],along=3)
	# 4. winter monthly average daily minimum
	ntw_slice <- apply(winter,c(1,2),min)
	# ntwstd_slice <- (ntw_slice - ntw_ltm)/ntm_std

	# 5. October monthly average daily minimum
	nto_slice <- tmn[,,10,(yr+1)]
	# ntostd_slice <- (nto_slice - nto_ltm)/nto_std

	# 6. January monthly average daily minimum
	ntj_slice <- tmn[,,1,(yr+2)]
	# ntjstd_slice <- (ntj_slice - ntj_ltm)/ntj_std

	# 7. March monthly average daily minimum
	ntm_slice <- tmn[,,3,(yr+2)]
	# ntmstd_slice <- (ntm_slice - ntm_ltm)/ntm_std

	# 8. August monthly average daily maximum (in the current year)
	xta_slice <- tmx[,,8,(yr+2)]
	# xtastd_slice <- (xta_slice - xta_ltm)/xta_std

	# 9. mean annual precipitation
	map_slice <- apply(abind(pre[,,10:12,(yr+1)],pre[,,1:9,(yr+2)],along=3),c(1,2),mean)
	# mapstd_slice <- (map_slice - map_ltm)/map_std

	# 10. cumulative precipitation from June to August in the current and previous years
	cpja_slice <- apply(abind(pre[,,6:8,(yr+1):(yr+2)],along=3),c(1,2),sum)
	# cpjastd_slice <- (cpja_slice - cpja_ltm)/cpja_std

	# 11. precipitation from June to August in the previous year
	pja_slice <- apply(abind(pre[,,6:8,(yr+1)],along=3),c(1,2),sum)
	# pjastd_slice <- (pja_slice - pja_ltm)/pja_std

	# 12. cumulative precipitation from October to September in the current and previous years
	cpos_slice <- apply(abind(pre[,,10:12,yr:(yr+1)],pre[,,1:9,(yr+1):(yr+2)],along=3),c(1,2),sum)
	# cposstd_slice <- (cpos_slice - cpos_ltm)/cpos_std

	# 13. precipitation from October to September in the previous year
	pos_slice <- apply(abind(pre[,,10:12,yr],pre[,,1:9,(yr+1)],along=3),c(1,2),sum)
	# posstd_slice <- (pos_slice - pos_ltm)/pos_std

	# 14. total growing season precipitation
	gsp_slice <- apply(pre[,,4:6,(yr+2)],c(1,2),sum)
	# gspstd_slice <- (gsp_slice - gsp_ltm)/gsp_std

	# 15. variability of growing season precipitation
	vgp_slice <- apply(abind(pre[,,4:6,(yr+2)],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	# vgpstd_slice <- (vgp_slice - vgp_ltm)/vgp_std
	
	# mask the points in land
	mat_slice_msk <- mat_slice[!is.na(landmask)]
	mtaa_slice_msk <- mtaa_slice[!is.na(landmask)] 
	mta_slice_msk <- mta_slice[!is.na(landmask)]
	ntw_slice_msk <- ntw_slice[!is.na(landmask)] 
	nto_slice_msk <- nto_slice[!is.na(landmask)] 
	ntj_slice_msk <- ntj_slice[!is.na(landmask)] 
	ntm_slice_msk <- ntm_slice[!is.na(landmask)] 
	xta_slice_msk <- xta_slice[!is.na(landmask)] 
	map_slice_msk <- map_slice[!is.na(landmask)] 
	cpja_slice_msk <- cpja_slice[!is.na(landmask)] 
	pja_slice_msk <- pja_slice[!is.na(landmask)]  
	cpos_slice_msk <- cpos_slice[!is.na(landmask)] 
	pos_slice_msk <- pos_slice[!is.na(landmask)] 
	gsp_slice_msk <- gsp_slice[!is.na(landmask)] 
	vgp_slice_msk <- vgp_slice[!is.na(landmask)] 
	
	# get array data
	if (yr == 1){
		# reshape to 2d
		print(paste0("start to reshape 2d to 1d in year ", years[yr]))
		mat_msk <- cbind(mat_slice_msk) 
		mtaa_msk <- cbind(mtaa_slice_msk)
		mta_msk <- cbind(mta_slice_msk)
		ntw_msk <- cbind(ntw_slice_msk)
		nto_msk <- cbind(nto_slice_msk)
		ntj_msk <- cbind(ntj_slice_msk)
		ntm_msk <- cbind(ntm_slice_msk) 
		xta_msk <- cbind(xta_slice_msk)	
		map_msk <- cbind(map_slice_msk)
		cpja_msk <- cbind(cpja_slice_msk)
		pja_msk <- cbind(pja_slice_msk)
		cpos_msk <- cbind(cpos_slice_msk)
		pos_msk <- cbind(pos_slice_msk)
		gsp_msk <- cbind(gsp_slice_msk)
		vgp_msk <- cbind(vgp_slice_msk)
		mapstd_msk <- cbind(mapstd_slice_msk)
		cpjastd_msk <- cbind(cpjastd_slice_msk)
		pjastd_msk <- cbind(pjastd_slice_msk)
		cposstd_msk <- cbind(cposstd_slice_msk)
		posstd_msk <- cbind(posstd_slice_msk)
		gspstd_msk <- cbind(gspstd_slice_msk)
		vgpstd_msk <- cbind(vgpstd_slice_msk)
		
	} else{
		print(paste0("start to reshape 2d to 1d in year ", years[yr]))
		mat_msk <- cbind(mat_msk, mat_slice_msk) 
		mtaa_msk <- cbind(mtaa_msk, mtaa_slice_msk)
		mta_msk <- cbind(mta_msk, mta_slice_msk)
		ntw_msk <- cbind(ntw_msk, ntw_slice_msk)
		nto_msk <- cbind(nto_msk, nto_slice_msk)
		ntj_msk <- cbind(ntj_msk, ntj_slice_msk)
		ntm_msk <- cbind(ntm_msk, ntm_slice_msk) 
		xta_msk <- cbind(xta_msk, xta_slice_msk)	
		map_msk <- cbind(map_msk, map_slice_msk)
		cpja_msk <- cbind(cpja_msk, cpja_slice_msk)
		pja_msk <- cbind(pja_msk, pja_slice_msk)
		cpos_msk <- cbind(cpos_msk, cpos_slice_msk)
		pos_msk <- cbind(pos_msk, pos_slice_msk)
		gsp_msk <- cbind(gsp_msk, gsp_slice_msk)
		vgp_msk <- cbind(vgp_msk, vgp_slice_msk)		
	}	
	print(paste0(years[yr], " is done!"))
}
proc.time() - ptm

if (all(sapply(list(mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk), function(x) identical(dim(x)[1],dim(na10km_2d)[1])))){
	
	na10km_2d_n3 <- cbind(na10km_2d, mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
	map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk)

	print("write csvfiles...")
	write.csv(na10km_2d_n3, paste0(csvpath,"climatic_variables_longlat_var_longterm.csv"))
	
} else{
	
	sapply(list(mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
	map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk), function(x) identical(dim(x)[1],dim(na10km_2d)[1]))

}
print("all done!")