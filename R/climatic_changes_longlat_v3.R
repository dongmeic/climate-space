# Created by Dongmei Chen
# To generate yearly climatic data with coordinates including NA land and vegetation
# Based on climatic_variable_ncfiles.R and climatic_variable_ncfiles_v2.R

print("load libraries...")
library(ncdf4)
library(abind)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=55)

path <- "/projects/bonelab/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/projects/bonelab/dongmeic/sdm/data/cluster/historic/"

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
ptm <- proc.time()
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

years = 1903:2014;

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

csvpath <- "/projects/bonelab/dongmeic/beetle/csvfiles/"
na10km_2d <- read.csv(paste0(csvpath, "na10km_v2.csv"))

print("starting a loop to get 2d climatic variables")
proc.time() - ptm

#ptm1 <- proc.time()
foreach(yr=1:112) %dopar%{
	ptm <- proc.time()
	# 1. annual mean temperature
	mat_slice <- apply(abind(tmp[,,10:12,(yr+1)],tmp[,,1:9,(yr+2)],along=3),c(1,2),mean)
	
	# 2. mean temperature between April and August (in the current year)
	mtaa_slice <- apply(abind(tmp[,,4:8,(yr+2)],along=3),c(1,2),mean)

	# 3. current year August mean temperature
	mta_slice <- tmp[,,8,(yr+2)]

	# winter as December, January, February
	winter <- abind(tmn[,,12,(yr+1)],tmn[,,1:2,(yr+2)],along=3)
	# 4. winter monthly average daily minimum
	ntw_slice <- apply(winter,c(1,2),min)

	# 5. October monthly average daily minimum
	nto_slice <- tmn[,,10,(yr+1)]

	# 6. January monthly average daily minimum
	ntj_slice <- tmn[,,1,(yr+2)]

	# 7. March monthly average daily minimum
	ntm_slice <- tmn[,,3,(yr+2)]

	# 8. August monthly average daily maximum (in the current year)
	xta_slice <- tmx[,,8,(yr+2)]

	# 9. mean annual precipitation
	map_slice <- apply(abind(pre[,,10:12,(yr+1)],pre[,,1:9,(yr+2)],along=3),c(1,2),mean)

	# 10. cumulative precipitation from June to August in the current and previous years
	cpja_slice <- apply(abind(pre[,,6:8,(yr+1):(yr+2)],along=3),c(1,2),sum)

	# 11. precipitation from June to August in the previous year
	pja_slice <- apply(abind(pre[,,6:8,(yr+1)],along=3),c(1,2),sum)

	# 12. cumulative precipitation from October to September in the current and previous years
	cpos_slice <- apply(abind(pre[,,10:12,yr:(yr+1)],pre[,,1:9,(yr+1):(yr+2)],along=3),c(1,2),sum)

	# 13. precipitation from October to September in the previous year
	pos_slice <- apply(abind(pre[,,10:12,yr],pre[,,1:9,(yr+1)],along=3),c(1,2),sum)

	# 14. total growing season precipitation
	gsp_slice <- apply(pre[,,4:6,(yr+2)],c(1,2),sum)

	# 15. variability of growing season precipitation
	vgp_slice <- apply(abind(pre[,,4:6,(yr+2)],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	
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
	
	# reshape to 2d
	print(paste0("start to reshape 2d to 1d in year ", years[yr]))
	mat_msk <- cbind(mat_slice_msk, vgt_mat_msk) 
	mtaa_msk <- cbind(mtaa_slice_msk, vgt_mtaa_msk)
	mta_msk <- cbind(mta_slice_msk, vgt_mta_msk)
	ntw_msk <- cbind(ntw_slice_msk, vgt_ntw_msk)
	nto_msk <- cbind(nto_slice_msk, vgt_nto_msk)
	ntj_msk <- cbind(ntj_slice_msk, vgt_ntj_msk)
	ntm_msk <- cbind(ntm_slice_msk, vgt_ntm_msk) 
	xta_msk <- cbind(xta_slice_msk, vgt_xta_msk)	

	map_msk <- cbind(map_slice_msk, vgt_map_msk)
	cpja_msk <- cbind(cpja_slice_msk, vgt_cpja_msk)
	pja_msk <- cbind(pja_slice_msk, vgt_pja_msk)
	cpos_msk <- cbind(cpos_slice_msk, vgt_cpos_msk)
	pos_msk <- cbind(pos_slice_msk, vgt_pos_msk)
	gsp_msk <- cbind(gsp_slice_msk, vgt_gsp_msk)
	vgp_msk <- cbind(vgp_slice_msk, vgt_vgp_msk)
	
	if (all(sapply(list(mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
	map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk), function(x) identical(dim(x)[1],dim(na10km_2d)[1])))){
	
		na10km_2d_n3 <- cbind(na10km_2d, mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
		map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk)

		print(paste0("write csvfiles in year ", years[yr]))
		write.csv(na10km_2d_n3, paste0(csvpath,"climatic_variables_longlat_var_",years[yr],".csv"))
		write.csv(na10km_2d_n3, paste0(out,"climatic_variables_longlat_var_",years[yr],".csv"))
	
	} else{
	
		sapply(list(mat_msk, mtaa_msk, mta_msk, ntw_msk, nto_msk, ntj_msk, ntm_msk, xta_msk,
		map_msk, cpja_msk, pja_msk, cpos_msk, pos_msk, gsp_msk, vgp_msk), function(x) identical(dim(x)[1],dim(na10km_2d)[1]))

	}

	print(paste0(years[yr], " is done!"))
	proc.time() - ptm
}
#print(paste0("total time for the loop is ", proc.time() - ptm1))

print("all done!")