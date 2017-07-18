# Created by Dongmei Chen based on exploratory_plots_temporal.R
# Exploratory data analysis: generate a datafile

library(ncdf4)
install.packages("abind", repos='http://cran.us.r-project.org')
library(abind)
library(lattice)
library(RColorBrewer)

path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/"

year1 <- 1901; year2 <- 1961; year3 <- 1998; year4 <- 1999; year5 <- 2014
nyrs.t <- year5-year4+1;
nyrs.p <- year5-year3+1;

# subscripts of year 1961 and year 1999
n1 <- year2-year1+1; # long-term (1961-1990) 30 years
n2 <- year3-year1+1;
n3 <- year4-year1+1

## 1: annual mean monthly average of daily mean temperature in the past year - mat
## 2: mean of monthly average of daily mean temperature from April to August - mtaa
## 3: monthly average of daily mean temperature in August - mta
# near surface air temperature - tmp
ncfile_tmp <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs4d.nc"
ncin_tmp <- nc_open(paste(path, ncfile_tmp, sep=""))
print(ncin_tmp)
dname <- "tmp"
tmp_ltm <- ncvar_get(ncin_tmp,dname,start=c(x=1,y=1,month=1,year=n1),count=c(x=1078, y=900, month=12, year=30))
dim(tmp_ltm)
tmp <- ncvar_get(ncin_tmp,dname,start=c(x=1,y=1,month=1,year=n3),count=c(x=1078, y=900, month=12, year=nyrs.t))
dim(tmp)
fillvalue <- ncatt_get(ncin_tmp,dname,"_FillValue")
# get x, y for mapping; same coordinates for precipitation data
x <- ncvar_get(ncin_tmp, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_tmp, varid="y"); ny <- length(y)
nc_close(ncin_tmp)
tmp[tmp==fillvalue$value] <- NA
tmp_ltm[tmp_ltm==fillvalue$value] <- NA

## 4: minimum of monthly average of daily minimum temperature between Dec and Feb - ntw
## 5: monthly average of daily minimum temperature in October - nto
## 6: monthly average of daily minimum temperature in January - ntj
## 7: monthly average of daily minimum temperature in March - ntm
# near surface air temperature minimum - tmn
ncfile_tmn <- "na10km_v2_cru_ts4.00.1999.2014.tmn.4d.nc"
ncfile_tmn_ltm <- "na10km_v2_cru_ts4.00.1961.1990.tmn.4d.nc"
ncin_tmn <- nc_open(paste(path, ncfile_tmn, sep=""))
ncin_tmn_ltm <- nc_open(paste(path, ncfile_tmn_ltm, sep=""))
print(ncin_tmn)
print(ncin_tmn_ltm)
dname <- "tmn"
tmn_ltm <- ncvar_get(ncin_tmn_ltm,dname)
dim(tmn_ltm)
tmn <- ncvar_get(ncin_tmn,dname)
dim(tmn)
nc_close(ncin_tmn)
tmn[tmn==fillvalue$value] <- NA
tmn_ltm[tmn_ltm==fillvalue$value] <- NA

# quick map to check data
m<- 12; n <- 30
tmn_ltm_3d <- tmn_ltm[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.1990.tmn.png",sep=""))
levelplot(tmn_ltm_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

## 8: monthly average of daily maximum temperature in August - xta
# near surface air temperature maximum - tmx
ncfile_tmx <- "na10km_v2_cru_ts4.00.2000.2014.tmx.4d.nc"
ncfile_tmx_ltm <- "na10km_v2_cru_ts4.00.1961.1990.tmx.4d.nc"
ncin_tmx <- nc_open(paste(path, ncfile_tmx, sep=""))
ncin_tmx_ltm <- nc_open(paste(path, ncfile_tmx_ltm, sep=""))
print(ncin_tmx)
dname <- "tmx"
tmx_ltm <- ncvar_get(ncin_tmx_ltm,dname)
dim(tmx_ltm)
tmx <- ncvar_get(ncin_tmx,dname)
dim(tmx)
nc_close(ncin_tmx)
tmx[tmx==fillvalue$value] <- NA
tmx_ltm[tmx_ltm==fillvalue$value] <- NA

## 9: mean annual precipitation - map; use precipitation data from January to December in the past five years
## 10: cumulative precipitation from June to August in current and previous year - cpja; use precipitation data in the previous and current year
## 11: precipitation from June to August in previous year - pja;
## 12: cumulative precipitation from October to September in current and previous year - cpos;
## 13: precipitation from October to September in previous year - pos;
## 14: growing season precipitation in current year - gsp;
## 15: variability of growing season precipitation - vgp
ncfile_pre <- "na10km_v2_cru_ts4.00.1901.2015.pre.abs4d.nc"
ncin_pre <- nc_open(paste(path, ncfile_pre, sep=""))
print(ncin_pre)
dname <- "pre"
pre_ltm <- ncvar_get(ncin_pre,dname,start=c(x=1,y=1,month=1,year=n1),count=c(x=1078,y=900, month=12, year=30))
dim(pre_ltm)
pre <- ncvar_get(ncin_pre,dname,start=c(x=1,y=1,month=1,year=n2),count=c(x=1078,y=900, month=12, year=nyrs.p))
dim(pre)
nc_close(ncin_pre)
pre[pre==fillvalue$value] <- NA
pre_ltm[pre_ltm==fillvalue$value] <- NA

years = 2000:2014; nyr <- length(years)
# get long-term means
# define array for long-term means
mat <- array(NA, dim=c(nx,ny,29))
mtaa <- array(NA, dim=c(nx,ny,30))
mta <- array(NA, dim=c(nx,ny,30))
ntw <- array(NA, dim=c(nx,ny,29))
nto <- array(NA, dim=c(nx,ny,30))
ntj <- array(NA, dim=c(nx,ny,30))
ntm <- array(NA, dim=c(nx,ny,30))
xta <- array(NA, dim=c(nx,ny,30))
map <- array(NA, dim=c(nx,ny,29))
cpja <- array(NA, dim=c(nx,ny,29))
pja <- array(NA, dim=c(nx,ny,30))
cpos <- array(NA, dim=c(nx,ny,28))
pos <- array(NA, dim=c(nx,ny,30))
gsp <- array(NA, dim=c(nx,ny,30))
vgp <- array(NA, dim=c(nx,ny,30))

# long-term means
ptm <- proc.time()
for (k in 1:30){
	if (k > 1 && k < 30){
		cpos[,,(k-1)] <- apply(abind(pre_ltm[,,10:12,(k-1):k],pre_ltm[,,1:9,k:(k+1)],along=3),c(1,2),sum)
	} 
	if (k > 1){
		mat[,,(k-1)] <- apply(abind(tmp_ltm[,,10:12,(k-1)],tmp_ltm[,,1:9,k],along=3),c(1,2),mean)
		map[,,(k-1)] <- apply(abind(pre_ltm[,,10:12,(k-1)],pre_ltm[,,1:9,k],along=3),c(1,2),mean)
		cpja[,,(k-1)] <- apply(abind(pre_ltm[,,6:8,(k-1):k],along=3),c(1,2),sum)
		pos[,,(k-1)] <- apply(abind(pre_ltm[,,10:12,(k-1)],pre_ltm[,,1:9,k],along=3),c(1,2),sum)		
	}
	if (k < 30){
		ntw[,,k] <- apply(abind(tmn_ltm[,,12,k],tmn_ltm[,,1:2,(k+1)],along=3),c(1,2),min)
	}
	mtaa[,,k] <- apply(abind(tmp_ltm[,,4:8,k], along=3),c(1,2),mean)
	mta[,,k] <- tmp_ltm[,,8,k]
	nto[,,k] <- tmn_ltm[,,10,k]
	ntj[,,k] <- tmn_ltm[,,1,k]
	ntm[,,k] <- tmn_ltm[,,3,k]
	xta[,,k] <- tmx_ltm[,,8,k]
	pja[,,k] <- apply(abind(pre_ltm[,,6:8,k],along=3),c(1,2),sum)
	gsp[,,k] <- apply(abind(pre_ltm[,,4:6,k],along=3),c(1,2),sum)
	vgp[,,k] <- apply(abind(pre_ltm[,,4:6,k],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	print(k)
}

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

## read vegetation and bettle presence data
prs_path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/prs/"
vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste(prs_path,vgt_ncfile,sep=""))
print(ncin_vgt)
vgt <- ncvar_get(ncin_vgt,"navgtprs")
btl_ncfile <- "na10km_v2_mpb_presence.nc"
ncin_btl <- nc_open(paste(prs_path,btl_ncfile, sep=""))
print(ncin_btl)
btl <- ncvar_get(ncin_btl,"mpb_prs")

ptm <- proc.time()
df.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.tstd <- data.frame(matstd=double(),mtaastd=double(),mtastd=double(),ntwstd=double(),ntostd=double(),ntjstd=double(),ntmstd=double(),xtastd=double(),prs=factor(),yr=factor())
df.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.pstd <- data.frame(mapstd=double(),cpjastd=double(),pjastd=double(),cposstd=double(),posstd=double(),gspstd=double(),vgpstd=double(),prs=factor(),yr=factor())

for (yr in 1:nyr){
	# 1. annual mean temperature
	mat_slice <- apply(abind(tmp[,,10:12,yr],tmp[,,1:9,(yr+1)],along=3),c(1,2),mean)
	matstd_slice <- (mat_slice - mat_ltm)/mat_std
	clm_mat <- mat_slice[!is.na(mat_slice)]
	clm_matstd <- matstd_slice[!is.na(matstd_slice)]
	# 2. mean temperature between April and August
	mtaa_slice <- apply(abind(tmp[,,4:8,(yr+1)],along=3),c(1,2),mean)
	mtaastd_slice <- (mtaa_slice - mtaa_ltm)/mtaa_std
	clm_mtaa <- mtaa_slice[!is.na(mtaa_slice)]
	clm_mtaastd <- mtaastd_slice[!is.na(mtaastd_slice)]
	# 3. current year August mean temperature
	mta_slice <- tmp[,,8,(yr+1)]
	mtastd_slice <- (mta_slice - mta_ltm)/mta_std
	clm_mta <- mta_slice[!is.na(mta_slice)]
	clm_mtastd <- mtastd_slice[!is.na(mtastd_slice)]
	# winter as December, January, February
	winter <- abind(tmn[,,12,yr],tmn[,,1:2,(yr+1)],along=3)
	# 4. winter monthly average daily minimum
	ntw_slice <- apply(winter,c(1,2),min)
	ntwstd_slice <- (ntw_slice - ntw_ltm)/ntm_std
	clm_ntw <- ntw_slice[!is.na(ntw_slice)]
	clm_ntwstd <- ntwstd_slice[!is.na(ntwstd_slice)]
	# 5. October monthly average daily minimum
	nto_slice <- tmn[,,10,yr]
	ntostd_slice <- (nto_slice - nto_ltm)/nto_std
	clm_nto <- nto_slice[!is.na(nto_slice)]
	clm_ntostd <- ntostd_slice[!is.na(ntostd_slice)]
	# 6. January monthly average daily minimum
	ntj_slice <- tmn[,,1,(yr+1)]
	ntjstd_slice <- (ntj_slice - ntj_ltm)/ntj_std
	clm_ntj <- ntj_slice[!is.na(ntj_slice)]
	clm_ntjstd <- ntjstd_slice[!is.na(ntjstd_slice)]
	# 7. March monthly average daily minimum
	ntm_slice <- tmn[,,3,(yr+1)]
	ntmstd_slice <- (ntm_slice - ntm_ltm)/ntm_std
	clm_ntm <- ntm_slice[!is.na(ntm_slice)]
	clm_ntmstd <- ntmstd_slice[!is.na(ntmstd_slice)]
	# 8. August monthly average daily maximum
	tmx_slice <- tmx[,,8,yr]
	xtastd_slice <- (tmx_slice - xta_ltm)/xta_std
	clm_xta <- tmx_slice[!is.na(tmx_slice)]
	clm_xtastd <- xtastd_slice[!is.na(xtastd_slice)]
	# 9. mean annual precipitation
	map_slice <- apply(abind(pre[,,10:12,yr],pre[,,1:9,(yr+1)],along=3),c(1,2),mean)
	mapstd_slice <- (map_slice - map_ltm)/map_std
	clm_map <- map_slice[!is.na(map_slice)]
	clm_mapstd <- mapstd_slice[!is.na(mapstd_slice)]
	# 10. cumulative precipitation from June to August in current and previous years
	cpja_slice <- apply(abind(pre[,,6:8,yr:(yr+1)],along=3),c(1,2),sum)
	cpjastd_slice <- (cpja_slice - cpja_ltm)/cpja_std
	clm_cpja <- cpja_slice[!is.na(cpja_slice)]
	clm_cpjastd <- cpjastd_slice[!is.na(cpjastd_slice)]
	# 11. precipitation from June to August in previous year
	pja_slice <- apply(abind(pre[,,6:8,yr],along=3),c(1,2),sum)
	pjastd_slice <- (pja_slice - pja_ltm)/pja_std
	clm_pja <- pja_slice[!is.na(pja_slice)]
	clm_pjastd <- pjastd_slice[!is.na(pjastd_slice)]
	# 12. cumulative precipitation from October to September in current and previous years
	cpos_slice <- apply(abind(pre[,,10:12,(yr-1):yr],pre[,,1:9,yr:(yr+1)],along=3),c(1,2),sum)
	cposstd_slice <- (cpos_slice - cpos_ltm)/cpos_std
	clm_cpos <- cpos_slice[!is.na(cpos_slice)]
	clm_cposstd <- cposstd_slice[!is.na(cposstd_slice)]	
	# 13. precipitation from October to September in previous year
	pos_slice <- apply(abind(pre[,,10:12,(yr-1)],pre[,,1:9,yr],along=3),c(1,2),sum)
	posstd_slice <- (pos_slice - pos_ltm)/pos_std
	clm_pos <- pos_slice[!is.na(pos_slice)]
	clm_posstd <- posstd_slice[!is.na(posstd_slice)]	
	# 14. total growing season precipitation
	gsp_slice <- apply(pre[,,4:6,(yr+1)],c(1,2),sum)
	gspstd_slice <- (gsp_slice - gsp_ltm)/gsp_std
	clm_gsp <- gsp_slice[!is.na(gsp_slice)]
	clm_gspstd <- gspstd_slice[!is.na(gspstd_slice)]
	# 15. variability of growing season precipitation
	vgp_slice <- apply(abind(pre[,,4:6,(yr+1)],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
	vgpstd_slice <- (vgp_slice - vgp_ltm)/vgp_std
	clm_vgp <- vgp_slice[!is.na(vgp_slice)]
	clm_vgpstd <- vgpstd_slice[!is.na(vgpstd_slice)]
	
	# get climate data with the presence of vegetation
	vgt_mat <- mat_slice[which(vgt==1)]
	vgt_mtaa <- mtaa_slice[which(vgt==1)]
	vgt_mta <- mta_slice[which(vgt==1)]
	vgt_ntw <- ntw_slice[which(vgt==1)]
	vgt_nto <- nto_slice[which(vgt==1)]
	vgt_ntj <- ntj_slice[which(vgt==1)]
	vgt_ntm <- ntm_slice[which(vgt==1)]
	vgt_xta <- tmx_slice[which(vgt==1)]
	vgt_map <- map_slice[which(vgt==1)]
	vgt_cpja <- cpja_slice[which(vgt==1)]
	vgt_pja <- pja_slice[which(vgt==1)]
	vgt_cpos <- cpos_slice[which(vgt==1)]
	vgt_pos <- pos_slice[which(vgt==1)]	
	vgt_gsp <- gsp_slice[which(vgt==1)]
	vgt_vgp <- vgp_slice[which(vgt==1)]
	
	# get climate data with the presence of all mpb
	btl_slice <- btl[,,yr]
	btl_mat <- mat_slice[which(btl_slice==1)]
	btl_mtaa <- mtaa_slice[which(btl_slice==1)]
	btl_mta <- mta_slice[which(btl_slice==1)]
	btl_ntw <- ntw_slice[which(btl_slice==1)]
	btl_nto <- nto_slice[which(btl_slice==1)]
	btl_ntj <- ntj_slice[which(btl_slice==1)]
	btl_ntm <- ntm_slice[which(btl_slice==1)]
	btl_xta <- tmx_slice[which(btl_slice==1)]
	btl_map <- map_slice[which(btl_slice==1)]
	btl_cpja <- cpja_slice[which(btl_slice==1)]
	btl_pja <- pja_slice[which(btl_slice==1)]
	btl_cpos <- cpos_slice[which(btl_slice==1)]
	btl_pos <- pos_slice[which(btl_slice==1)]	
	btl_gsp <- gsp_slice[which(btl_slice==1)]
	btl_vgp <- vgp_slice[which(btl_slice==1)]

	# get standard deviation
	vgt_matstd <- matstd_slice[which(vgt==1)]
	vgt_mtaastd <- mtaastd_slice[which(vgt==1)]
	vgt_mtastd <- mtastd_slice[which(vgt==1)]
	vgt_ntwstd <- ntwstd_slice[which(vgt==1)]
	vgt_ntostd <- ntostd_slice[which(vgt==1)]
	vgt_ntjstd <- ntjstd_slice[which(vgt==1)]
	vgt_ntmstd <- ntmstd_slice[which(vgt==1)]
	vgt_xtastd <- xtastd_slice[which(vgt==1)]
	vgt_mapstd <- mapstd_slice[which(vgt==1)]
	vgt_cpjastd <- cpjastd_slice[which(vgt==1)]
	vgt_pjastd <- pjastd_slice[which(vgt==1)]
	vgt_cposstd <- cposstd_slice[which(vgt==1)]
	vgt_posstd <- posstd_slice[which(vgt==1)]
	vgt_gspstd <- gspstd_slice[which(vgt==1)]
	vgt_vgpstd <- vgpstd_slice[which(vgt==1)]
	
	btl_matstd <- matstd_slice[which(btl_slice==1)]
	btl_mtaastd <- mtaastd_slice[which(btl_slice==1)]
	btl_mtastd <- mtastd_slice[which(btl_slice==1)]
	btl_ntwstd <- ntwstd_slice[which(btl_slice==1)]
	btl_ntostd <- ntostd_slice[which(btl_slice==1)]
	btl_ntjstd <- ntjstd_slice[which(btl_slice==1)]
	btl_ntmstd <- ntmstd_slice[which(btl_slice==1)]
	btl_xtastd <- xtastd_slice[which(btl_slice==1)]
	btl_mapstd <- mapstd_slice[which(btl_slice==1)]
	btl_cpjastd <- cpjastd_slice[which(btl_slice==1)]
	btl_pjastd <- pjastd_slice[which(btl_slice==1)]
	btl_cposstd <- cposstd_slice[which(btl_slice==1)]
	btl_posstd <- posstd_slice[which(btl_slice==1)]
	btl_gspstd <- gspstd_slice[which(btl_slice==1)]
	btl_vgpstd <- vgpstd_slice[which(btl_slice==1)]
	
	# data frame
	df_mat <- c(clm_mat,vgt_mat,btl_mat)
	df_matstd <- c(clm_matstd,vgt_matstd,btl_matstd)
	df_mtaa <- c(clm_mtaa,vgt_mtaa,btl_mtaa)
	df_mtaastd <- c(clm_mtaastd,vgt_mtaastd,btl_mtaastd)
	df_mta <- c(clm_mta,vgt_mta,btl_mta)
	df_mtastd <- c(clm_mtastd,vgt_mtastd,btl_mtastd)
	df_ntw <- c(clm_ntw,vgt_ntw,btl_ntw)
	df_ntwstd <- c(clm_ntwstd,vgt_ntwstd,btl_ntwstd)
	df_nto <- c(clm_nto,vgt_nto,btl_nto)
	df_ntostd <- c(clm_ntostd,vgt_ntostd,btl_ntostd)
	df_ntj <- c(clm_ntj,vgt_ntj,btl_ntj)
	df_ntjstd <- c(clm_ntjstd,vgt_ntjstd,btl_ntjstd)
	df_ntm <- c(clm_ntm,vgt_ntm,btl_ntm)
	df_ntmstd <- c(clm_ntmstd,vgt_ntmstd,btl_ntmstd)
	df_xta <- c(clm_xta,vgt_xta,btl_xta)
	df_xtastd <- c(clm_xtastd,vgt_xtastd,btl_xtastd)
	df_map <- c(clm_map,vgt_map,btl_map)
	df_mapstd <- c(clm_mapstd,vgt_mapstd,btl_mapstd)
	df_cpja <- c(clm_cpja,vgt_cpja,btl_cpja)
	df_cpjastd <- c(clm_cpjastd,vgt_cpjastd,btl_cpjastd)
	df_pja <- c(clm_pja,vgt_pja,btl_pja)
	df_pjastd <- c(clm_pjastd,vgt_pjastd,btl_pjastd)
	df_cpos <- c(clm_cpos,vgt_cpos,btl_cpos)
	df_cposstd <- c(clm_cposstd,vgt_cposstd,btl_cposstd)
	df_pos <- c(clm_pos,vgt_pos,btl_pos)
	df_posstd <- c(clm_posstd,vgt_posstd,btl_posstd)
	df_gsp <- c(clm_gsp,vgt_gsp,btl_gsp)
	df_gspstd <- c(clm_gspstd,vgt_gspstd,btl_gspstd)
	df_vgp <- c(clm_vgp,vgt_vgp,btl_vgp)
	df_vgpstd <- c(clm_vgpstd,vgt_vgpstd,btl_vgpstd)
	df_prs <- c(rep("climate",length(clm_mat)),rep("hosts",length(vgt_mat)),rep("mpb",length(btl_mat)))
	len <- length(df_mat)
	df_all.tmp <- data.frame(df_mat,df_mtaa,df_mta,df_ntw,df_nto,df_ntj,df_ntm,df_xta,df_prs,df_yr=rep(toString(years[yr]),length(df_prs)))
	df_all.tstd <- data.frame(df_matstd,df_mtaastd,df_mtastd,df_ntwstd,df_ntostd,df_ntjstd,df_ntmstd,df_xtastd,df_prs,df_yr=rep(toString(years[yr]),length(df_prs)))
	df_all.pre <- data.frame(df_map,df_cpja,df_pja,df_cpos,df_pos,df_gsp,df_vgp=c(df_vgp,rep(NA,(len-length(df_vgp)))),df_prs,df_yr=rep(toString(years[yr]),length(df_prs)))
	df_all.pstd <- data.frame(df_mapstd=c(df_mapstd,rep(NA,(len-length(df_mapstd)))),df_cpjastd=c(df_cpjastd,rep(NA,(len-length(df_cpjastd)))),df_pjastd=c(df_pjastd,rep(NA,(len-length(df_pjastd)))),df_cposstd=c(df_cposstd,rep(NA,(len-length(df_cposstd)))),df_posstd=c(df_posstd,rep(NA,(len-length(df_posstd)))),df_gspstd=c(df_gspstd,rep(NA,(len-length(df_gspstd)))),df_vgpstd=c(df_vgpstd,rep(NA,(len-length(df_vgpstd)))),df_prs,df_yr=rep(toString(years[yr]),length(df_prs)))
	df.tmp <- rbind(df.tmp, df_all.tmp)
	df.tstd <- rbind(df.tstd, df_all.tstd)
	df.pre <- rbind(df.pre, df_all.pre)
	df.pstd <- rbind(df.pstd, df_all.pstd)
	print(years[yr])
}
proc.time() - ptm

write.csv(df.tmp, paste0(out, "climatic_changes_data_tmp_1.csv"))
write.csv(df.tstd, paste0(out, "climatic_changes_data_tstd_1.csv"))
write.csv(df.pre, paste0(out, "climatic_changes_data_pre_1.csv"))
write.csv(df.pstd, paste0(out, "climatic_changes_data_pstd_1.csv"))