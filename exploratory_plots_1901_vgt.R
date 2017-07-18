# Created by Dongmei Chen
# Exploratory data analysis: check long-term climate data

library(ncdf4)
library(abind)

path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/"

ncfile_tmp <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs4d.nc"
ncin_tmp <- nc_open(paste(path, ncfile_tmp, sep=""))
print(ncin_tmp)
dname <- "tmp"
tmp <- ncvar_get(ncin_tmp,dname)
dim(tmp)
fillvalue <- ncatt_get(ncin_tmp,dname,"_FillValue")
# get x, y for mapping; same coordinates for precipitation data
x <- ncvar_get(ncin_tmp, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_tmp, varid="y"); ny <- length(y)
nc_close(ncin_tmp)
tmp[tmp==fillvalue$value] <- NA

ncfile_pre <- "na10km_v2_cru_ts4.00.1901.2015.pre.abs4d.nc"
ncin_pre <- nc_open(paste(path, ncfile_pre, sep=""))
print(ncin_pre)
dname <- "pre"
pre <- ncvar_get(ncin_pre,dname)
dim(pre)
nc_close(ncin_pre)
pre[pre==fillvalue$value] <- NA

ncfile_tmn <- "na10km_v2_cru_ts4.00.1901.2015.tmn.4d.nc"
ncin_tmn <- nc_open(paste(path, ncfile_tmn, sep=""))
print(ncin_tmn)
dname <- "tmn"
tmn <- ncvar_get(ncin_tmn,dname)
dim(tmn)
nc_close(ncin_tmn)
tmn[tmn==fillvalue$value] <- NA

ncfile_tmx <- "na10km_v2_cru_ts4.00.1901.2015.tmx.4d.nc"
ncin_tmx <- nc_open(paste(path, ncfile_tmx, sep=""))
print(ncin_tmx)
dname <- "tmx"
tmx <- ncvar_get(ncin_tmx,dname)
dim(tmx)
nc_close(ncin_tmx)
tmx[tmx==fillvalue$value] <- NA

prs_path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/prs/"
vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste(prs_path,vgt_ncfile,sep=""))
print(ncin_vgt)
vgt <- ncvar_get(ncin_vgt,"navgtprs")
jck <- ncvar_get(ncin_vgt,"najckprs")
ldp <- ncvar_get(ncin_vgt,"naldpprs")
pdr <- ncvar_get(ncin_vgt,"napdrprs")
wbk <- ncvar_get(ncin_vgt,"nawbkprs")

years = 1903:2015; nyr <- length(years)

df.vgt.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.vgt.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.jck.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.jck.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.ldp.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.ldp.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.pdr.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.pdr.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.wbk.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.wbk.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
for (yr in 2:nyr){
  # 1. annual mean temperature
  mat_slice <- apply(abind(tmp[,,10:12,yr],tmp[,,1:9,(yr+1)],along=3),c(1,2),mean)
  # 2. mean temperature between April and August
  mtaa_slice <- apply(abind(tmp[,,4:8,(yr+1)],along=3),c(1,2),mean)
  # 3. current year August mean temperature
  mta_slice <- tmp[,,8,(yr+1)]
  # winter as December, January, February
  winter <- abind(tmn[,,12,yr],tmn[,,1:2,(yr+1)],along=3)
  # 4. winter monthly average daily minimum
  ntw_slice <- apply(winter,c(1,2),min)
  # 5. October monthly average daily minimum
  nto_slice <- tmn[,,10,yr]
  # 6. January monthly average daily minimum
  ntj_slice <- tmn[,,1,(yr+1)]
  # 7. March monthly average daily minimum
  ntm_slice <- tmn[,,3,(yr+1)]
  # 8. August monthly average daily maximum
  tmx_slice <- tmx[,,8,(yr+1)]
  # 9. mean annual precipitation
  map_slice <- apply(abind(pre[,,10:12,yr],pre[,,1:9,(yr+1)],along=3),c(1,2),mean)
  # 10. cumulative precipitation from June to August in current and previous years
  cpja_slice <- apply(abind(pre[,,6:8,yr:(yr+1)],along=3),c(1,2),sum)
  # 11. precipitation from June to August in previous year
  pja_slice <- apply(abind(pre[,,6:8,yr],along=3),c(1,2),sum)
  # 12. cumulative precipitation from October to September in current and previous years
  cpos_slice <- apply(abind(pre[,,10:12,(yr-1):yr],pre[,,1:9,yr:(yr+1)],along=3),c(1,2),sum)
  # 13. precipitation from October to September in previous year
  pos_slice <- apply(abind(pre[,,10:12,(yr-1)],pre[,,1:9,yr],along=3),c(1,2),sum)
  # 14. total growing season precipitation
  gsp_slice <- apply(pre[,,4:6,(yr+1)],c(1,2),sum)
  # 15. variability of growing season precipitation
  vgp_slice <- apply(abind(pre[,,4:6,(yr+1)],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))

  # get climate data with the presence of all core hosts
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
  vgt_prs <- rep("Core hosts",length(vgt_mat))

  # get climate data with the presence of jack pine
  jck_mat <- mat_slice[which(jck==1)]
  jck_mtaa <- mtaa_slice[which(jck==1)]
  jck_mta <- mta_slice[which(jck==1)]
  jck_ntw <- ntw_slice[which(jck==1)]
  jck_nto <- nto_slice[which(jck==1)]
  jck_ntj <- ntj_slice[which(jck==1)]
  jck_ntm <- ntm_slice[which(jck==1)]
  jck_xta <- tmx_slice[which(jck==1)]
  jck_map <- map_slice[which(jck==1)]
  jck_cpja <- cpja_slice[which(jck==1)]
  jck_pja <- pja_slice[which(jck==1)]
  jck_cpos <- cpos_slice[which(jck==1)]
  jck_pos <- pos_slice[which(jck==1)]
  jck_gsp <- gsp_slice[which(jck==1)]
  jck_vgp <- vgp_slice[which(jck==1)]
  jck_prs <- rep("Jack pine",length(jck_mat))
  
  # get climate data with the presence of jack pine
  ldp_mat <- mat_slice[which(ldp==1)]
  ldp_mtaa <- mtaa_slice[which(ldp==1)]
  ldp_mta <- mta_slice[which(ldp==1)]
  ldp_ntw <- ntw_slice[which(ldp==1)]
  ldp_nto <- nto_slice[which(ldp==1)]
  ldp_ntj <- ntj_slice[which(ldp==1)]
  ldp_ntm <- ntm_slice[which(ldp==1)]
  ldp_xta <- tmx_slice[which(ldp==1)]
  ldp_map <- map_slice[which(ldp==1)]
  ldp_cpja <- cpja_slice[which(ldp==1)]
  ldp_pja <- pja_slice[which(ldp==1)]
  ldp_cpos <- cpos_slice[which(ldp==1)]
  ldp_pos <- pos_slice[which(ldp==1)]
  ldp_gsp <- gsp_slice[which(ldp==1)]
  ldp_vgp <- vgp_slice[which(ldp==1)]
  ldp_prs <- rep("Lodgepole pine",length(ldp_mat))
  
  # get climate data with the presence of jack pine
  pdr_mat <- mat_slice[which(pdr==1)]
  pdr_mtaa <- mtaa_slice[which(pdr==1)]
  pdr_mta <- mta_slice[which(pdr==1)]
  pdr_ntw <- ntw_slice[which(pdr==1)]
  pdr_nto <- nto_slice[which(pdr==1)]
  pdr_ntj <- ntj_slice[which(pdr==1)]
  pdr_ntm <- ntm_slice[which(pdr==1)]
  pdr_xta <- tmx_slice[which(pdr==1)]
  pdr_map <- map_slice[which(pdr==1)]
  pdr_cpja <- cpja_slice[which(pdr==1)]
  pdr_pja <- pja_slice[which(pdr==1)]
  pdr_cpos <- cpos_slice[which(pdr==1)]
  pdr_pos <- pos_slice[which(pdr==1)]
  pdr_gsp <- gsp_slice[which(pdr==1)]
  pdr_vgp <- vgp_slice[which(pdr==1)]
  pdr_prs <- rep("Ponderosa pine",length(pdr_mat))
  
  # get climate data with the presence of jack pine
  wbk_mat <- mat_slice[which(wbk==1)]
  wbk_mtaa <- mtaa_slice[which(wbk==1)]
  wbk_mta <- mta_slice[which(wbk==1)]
  wbk_ntw <- ntw_slice[which(wbk==1)]
  wbk_nto <- nto_slice[which(wbk==1)]
  wbk_ntj <- ntj_slice[which(wbk==1)]
  wbk_ntm <- ntm_slice[which(wbk==1)]
  wbk_xta <- tmx_slice[which(wbk==1)]
  wbk_map <- map_slice[which(wbk==1)]
  wbk_cpja <- cpja_slice[which(wbk==1)]
  wbk_pja <- pja_slice[which(wbk==1)]
  wbk_cpos <- cpos_slice[which(wbk==1)]
  wbk_pos <- pos_slice[which(wbk==1)]
  wbk_gsp <- gsp_slice[which(wbk==1)]
  wbk_vgp <- vgp_slice[which(wbk==1)]
  wbk_prs <- rep("Whitebark pine",length(wbk_mat))

  len.vgt <- length(vgt_mat)
  len.jck <- length(jck_mat)
  len.ldp <- length(ldp_mat)
  len.pdr <- length(pdr_mat)
  len.wbk <- length(wbk_mat)

  df_vgt.tmp <- data.frame(vgt_mat,vgt_mtaa,vgt_mta,vgt_ntw,vgt_nto,vgt_ntj,vgt_ntm,vgt_xta,vgt_prs,vgt_yr=rep(toString(years[yr-1]),length(vgt_mat)))
  df_vgt.pre <- data.frame(vgt_map,vgt_cpja,vgt_pja,vgt_cpos,vgt_pos,vgt_gsp,vgt_vgp=c(vgt_vgp,rep(NA,(len.vgt-length(vgt_vgp)))),vgt_prs,vgt_yr=rep(toString(years[yr-1]),length(vgt_map)))
  df_jck.tmp <- data.frame(jck_mat,jck_mtaa,jck_mta,jck_ntw,jck_nto,jck_ntj,jck_ntm,jck_xta,jck_prs,jck_yr=rep(toString(years[yr-1]),length(jck_mat)))
  df_jck.pre <- data.frame(jck_map,jck_cpja,jck_pja,jck_cpos,jck_pos,jck_gsp,jck_vgp=c(jck_vgp,rep(NA,(len.jck-length(jck_vgp)))),jck_prs,jck_yr=rep(toString(years[yr-1]),length(jck_map)))
  df_ldp.tmp <- data.frame(ldp_mat,ldp_mtaa,ldp_mta,ldp_ntw,ldp_nto,ldp_ntj,ldp_ntm,ldp_xta,ldp_prs,ldp_yr=rep(toString(years[yr-1]),length(ldp_mat)))
  df_ldp.pre <- data.frame(ldp_map,ldp_cpja,ldp_pja,ldp_cpos,ldp_pos,ldp_gsp,ldp_vgp=c(ldp_vgp,rep(NA,(len.ldp-length(ldp_vgp)))),ldp_prs,ldp_yr=rep(toString(years[yr-1]),length(ldp_map)))
  df_pdr.tmp <- data.frame(pdr_mat,pdr_mtaa,pdr_mta,pdr_ntw,pdr_nto,pdr_ntj,pdr_ntm,pdr_xta,pdr_prs,pdr_yr=rep(toString(years[yr-1]),length(pdr_mat)))
  df_pdr.pre <- data.frame(pdr_map,pdr_cpja,pdr_pja,pdr_cpos,pdr_pos,pdr_gsp,pdr_vgp=c(pdr_vgp,rep(NA,(len.pdr-length(pdr_vgp)))),pdr_prs,pdr_yr=rep(toString(years[yr-1]),length(pdr_map)))
  df_wbk.tmp <- data.frame(wbk_mat,wbk_mtaa,wbk_mta,wbk_ntw,wbk_nto,wbk_ntj,wbk_ntm,wbk_xta,wbk_prs,wbk_yr=rep(toString(years[yr-1]),length(wbk_mat)))
  df_wbk.pre <- data.frame(wbk_map,wbk_cpja,wbk_pja,wbk_cpos,wbk_pos,wbk_gsp,wbk_vgp=c(wbk_vgp,rep(NA,(len.wbk-length(wbk_vgp)))),wbk_prs,wbk_yr=rep(toString(years[yr-1]),length(wbk_map)))
  
  df.vgt.tmp <- rbind(df.vgt.tmp, df_vgt.tmp)
  df.vgt.pre <- rbind(df.vgt.pre, df_vgt.pre)
  df.jck.tmp <- rbind(df.jck.tmp, df_jck.tmp)
  df.jck.pre <- rbind(df.jck.pre, df_jck.pre)
  df.ldp.tmp <- rbind(df.ldp.tmp, df_ldp.tmp)
  df.ldp.pre <- rbind(df.ldp.pre, df_ldp.pre)
  df.pdr.tmp <- rbind(df.pdr.tmp, df_pdr.tmp)
  df.pdr.pre <- rbind(df.pdr.pre, df_pdr.pre)
  df.wbk.tmp <- rbind(df.wbk.tmp, df_wbk.tmp)
  df.wbk.pre <- rbind(df.wbk.pre, df_wbk.pre)
  print(years[yr-1])
}

write.csv(df.vgt.tmp, paste0(out, "climatic_changes_data_tmp_1903_chs.csv"))
write.csv(df.vgt.pre, paste0(out, "climatic_changes_data_pre_1903_chs.csv"))
write.csv(df.jck.tmp, paste0(out, "climatic_changes_data_tmp_1903_jck.csv"))
write.csv(df.jck.pre, paste0(out, "climatic_changes_data_pre_1903_jck.csv"))
write.csv(df.ldp.tmp, paste0(out, "climatic_changes_data_tmp_1903_ldp.csv"))
write.csv(df.ldp.pre, paste0(out, "climatic_changes_data_pre_1903_ldp.csv"))
write.csv(df.pdr.tmp, paste0(out, "climatic_changes_data_tmp_1903_pdr.csv"))
write.csv(df.pdr.pre, paste0(out, "climatic_changes_data_pre_1903_pdr.csv"))
write.csv(df.wbk.tmp, paste0(out, "climatic_changes_data_tmp_1903_wbk.csv"))
write.csv(df.wbk.pre, paste0(out, "climatic_changes_data_pre_1903_wbk.csv"))

tnames <- c("mat","mtaa","mta","ntw","nto","ntj","ntm","xta")
pnames <- c("map","cpja","pja","cpos","pos","gsp","vgp")
names <-c(tnames,pnames)

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_chs.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_chs.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, vgt_yr == years[y])
		vgt <- mean(df.ss[,v], na.rm = TRUE)
		df.v <- rbind(df.v, data.frame(vgt))
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("Core hosts",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_chs.csv"))

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_jck.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_jck.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, jck_yr == years[y])
		jck <- mean(df.ss[,v], na.rm = TRUE)
		df.v <- rbind(df.v, data.frame(jck))
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("Jack pine",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_jck.csv"))

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_ldp.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_ldp.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, ldp_yr == years[y])
		ldp <- mean(df.ss[,v], na.rm = TRUE)
		df.v <- rbind(df.v, data.frame(ldp))
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("Lodgepole pine",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_ldp.csv"))

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_pdr.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_pdr.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, pdr_yr == years[y])
		pdr <- mean(df.ss[,v], na.rm = TRUE)
		df.v <- rbind(df.v, data.frame(pdr))
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("Ponderosa pine",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_pdr.csv"))

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_wbk.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_wbk.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, wbk_yr == years[y])
		wbk <- mean(df.ss[,v], na.rm = TRUE)
		df.v <- rbind(df.v, data.frame(wbk))
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("Whitebark pine",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_wbk.csv"))
