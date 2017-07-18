# Created by Dongmei Chen
# Exploratory data analysis: check long-term climate data

library(ncdf4)
install.packages("abind", repos='http://cran.us.r-project.org')
library(abind)

path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/"

ncfile_tmp <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs4d.nc"
ncin_tmp <- nc_open(paste(path, ncfile_tmp, sep=""))
print(ncin_tmp)
dname <- "tmp"
tmp <- ncvar_get(ncin_tmp,dname,start=c(x=1,y=1,month=1,year=2),count=c(x=1078, y=900, month=12, year=114))
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
tmn <- ncvar_get(ncin_tmn,dname,start=c(x=1,y=1,month=1,year=2),count=c(x=1078, y=900, month=12, year=114))
dim(tmn)
nc_close(ncin_tmn)
tmn[tmn==fillvalue$value] <- NA

ncfile_tmx <- "na10km_v2_cru_ts4.00.1901.2015.tmx.4d.nc"
ncin_tmx <- nc_open(paste(path, ncfile_tmx, sep=""))
print(ncin_tmx)
dname <- "tmx"
tmx <- ncvar_get(ncin_tmx,dname,start=c(x=1,y=1,month=1,year=2),count=c(x=1078, y=900, month=12, year=114))
dim(tmx)
nc_close(ncin_tmx)
tmx[tmx==fillvalue$value] <- NA

prs_path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/prs/"
vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste(prs_path,vgt_ncfile,sep=""))
print(ncin_vgt)
vgt <- ncvar_get(ncin_vgt,"navgtprs")
btl <- ncvar_get(ncin_vgt,"nabtlprsayr")

win <- 1
years = 1903:2015; nyr <- length(years)
df.clm.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.clm.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.vgt.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.vgt.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
df.btl.tmp <- data.frame(mat=double(),mtaa=double(),mta=double(),ntw=double(),nto=double(),ntj=double(),ntm=double(),xta=double(),prs=factor(),yr=factor())
df.btl.pre <- data.frame(map=double(),cpja=double(),pja=double(),cpos=double(),pos=double(),gsp=double(),vgp=double(),prs=factor(),yr=factor())
for (yr in 1:nyr){
  # 1. annual mean temperature
  mat_slice <- apply(abind(tmp[,,10:12,yr:(yr+win-1)],tmp[,,1:9,(yr+1):(yr+win)],along=3),c(1,2),mean)
  clm_mat <- mat_slice[!is.na(mat_slice)]
  # 2. mean temperature between April and August
  mtaa_slice <- apply(abind(tmp[,,4:8,yr:(yr+win)],along=3),c(1,2),mean)
  clm_mtaa <- mtaa_slice[!is.na(mtaa_slice)]
  # 3. current year August mean temperature
  mta_slice <- tmp[,,8,(yr+win)]
  clm_mta <- mta_slice[!is.na(mta_slice)]
  # winter as December, January, February
  winter <- abind(tmn[,,12,(yr+win-1)],tmn[,,1:2,(yr+win)],along=3)
  # 4. winter monthly average daily minimum
  ntw_slice <- apply(winter,c(1,2),min)
  clm_ntw <- ntw_slice[!is.na(ntw_slice)]
  # 5. October monthly average daily minimum
  nto_slice <- tmn[,,10,(yr+win-1)]
  clm_nto <- nto_slice[!is.na(nto_slice)]
  # 6. January monthly average daily minimum
  ntj_slice <- tmn[,,1,(yr+win)]
  clm_ntj <- ntj_slice[!is.na(ntj_slice)]
  # 7. March monthly average daily minimum
  ntm_slice <- tmn[,,3,(yr+win)]
  clm_ntm <- ntm_slice[!is.na(ntm_slice)]
  # 8. August monthly average daily maximum
  tmx_slice <- tmx[,,8,(yr+win)]
  clm_xta <- tmx_slice[!is.na(tmx_slice)]
  # 9. mean annual precipitation
  map_slice <- apply(abind(pre[,,10:12,yr:(yr+win-1)],pre[,,1:9,(yr+1):(yr+win)],along=3),c(1,2),mean)
  clm_map <- map_slice[!is.na(map_slice)]
  # 10. cumulative precipitation from June to August in current and previous years
  cpja_slice <- apply(abind(pre[,,6:8,(yr+win-1):(yr+win)],along=3),c(1,2),sum)
  clm_cpja <- cpja_slice[!is.na(cpja_slice)]
  # 11. precipitation from June to August in previous year
  pja_slice <- apply(abind(pre[,,6:8,(yr+win-1)],along=3),c(1,2),sum)
  clm_pja <- pja_slice[!is.na(pja_slice)]
  # 12. cumulative precipitation from October to September in current and previous years
  cpos_slice <- apply(abind(pre[,,10:12,(yr+win-1):(yr+win)],pre[,,1:9,(yr+win):(yr+win+1)],along=3),c(1,2),sum)
  clm_cpos <- cpos_slice[!is.na(cpos_slice)]
  # 13. precipitation from October to September in previous year
  pos_slice <- apply(abind(pre[,,10:12,(yr+win-1)],pre[,,1:9,(yr+win)],along=3),c(1,2),sum)
  clm_pos <- pos_slice[!is.na(pos_slice)]
  # 14. total growing season precipitation
  gsp_slice <- apply(pre[,,4:6,(yr+win)],c(1,2),sum)/win
  clm_gsp <- gsp_slice[!is.na(gsp_slice)]
  # 15. variability of growing season precipitation
  vgp_slice <- apply(abind(pre[,,4:6,(yr+win)],along=3),c(1,2),function(x) sqrt(var(x))/mean(x))
  clm_vgp <- vgp_slice[!is.na(vgp_slice)]
  clm_prs <- rep("climate",length(clm_mat))

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
  vgt_prs <- rep("hosts",length(vgt_mat))

  # get climate data with the presence of all mpb
  btl_mat <- mat_slice[which(btl==1)]
  btl_mtaa <- mtaa_slice[which(btl==1)]
  btl_mta <- mta_slice[which(btl==1)]
  btl_ntw <- ntw_slice[which(btl==1)]
  btl_nto <- nto_slice[which(btl==1)]
  btl_ntj <- ntj_slice[which(btl==1)]
  btl_ntm <- ntm_slice[which(btl==1)]
  btl_xta <- tmx_slice[which(btl==1)]
  btl_map <- map_slice[which(btl==1)]
  btl_cpja <- cpja_slice[which(btl==1)]
  btl_pja <- pja_slice[which(btl==1)]
  btl_cpos <- cpos_slice[which(btl==1)]
  btl_pos <- pos_slice[which(btl==1)]
  btl_gsp <- gsp_slice[which(btl==1)]
  btl_vgp <- vgp_slice[which(btl==1)]
  btl_prs <- rep("mpb",length(btl_mat))

  len.clm <- length(clm_mat)
  len.vgt <- length(vgt_mat)
  len.btl <- length(btl_mat)
  df_clm.tmp <- data.frame(clm_mat,clm_mtaa,clm_mta,clm_ntw,clm_nto,clm_ntj,clm_ntm,clm_xta,clm_prs,clm_yr=rep(toString(years[yr]),length(clm_mat)))
  df_clm.pre <- data.frame(clm_map,clm_cpja,clm_pja,clm_cpos,clm_pos,clm_gsp,clm_vgp=c(clm_vgp,rep(NA,(len.clm-length(clm_vgp)))),clm_prs,clm_yr=rep(toString(years[yr]),length(clm_map)))
  df_vgt.tmp <- data.frame(vgt_mat,vgt_mtaa,vgt_mta,vgt_ntw,vgt_nto,vgt_ntj,vgt_ntm,vgt_xta,vgt_prs,vgt_yr=rep(toString(years[yr]),length(vgt_mat)))
  df_vgt.pre <- data.frame(vgt_map,vgt_cpja,vgt_pja,vgt_cpos,vgt_pos,vgt_gsp,vgt_vgp=c(vgt_vgp,rep(NA,(len.vgt-length(vgt_vgp)))),vgt_prs,vgt_yr=rep(toString(years[yr]),length(vgt_map)))
  df_btl.tmp <- data.frame(btl_mat,btl_mtaa,btl_mta,btl_ntw,btl_nto,btl_ntj,btl_ntm,btl_xta,btl_prs,btl_yr=rep(toString(years[yr]),length(btl_mat)))
  df_btl.pre <- data.frame(btl_map,btl_cpja,btl_pja,btl_cpos,btl_pos,btl_gsp,btl_vgp=c(btl_vgp,rep(NA,(len.btl-length(btl_vgp)))),btl_prs,btl_yr=rep(toString(years[yr]),length(btl_map)))

  df.clm.tmp <- rbind(df.clm.tmp, df_clm.tmp)
  df.clm.pre <- rbind(df.clm.pre, df_clm.pre)
  df.vgt.tmp <- rbind(df.vgt.tmp, df_vgt.tmp)
  df.vgt.pre <- rbind(df.vgt.pre, df_vgt.pre)
  df.btl.tmp <- rbind(df.btl.tmp, df_btl.tmp)
  df.btl.pre <- rbind(df.btl.pre, df_btl.pre)
  print(years[yr])
}

write.csv(df.clm.tmp, paste0(out, "climatic_changes_data_tmp_1903_clm.csv"))
write.csv(df.clm.pre, paste0(out, "climatic_changes_data_pre_1903_clm.csv"))
write.csv(df.vgt.tmp, paste0(out, "climatic_changes_data_tmp_1903_vgt.csv"))
write.csv(df.vgt.pre, paste0(out, "climatic_changes_data_pre_1903_vgt.csv"))
write.csv(df.btl.tmp, paste0(out, "climatic_changes_data_tmp_1903_btl.csv"))
write.csv(df.btl.pre, paste0(out, "climatic_changes_data_pre_1903_btl.csv"))

tnames <- c("mat","mtaa","mta","ntw","nto","ntj","ntm","xta")
pnames <- c("map","cpja","pja","cpos","pos","gsp","vgp")
names <-c(tnames,pnames)

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_clm.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_clm.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])

years = 1903:2015; nyr <- length(years)
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, clm_yr == years[y])
		if (v == 8) {
			clm <- max(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(clm)) 	
		} else if (v >= 4 && v <=7){
			clm <- min(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(clm))		
		} else{
			clm <- mean(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(clm))
		}
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("climate",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_clm.csv"))

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_vgt.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_vgt.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, vgt_yr == years[y])
		if (v == 8) {
			vgt <- max(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(vgt)) 	
		} else if (v >= 4 && v <=7){
			vgt <- min(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(vgt))		
		} else{
			vgt <- mean(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(vgt))
		}
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("hosts",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_vgt.csv"))

df.t <- read.csv(paste0(out,"climatic_changes_data_tmp_1903_btl.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(out,"climatic_changes_data_pre_1903_btl.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])
df.n <- data.frame(yr=years)
for (v in 1:15){
	df.v <- data.frame()
	for (y in 1:nyr){
		df.ss <- subset(df, btl_yr == years[y])
		if (v == 8) {
			btl <- max(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(btl)) 	
		} else if (v >= 4 && v <=7){
			btl <- min(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(btl))		
		} else{
			btl <- mean(df.ss[,v], na.rm = TRUE)
			df.v <- rbind(df.v, data.frame(btl))
		}
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, prs=rep("mpb",length=nyr))
write.csv(df.n, paste0(out,"climatic_changes_temporal_stat_1903_btl.csv"))