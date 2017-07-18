# Created by Dongmei Chen
# Exploratory data analysis: temporal plots

library(ncdf4)
install.packages("abind", repos='http://cran.us.r-project.org')
library(lattice)
library(RColorBrewer)
library(abind)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=12)
library(ggplot2)
library(grid)

# functions to return text in a boxplot
max.n <- function(x){
  if (max(x)< 10){
  return(c(y = max(x)*1.32, label = round(max(x),1))) 
  } else {
  return(c(y = max(x)*1.12, label = round(max(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
min.n <- function(x){
  if (min(x) < 0) {
    return(c(y = min(x)*1.16, label = round(min(x),1)))
  } else {
    return(c(y = min(x)*0.32, label = round(min(x),1))) 
  }
  # experiment with the multiplier to find the perfect position
}
# function for mean labels
mean.n <- function(x){
  if (mean(x) < 0) {
    return(c(y = mean(x)*0.88, label = round(mean(x),1)))
  } else {
    return(c(y = mean(x)*1.02, label = round(mean(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# (total growing season precipitation, variability of growing season precipitation, 
# mean August temperature, mean temperature between Oct and Sep, minimum winter temperature, 
# mean annual precipitation) 

## plot climatic variables: 
# 1. Minimum winter temperature - minimum monthly temperature between Dec and Feb;
# 2. Mean annual precipitation - the average of monthly precipitation from Jan to Dec in current year
# 3. Total growing season precipitation - the sum of monthly precipitation from Apr to Jun
# 4. Variability of growing season precipitation - the coefficient of variation (i.e. square root of variance divided by mean) of monthly 
# precipitation from Apr to Jun
# 5. Mean August temperature - mean monthly temperature in August
# 6. Mean temperature between Oct and Sep - yearly mean of monthly temperature between Oct and Sep
# 7. Cumulative precipitation in current and previous year
# 8. Precipitation in previous year

# notes: pre - current year precipitation; pre_2 - previous year precipitation; 
# pre_3 - precipitation in current and previous years


path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/"

## near surface air temperature
ncfile_tmp <- "na10km_v2_cru_ts4.00.2000.2015.tmp.abs4d.nc"
ncin_tmp <- nc_open(paste(path, ncfile_tmp, sep=""))
print(ncin_tmp)
dname <- "tmp"
tmp <- ncvar_get(ncin_tmp,dname)
fillvalue <- ncatt_get(ncin_tmp,dname,"_FillValue")
# get x, y for mapping;
x <- ncvar_get(ncin_tmp, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_tmp, varid="y"); ny <- length(y)
nc_close(ncin_tmp)

years = 2001:2015; nyr <- length(years)

# quick map to check the data
m <- 1; n <- nyr-2
tmp_slice_4d <- tmp[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.2013.abs.tmp4d.png",sep=""))
levelplot(tmp_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

tmp[tmp==fillvalue$value] <- NA

## near surface air temperature minimum
ncfile_tmn <- "na10km_v2_cru_ts4.00.2000.2015.tmn.4d.nc"
ncin_tmn <- nc_open(paste(path, ncfile_tmn, sep=""))
print(ncin_tmn)
dname <- "tmn"
tmn <- ncvar_get(ncin_tmn,dname)
fillvalue <- ncatt_get(ncin_tmn,dname,"_FillValue")
# get x, y for mapping;
x <- ncvar_get(ncin_tmn, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_tmn, varid="y"); ny <- length(y)
nc_close(ncin_tmn)

# quick map to check the data
m <- 1; n <- nyr-2
tmn_slice_4d <- tmn[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.2013.abs.tmn4d.png",sep=""))
levelplot(tmn_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

tmn[tmn==fillvalue$value] <- NA

## near surface air temperature maximum
# use August maximum temperature data in the current year
ncfile_tmx <- "na10km_v2_cru_ts4.00.2001.2015.tmx.4d.nc"
ncin_tmx <- nc_open(paste(path, ncfile_tmx, sep=""))
print(ncin_tmx)
tmx <- ncvar_get(ncin_tmx,"tmx")
nc_close(ncin_tmx)

# quick map to check the data
m <- 8; n <- nyr-2
tmx_slice_4d <- tmx[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.2013.abs.tmx4d.png",sep=""))
levelplot(tmx_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

tmx[tmx==fillvalue$value] <- NA

## mean annual precipitation
# use precipitation data in the current year
ncfile_pre <- "na10km_v2_cru_ts4.00.2001.2015.pre.abs4d.nc"
ncin_pre <- nc_open(paste(path, ncfile_pre, sep=""))
print(ncin_pre)
dname <- "pre"
pre <- ncvar_get(ncin_pre,dname)
nc_close(ncin_pre)

# quick map to check the data
m <- 8; n <- nyr-2
pre_slice_4d <- pre[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,50,100,250,500,1000,1500,2000,2500,3000,5000)
png(file=paste(out,"na10km_v2_cru_4.00.2013.abs.pre4d.png",sep=""))
levelplot(pre_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

pre[pre==fillvalue$value] <- NA

# precipitation in previous year
# use precipitation data in the previous year
ncfile_pre_2 <- "na10km_v2_cru_ts4.00.2000.2014.pre.abs4d.nc"
ncin_pre_2 <- nc_open(paste(path, ncfile_pre_2, sep=""))
print(ncin_pre_2)
dname <- "pre"
pre_2 <- ncvar_get(ncin_pre_2,dname)
nc_close(ncin_pre_2)

# quick map to check the data
m <- 8; n <- nyr-2
pre_slice_4d <- pre_2[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,50,100,250,500,1000,1500,2000,2500,3000,5000)
png(file=paste(out,"na10km_v2_cru_4.00.2012.abs.pre4d.png",sep=""))
levelplot(pre_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

pre_2[pre_2==fillvalue$value] <- NA

# cumulative precipitation in current and previous year
# use precipitation data in the previous and current year
ncfile_pre_3 <- "na10km_v2_cru_ts4.00.2000.2015.pre.abs4d.nc"
ncin_pre_3 <- nc_open(paste(path, ncfile_pre_3, sep=""))
print(ncin_pre_3)
dname <- "pre"
pre_3 <- ncvar_get(ncin_pre_3,dname)
nc_close(ncin_pre_3)

# quick map to check the data
m <- 8; n <- nyr
pre_slice_4d <- pre_3[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,50,100,250,500,1000,1500,2000,2500,3000,5000)
png(file=paste(out,"na10km_v2_cru_4.00.2015.abs.pre4d.png",sep=""))
levelplot(pre_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

pre_3[pre_3==fillvalue$value] <- NA

# read vegetation and bettle presence data
prs_path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/prs/"
vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste(prs_path,vgt_ncfile,sep=""))
print(ncin_vgt)
vgt <- ncvar_get(ncin_vgt,"navgtprs")
btl_ncfile <- "na10km_v2_mpb_presence.nc"
ncin_btl <- nc_open(paste(prs_path,btl_ncfile, sep=""))
print(ncin_btl)
btl <- ncvar_get(ncin_btl,"mpb_prs") # with all hosts
mpb <- ncvar_get(ncin_btl,"chosts_mpb_prs") # with all core hosts
nc_close(ncin_btl)

ptm <- proc.time()
# Temporal plots: plot climatic variables (total growing season precipitation, variability of growing season precipitation,
# mean August temperature, mean temperature between Oct and Sep, minimum winter temperature, mean annual precipitation) 
# over time in a box-plot, three plots (MPB, host, North America) listed in one figure, and six figures totally
df <- data.frame(tmp=double(), tmx=double(), tmn=double(), map=double(), tpp=double(), gsp=double(), cum=double(), cvp=double(), prs=numeric(), yr=factor())
for (yr in 1:nyr){
	# 1. water year mean temperature data
	wy <- abind(tmp[,,10:12,yr],tmp[,,1:9,(yr+1)],along=3)
	wyt_slice <- apply(wy,c(1,2),mean)
	clm_tmp <- wyt_slice[!is.na(wyt_slice)]
	# winter as December, January, February
	winter <- abind(tmn[,,12,yr],tmn[,,1:2,(yr+1)],along=3)
	# get climate data
	# 2. winter monthly average daily minimum
	tmn_slice <- apply(winter,c(1,2),min)
	clm_tmn <- tmn_slice[!is.na(tmn_slice)]
	# 3. August monthly average daily maximum
	tmx_slice <- tmx[,,8,yr]
	clm_tmx <- tmx_slice[!is.na(tmx_slice)]
	# 4. mean annual precipitation
	map_slice <- apply(pre[,,1:12,yr],c(1,2),mean)
	clm_map <- map_slice[!is.na(map_slice)]
	# 5. total precipitation in previous years
	tpp_slice <- apply(pre_2[,,1:12,yr],c(1,2),sum)
	clm_tpp <- tpp_slice[!is.na(tpp_slice)]
	# 6. total growing season precipitation
	gsp_slice <- apply(pre[,,4:6,yr],c(1,2),sum)
	clm_gsp <- gsp_slice[!is.na(gsp_slice)]
	# 7. cumulative precipitation in current and previous year
	cum_slice <- apply(abind(pre_3[,,1:12,yr], pre_3[,,1:12,(yr+1)],along=3),c(1,2),sum)
	clm_cum <- cum_slice[!is.na(cum_slice)]
	# 8. variability of growing season precipitation
	cvp_slice <- apply(pre[,,4:6,yr],c(1,2), function(x) sqrt(var(x))/mean(x))
	clm_cvp <- cvp_slice[!is.na(cvp_slice)]
	
	# get climate data with the presence of vegetation
	vgt_tmp <- wyt_slice[which(vgt==1)]
	vgt_tmx <- tmx_slice[which(vgt==1)]
	vgt_tmn <- tmn_slice[which(vgt==1)]
	vgt_map <- map_slice[which(vgt==1)]
	vgt_tpp <- tpp_slice[which(vgt==1)]
	vgt_gsp <- gsp_slice[which(vgt==1)]
	vgt_cum <- cum_slice[which(vgt==1)]
	vgt_cvp <- cvp_slice[which(vgt==1)]
	
	# get climate data with the presence of all mpb
	btl_slice <- btl[,,yr]
	btl_tmp <- wyt_slice[which(btl_slice==1)]
	btl_tmx <- tmx_slice[which(btl_slice==1)]
	btl_tmn <- tmn_slice[which(btl_slice==1)]
	btl_map <- map_slice[which(btl_slice==1)]	
	btl_tpp <- tpp_slice[which(btl_slice==1)]
	btl_gsp <- gsp_slice[which(btl_slice==1)]
	btl_cum <- cum_slice[which(btl_slice==1)]
	btl_cvp <- cvp_slice[which(btl_slice==1)]	
	
	# data frame
	df_tmp <- c(clm_tmp,vgt_tmp,btl_tmp)
	df_tmx <- c(clm_tmx,vgt_tmx,btl_tmx)
	df_tmn <- c(clm_tmn,vgt_tmn,btl_tmn)
	df_map <- c(clm_map,vgt_map,btl_map)
	df_tpp <- c(clm_tpp,vgt_tpp,btl_tpp)
	df_gsp <- c(clm_gsp,vgt_gsp,btl_gsp)
	df_cum <- c(clm_cum,vgt_cum,btl_cum)
	df_cvp <- c(clm_cvp,vgt_cvp,btl_cvp)
	df_prs <- c(rep("climate",length(clm_tmn)),rep("hosts",length(vgt_tmn)),rep("mpb",length(btl_tmn)))
	df_all <- data.frame(df_tmp,df_tmx,df_tmn,df_map,df_tpp,df_gsp,df_cum,df_cvp=c(df_cvp,rep(NA,(length(df_tmp)-length(df_cvp)))),df_prs,df_yr=rep(toString(years[yr]),length(df_prs)))
	df <- rbind(df, df_all)
	print(years[yr])	
}

write.csv(df, paste0(out, "climatic_changes_temporal_plots.csv"))

#ylabs <- c(expression("Water year mean temperature"( degree*C)),expression("Maximum August temperature"( degree*C)),expression("Minimum winter temperature"( degree*C)),
#"Mean monthly precipitation (mm)","Total precipitation from previous year (mm)","Total growing season precipitation (mm)",
#"Cumulative precipitation in current and previous year (mm)","Variability of growing season precipitation")

ylabs <- c("Water year mean monthly T (°C)","Maximum August T (°C)","Minimum winter T (°C)",
"Mean monthly P (mm)","Total P from previous year (mm)","Total growing season P (mm)",
"Cumulative P in two years (mm)","Variability of growing season P")

names <- c("tmp","tmx","tmn","map","tpp","gsp","cum","cvp")

foreach(n=1:8) %dopar% {
	df.ss.1 <- subset(df, df_prs == "climate")
	p1 <- ggplot(df.ss.1, aes(x = df_yr, y = df.ss.1[,n])) +geom_boxplot(fill = "grey", colour = "black")+labs(x="Year", y=ylabs[n])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+
        ggtitle("Climatic changes in North America")
  	df.ss.2 <- subset(df, df_prs == "hosts")
	p2 <- ggplot(df.ss.2, aes(x = df_yr, y = df.ss.2[,n])) +geom_boxplot(fill = "green", colour = "black")+labs(x="Year", y=ylabs[n])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+
        ggtitle("Climatic changes in areas where core hosts exist")
	df.ss.3 <- subset(df, df_prs == "mpb")
	p3 <- ggplot(df.ss.3, aes(x = df_yr, y = df.ss.3[,n])) +geom_boxplot(fill = "red", colour = "black")+labs(x="Year", y=ylabs[n])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+
        ggtitle("Climatic changes in areas where mountain pine beetle exists")
	png(paste0(out,"temporal_plots_", names[n], ".png"), width=14, height=12, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(3, 1))) # 3 rows, 1 column
	print(p1, vp = vplayout(1, 1))
	print(p2, vp = vplayout(2, 1))
	print(p3, vp = vplayout(3, 1))
	dev.off()
	print(paste(names[n],"done!"))	
}
proc.time() - ptm