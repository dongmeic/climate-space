# load required libraries
library(ncdf4)
library(ggplot2)
library(lattice)
library(RColorBrewer)
#library(doParallel)
#library(foreach)
library(animation)
library(grid)
install.packages("abind", repos='http://cran.us.r-project.org')
#chooseCRANmirror(131)
#library(abind)
#registerDoParallel(cores=12)

# editted from 20160424 cs_climate_plot_p.R &
# /Volumes/dongmeic/4.summer_2016/climate_space/scripts/climate_space/cs_climate_plot_pre.R
# Sum of precipitation from April to June VS CV of growing-season precipitation

# run in ACISS 
path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/maps/"

# near surface air temperature minimum
ncfile_pre <- "na10km_v2_cru_ts4.00.2001.2015.pre.abs4d.nc"
ncin_pre <- nc_open(paste(path, ncfile_pre, sep=""))
print(ncin_pre)
dname <- "pre"
pre <- ncvar_get(ncin_pre,dname)
fillvalue <- ncatt_get(ncin_pre,dname,"_FillValue")
# get x, y for mapping; same coordinates for precipitation data
x <- ncvar_get(ncin_pre, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_pre, varid="y"); ny <- length(y)
nc_close(ncin_pre)

# quick map to check the data
years = 2001:2015
nyr <- length(years)
m <- 1; n <- nyr-2
pre_slice_4d <- pre[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(0,50,100,250,500,1000,1500,2000,2500,3000,5000)
png(file=paste(out,"na10km_v2_cru_4.00.2013.abs.pre4d.png",sep=""))
levelplot(pre_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

pre[pre==fillvalue$value] <- NA

# # near surface air temperature maximum
# ltmpath <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ltm/"
# ncfile_ltm <- "na10km_v2_pre.nc"
# ncin_ltm <- nc_open(paste(ltmpath, ncfile_ltm, sep=""))
# print(ncin_ltm)
# ltm <- ncvar_get(ncin_ltm,"pre")
# nc_close(ncin_ltm)

# # get long-term average 
# pre_ltm <- apply(pre[,,4:6,yr],c(1,2),sum)

# # quick map to check the data
# m <- 8; 
# ltm_slice_3d <- ltm[,,m]
# grid <- expand.grid(x=x, y=y)
# cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
# #png(file=paste(out,"na10km_v2_ltm.png",sep=""))
# levelplot(ltm_slice_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
# col.regions=(rev(brewer.pal(10,"RdBu"))))
# #dev.off()

# ltm[ltm==fillvalue$value] <- NA

## read vegetation and bettle presence data
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
# plot climate space of host species with beetle presence
#month <- c("January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December")

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

# coefficience of variation
cv <- function(x){
	sqrt(var(x))/mean(x)
}

# plot climate space of all mpb with all host species
ptm <- proc.time()
#loop in year
for (yr in 1:nyr){
	# growing season as April, May, and June
	# get climate data
	# Sum of precipitation from April to June
	pre_slice <- apply(pre[,,4:6,yr],c(1,2),sum)
	clm_pre <- pre_slice[!is.na(pre_slice)] # 227910
	# CV of growing-season precipitation
	# pre_mean <- apply(pre[,,4:6,yr],c(1,2),mean)
	# pre_sd <- apply(pre[,,4:6,yr],c(1,2),sd)
	# cvp_slice <- pre_sd/pre_mean
	cvp_slice <- apply(pre[,,4:6,yr],c(1,2),cv)
	clm_cvp <- cvp_slice[!is.na(cvp_slice)] # 227036
	# # make adjustments
	# pre_slice[which(cvp_slice==NA)] <- NA 
	clm_cvp <- append(clm_cvp, rep(NA, c(abs(length(clm_cvp)-length(clm_pre)))))
	# get climate data with the presence of vegetation
	vgt_pre <- pre_slice[which(vgt==1)]
	vgt_cvp <- cvp_slice[which(vgt==1)]
	# get climate data with the presence of all mpb
	btl_slice <- btl[,,yr]
	btl_pre <- pre_slice[which(btl_slice==1)]
	btl_cvp <- cvp_slice[which(btl_slice==1)]
	# get climate data with the presence of mpb in all core hosts
	mpb_slice <- mpb[,,yr]
	mpb_pre <- pre_slice[which(mpb_slice==1)]
	mpb_cvp <- cvp_slice[which(mpb_slice==1)]
	# plot vegetation climate space
	cs_pre <- c(clm_pre,vgt_pre,btl_pre,mpb_pre)
	cs_cvp <- c(clm_cvp,vgt_cvp,btl_cvp,mpb_cvp)
	presence <- c(rep("1 climate",length(clm_pre)),rep("2 core hosts",length(vgt_pre)),rep("3 mpb in all hosts",length(btl_pre)),rep("4 mpb in core hosts",length(mpb_pre)))
	df <- data.frame(cs_pre,cs_cvp,presence)
	plot1 <- qplot(cs_pre,cs_cvp, data=df, color=factor(presence), alpha=I(0.7), xlab = "Sum of precipitation from April to June (mm)", ylab = "CV of growing-season precipitation", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(values = c("grey","green","blue","red"))+ labs(color="Presence")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot2 <- ggplot(df, aes(x=presence, y=cs_pre,fill=factor(presence)))+geom_boxplot()+scale_fill_manual(values = c("grey","green","blue","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(y="Sum of precipitation from April to June (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df, aes(x=presence, y=cs_cvp,fill=factor(presence)))+geom_boxplot()+scale_fill_manual(values = c("grey","green","blue","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(y="CV of growing-season precipitation")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	png(paste(out,"cs_beetles_chosts_", toString(years[yr]), "_gsp_cvp.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()				
}
#library(gridExtra)
#grid.arrange(plot1, plot2, plot3, ncol=3)
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_20*gsp_cvp.png", sep = ""), output = paste(out,"cs_beetles_chosts_gsp_cvp.gif", sep=""))