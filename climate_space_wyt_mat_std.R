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
library(abind)
#registerDoParallel(cores=12)

# editted from 20160513 cs_std_t_test.R 20160811 cs_std_tmp.R

# run in ACISS 
path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/maps/"

year1 <- 1901; year2 <- 1961; year3 <- 2001

# subscripts of year 1961 and year 2001
n1 <- year2-year1+1; n2 <- year3-year1+1


# near surface air temperature
ncfile_tmp <- "na10km_v2_cru_ts4.00.1901.2015.tmp.abs4d.nc"
ncin_tmp <- nc_open(paste(path, ncfile_tmp, sep=""))
print(ncin_tmp)
dname <- "tmp"
tmp_ltm <- ncvar_get(ncin_tmp,dname,start=c(x=1,y=1,month=1,year=n1),count=c(x=1078,y=900, month=12, year=30))
dim(tmp_ltm)
tmp <- ncvar_get(ncin_tmp,dname,start=c(x=1,y=1,month=1,year=n2),count=c(x=1078,y=900, month=12, year=15))
dim(tmp)
month <- ncvar_get(ncin_tmp,"month", start=1, count=12); mn <- length(month)
yrs1 <- ncvar_get(ncin_tmp,"year", start=n1, count=30); nyr1 <- length(yrs1)
yrs2 <- ncvar_get(ncin_tmp,"year", start=n2, count=15); nyr2 <- length(yrs2)
fillvalue <- ncatt_get(ncin_tmp,dname,"_FillValue")
# get x, y for mapping; same coordinates for precipitation data
x <- ncvar_get(ncin_tmp, varid="x"); nx <- length(x)
y <- ncvar_get(ncin_tmp, varid="y"); ny <- length(y)

nc_close(ncin_tmp)

tmp[tmp==fillvalue$value] <- NA
tmp_ltm[tmp_ltm==fillvalue$value] <- NA

# get long-term means and standard deviation

# define array for long-term means
wym <- array(NA, dim=c(nx,ny,(nyr1-1)))
mam <- array(NA, dim=c(nx,ny,nyr1))
# long-term means
ptm <- proc.time()
for (yr1 in 1:(nyr1-1)){
	wy_ltm <- abind(tmp_ltm[,,10:12,yr1], tmp_ltm[,,1:9,(yr1+1)], along=3)
	wym[,,yr1] <- apply(wy_ltm,c(1,2),mean) # water year mean temperature	
	print(yr1)
}
proc.time() - ptm

ptm <- proc.time()
for (yr2 in 1:nyr1){
	mam[,,yr2] <- apply(tmp_ltm[,,3:8,yr2],c(1,2),mean) # March to August mean temperature
	print(yr2)
}
proc.time() - ptm

ltm_wy <- apply(wym,c(1,2),mean)
std_wy <- apply(wym,c(1,2),sd)
ltm_ma <- apply(mam,c(1,2),mean)
std_ma <- apply(mam,c(1,2),sd)

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

# plot climate space of all mpb with all host species
ptm <- proc.time()
years <- 2001:2015
#loop in year
for (yr3 in 2:nyr2){
	# water year mean temperatures
	wy <- abind(tmp[,,10:12,(yr3-1)],tmp[,,1:9,yr3],along=3)
	# get climate data
	# water year monthly average daily temperature
	mean_slice <- apply(wy,c(1,2),mean)
	wystd_slice <- (mean_slice - ltm_wy)/std_wy
	clm_wystd <- wystd_slice[!is.na(wystd_slice)]
	# March to August monthly average daily temperature
	mamean_slice <- apply(tmp[,,3:8,yr3],c(1,2),mean) 
	mastd_slice <- (mamean_slice - ltm_ma)/std_ma
	clm_mastd <- mastd_slice[!is.na(mastd_slice)]
	# get climate data with the presence of vegetation
	vgt_wystd <- wystd_slice[which(vgt==1)]
	vgt_mastd <- mastd_slice[which(vgt==1)]
	# get climate data with the presence of all mpb
	btl_slice <- btl[,,yr3]
	btl_wystd <- wystd_slice[which(btl_slice==1)]
	btl_mastd <- mastd_slice[which(btl_slice==1)]
	sd1 <- length(btl_mastd[(btl_mastd <= 1 & btl_mastd > 0) | (btl_mastd >= -1 & btl_mastd < 0)])
	sd2 <- length(btl_mastd[(btl_mastd <= 2 & btl_mastd > 1) | (btl_mastd < -1 & btl_mastd >= -2)])
	sd3 <- length(btl_mastd[(btl_mastd <= 3 & btl_mastd > 2) | (btl_mastd < -2 & btl_mastd >= -3)])
	sdsum <- length(btl_mastd)
	n1 <- round(sd1/sdsum*100, digits = 1)
	n2 <- round(sd2/sdsum*100, digits = 1)
	n3 <- round(sd3/sdsum*100, digits = 1)
	# get climate data with the presence of mpb in all core hosts
	mpb_slice <- mpb[,,yr3]
	mpb_wystd <- wystd_slice[which(mpb_slice==1)]
	mpb_mastd <- mastd_slice[which(mpb_slice==1)]
	# plot vegetation climate space
	cs_wystd <- c(clm_wystd,vgt_wystd,btl_wystd,mpb_wystd)
	cs_mastd <- c(clm_mastd,vgt_mastd,btl_mastd,mpb_mastd)
	presence <- c(rep("1 climate",length(clm_wystd)),rep("2 core hosts",length(vgt_wystd)),rep("3 mpb in all hosts",length(btl_wystd)),rep("4 mpb in core hosts",length(mpb_wystd)))
	df <- data.frame(cs_wystd,cs_mastd,presence)
	plot1 <- qplot(cs_wystd,cs_mastd, data=df, color=factor(presence), alpha=I(0.7), xlab = "Water year mean temperature SD", ylab = "March to August
mean temperature SD", main = paste("MPB climate space in", toString(years[yr3]))) + xlim(-4,4)+ylim(-4,4)
	plot1 <- plot1 + scale_colour_manual(values = c("grey","green","blue","red"))+ labs(color="Presence")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	# plot1 <- plot1 + geom_vline(xintercept = 0)
	# plot1 <- plot1 + geom_hline(yintercept = 0)
	#d=data.frame(x1=c(0,1,2), x2=c(1,2,3), y1=c(0,1,2), y2=c(1,2,3), x=c(0.5,1.5,2.5),y=c(0.5,1.5,2.5),lab=paste(toString(1:3), "SD"))
	#d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1), x=c(0.5,1.5,2.5),y=c(0.5,1.5,2.5),lab=c(paste(toString(n1), "%"), paste(toString(n2), "%"), paste(toString(n3), "%")))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(toString(n3), "%"), paste(toString(n2), "%"), paste(toString(n1), "%")))
	#ggplot(data=d)+ geom_rect(mapping=aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2), fill=NA, color="black")
	#plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black")
	#geom_text(data=d, aes(xmin=NULL, xmax=NULL, ymin=NULL, ymax=NULL, x=x,y=y,label=lab))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.25, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df, aes(x=presence, y=cs_wystd,fill=factor(presence)))+geom_boxplot()+scale_fill_manual(values = c("grey","green","blue","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(y="Water year mean temperature SD")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df, aes(x=presence, y=cs_mastd,fill=factor(presence)))+geom_boxplot()+scale_fill_manual(values = c("grey","green","blue","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(y="March to August
mean temperature SD")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	png(paste(out,"cs_beetles_chosts_", toString(years[yr3]), "_wyt_mat.png", sep = ""), width=12, height=6, units="in", res=300)
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
im.convert(paste(out,"cs_beetles_chosts_20*.png", sep = ""), output = paste(out,"cs_beetles_chosts_wyt_mat.gif", sep=""))