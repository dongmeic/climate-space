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

# editted from 20160424 cs_climate_plot_t.R & 
# /Volumes/dongmeic/4.summer_2016/climate_space/scripts/climate_space/cs_climate_plot_tmp.R
# winter monthly average daily minimum VS August monthly average daily maximum

# run in ACISS 
path <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
out <- "/home2/dongmeic/beetle/output/maps/"

# near surface air temperature minimum
ncfile_tmn <- "na10km_v2_cru_ts4.00.2001.2015.tmn.4d.nc"
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
years = 2001:2015
nyr <- length(years)
m <- 1; n <- nyr-2
tmn_slice_4d <- tmn[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-40,-30,-20,-10,0,10,20,30,40,50)
png(file=paste(out,"na10km_v2_cru_4.00.2013.abs.tmn4d.png",sep=""))
levelplot(tmn_slice_4d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
col.regions=(rev(brewer.pal(10,"RdBu"))))
dev.off()

tmn[tmn==fillvalue$value] <- NA

# near surface air temperature maximum
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
#loop in year
for (yr in 2:nyr){
	# winter as December, January, February
	winter <- abind(tmn[,,12,(yr-1)],tmn[,,1:2,yr],along=3)
	# get climate data
	# winter monthly average daily minimum
	tmn_slice <- apply(winter,c(1,2),min)
	clm_tmn <- tmn_slice[!is.na(tmn_slice)]
	# August monthly average daily maximum
	tmx_slice <- tmx[,,8,yr]
	clm_tmx <- tmx_slice[!is.na(tmx_slice)]
	# get climate data with the presence of vegetation
	vgt_tmn <- tmn_slice[which(vgt==1)]
	vgt_tmx <- tmx_slice[which(vgt==1)]
	# get climate data with the presence of all mpb
	btl_slice <- btl[,,yr]
	btl_tmn <- tmn_slice[which(btl_slice==1)]
	btl_tmx <- tmx_slice[which(btl_slice==1)]
	# calculate the points in each "space"
	tp1 <- length(btl_tmn[btl_tmn > -40 & btl_tmx  >= 18.3])
	tp2 <- length(btl_tmn[btl_tmn > -40 & btl_tmx  < 18.3])
	tp3 <- length(btl_tmn[btl_tmn <= -40 & btl_tmx  >= 18.3])
	tp4 <- length(btl_tmn[btl_tmn <= -40 & btl_tmx  < 18.3])
	tpsum <- length(btl_tmn)
	n1 <- round(tp1/tpsum*100, digits = 1)
	n2 <- round(tp2/tpsum*100, digits = 1)
	n3 <- round(tp3/tpsum*100, digits = 1)
	n4 <- round(tp4/tpsum*100, digits = 1)
	# get climate data with the presence of mpb in all core hosts
	mpb_slice <- mpb[,,yr]
	mpb_tmn <- tmn_slice[which(mpb_slice==1)]
	mpb_tmx <- tmx_slice[which(mpb_slice==1)]
	# plot climate space
	cs_tmn <- c(clm_tmn,vgt_tmn,btl_tmn,mpb_tmn)
	cs_tmx <- c(clm_tmx,vgt_tmx,btl_tmx,mpb_tmx)
	presence <- c(rep("1 climate",length(clm_tmn)),rep("2 core hosts",length(vgt_tmn)),rep("3 mpb in all hosts",length(btl_tmn)),rep("4 mpb in core hosts",length(mpb_tmn)))
	df <- data.frame(cs_tmn,cs_tmx,presence)
	plot1 <- qplot(cs_tmn,cs_tmx, data=df, color=factor(presence), alpha=I(0.7), xlab = "Minimum winter T (°C)", ylab = "Maximum August T (°C)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(values = c("grey","green","blue","red"))+ labs(color="Presence")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot1 <- plot1 + geom_vline(xintercept = -40, color = "red")
	plot1 <- plot1 + geom_hline(yintercept = 18.3, color = "red")
	d=data.frame(x1=c(-32,-32,-48,-48), y1=c(25,13,25,13), lab=c(paste(toString(n1), "%"), paste(toString(n2), "%"), paste(toString(n3), "%"), paste(toString(n4), "%")))
	plot1 <- plot1 + geom_text(data=d, aes(x=x1, y=y1,label=lab), color="black")
	plot2 <- ggplot(df, aes(x=presence, y=cs_tmn,fill=factor(presence)))+geom_boxplot()+scale_fill_manual(values = c("grey","green","blue","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum winter T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df, aes(x=presence, y=cs_tmx,fill=factor(presence)))+geom_boxplot()+scale_fill_manual(values = c("grey","green","blue","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Maximum August T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	png(paste(out,"cs_beetles_chosts_", toString(years[yr]), "_wnt_axt.png", sep = ""), width=12, height=6, units="in", res=300)
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
im.convert(paste(out,"cs_beetles_chosts_20*wnt_axt.png", sep = ""), output = paste(out,"cs_beetles_chosts_wnt_axt.gif", sep=""))