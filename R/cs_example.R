library(rgdal)
library(ncdf4)
library(lattice)
library(RColorBrewer)
library(ggplot2)

shppath <- "/home2/dongmeic/beetle/shapefiles"
out <- "/home2/dongmeic/beetle/output/20170804/"

mask.pts <- readOGR(dsn=shppath, layer="na10km_mask_pts")
coast <- readOGR(dsn=shppath, layer="na10km_coast")
bb <- readOGR(dsn=shppath, layer="na10km_bb")
png(paste0(out, "maskpts.png"), width=8, height=6, units="in", res=300)
plot(mask.pts, col="#ef8a62", pch=19, cex=0.005)
plot(coast, col="#67a9cf", add=T)
plot(bb, bord="grey", add=T)
dev.off()

ncpath <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/"
ncfile <- "na10km_v2_cru_ts4.00.1999.2014.tmp.abs4d.nc"
ncin <- nc_open(paste0(ncpath, ncfile))
dname <- "tmp"
tmp <- ncvar_get(ncin,dname)
dim(tmp)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
nc_close(ncin)
tmp[tmp==fillvalue$value] <- NA

png(paste0(out, "tmp.png"), width=8, height=6, units="in", res=300)
m<- 12; n <- 16
tmp_3d <- tmp[,,m,n]
grid <- expand.grid(x=x, y=y)
cutpts <- c(-50,-25,-10,-5,0,5,10,15,25,40,50)
levelplot(tmp_3d ~ x * y, data=grid, at=cutpts, cuts=11, pretty=T, 
          col.regions=(rev(brewer.pal(10,"RdBu"))), xlab="", ylab="", scales=list(draw=FALSE))
dev.off()

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
df.t <- read.csv(paste0(path,"climatic_changes_data_tmp_2.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(path,"climatic_changes_data_pre_2.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])

years = 2000:2014; nyr <- length(years)
legendlabs <- c("Land","Trees","Beetles")
cols <- c("grey70", "#1b9e77", "#d95f02")
yr <- 1
df.s <- subset(df, df_yr == years[yr])
# calculate the points in each "space"
tp1 <- length(na.omit(df.s[df.s$df_ntw > -5 & df.s$df_mta > 15 & df.s$df_prs=="mpb",]$df_ntw))
tp2 <- length(na.omit(df.s[df.s$df_ntw > -5 & df.s$df_mta <= 15 & df.s$df_prs=="mpb",]$df_ntw))
tp3 <- length(na.omit(df.s[df.s$df_ntw <= -5 & df.s$df_mta > 15 & df.s$df_prs=="mpb",]$df_ntw))
tp4 <- length(na.omit(df.s[df.s$df_ntw <= -5 & df.s$df_mta <= 15 & df.s$df_prs=="mpb",]$df_ntw))
tpsum <- length(na.omit(df.s[df.s$df_prs=="mpb",]$df_ntw))
n1 <- round(tp1/tpsum*100, digits = 1)
n2 <- round(tp2/tpsum*100, digits = 1)
n3 <- round(tp3/tpsum*100, digits = 1)
n4 <- round(tp4/tpsum*100, digits = 1)

plot1 <- qplot(df_ntw,df_mta, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum winter T (°C)", ylab = "Mean August T (°C)", main = paste("MPB climate space in", toString(years[yr])))
plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)
plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
plot1 <- plot1 + geom_vline(xintercept = -40, color = "black", linetype=4)
plot1 <- plot1 + geom_vline(xintercept = -5, color = "black", linetype=4)
plot1 <- plot1 + geom_hline(yintercept = 15, color = "black", linetype=4)
plot1 <- plot1 + xlim(-55, 28) + ylim(-18,39)
d=data.frame(x1=c(10,10,-20,-20), y1=c(20,10,20,10), lab=c(paste(toString(n1), "%"), paste(toString(n2), "%"), paste(toString(n3), "%"), paste(toString(n4), "%")))
plot1 <- plot1 + geom_text(data=d, aes(x=x1, y=y1,label=lab), color="black")

png(paste0(out, "cs.png"), width=8, height=6, units="in", res=300)
print(plot1)
dev.off()