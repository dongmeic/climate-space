# Created by Dongmei Chen
# Examine climate data
library(ncdf4)
library(grid)
library(ggplot2)

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
ncfolder <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/"
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

dname <- "tmp"
ncfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.", dname, ".abs3d.nc")
ncin <- nc_open(paste0(ncfolder, ncfile))
var3d <- ncvar_get(ncin, dname)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
var3d[var3d==fillvalue$value] <- NA

years <- c(1901,1901,1902,1902,1902,
		   1902,1902,1903,1903,1903,
		   1903,1903,1904,1904,1904,
		   1904,1904,1905,1905,1905,
		   1905,1905,1906,1906,1906)
months <- rep(c(11,12,1,2,3),5)
seqs <- c(11:15,23:27,35:39,47:51,59:63)
ndf <- data.frame(tmp=double(),time=factor())
for (i in 1:length(seqs)){
    na <- var3d[,,seqs[i]]
    tmp <- na[!is.na(na)]
    time <- rep(paste0(years[i], "_", sprintf("%02d", months[i])),length(tmp))
    df <- data.frame(tmp,time)
    ndf <- rbind(ndf, df)
}
write.csv(ndf, paste0(out, "na10km_tmp.csv"), row.names = FALSE)

dname <- "dtr"
ncfile <- paste0("na10km_v2_cru_ts4.01.1901.2016.", dname, ".abs3d.nc")
ncin <- nc_open(paste0(ncfolder, ncfile))
var3d <- ncvar_get(ncin, dname)
var3d[var3d==fillvalue$value] <- NA

ndf <- data.frame(tmp=double(),time=factor())
for (i in 1:length(seqs)){
    na <- var3d[,,seqs[i]]
    dtr <- na[!is.na(na)]
    time <- rep(paste0(years[i], "_", sprintf("%02d", months[i])),length(dtr))
    df <- data.frame(dtr,time)
    ndf <- rbind(ndf, df)
}
write.csv(ndf, paste0(out, "na10km_dtr.csv"), row.names = FALSE)

df1 <- read.csv(paste0(out, "na10km_tmp.csv"))
df2 <- read.csv(paste0(out, "na10km_dtr.csv"))
p1 <- ggplot(df1, aes(x = time, y = tmp)) +geom_boxplot(colour = "black", outlier.size = 0.75, 
	outlier.shape = 1, outlier.alpha = 0.35)+
    labs(x="Time", y="Mean temperature")+
    ggtitle("Mean temperature in the selected months")    
    
p2 <- ggplot(df2, aes(x = time, y = dtr)) +geom_boxplot(colour = "black", outlier.size = 0.75, 
	outlier.shape = 1, outlier.alpha = 0.35)+
    labs(x="Time", y="Diurnal temperature range")+
    ggtitle("Diurnal temperature range in the selected months")
    
png(paste0(out,"temporal_plots_checking_data.png"), width=14, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1)))
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
dev.off()

print("all done!")