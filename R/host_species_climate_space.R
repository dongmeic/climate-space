library(ncdf4)
library(ggplot2)
library(grid)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/data_transform.R")

library(RColorBrewer)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlstat <- read.csv(paste0(csvpath, "beetle_presence_statistics.csv"))
btlstat <- btlstat[rows$rows,]
indata <- get_data()
indata$hosts <- ifelse(indata$beetles==1 & indata$hosts==0, 1, indata$hosts)
indata$prs <- indata$beetles + indata$hosts

ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncfile <- paste0(ncpath, "prs/na10km_v2_presence_pines.nc")
ncin_vgt <- nc_open(ncfile)
print(ncin_vgt)

na10km <- nc_open(paste0(ncpath, "na10km_v2.nc"))
landmask <- ncvar_get(na10km, "landmask")
fillvalue <- ncatt_get(na10km, "landmask", "_FillValue")$value
landmask[landmask==fillvalue] <- NA

read.vgt.data <- function(var){
	vgt <- ncvar_get(ncin_vgt,var)
	vgt[is.na(landmask)] <- NA
	v <- vgt[!is.na(vgt)]
	v[rows$rows]
}

vars <- c("najckprs", "naldpprs", "napdrprs", "nawbkprs")
varnms <- c("Jack pine", "Lodgepole pine", "Ponderosa pine", "Whitebark pine")

df <- data.frame(matrix(NA, nrow = 210748, ncol = 0))
for(var in vars){
  v <- read.vgt.data(var)
  df <- cbind(df, v)
  colnames(df)[dim(df)[2]] <- var
}

host_cs_plot <- function(var){
	ndf <- cbind(indata[,c("JanTmin", "summerP0", "beetles")], var=rep(df[,var],20))
	labs <- colnames(ndf)[1:2]
	colnames(ndf)[1:2] <- c("var1", "var2")
	ndf <- ndf[ndf$var==1,]
	#df1 <- ndf[sample(nrow(ndf), 500),]
	p <- ggplot(ndf, aes(x=var1, y=var2, z = beetles))
	p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
									panel.background = element_blank(), axis.line = element_line(colour = "black"))
	p <- p + theme(legend.position="none")
	p <- p + stat_density_2d(geom = "point", aes(alpha=..density..,size = ..density..), n=10, contour = FALSE)
	p <- p + stat_summary_hex(bins=30,colour=rgb(1,1,1,0),fun=function(x) sum(x)/length(x))
	p <- p + scale_fill_gradient(low=rgb(0.8,0.8,0.8,0.8), high=rgb(0.5,0,0,0.8), limits = c(0, 1))
	p <- p + xlim(range(ndf$var1)) + ylim(range(ndf$var2))
	p <- p + labs(x=labs[1], y=labs[2], title=varnms[which(vars==var)])
	return(p)	
}

n1 <- rep(c(1,2),2); n2 <- c(rep(1,2),rep(2,2))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
png(paste0(out, "cs_host_union.png"), width=6, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 2)))
for(var in vars){
	i <- which(vars==var)
	p <- host_cs_plot(var)
	print(p, vp = vplayout(n2[i], n1[i]))
}
dev.off()