# Created by Dongmei Chen
# Exploratory data analysis: beetle points over time

library(ncdf4)
library(ggplot2)
library(grid)
library(reshape)

prs_path <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"

vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste0(prs_path, vgt_ncfile))
vgt <- ncvar_get(ncin_vgt,"navgtprs")
btl_ncfile <- "na10km_v2_mpb_presence.nc"
ncin_btl <- nc_open(paste0(prs_path, btl_ncfile))
print(ncin_btl)
btl <- ncvar_get(ncin_btl,"mpb_prs") # with all hosts
mpb <- ncvar_get(ncin_btl,"chosts_mpb_prs") # with all core hosts
nc_close(ncin_btl)
years = 1997:2016; nyr <- length(years)
df <- data.frame(time=years)
btl.t <- vector(); mpb.t <- vector()
for (yr in 1:nyr){
	btl_slice <- btl[,,yr]
	mpb_slice <- mpb[,,yr]
	btl.l <- length(btl_slice[which(btl_slice==1)])
	mpb.l <- length(mpb_slice[which(mpb_slice==1)])
	btl.t <- c(btl.t, btl.l); mpb.t <- c(mpb.t, mpb.l)
	print(years[yr])
}
df <- cbind(df, btl.t, mpb.t)
write.csv(df, paste0(out, "beetle_grids_time.csv"))
df2 <- melt(data = df, id.vars = "time")
rect <- data.frame(xmin=2005, xmax=2011, ymin=-Inf, ymax=Inf)
png(paste0(out,"beetle_gridcells.png"), width=8, height=4, units="in", res=300)
ggplot(data = df2, aes(x = time, y = value, colour = variable)) + geom_line(size=1.2)+ 
  labs(x="Year", y="Number of beetle grid cells")+ geom_point(size=2) + 
  scale_colour_manual(name="Beetle", labels=c("All hosts", "Core hosts"), values = c("red","green"))+
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)
dev.off()