# visualize time-series climatic variables
library(ncdf4)
library(lattice)
library(RColorBrewer)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=12)
install.packages("BAMMtools", repos='http://cran.us.r-project.org')
library(BAMMtools)
library(animation)
library(rgdal)

tspath <- "/home2/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
shppath <- "/home2/dongmeic/beetle/shapefiles"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/maps/"
ncinfile <- "na10km_v2_ntw_2000.2014.4d.nc"

ca.shp <- readOGR(shppath, "na10km_can_prov", stringsAsFactors=FALSE)
us.shp <- readOGR(shppath, "na10km_us_state", stringsAsFactors=FALSE)
coast.shp <- readOGR(shppath, "na10km_coast", stringsAsFactors=FALSE)

ncin <- nc_open(paste(tspath,ncinfile,sep=""))
print(ncin)
dname <- "ntw"
var4d <- ncvar_get(ncin,dname)
fillvalue <- ncatt_get(ncin,dname,"_FillValue")
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
nc_close(ncin)
var4d[var4d==fillvalue$value] <- NA

years <- 2000:2014
foreach(i=1:15) %dopar%{
	var_slice_4d <- var4d[,,3,i]
	z <- na.omit(var_slice_4d)
	cutpts <- getJenksBreaks(z, 8)
	grid <- expand.grid(x=x, y=y)
	cutpts <- c(-35,-25,-10,-5,0,5,10)
	png(file=paste0(out,"ntw_", years[i], ".png"), width=8, height=6, units="in", res=300)
	p <- levelplot(var_slice_4d ~ x * y, data=grid, at=cutpts, cuts=8, pretty=T, 
			  col.regions=(rev(brewer.pal(7,"RdBu"))), scales = list(draw = FALSE), 
			  margin=F, main=paste0("Minimum winter temperature in ", years[i]))+
			  latticeExtra::layer(sp.polygons(ca.shp, lwd=0.5, col='lightgrey'))+
			  latticeExtra::layer(sp.polygons(us.shp, lwd=0.5, col='lightgrey'))+
			  latticeExtra::layer(sp.polygons(coast.shp, lwd=0.5, col='lightgrey'))
	print(p)
	dev.off()	
}
im.convert(paste0(out,"ntw_*.png"), output = paste0(out,"ntw.gif"))

