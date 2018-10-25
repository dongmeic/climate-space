# Dongmei Chen
# Corrected missing values in daymet data tables

library(raster)
inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

# test
tiles <- 12626
years <- 2008
vtype <- "prcp"
doy <- 257
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
correction <- function(years, vtype, doy, tiles){
	for(y in years){
		pdf(paste0(out,y,"_raster_correction.pdf"), width=12, height=6)
		for(i in doy){
			file <- paste0(vtype, substrRight(as.character(y), 2), formatC(i, width = 3, format = "d", flag = "0"), ".tif")
			r1 <- raster(paste0(inpath, "/", y, "/", vtype, "/", file))
			par(mfrow=c(1,2),mar=c(2,2,2,5))
			plot(r1, main=paste(vtype, y, i, "before"))
			for(tile in tiles){
				r2 <- raster(paste0(inpath, y, "/", tile, "/", vtype, "/", vtype, "_", i, ".tif"))
				r2 <- projectRaster(r2, crs = crs(r1), method = 'bilinear')
				s <- raster(res=res(r1), ext=extent(r2))
				r2 <- resample(r2, s, method='bilinear', format="GTiff")
				r1 <- merge(r1, r2, tolerance = 0.5)
				print(paste(y, i, tile))	
			}
			plot(r1, main=paste(vtype, y, i, "after"))
			writeRaster(r1, filename=paste0(inpath, "/", y, "/", vtype, "/", file), datatype='INT4S', overwrite=TRUE)
			print(paste("wrote", vtype, y, i))
		}	
		print(paste("finished", y))
		dev.off()
	}
}

correction(years, vtype, doy, tiles)
print("all done!")

# checking corrected data
if(0){ # check single day
	vtype <- "tmin"; y <- 2011; i <- 309
	file <- paste0(vtype, substrRight(as.character(y), 2), formatC(i, width = 3, format = "d", flag = "0"), ".tif")
	r <- raster(paste0(inpath, "/", y, "/", vtype, "/", file))
	par(mfrow=c(1,1),mar=c(2,2,2,5))
	plot(r, main=paste(y, vtype, i))
}

if(0){ # check one year
	doy <- 1:365
	out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
	vtype <- "prcp"; y <- 2003
	ptm <- proc.time()
	pdf(paste0(out,y,"_",vtype,"_raster_correction.pdf"), width=12, height=6)
	par(mfrow=c(1,2),mar=c(2,2,2,5))	
	for(i in doy){	
		file <- paste0(vtype, substrRight(as.character(y), 2), formatC(i, width = 3, format = "d", flag = "0"), ".tif")
		r <- raster(paste0(inpath, "/", y, "/", vtype, "/", file))	
		plot(r, main=paste(y, vtype, i))
		print(i)
	}
	dev.off()
	proc.time() - ptm
}


