# Created by Dongmei Chen
# Checking missing values in Daymet

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)
start_year <- 2009; end_year <- 2009; years <- start_year:end_year; nt <- length(years)

print("calculating the biocliamtic variables using daily data")
dim1 <- 77369; dim2 <- nt

ptm <- proc.time()
for(i in 1:nt){
	print(paste("checking", years[i]))
	tmax.df <- read.csv(paste0(inpath, "tmax/tmax", years[i],".csv"))
	tmin.df <- read.csv(paste0(inpath, "tmin/tmin", years[i],".csv"))
	k <- 0; m <- 0; n <- 0; l <- 0
	ptm <- proc.time()
	for(j in 1:dim1){
		tmx <- as.numeric(tmax.df[j,3:367])
		tmn <- as.numeric(tmin.df[j,3:367])
		if(sum(is.na(tmx))==0 && sum(is.na(tmn))==0){
			#print(paste("no missing values in the point of x(", tmax.df[j,1], ") and y(", tmax.df[j,2], ")"))
			k <- k + 1
		}else if(sum(is.na(tmx))==0 && sum(is.na(tmn))!=0){
			print(paste("In tmin, the point of x(", tmin.df[j,1], ") and y(", tmin.df[j,2], ")", "is not available and tmn has", sum(is.na(tmn)), "NA values"))
			m <- m + 1
			if(sum(is.na(tmn))!=365){
				print(which(is.na(tmn)))
			}
		}else if(sum(is.na(tmx))!=0 && sum(is.na(tmn))==0){
			print(paste("In tmax, the point of x(", tmax.df[j,1], ") and y(", tmax.df[j,2], ")", "is not available and tmx has", sum(is.na(tmx)), "NA values"))
			n <- n + 1
			if(sum(is.na(tmx))!=365){
				print(which(is.na(tmx)))
			}
		}else{
			if(sum(is.na(tmx))==365 && sum(is.na(tmn))==365){
				print(paste("In tmax and tmin, the point of x(", tmax.df[j,1], ") and y(", tmax.df[j,2], ")", "is not available"))
				l <- l + 1
			}		
		}
	}
	print(paste("there are", k, "grid cells without NA values"))
	print(paste("there are", m, "grid cells with NA values in tmin"))
	print(paste("there are", n, "grid cells with NA values in tmax"))
	print(paste("there are", l, "grid cells with NA values in tmax and tmin"))
	print(paste("checked", years[i]))
	proc.time() - ptm
}
proc.time() - ptm
print("all done!")