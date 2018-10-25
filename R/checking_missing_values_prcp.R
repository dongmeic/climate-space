# Created by Dongmei Chen
# Checking missing values in Daymet

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)
start_year <- 1995; end_year <- 2015; years <- start_year:end_year; nt <- length(years)

print("calculating the biocliamtic variables using daily data")
dim1 <- 77369; dim2 <- nt

ptm <- proc.time()
for(i in 1:nt){
	print(paste("checking", years[i]))
	prcp.df <- read.csv(paste0(inpath, "prcp/prcp", years[i],".csv"))
	k <- 0; n <- 0; l <- 0
	ptm <- proc.time()
	for(j in 1:dim1){
		prcp <- as.numeric(prcp.df[j,3:367])
		if(sum(is.na(prcp))==0){
			k <- k + 1
		}else if(sum(is.na(prcp))!=0){
			print(paste("In prcp, the point of x(", prcp.df[j,1], ") and y(", prcp.df[j,2], ")", "is not available and prcp has", sum(is.na(prcp)), "NA values"))
			n <- n + 1
			if(sum(is.na(prcp))!=365){
				print(which(is.na(prcp)))
			}
		}else{
			if(sum(is.na(prcp))==365){
				print(paste("In prcp, the point of x(", prcp.df[j,1], ") and y(", prcp.df[j,2], ")", "is completely not available"))
				l <- l + 1
			}		
		}
	}
	print(paste("there are", k, "grid cells without NA values"))
	print(paste("there are", n, "grid cells with NA values in prcp"))
	print(paste("there are", l, "grid cells with 365 NA values in prcp"))
	print(paste("checked", years[i]))
	proc.time() - ptm
}
proc.time() - ptm
print("all done!")