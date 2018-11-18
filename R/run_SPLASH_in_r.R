library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/SPLASH/const.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/SPLASH/data.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/SPLASH/evap.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/SPLASH/splash.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/evapo_na/input"
outpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/evapo_na/output"
setwd(outpath)

years <- 1991:2015
rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")
d <- dim(rows)[1]

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	yr <- as.numeric(args[1])
	print(paste('year:', years[yr]))
}

ifelse(!dir.exists(file.path(years[yr])), dir.create(file.path(years[yr])), FALSE)

# initial results
daily_totals <- matrix(data=rep(0, 3285), nrow=365, ncol=9)
daily_totals <- as.data.frame(daily_totals)
names(daily_totals) <- c("ho",   # daily solar irradiation, J/m2
                         "hn",   # daily net radiation, J/m2
                         "qn",   # daily PPFD, mol/m2
                         "cn",   # daily condensation, mm
                         "wn",   # daily soil moisture, mm
                         "ro",   # daily runoff, mm
                         "eq_n", # daily equilibrium ET, mm
                         "ep_n", # daily potential ET, mm
                         "ea_n") # daily actual ET, mm
sink("aux")
foreach(i=1:d)%dopar%{
	file <- paste0("s",rows$rows[i], "_", rows$lat[i], "_", rows$etopo1[i], "_", years[yr],".csv")
	outnm <- paste0(years[yr], "/ET_", file)	
	file_path <- file.path(inpath, years[yr], file)
	mdat <- read_csv(file_path, years[yr])
	mdat$lat_deg <- rows$lat[i]
	mdat$elv_m <- rows$etopo1[i]
	wn <- spin_up(mdat, daily_totals)$wn
	out <- list(eet=c(), pet=c(), aet=c(), wn=c())
	for(j in 1:365){
		daily_data <- run_one_day(mdat$lat_deg, mdat$elv_m, j, mdat$year, wn[j], mdat$sf[j], mdat$tair[j], mdat$pn[j])
		out[['eet']] <- append(out[['eet']], daily_data$eet)
		out[['pet']] <- append(out[['pet']], daily_data$pet)
		out[['aet']] <- append(out[['aet']], daily_data$aet)
		out[['wn']] <- append(out[['wn']], daily_data$wn)
	}
	df <- data.frame(equilET=out$eet, potentialET=out$pet, actualET=out$aet, soilMoisture=out$wn)
	write.csv(df, outnm, row.names = FALSE)
}
sink(NULL)

cat(sprintf("got data for year %d\n", years[yr]))