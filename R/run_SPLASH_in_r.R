library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/SPLASH/evap.R")
inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/evapo_na/input"
outpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/evapo_na/output"
setwd(outpath)

rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")

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
daily_totals <- matrix(data=rep(0, 3294), nrow=365, ncol=9)
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

foreach(i=1:d)%dopar%{
	file <- paste0("s",rows$rows[i], "_", rows$lat[i], "_", rows$etopo1[i], "_", years[yr],".csv")
	file_path <- file.path(inpath, years[yr], file)
	
}