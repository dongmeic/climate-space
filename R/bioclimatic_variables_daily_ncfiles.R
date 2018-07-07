# Created by Dongmei Chen
# Writing netCDFfiles for daily bioclimatic variables

library(ncdf4)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/getDailyStats.R")

start_year <- 1901; end_year <- 2015; years <- start_year:end_year; nyr <- length(years)

for (i in 1:nyr){
  
}
