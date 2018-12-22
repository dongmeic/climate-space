# Created by Dongmei Chen
# Date: 2018/12/22
# Update beetle presence/absence data
# Interactive mode

library(rgdal)

# check previous data
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlprs <- read.csv(paste0(csvpath, "beetle_presence.csv"))
head(btlprs); dim(btlprs)

# check updated presence
path <- "/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles"
btl_fishnet <- readOGR(dsn=path, layer="na_fishnet_presence_beetle_cohosts", stringsAsFactors = FALSE)
head(btl_fishnet@data); dim(btl_fishnet@data)
outdata <- cbind(btlprs[,1:5],btl_fishnet@data[,6:27])
head(outdata); dim(outdata)

write.csv(outdata, paste0(csvpath, "beetle_presence_updated.csv"), row.names=FALSE)