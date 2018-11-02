library(raster)
inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/"

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

file <- paste0("/gpfs/projects/gavingrp/dongmeic/beetle/text/", "tiles_bb.txt")
tiles <- read.table(file, header = FALSE, sep = "", dec = ".")
