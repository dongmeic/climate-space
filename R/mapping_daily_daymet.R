library(RColorBrewer)
library(classInt)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/daily_climate/Daymet/"
varnms <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", "drop5", "drop10", "drop15",
					 "drop20", "drop20plus", "max.drop", "ddAugJul", "ddAugJun", "min20", "min22", "min24", "min26", "min28", 
					 "min30", "min32", "min34", "min36", "min38", "min40")
roi.shp <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles", layer = "na10km_roi")
roi.xy <- roi.shp@data[,c("x", "y")]

df <- read.csv(paste0(inpath, "daymet_bioclimatic_variables_2015.csv"))
plotvar <- df[,var]
nclr <- 8
plotclr <- brewer.pal(nclr,"BuPu")
class <- classIntervals(plotvar, nclr, style="quantile")
colcode <- findColours(class, plotclr)
plot(roi.xy$x, roi.xy$y, pch=16, cex=0.5, col=colcode)
