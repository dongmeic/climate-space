# Created by Dongmei Chen
# run in an interactive mode
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/data_transform.R")

library(MASS)
library(Hmisc)
library(dplyr)
library(ggpubr)
library(car)
library(rcompanion)
library(rgdal)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

if(0){
	data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
	head(data)
	d <- dim(data)[1]/20
	df_monthly <- data[,1:22]
	df_monthly <- df_monthly[, -which(names(df_monthly) %in% c("winterTmin"))]
	write.csv(df_monthly, paste0(inpath, "bioclim_vars_monthly_1996_2015_r.csv"), row.names=FALSE)
	dim(df_monthly)
	roi.shp <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles", layer = "na10km_roi")
	rows <- roi.shp@data[,"seq_1_leng"]
	allrows <- c()
	for(i in 1:20){allrows <- c(allrows, rows+(i-1)*d)}
	df_monthly_roi <- df_monthly[allrows,]
	#write.csv(df_monthly_roi, paste0(inpath, "bioclim_vars_m_roi_1996_2015_r.csv"), row.names=FALSE)
	#df_monthly_roi <- read.csv(paste0(inpath, "bioclim_vars_m_roi_1996_2015_r.csv"))
	dim(df_monthly_roi)
	dmClim <- read.csv(paste0(inpath, "daymet_bioclim_1996_2015_r.csv")) # from daily_bioclimate_presence.R
	bioClim <- cbind(df_monthly_roi, dmClim)
	bioClim$hosts <- roi.shp$hosts
	write.csv(bioClim, paste0(inpath, "bioclim_vars_both_1996_2015_r.csv"), row.names=FALSE)
}
bioClim <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_r.csv"))

head(bioClim)
bioClim.t <- get.transformed.dt(bioClim)
write.csv(bioClim.t, paste0(inpath, "bioclim_vars_both_1996_2015_t.csv"), row.names=FALSE)

dat <- bioClim[,!(names(bioClim) %in% ignore)]
png("histograms_trans_both.png", width=14, height=10, units="in", res=300)
par(mfrow=c(5,7))
for(i in 1:dim(dat)[2]){
	plotNormalHistogram(dat[,i], main=colnames(dat)[i])
	print(i)
}
dev.off()

# correlation matrix
#bioClim <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_t.csv"))

# rescale the predictors
my_data <- scale(dat)
res <- cor(my_data, use = "complete.obs")
sink(paste0(inpath,"CorrMatrix_daymet.txt"))
round(res, 2)
sink()
#res2 <- rcorr(my_data)

dt <- as.data.frame(my_data)
dt$beetles <- bioClim$beetles
# Linear Discriminant Analysis
dt.lda <- lda(beetles ~ summerP2 + JanMin + Tvar + PPT + OctMin + drop5 + ddAugJul + maxAugT + max.drop + OptTsum + AugMax + MarTmin + cv.gsp, data=dt)
#dt.lda <- lda(beetles ~ ., data=dt)
sink(paste0(inpath,"lda_daymet_JanMin.txt"))
print(dt.lda)
sink()

print("all done")
#load("/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/LDA_daymet.RData")
#save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/LDA_daymet.RData")
