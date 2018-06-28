# Created by Dongmei Chen

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
infile <- "bioclimatic_variables_1997_2016.csv"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(out)
indata <- read.csv(paste0(inpath,infile))
dim(indata)
head(indata)

df <- subset(indata, prs=="continent")
vars <- 1:22
pca <- princomp(df[, vars], cor=T)
summary(pca, loadings <- T)

sink("pca_summary.txt")
summary(pca)
sink()
#file.show("pca_summary.txt")

write.csv(x = unclass(loadings(pca)), file = "pca_loading.csv")
#write.csv(x = unclass(loadings(pca))[,(pca$sdev^2 > 1),drop = FALSE], file = "pca_loading.csv")
str(pca)

