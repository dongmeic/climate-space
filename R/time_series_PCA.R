# Created by Damian Satterthwaite-Phillips
# Modified by Dongmei Chen, from PCA_bioclimatic_variables.R

library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

na10km_btl_df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
head(na10km_btl_df)
years <- 1996:2015

pca_plot <- function(df,i){
	png(paste0("PCA_variable_selection_",years[i],".png"), width=12, height=9, units="in", res=300)
	par(mfrow=c(2, 2))
	par(mar=c(4, 4, 3, 3))
	t = 0.01
	for(i in c(1, 3)) {
	  plot(pca$scores[df$hosts == 0, i], 
		   pca$scores[df$hosts == 0, i + 1], 
		   col=rgb(1, 0, 0, t), 
		   pch=16,
		   xlab=paste('PC', i, sep=''),
		   ylab=paste('PC', i + 1, sep=''), 
		   main='Host Tree')
	  points(pca$scores[df$hosts == 1, i],
			 pca$scores[df$hosts == 1, i + 1],
			 pch=16,
			 col=rgb(0, 1, 0, t))
	  legend('bottomleft', pch=16, col=c(2, 3), legend=c('absent', 'present'))
	  plot(pca$scores[df$beetles == 0, i], 
		   pca$scores[df$beetles == 0, i + 1], 
		   col=rgb(1, 0, 1, t), 
		   pch=16,
		   xlab=paste('PC', i, sep=''),
		   ylab='', 
		   yaxt='n',
		   main='Beetle')
	  points(pca$scores[df$beetles == 1, i],
			 pca$scores[df$beetles == 1, i + 1],
			 pch=16,
			 col=rgb(0, 1, 1, t))
	  legend('bottomleft', pch=16, col=c(6, 5), legend=c('absent', 'present'))

	}
	dev.off()
}

foreach(i in 1:length(years)){
  df <- read.csv(paste0(csvpath,"bioclimatic_values_",years[i],".csv"))
  df <- cbind(df,na10km_btl_df[,c(paste0("prs_",(years[i]+1)),"vegetation")])
  colnames(df)[23:24] <- c("beetles","hosts")
  vars <- 1:22
  pca <- princomp(ndf[, vars], cor=T)
  summary(pca, loadings <- T)
	sink(paste0(csvpath,"pca_summary_",years[i],".txt"))
	summary(pca)
	sink()
	write.csv(x = unclass(loadings(pca)), file = paste0(csvpath,"pca_loading_",years[i],".csv"))
  pca_plot(df,i)
	print(paste(years[i], "done!"))
}

print("all done!")