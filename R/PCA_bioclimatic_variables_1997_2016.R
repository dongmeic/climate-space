# Created by Dongmei Chen

#inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
#infile <- "bioclimatic_variables_1997_2016.csv"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)
#indata <- read.csv(paste0(inpath,infile))
#dim(indata);head(indata)

#df <- subset(indata, prs=="continent")
#vars <- 1:22
#pca <- princomp(df[, vars], cor=T)
#summary(pca, loadings <- T)

#sink("pca_summary.txt")
#summary(pca)
#sink()
#file.show("pca_summary.txt")

#write.csv(x = unclass(loadings(pca)), file = "pca_loading.csv")
#write.csv(x = unclass(loadings(pca))[,(pca$sdev^2 > 1),drop = FALSE], file = "pca_loading.csv")
#str(pca)

na10km_btl_df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
head(na10km_btl_df)
years <- 1997:2016
ndf <- read.csv(paste0(csvpath,"bioclimatic_values_",years[1],".csv"))
ndf <- cbind(ndf, na10km_btl_df[,c(paste0("prs_",years[1]),"vegetation")])
colnames(ndf)[23:24] <- c("beetles","hosts")
for(i in 2:length(years)){
  df <- read.csv(paste0(csvpath,"bioclimatic_values_",years[i],".csv"))
  df <- cbind(df,na10km_btl_df[,c(paste0("prs_",years[1]),"vegetation")])
  colnames(df)[23:24] <- c("beetles","hosts")
  ndf <- rbind(ndf,df)
  print(paste(years[i], "done!"))
}
ndf <- cbind(ndf, year=unlist(lapply(1997:2016,function(i) rep(i,dim(ndf)[1]/length(1997:2016)))))
write.csv(ndf, paste0(csvpath, "bioclimatic_values_1997_2016.csv"), row.names=FALSE)

vars <- 1:22
pca <- princomp(ndf[, vars], cor=T)
summary(pca, loadings <- T)

sink(paste0(csvpath,"pca_summary.txt"))
summary(pca)
sink()

write.csv(x = unclass(loadings(pca)), file = "pca_loading.csv")

png("PCA_variable_selection.png", width=12, height=9, units="in", res=300)
par(mfrow=c(2, 2))
par(mar=c(4, 4, 3, 3))
for(i in c(1, 3)) {
  plot(pca$scores[, i], 
       pca$scores[, i + 1], 
       col=(ndf$hosts + 1), 
       pch=16,
       cex=0.5,
       xlab=paste('PC', i, sep=''),
       ylab=paste('PC', i + 1, sep=''), 
       main='Host presence')
  legend('bottomleft', pch=16, col=1:2, legend=c('Absent', 'Present'))
  plot(pca$scores[, i], 
       pca$scores[, i + 1], 
       col=(ndf$beetles + 3),
       pch=16,
       cex=0.5,
       xlab=paste('PC', i, sep=''),
       ylab='', 
       yaxt='n',
       main='Beetle presence')
  legend('bottomleft', pch=16, col=3:4, legend=c('Absent', 'Present'))
}
dev.off()

