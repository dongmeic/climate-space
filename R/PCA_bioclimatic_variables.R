# Created by Damian Satterthwaite-Phillips
# Modified by Dongmei Chen

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

na10km_btl_df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
head(na10km_btl_df)
years <- 1996:2015

ndf <- read.csv(paste0(csvpath,"bioclim_values_CRU_",years[1],".csv")) # from bioclimatic_values_time_series_combined.R
ndf <- cbind(ndf, na10km_btl_df[,c(paste0("prs_",(years[1]+1)),"vegetation")])
b <- dim(ndf)[2]; a <- b - 1
colnames(ndf)[a:b] <- c("beetles","hosts")

for(i in 2:length(years)){
  df <- read.csv(paste0(csvpath,"bioclim_values_CRU_",years[i],".csv"))
  df <- cbind(df,na10km_btl_df[,c(paste0("prs_",(years[i]+1)),"vegetation")])
  colnames(df)[a:b] <- c("beetles","hosts")
  ndf <- rbind(ndf,df)
  print(paste(years[i], "done!"))
}
ndf <- cbind(ndf, year=unlist(lapply(1996:2015,function(i) rep(i,dim(ndf)[1]/length(1996:2015)))))
write.csv(ndf, paste0(csvpath, "bioclimatic_values_1996_2015_r.csv"), row.names=FALSE)
#ndf <- read.csv(paste0(csvpath, "bioclimatic_values_1996_2015_r.csv"))

svars <- c("GSP", "PMarAug", "summerP0","summerP1", "summerP2", 
           "Pmean","POctSep", "PcumOctSep", "PPT", "ddAugJul", "ddAugJun")
sdf <- sqrt(ndf[, svars])
df <- cbind(sdf, ndf[,-which(colnames(ndf) %in% svars)])
write.csv(df, paste0(csvpath, "bioclimatic_values_1996_2015.csv"), row.names=FALSE)

vars <- c("JanTmin", "MarTmin", "TMarAug", "summerTmean",
          "AugTmean", "AugTmax", "GSP", "PMarAug", "summerP0",
          "OctTmin", "fallTmean", "winterTmin", "Tmin", "Tmean",
          "Tvar", "TOctSep", "summerP1", "summerP2", "Pmean",
          "POctSep", "PcumOctSep", "PPT", "drop0", "drop5",
          "ddAugJul", "ddAugJun", "Acs", "Ecs", "Lcs", "Ncs", "maxAugT", "summerT40",
          "min20", "min22", "min24", "min26", "min28", "min30", "min32", "min34", "min36", "min38", "min40")

head(df)
pca <- princomp(df[,vars], cor=T)
summary(pca, loadings <- T)

sink(paste0(csvpath,"pca_summary.txt"))
summary(pca)
sink()

write.csv(x = unclass(loadings(pca)), file = paste0(csvpath,"pca_loading.csv"))

png("PCA_variable_selection.png", width=12, height=9, units="in", res=300)
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
print("all done")