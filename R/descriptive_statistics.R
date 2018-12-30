# view statistics

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
ndf <- ndf[, -which(names(ndf) %in% c("winterTmin.1"))]
write.csv(ndf, paste0(csvpath, "bioclimatic_values_1996_2015_r.csv"), row.names=FALSE)

df <- ndf[ndf$beetles == 1,]

sink(paste0(csvpath,"bioclim_summary_statistics.txt"))
summary(df)
sink()
