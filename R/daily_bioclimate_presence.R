# Run in an interactive mode

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"

na10km_btl_df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
head(na10km_btl_df)
years <- 1996:2015

ndf <- read.csv(paste0(inpath,"daily_climate/CRU/CRU_bioclim_var_",years[1],".csv")) # from bioclimatic_values_time_series_combined.R
ndf <- cbind(ndf, na10km_btl_df[,c(paste0("prs_",(years[1]+1)),"vegetation")])
b <- dim(ndf)[2]; a <- b - 1
colnames(ndf)[a:b] <- c("beetles","hosts")

for(i in 2:length(years)){
  df <- read.csv(paste0(inpath,"daily_climate/CRU/CRU_bioclim_var_",years[i],".csv"))
  df <- cbind(df,na10km_btl_df[,c(paste0("prs_",(years[i]+1)),"vegetation")])
  colnames(df)[a:b] <- c("beetles","hosts")
  ndf <- rbind(ndf,df)
  print(paste(years[i], "done!"))
}
crubioclm <- cbind(ndf, year=unlist(lapply(1996:2015,function(i) rep(i,dim(ndf)[1]/length(1996:2015)))))
#write.csv(crubioclm, paste0(inpath, "daily_bioclimatic_values_1996_2015_r.csv"), row.names=FALSE)

#ndf <- read.csv(paste0(inpath, "daily_bioclimatic_values_1996_2015_r.csv"))
vars <- c("ddAugJun", "ddAugJul", "winterTmin", "Acs", "Ecs", "Lcs", "min20", "min22", "min24", "min26", 
					"min28", "min30", "min32", "min34", "min36", "min38", "min40", "maxAugT", "summerT40")
								
ClimDaily <- crubioclm[crubioclm$beetles==1,]
btlClim <- ClimDaily[,vars]
t <- dim(ClimDaily)[1]
hist(btlClim$Ecs)
hist(ClimDaily$maxAugT)
hist(ClimDaily$summerT40)

sum(ClimDaily[,"maxAugT"]>=2)/t
sum(ClimDaily[,"summerT40"]>0)/t

d1 <- vector(); d2 <- vector()
for(i in 1:length(vars)){
	if(i == 1){
		d1[i] <- sum(btlClim[,i]>305) / t		
	}else if(i==2){
		d1[i] <- sum(btlClim[,i]>833)/t	
	}else if(i==3){
		d1[i] <- sum(btlClim[,i]>-40)/t
	}else if(i==4){	
		d1[i] <- sum(btlClim[,i]<=4)/t
	}else if(i==18){
		d1[i] <- sum(btlClim[,i]>2)/t	
	}else if(i==19){
		d1[i] <- sum(btlClim[,i]==0)/t
	}else{
		d1[i] <- sum(btlClim[,i]==0)/t
	}
	d2[i] <- 1 - d1[i]	
}

df <- data.frame(prs=d1, abs=d2, var=vars)
write.csv(df, paste0(inpath, "daily_winter_tmp.csv"), row.names=FALSE)

# read from daymet
years <- 1996:2015
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
drops <- c("drop5", "drop10", "drop15", "drop20", "drop20plus", "max.drop")
mins <- c("min20", "min22", "min24", "min26", "min28", "min30", "min32", "min34", "min36", "min38", "min40")
cs <- c("Lcs", "Ecs", "Ncs", "Acs")
more <- c('Oct20', 'Oct30', 'Oct40', 'OctMin','Jan20', 'Jan30', 'Jan40', 'JanMin',
					'Mar20', 'Mar30', 'Mar40', 'MarMin', 'minT', 'AugMax', 'maxT', 'OptTsum')
vars <- c("maxAugT", "summerT40", "winterTmin", "drop0", drops, "ddAugJul", "ddAugJun", mins)

library(rgdal)
#roi.shp <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles", layer = "na10km_roi")

#ndf <- read.csv(paste0(csvpath,"bioclim_values_Daymet_",years[1],".csv")) # from daymet_combined.R
#ndf <- cbind(ndf, roi.shp@data[,paste0("prs_",(years[1]+1))])
#colnames(ndf)[dim(ndf)[2]] <- "beetles"

for(i in 2:length(years)){
  df <- read.csv(paste0(csvpath,"bioclim_values_Daymet_",years[i],".csv"))
  df <- cbind(df, roi.shp@data[,paste0("prs_",(years[1]+1))])
  colnames(df)[dim(df)[2]] <- "beetles"
  ndf <- rbind(ndf,df)
  print(paste(years[i], "done!"))
}
dmClim <- cbind(ndf, year=unlist(lapply(1996:2015,function(i) rep(i,dim(ndf)[1]/length(1996:2015)))))
write.csv(dmClim, paste0(csvpath, "daymet_bioclim_1996_2015_r.csv"), row.names=FALSE)

ClimDaily <- dmClim[dmClim$beetles==1,]

sink(paste0(csvpath,"bioclim_summary_statistics_daymet.txt"))
summary(ClimDaily)
sink()

vars <- c("ddAugJun", "ddAugJul", "Acs", "Ecs", "Lcs", "Oct20", "Oct30", "Oct40", "OctMin",
					"Jan20", "Jan30", "Jan40", "JanMin", "Mar20", "Mar30", "Mar40", "MarMin",
					"winter20", "winter30", "winter40", "winterMin", "maxAugT", "OptTsum","summerT40")
btlC
#btlClim <- btlClim[complete.cases(btlClim),]
k <- dim(btlClim)[1]
d1 <- vector(); d2 <- vector()
for(i in vars){
	if(i == "ddAugJun"){
		d1[i] <- sum(btlClim[,i]>305) / k		
	}else if(i=="ddAugJul"){
		d1[i] <- sum(btlClim[,i]>833)/k		
	}else if(i=="Acs"){
		d1[i] <- sum(btlClim[,i]<=4)/k	
	}else if(i %in% c("OctMin", "JanMin", "MarMin", "winterMin")){	
		d1[i] <- sum(btlClim[,i]>-40)/k
	}else if(i=="maxAugT"){
		d1[i] <- sum(btlClim[,i]>2)/k	
	}else if(i=="summerT40"){
		d1[i] <- sum(btlClim[,i]==0)/k
	}else if(i=="OptTsum"){
		d1[i] <- sum(btlClim[,i]>=18 & btlClim[,i]<=30)/k
	}else{
		d1[i] <- sum(btlClim[,i]==0)/k
	}
	d2[i] <- 1 - d1[i]	
}
df <- data.frame(prs=d1, abs=d2, var=vars)
write.csv(df, paste0(inpath, "daily_winter_tmp_daymet.csv"), row.names=FALSE)

save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/summary_daily.RData")