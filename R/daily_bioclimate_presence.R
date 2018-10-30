
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
#data <- read.csv(paste0(inpath, "daymet_bioclimatic_variables_1996_2015_r.csv"))
vars <- c("drop0", "drop5", "ddAugJul", "ddAugJun", "Acs", "Ecs", "Lcs", "Ncs", "maxAugT", "summerT40", 
					"min20", "min22", "min24", "min26", "min28", "min30", "min32", "min34", "min36", "min38", "min40")
vars1 <- c("beetles", "hosts", "year")
vars2 <- c("Acs", "Ecs", "Lcs", "Ncs", "min20", "min22", "min24", "min26", "min28", 
						"min30", "min32", "min34", "min36", "min38", "min40", "maxAugT", "summerT40")					
#dailyClim <- data[,c(vars, vars1)]
#write.csv(dailyClim, paste0(inpath, "daily_daymet_bioclimatic_variables_1996_2015_r.csv"), row.names=FALSE)
dailyClim <- read.csv(paste0(inpath, "daily_daymet_bioclimatic_variables_1996_2015_r.csv"))

ClimDaily <- dailyClim[dailyClim$beetles==1,]
btlClim <- ClimDaily[,vars2]
n <- dim(ClimDaily)[1]
hist(btlClim$Ecs)
hist(ClimDaily$maxAugT)
hist(ClimDaily$summerT40)

sum(ClimDaily[,"maxAugT"]>=30)/n
sum(ClimDaily[,"summerT40"]>0)/n

d1 <- vector(); d2 <- vector()
for(i in 1:length(vars2)){
	if(i > 15){
		d2[i] <- sum(btlClim[,i]==0)/n
		d1[i] <- (n - sum(btlClim[,i]==0))/n	
	}else{
		d1[i] <- sum(btlClim[,i]==0)/n
		d2[i] <- (n - sum(btlClim[,i]==0))/n	
	}
}

df <- data.frame(warm=d1, cold=d2, var=vars2)
write.csv(df, paste0(inpath, "daily_winter_tmp.csv"), row.names=FALSE)

# read from daymet
years <- 1996:2015
drops <- c("drop5", "drop10", "drop15", "drop20", "drop20plus", "max.drop")
mins <- c("min20", "min22", "min24", "min26", "min28", "min30", "min32", "min34", "min36", "min38", "min40")
vars <- c("Lcs", "maxAugT", "summerT40", "winterTmin", "Ecs", "Ncs", "Acs", "drop0", drops, "ddAugJul", "ddAugJun", mins)
roi.shp <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles", layer = "na10km_roi")
roi.xy <- roi.shp@data[,c("x", "y")]

ndf <- read.csv(paste0(csvpath,"daymet_bioclimatic_variables_",years[1],".csv")) # from daymet_bioclimatic_variables_time_series_combined.R
ndf <- cbind(ndf, na10km_btl_df[,c(paste0("prs_",(years[1]+1)),"vegetation")])
b <- dim(ndf)[2]; a <- b - 1
colnames(ndf)[a:b] <- c("beetles","hosts")

for(i in 2:length(years)){
  df <- read.csv(paste0(csvpath,"daymet_bioclimatic_variables_",years[i],".csv"))
  df <- cbind(df,na10km_btl_df[,c(paste0("prs_",(years[i]+1)),"vegetation")])
  colnames(df)[a:b] <- c("beetles","hosts")
  ndf <- rbind(ndf,df)
  print(paste(years[i], "done!"))
}
ndf <- cbind(ndf, year=unlist(lapply(1996:2015,function(i) rep(i,dim(ndf)[1]/length(1996:2015)))))
write.csv(ndf, paste0(csvpath, "daymet_bioclim_1996_2015_r.csv"), row.names=FALSE)