
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
#data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
vars <- c("drop0", "drop5", "ddAugJul", "ddAugJun", "Acs", "Ecs", "Lcs", "Ncs", "maxAugT", "summerT40", 
					"min20", "min22", "min24", "min26", "min28", "min30", "min32", "min34", "min36", "min38", "min40")
vars1 <- c("beetles", "hosts", "year")
vars2 <- c("Acs", "Ecs", "Lcs", "Ncs", "min20", "min22", "min24", "min26", "min28", 
						"min30", "min32", "min34", "min36", "min38", "min40", "maxAugT", "summerT40")					
#dailyClim <- data[,c(vars, vars1)]
#write.csv(dailyClim, paste0(inpath, "daily_bioclimatic_values_1996_2015_r.csv"), row.names=FALSE)
dailyClim <- read.csv(paste0(inpath, "daily_bioclimatic_values_1996_2015_r.csv"))

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