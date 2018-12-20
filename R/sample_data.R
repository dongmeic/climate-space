#library(xlsx)

path <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/SPLASH/"
years <- 1996:2001

dt1996 <- read.csv(paste0(path, "1996/s113614_45.6489179085154_1094_1996.csv"))
dt1997 <- read.csv(paste0(path, "1997/s103319_43.3736358676106_1510_1997.csv"))
dt1998 <- read.csv(paste0(path, "1998/s112813_45.2797853220196_1531_1998.csv"))
dt1999 <- read.csv(paste0(path, "1999/s120131_47.311530211647_722_1999.csv"))
dt2000 <- read.csv(paste0(path, "2000/s100010_41.0407049069973_1565_2000.csv"))
dt2001 <- read.csv(paste0(path, "2001/s112088_45.5013685211166_591_2001.csv"))
dt <- cbind(dt1996[,2:3],dt1997[,2:3],dt1998[,2:3],dt1999[,2:3],dt2000[,2:3],dt2001[,2:3])

aet1996 <- as.numeric(names(read.csv(paste0(path, "1996/ET_s113614_45.6489179085154_1094_1996.csv"),check.names=FALSE)))[1:365]
aet1997 <- as.numeric(names(read.csv(paste0(path, "1997/ET_s103319_43.3736358676106_1510_1997.csv"),check.names=FALSE)))[1:365]
aet1998 <- as.numeric(names(read.csv(paste0(path, "1998/ET_s112813_45.2797853220196_1531_1998.csv"),check.names=FALSE)))[1:365]
aet1999 <- as.numeric(names(read.csv(paste0(path, "1999/ET_s120131_47.311530211647_722_1999.csv"),check.names=FALSE)))[1:365]
aet2000 <- as.numeric(names(read.csv(paste0(path, "2000/ET_s100010_41.0407049069973_1565_2000.csv"),check.names=FALSE)))[1:365]
aet2001 <- as.numeric(names(read.csv(paste0(path, "2001/ET_s112088_45.5013685211166_591_2001.csv"),check.names=FALSE)))[1:365]
aet <- data.frame(aet1996=aet1996,aet1997=aet1997,aet1998=aet1998,aet1999=aet1999,aet2000=aet2000,aet2001=aet2001)

ndf <- as.data.frame(matrix(,ncol=4,nrow=0))
for(year in years){
	df <- as.data.frame(matrix(,ncol=0,nrow=365))
	i <- which(years==year)
	df <- cbind(df, dt[,(2*i-1):(2*i)], aet=aet[,i], year=rep(year,365))
	ndf <- rbind(ndf, df)
	print(year)
}
write.csv(ndf, "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/test/water_deficit.csv", row.names = FALSE)	
	


