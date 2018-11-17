# calculate water deficit, vapor pressure deficit, cumulative climatic water deficit, 
# moisture index, Priestley-Taylor Coefficient
# input data is daily air temperature, precipitation, vapor pressure, evapotranspiration

JAN <- 1:31; FEB <- 32:59; MAR <- 60:90; APR <- 91:120;
MAY <- 121:151; JUN <- 152:181; JUL <- 182:212; AUG <- 213:243;
SEP <- 244:273; OCT <- 274:304; NOV <- 305:334; DEC <- 335:365;
YEAR <- 365

doy <- c(1,32,60,91,121,152,182,213,244,274,305,335)
#mdays <- c(31,28,31,30,31,30,31,31,30,31,30,31)
mdays <- c(31,30,31,30,31,31,28,31,30,31,30,31)


monthly.df <- function(dt, fun){
	#df <- data.frame(var=var.v,month=unlist(sapply(1:12, function(x) rep(x, mdays[x]))))
	aug.jul.range <- doy[8]:(doy[8]+YEAR-1)
	var.v <- dt[aug.jul.range]
	df <- data.frame(var=var.v,month=unlist(sapply(c(8:12,1:7), function(x) rep(x, mdays[x]))))
	aggregate(var ~ month, data = df, fun)
}


wd <- function(prcp, aet, tmp){
	Tmean_above_zero <- monthly.df(tmp, mean)$var>0
	evapo_sum <- sum(monthly.df(aet,sum)[Tmean_above_zero,2])
	sum(prcp)-evapo_sum
}

vpd <- function(tmp, vp){
	tmp <- tmp[c(MAY, JUN, JUL, AUG, SEP, OCT)]
	vp <- vp[c(MAY, JUN, JUL, AUG, SEP, OCT)]
	svp <- 610.7*10^((7.5*tmp)/(237.3+tmp))
	sum(svp - vp)/(10-5+1)
}

mi <- function(prcp, aet){
	sum(prcp)/sum(aet)
}

cwd <- function(pet, aet){
	pet <- pet[c(MAY, JUN, JUL, AUG, SEP, OCT)]
	aet <- aet[c(MAY, JUN, JUL, AUG, SEP, OCT)]
	sum(pet-aet)/(10-5+1)
}

pt.coef <- function(eet,aet){
	(sum(aet)/12)/(sum(eet)/12)
}

# Test
# simulate daily data
#test <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/test/water_deficit.csv")
#var.v <- subset(test, year==1996)[,1]
#vp <- read.csv("/gpfs/projects/gavingrp/dongmeic/daymet/datatable_na/vp/vp_1996.csv")
#df <- cbind(subset(test, year==1996)[,1:3], vp=as.numeric(vp[113614,]))
#write.csv(df, "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/test/vpd.csv", row.names = FALSE)