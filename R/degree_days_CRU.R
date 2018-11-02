# Created by Dongmei Chen
# Get degree day variables (corrections in bioclimatic_variables_daily.R)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/getDailyStatsfromMonthly.R")

# 1 - run in bash; 0 - run in R
if(1){
	args <- commandArgs(trailingOnly=T)
	print(paste('args:', args))
	print("Starting...")
	i <- as.numeric(args[1])
	print(paste('i:', i))
}

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/monthly_mean"
setwd(inpath)
start_year <- 1996; end_year <- 2016; years <- start_year:end_year; nt <- length(years)

print("calculating degree days using monthly data")
dim1 <- 277910; dim2 <- nt

thresholds <- c(2.2,   5.5,   10,    15,    5.5)
day.ranges <- c('all', 'all', 'all', 'all', 'aug.jun')

get.degree.days <- function(daily.means, threshold, day.range='all', ...) {
  if (!(day.range %in% c('all', 'aug.jun'))) {
    stop('day.range can only be "all" or "aug.jun"')
  }
  n <- length(daily.means)
  if (day.range == 'aug.jun') daily.means <- daily.means[1:(n - 31)]
  
  days.above.threshold <- round(daily.means[daily.means > threshold] - threshold - 0.5)
  sum(days.above.threshold, na.rm=T)
}

# Get degree days for all thresholds and date ranges given
get.degree.days.for.all.thresholds <- function(
    daily.means, thresholds, day.ranges) {
  if (length(thresholds) != length(day.ranges)) {
    stop('"thresholds" and "day.ranges" must have same length')
  }
  out <- numeric(length(thresholds))
  for (i in 1:length(thresholds)) {
  	out[i] <- get.degree.days(daily.means, thresholds[i], day.ranges[i])
  }
  names(out) <- paste0("thres",thresholds, day.ranges)
  out
}

ptm <- proc.time()
#for(i in 1:(nt-1)){
indata1 <- read.csv(paste0("na10km_v2_monthly_mean_",years[i],".csv"))
indata2 <- read.csv(paste0("na10km_v2_monthly_mean_",years[i+1],".csv"))
indata <- rbind(indata1, indata2)

y2.leap <- is.leap.year(i+1)
n.days <- ifelse(y2.leap, 366, 365)

AUG <- 8
YEAR <- 12

aug.jul.range <- AUG:(AUG + YEAR - 1)

df <- data.frame(thres2.2all=double(), thres5.5all=double(), thres10all=double(), thres15all=double(), thres5.5aug.jun=double()) 
for(j in 1:dim1){
	df.j <- indata[j,]
	for(m in 1:23){
		df.m <- rbind(df.j, indata[j+dim1*m,])
		df.j <- df.m
		#print(m)
	}
	monthly.means <- df.m[aug.jul.range]
	daily.means <- get.daily.from.monthly(monthly.means, n.days)
	df[j,] <- get.degree.days.for.all.thresholds(daily.means, thresholds, day.ranges)
}
print(paste("got data from", years[i+1]))
write.csv(df, paste0("daily_climate/CRU/degree_days_CRU_",years[i+1],".csv"), row.names = FALSE)  
#}
proc.time() - ptm
print("all done!")