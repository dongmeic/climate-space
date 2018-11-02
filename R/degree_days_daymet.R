# Created by Dongmei Chen
# Get degree day variables (corrections in bioclimatic_variables_daymet.R)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)
start_year <- 1995; end_year <- 1996; years <- start_year:end_year; nt <- length(years)

print("calculating degree days using daily data")
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
for(y in 1:(nt-1)){
	tmean.df.1 <- read.csv(paste0(inpath, "tmean/tmean", years[y],".csv"))
	tmean.df.2 <- read.csv(paste0(inpath, "tmean/tmean", years[y+1],".csv"))
  
  AUG <- 213
  YEAR <- 365
  aug.jul.range <- AUG:(AUG + YEAR - 1)
  
	df <- data.frame(thres2.2all=double(), thres5.5all=double(), thres10all=double(), thres15all=double(), thres5.5aug.jun=double()) 
	for(j in 1:dim1){
		tmp <- c(as.numeric(tmean.df.1[j,3:367]), as.numeric(tmean.df.2[j,3:367]))
		daily.means <- tmp[aug.jul.range]
		df[j,] <- get.degree.days.for.all.thresholds(daily.means, thresholds, day.ranges)
	}
	print(paste("got data from", years[y+1]))
	write.csv(df, paste0("daily_climate/Daymet/degree_days_daymet_",years[y+1],".csv"), row.names = FALSE)  
}
proc.time() - ptm
print("all done!")