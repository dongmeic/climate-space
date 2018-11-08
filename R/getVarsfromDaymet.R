
get.min.data <- function(daily.lows) {
  min20 <- sum(daily.lows <= -20)
  min30 <- sum(daily.lows <= -30)
  min40 <- sum(daily.lows <= -40)
  min <- min(daily.lows)

  list(min20=min20,min30=min30,min40=min40,min=min)
}

get.opt.T <- function(daily.means) {
	OptTsum <- sum(daily.means >= 18 & daily.means <= 30)
	OptTsum
}

get.daily.stats <- function(daily.highs, daily.means, daily.lows) {
	YEAR <- 365
	oct.range <- 274:304
	jan.range <- (1 + YEAR - 1):(31 + YEAR - 1)
	mar.range <- (60 + YEAR - 1):(90 + YEAR - 1)
	AUG <- 213
  aug.range <- (213 + YEAR - 1):(243 + YEAR - 1)
  summer.range <- (152 + YEAR - 1):(243 + YEAR - 1)
  aug.jul.range <- AUG:(AUG + YEAR - 1)
  Octmins <- get.min.data(daily.lows[oct.range])
  Janmins <- get.min.data(daily.lows[jan.range])
  Marmins <- get.min.data(daily.lows[mar.range])
  minT <- min(daily.lows[aug.jul.range])
  AugMax <- max(daily.highs[aug.range])
  maxT <- max(daily.highs[aug.jul.range])
  OptTsum <- get.opt.T(daily.means[summer.range])
  out <- list(Octmins, Janmins, Marmins, minT=minT, AugMax=AugMax, maxT=maxT, OptTsum=OptTsum)
  out <- unlist(out)
  names(out) <- c('Oct20', 'Oct30', 'Oct40', 'OctMin', 
                  'Jan20', 'Jan30', 'Jan40', 'JanMin', 
                  'Mar20', 'Mar30', 'Mar40', 'MarMin', 
                  'minT', 'AugMax', 'maxT', 'OptTsum')
  out
}