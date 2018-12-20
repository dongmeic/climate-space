is.leap.year <- function(year) {
  ifelse(year %% 4 == 0 & year %% 100 != 0, T, F)
}


has.n.consecutive.trues <- function(x, n) {
  x <- x[!is.na(x)]
  if (!(length(x))){
    return (NA)
  }
  max.consec <- 0
  for (val in x) {
    if (val) {
      max.consec <- max.consec + 1
      if (max.consec >= n) {
        return (T)
      }
    } else {
      max.consec <- 0
    }
  }
  F
}


is.coldsnap <- function(daily.lows) {
  candidates <- daily.lows <= -20
  1 * has.n.consecutive.trues(candidates, 4)
}


get.summer.t40 <- function(daily.highs) {
  sum(daily.highs > 40)
}

get.opt.T <- function(daily.means) {
	sum(daily.means >= 18 & daily.means <= 30)
}

get.cv.gsp <- function(daily.prcp){
	gsp.range <- 91:181
	input <- na.omit(daily.prcp[gsp.range])
	sqrt(var(input))/mean(input)
}

get.single.year.data <- function(daily.lows, daily.means, daily.highs) {
  JAN <- 1
  MAR <- 60
  APR <- 91
  JUN <- 152
  AUG <- 213
  jan.range <- JAN:(JAN + 30)
  mar.range <- MAR:(MAR + 30)  
  Lcs.range <- MAR:(APR + 14)
  aug.range <- AUG:(AUG + 30)
  summer.range <- JUN:(AUG + 30)
  Lcs <- is.coldsnap(daily.lows[Lcs.range])
  maxAugT <- sum(daily.highs[aug.range] >= 18.3)
  summerT40 <- get.summer.t40(daily.highs[summer.range])
  OptTsum <- get.opt.T(daily.means[summer.range])
  AugMaxT <- max(daily.highs[aug.range])
  list(Lcs=Lcs, maxAugT=maxAugT, summerT40=summerT40,
  		 OptTsum=OptTsum, AugMaxT=AugMaxT)
}


get.coldsnap.stats <- function(daily.lows) {
  n.coldsnap <- 0
  length.coldsnap <- c()
  below.neg20 <- daily.lows <= -20
  consec.days <- 0
  for (day in below.neg20) {
    if (day) {
      consec.days <- consec.days + 1
    } else if (consec.days > 3){
        n.coldsnap <- n.coldsnap + 1
        length.coldsnap <- c(length.coldsnap, consec.days)
        consec.days <- 0
    } else {
      consec.days <- 0  	
    }
  }
  if (consec.days > 3) {
    n.coldsnap <- n.coldsnap + 1
    length.coldsnap <- c(length.coldsnap, consec.days)
  }
  Acs <- ifelse(length(length.coldsnap), mean(length.coldsnap), 0)
  list(Ncs=n.coldsnap, Acs=Acs)
}


get.drop.stats <- function(daily.means) {
  daily.changes <- diff(daily.means)
  drop0 <- sum(daily.changes >= 0)
  drop5 <- sum(daily.changes < 0 & daily.changes >= -5)
  drop10 <- sum(daily.changes < -5 & daily.changes >= -10)
  drop15 <- sum(daily.changes < -10 & daily.changes >= -15)
  drop20 <- sum(daily.changes < -15 & daily.changes >= -20)
  drop20plus <- sum(daily.changes < -20)
  max.drop <- abs(min(daily.changes))
  list(drop0=drop0, drop5=drop5, drop10=drop10, drop15=drop15, 
       drop20=drop20, drop20plus=drop20plus, max.drop=max.drop)
}


get.degree.days <- function(daily.means) {
  n <- length(daily.means)
  above5.5.aug.jul <- daily.means > 5.5
  above5.5.aug.jun <- above5.5.aug.jul[1:(n-31)]
  ddAugJul <- sum(round(daily.means[above5.5.aug.jul] - 5.5), na.rm=T)
  ddAugJun <- sum(round(daily.means[above5.5.aug.jun] - 5.5), na.rm=T)
  list(ddAugJul=ddAugJul, ddAugJun=ddAugJun)
}


get.min.data <- function(daily.lows) {
  YEAR <- 365
	oct.range <- 274:304
	jan.range <- (1 + YEAR):(31 + YEAR)
	mar.range <- (60 + YEAR):(90 + YEAR)
	winter.range <- 335:(YEAR + 59)
	AUG <- 213
	aug.jul.range <- AUG:(AUG + YEAR - 1)
	Oct20 <- sum(daily.lows[oct.range] <= -20)
  Oct30 <- sum(daily.lows[oct.range] <= -30)
  Oct40 <- sum(daily.lows[oct.range] <= -40)
  Jan20 <- sum(daily.lows[jan.range] <= -20)
  Jan30 <- sum(daily.lows[jan.range] <= -30)
  Jan40 <- sum(daily.lows[jan.range] <= -40)
  Mar20 <- sum(daily.lows[mar.range] <= -20)
  Mar30 <- sum(daily.lows[mar.range] <= -30)
  Mar40 <- sum(daily.lows[mar.range] <= -40)
  winter20 <- sum(daily.lows[winter.range] <= -20)
  winter30 <- sum(daily.lows[winter.range] <= -30)
  winter40 <- sum(daily.lows[winter.range] <= -40)
  OctMin <- min(daily.lows[oct.range])
  JanMin <- min(daily.lows[jan.range])
  MarMin <- min(daily.lows[mar.range])
  winterMin <- min(daily.lows[winter.range])
  minT <- min(daily.lows[aug.jul.range])
  list(Oct20=Oct20, Oct30=Oct30, Oct40=Oct40,
  		 Jan20=Jan20, Jan30=Jan30, Jan40=Jan40,
  		 Mar20=Mar20, Mar30=Mar30, Mar40=Mar40,
  		 winter20=winter20, winter30=winter30, winter40=winter40,
  		 OctMin=OctMin, JanMin=JanMin, MarMin=MarMin,
  		 winterMin=winterMin, minT=minT)
}


get.two.year.data <- function(daily.lows, daily.means, daily.highs) {
  OCT <- 62
  NOV <- 93
  DEC <- 123
  FEB <- 185
  Ecs.range <- (OCT + 14):(NOV + 29)
  winter.range <- DEC:(FEB+27)
  maxT <- max(daily.highs)
  Ecs <- is.coldsnap(daily.lows[Ecs.range])
  coldsnap.stats <- get.coldsnap.stats(daily.lows[winter.range])
  drop.data <- get.drop.stats(daily.means[winter.range])
  degree.days.data <- get.degree.days(daily.means) 
  list(maxT=maxT, Ecs=Ecs,coldsnap.stats=coldsnap.stats,
       drop.data=drop.data, degree.days.data=degree.days.data)
}

# get.daily.stats requires 2 years of daily statistics from Jan (t-1) 
# to Dec (t)
# Args:
#   start.year: numeric; year of t-1
#   daily.highs: numeric vector; 730-731 data points of daily highs
#   daily.means: ""      ""      ""  ""   ""     "" ""   ""   means
#   daily.lows:  ""      ""      ""  ""   ""     "" ""   ""   lows
get.daily.stats <- function(daily.lows, daily.means, daily.highs) {
  AUG <- 213
  YEAR <- 365
  # Months Aug (t-1)- Jul (t)
  aug.jul.range <- AUG:(AUG + YEAR - 1)
  # Months Jan (t) - Dec (t)
  jan.dec.range <- (YEAR + 1):(length(daily.highs))
  
  t.data <- get.single.year.data(
 		daily.lows[jan.dec.range], daily.means[jan.dec.range], daily.highs[jan.dec.range]) 
  two.year.data <- get.two.year.data(
    daily.lows[aug.jul.range], daily.means[aug.jul.range], daily.highs[aug.jul.range])
  min.data <- get.min.data(daily.lows)
  out <- c(unlist(t.data), unlist(two.year.data),unlist(min.data))
  names(out) <- gsub('coldsnap.stats.|drop.data.|degree.days.data.', 
                     '', 
                     names(out))
  out
}


# Test
# simulate daily data for 2 years
#n <- 2*365
#days <- 1:n
#x <- (20 * sin((pi*(days + 240)) / (n / 4))) + 10
#x <- x + rnorm(n, 0, 3)
#high <- x + rpois(n, 5)
#low <- x - rpois(n, 5)
#plot(x, type='l', ylim=range(c(low, high)))
#lines(high, col=2)
#lines(low, col=4)

#get.daily.stats(low, x, high)
