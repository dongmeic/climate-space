is.leap.year <- function(year) {
  ifelse(year %% 4 == 0 & year %% 100 != 0, T, F)
}


has.n.consecutive.trues <- function(x, n) {
  x <- x[!is.na(x)]
  if (!(len(x)) {
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


get.single.year.data <- function(year, daily.lows, daily.highs, leap.year.adj=F) {
  leap.year <- is.leap.year(year)
  MAR <- 60
  APR <- 91
  JUN <- 152
  AUG <- 213  
  Lcs.range <- MAR:(APR + 14)
  aug.range <- AUG:(AUG + 30)
  summer.range <- JUN:(AUG + 30)
  if (leap.year & leap.year.adj) {
    Lcs.range <- Lcs.range + 1
    aug.range <- aug.range + 1
    summer.range <- summer.range + 1
  }
  Lcs <- is.coldsnap(daily.lows[Lcs.range])
  maxAugT <- sum(daily.highs[aug.range] >= 18.3)
  summerT40 <- get.summer.t40(daily.highs[summer.range])
  list(Lcs=Lcs, maxAugT=maxAugT, summerT40=summerT40)
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
  ddAugJul <- sum(daily.means[which(above5.5.aug.jul)])
  ddAugJun <- sum(daily.means[which(above5.5.aug.jun)])
  list(ddAugJul=ddAugJul, ddAugJun=ddAugJun)
}


get.min.data <- function(daily.lows) {
  min20 <- sum(daily.lows <= -20)
  min22 <- sum(daily.lows <= -22)
  min24 <- sum(daily.lows <= -24)
  min26 <- sum(daily.lows <= -26)
  min28 <- sum(daily.lows <= -28)
  min30 <- sum(daily.lows <= -30)
  min32 <- sum(daily.lows <= -32)
  min34 <- sum(daily.lows <= -34)
  min36 <- sum(daily.lows <= -36)
  min38 <- sum(daily.lows <= -38)
  min40 <- sum(daily.lows <= -40)

  list(min20=min20, min22=min22, min24=min24, min26=min26, min28=min28, 
       min30=min30, min32=min32, min34=min34, min36=min36, min38=min38, 
       min40=min40)
}


get.two.year.data <- function(start.year, daily.means, daily.lows, leap.year.adj=F) {
  y2.leap <- is.leap.year(start.year + 1)
  OCT <- 62
  NOV <- 93
  DEC <- 123
  FEB <- 185
  winter.range <- DEC:(FEB + 27)
  Ecs.range <- (OCT + 14):(NOV + 29)
  if (y2.leap & leap.year.adj) {
    winter.range <- DEC:(FEB + 28)
  }
  winterTmin <- min(daily.lows[winter.range])
  Ecs <- is.coldsnap(daily.lows[Ecs.range])
  coldsnap.stats <- get.coldsnap.stats(daily.lows[winter.range])
  drop.data <- get.drop.stats(daily.means[winter.range])
  degree.days.data <- get.degree.days(daily.means)
  min.data <- get.min.data(daily.lows[winter.range])
  list(winterTmin=winterTmin, Ecs=Ecs, 
       coldsnap.stats=coldsnap.stats,
       drop.data=drop.data, degree.days.data=degree.days.data, 
       min.data=min.data
  )
}

# get.daily.stats requires 2 years of daily statistics from Jan (t-1) 
# to Dec (t)
# Args:
#   start.year: numeric; year of t-1
#   daily.highs: numeric vector; 730-731 data points of daily highs
#   daily.means: ""      ""      ""  ""   ""     "" ""   ""   means
#   daily.lows:  ""      ""      ""  ""   ""     "" ""   ""   lows
get.daily.stats <- function(start.year, daily.highs, daily.means, daily.lows) {
  t <- start.year + 1
  if (is.leap.year(start.year)) {
  	AUG <- 214
  	YEAR <- 366
  } else {
    AUG <- 213
    YEAR <- 365
  }
  
  # Months Aug (t-1)- Jul (t)
  aug.jul.range <- AUG:(AUG + YEAR - 1)
  # Months Jan (t) - Dec (t)
  jan.dec.range <- (YEAR + 1):(length(daily.highs))
  
  t.data <- get.single.year.data(
    t, daily.lows[jan.dec.range], daily.highs[jan.dec.range]) 
  two.year.data <- get.two.year.data(
    start.year, daily.means[aug.jul.range], daily.lows[aug.jul.range])
  out <- c(unlist(t.data), unlist(two.year.data))
  names(out) <- gsub('coldsnap.stats.|drop.data.|degree.days.data.|min.data.', 
                     '', 
                     names(out))
  out
}


# Test
# simulate daily data for 2 years
#n <- 2*365 # 365 + 366
#days <- 1:n
#x <- (20 * sin((pi*(days + 240)) / (n / 4))) + 10
#x <- x + rnorm(n, 0, 3)
#high <- x + rpois(n, 5)
#low <- x - rpois(n, 5)
#plot(x, type='l', ylim=range(c(low, high)))
#lines(high, col=2)
#lines(low, col=4)

#get.daily.stats(1999, high, x, low)
