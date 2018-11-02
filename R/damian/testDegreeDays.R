setwd('~/dongmei/climate-space/R/damian')
source('./getDegreeDays.R')

daily.means <- rnorm(365, mean=10, sd=10)
thresholds <- c(2.2,   5.5,   10,    15,    5.5)
day.ranges <- c('all', 'all', 'all', 'all', 'aug.jun')
get.degree.days.for.all.thresholds(daily.means, thresholds, day.ranges)