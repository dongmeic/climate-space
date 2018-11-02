# Gets degree days for a given temperature threshold and date range.  
#   NOTE: NA values will be omitted.
# @param daily.means: numeric vector: mean daily temperatures
# @param threshold: numeric scalar: temperature threshold
# @param day.range: either 'all' or 'aug.jun'
# @return: numeric scalar = sum of degrees above threshold for all days

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

# Test
daily.means <- rnorm(365, mean=10, sd=10)
thresholds <- c(2.2,   5.5,   10,    15,    5.5)
day.ranges <- c('all', 'all', 'all', 'all', 'aug.jun')
get.degree.days.for.all.thresholds(daily.means, thresholds, day.ranges)
