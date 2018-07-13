# @param cloud.coverage: numeric value between 0 and 100 (= % coverage)
get.percent.sunshine <- function(cloud.coverage) {
  if (cloud.coverage == 0) {
  	sun.percent <- runif(1, 0.95, 1)
  } else if (cloud.coverage <= 75) {
  	sun.percent <- 0.95 - (cloud.coverage / 125)
  } else if (cloud.coverage <= 87.5) {
  	sun.percent <- 0.35 - (((cloud.coverage / 1.25) - 60) / 50)
  } else {
  	sun.percent <- 0.15 - (((cloud.coverage / 1.25) - 70) / 100)
  }
  100 * sun.percent
}

# NOTE: To apply the function to a vector of cloud.coverage values, e.g.: use:
cloud.coverage.vector <- c(72, 47, 0, 0, 66)

# ...you can use:
unlist(lapply(cloud.coverage.vector, get.percent.sunshine))