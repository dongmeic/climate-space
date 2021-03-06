DATA_DIR <- '~/dongmei/sdm/data/cluster/year/' 

dat.early <- read.csv(paste(DATA_DIR, 'X_test.csv',  sep=''))
y.early   <- read.csv(paste(DATA_DIR, 'y_test.csv',  sep=''))
dat.late  <- read.csv(paste(DATA_DIR, 'X_train.csv', sep='')) 
y.late    <- read.csv(paste(DATA_DIR, 'y_train.csv', sep=''))
early <- cbind(y.early, dat.early)
late  <- cbind(y.late, dat.late)
data <- rbind(early, late)
data <- subset(data, beetle == 1)


get.yearly.data <- function(field, year) {
  data[data$year == year, field]
} 

density.plot <- function(xs, colors, ...) {
  d1 <- density(xs[[1]])
  d2 <- density(xs[[2]])
  plot(d1, col=colors[1], ylim=range(c(d1$y, d2$y), na.rm=T), ...) 
  lines(d2, col=colors[2])
}

names(data)
exclude <- c('beetle', 'year', 'vegetation', 'studyArea', 'x', 'y')
predictors <- names(data)[-which(names(data) %in% exclude)]
length(predictors)

par(mfrow=c(4, 5))
par(mar=c(0, 0, 2, 0))

for (p in predictors) {
  x2000 <- get.yearly.data(p, 2000)
  x2010 <- get.yearly.data(p, 2010)
  density.plot(list(x2000, x2010), 
               colors=c(2, 4), 
               main=p, 
               xaxt='n', 
               yaxt='n', 
               cex.main=0.7)
  if (p == predictors[1]) {
  	legend('topright', lty=1, col=c(2, 4), legend=c(2000, 2010), bty='n')
  }
}

