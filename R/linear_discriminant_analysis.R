# Created by Dongmei Chen
# run in an interactive mode

library(MASS)
library(Hmisc)
library(dplyr)
install.packages("corrplot", repos='http://cran.us.r-project.org')
library(corrplot)
install.packages("PerformanceAnalytics", repos='http://cran.us.r-project.org')
library(PerformanceAnalytics)
install.packages("ggpubr", repos='http://cran.us.r-project.org')
library(ggpubr)
library(car)
if(!require(rcompanion)){install.packages("rcompanion", repos='http://cran.us.r-project.org')}
library(rcompanion)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
head(data)
# remove the "drop" variables
# data <- data[, !(colnames(data) %in% c("drop0", "drop5"))]
# examine the distributions
subsets <- dplyr::sample_n(data, 5000)
png(paste0(out,"histograms.png"), width=18, height=10, units="in", res=300)
par(mfrow=c(5,9))
for(i in 1:43){
	#hist(data[,i])
	plotNormalHistogram(data[,i], main=colnames(data)[i])
	#qqnorm(data[,i])
	#qqline(data[,i])
	#print(ggqqplot(subsets[,i])) # use print function in the loop
	#summary(p1 <- powerTransform(subsets[,i]))
	#ggqqplot(bcPower(subsets[,i], p1$roundlam))
	#hist(bcPower(subsets[,i], p1$roundlam))
	#print(shapiro.test(subsets[,which(colnames(subsets)=="drop0")]))
	print(i)
}
dev.off()

# https://github.com/dongmeic/SDM/blob/master/R/models/logisticModEDA.R.ipynb
# Find the best exponential transform (x' = x^a) or log transform 
# (x' = log(x))that best normalizes the input vector x
get.best.transform <- function(x, 
                               min.exp=-2, 
                               max.exp=2, 
                               steps=100, 
                               min.x=NULL, 
                               include.log=T, 
                               plt=F, 
                               verbose=F) {
  t.string <- 'x'
  if (is.null(min.x)) { min.x <- min(x) }
    
  # Prevent 0 and negative values--not defined for certain tranformations
  if (min(x) <= 0) {
    xt <- x + abs(min.x) + 1
    t.string <- paste('(', t.string, ' + ', abs(min(x)) + 1, ')', sep='')
  } else {
    xt <- x
    t.string <- 'x'
  }
    
  exps <- seq(min.exp, max.exp, length=steps)
  ps <- rep(NA, steps)
    
  for (i in 1:length(exps)) {
    ex <- exps[i]
    ps[i] <- shapiro.test(xt^ex)$p
  }
    
  best.p <- which(ps == max(ps))[1]
  best.exp <- exps[best.p]
  best <- best.exp
    
  if (include.log) {
    if (shapiro.test(log(xt))$p > best.p) {
      t.string <- paste('log', t.string, sep='')
      xt <- log(xt)
      best <- 'log'
    } else {
      t.string <- paste(t.string, round(best.exp, 4), sep='^')
      xt <- xt^best.exp
    }
  }
    
  if (verbose) { cat(t.string, '\n')}
    
  if (plt) {
    plot(ps ~ exps, 
         xlab='exponent', 
         ylab='p (Shapiro-Wilk Test)', 
         type='l', 
         col=2)
    par(mfrow=c(1, 2))
    hist(x, main='x', col=4, xlab='')
    hist(xt, main=t.string, col=4, xlab='')
  }

  list(best=best, x.transform=xt)
}

# To compensate, we will take several random samples of size 5000, and 
# average their transformations
get.best.transform.big <- function(data, field, n.samples, plt=T, time=T) {
    
  start <- Sys.time()
  exps <- rep(NA, n.samples)
    
  for (s in 1:n.samples) {
    if (length(data) == 1) {
      x <- sample(data[[1]][, field], size=5000)
      min.x <- min(data[[1]][, field], na.rm=T)
    } else {
      x <- sample(data[[1]][, field], size=5000)
      min.xs <- rep(NA, length(data))
      for (i in 1:length(data)) {
        min.xs[i] <- min(data[[i]][, field], na.rm=T)
      }
      min.x <- min(min.xs)
    }
    
    x <- x[!is.na(x)]
    exps[s] <- get.best.transform(x, min.x=min.x, include.log=F)$best
  }

  if (plt) { hist(exps, main=field, col=4) }
  if (time) { cat('Time taken:', Sys.time() - start, '\n') }
    
  mean(exps)    
}

ignore <- c('Acs', 'Ecs', 'Lcs', 'Ncs', 'maxAugT', 'summerT40', 'min20',
						'min22', 'min24', 'min26', 'min28', 'min30', 'min32', 'min34',
						'min36', 'min38', 'min40', 'beetles', 'hosts', 'year')

SAMPLES <- 500
best.exps <- c()
par(mfrow=c(1, 1))

for (field in names(data)) {
  if (!(field %in% ignore)) {
    min.x <- min(data[, field],
                 na.rm=T)
    best.exp <- get.best.transform.big(
        list(data), field, SAMPLES, plt=T, time=T)
    cat(field, ': ', best.exp, '\n', sep='')
    best.exps[field] <- best.exp
  }
}

# transform and check the distributions before and after the transformation
par(mfrow=c(2, 2))
for (field in names(data)) {
  if (!(field %in% ignore)) {
    hist(data[, field], main=field, col=4)
      
    min.x <- min(data[, field], na.rm=T)
    data[, field] <- (data[, field] + abs(min.x) + 1)^best.exps[field]
    
    hist(data[, field], main=paste(field, "'", sep=''), col=4)
  }
}
write.csv(paste0(inpath, "bioclimatic_values_1996_2015_t.csv"), row.names=FALSE)

d <- dim(data)[2]
# Remove hosts and year columns
dat <- data[,-(d:(d-1))]; head(dat)
d <- dim(dat)[2]
# rescale the predictors
dt <- cbind(dat[d], scale(dat[,-d]))
# dt$beetles <- as.character(dt$beetles)
# Linear Discriminant Analysis
dt.lda <- lda(beetles ~ ., data=dt)
sink(paste0(inpath,"lda.txt"))
print(dt.lda)
sink()

# Assess the accuracy of the prediction
# percent correct for each category of "beetles"
fit <- lda(beetles ~ ., data=dt, na.action="na.omit", CV=TRUE)
ct <- table(dt$beetles, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# correlation matrix
d <- dim(dat)[2]
my_data <- scale(dat[,-d])
res <- cor(my_data)
sink(paste0(inpath,"CorrMatrix.txt"))
round(res, 2)
sink()
res2 <- rcorr(my_data)

# source: http://www.sthda.com/english/wiki/correlation-matrix-a-quick-start-guide-to-analyze-format-and-visualize-a-correlation-matrix-using-r-software
# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
    )
}
sink(paste0(inpath,"flattenCorrMatrix.txt"))
flattenCorrMatrix(res2$r, res2$P)
sink()

corrplot(res, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

chart.Correlation(my_data, histogram=TRUE, pch=19)

# checking climate space pairs
df <- dt[dt$beetles==1,]
par(mfrow=c(2,4))
plot(df$ddAugJul, df$Acs)
plot(df$ddAugJun,df$min20)
plot(df$AugTmax, df$Tvar)
plot(df$AugTmean, df$Ncs)
plot(df$Tmin, df$GSP)
plot(df$TOctSep, df$Pmean)
plot(df$OctTmin, df$PMarAug)
plot(df$fallTmean, df$PPT)

par(mfrow=c(2,3))
plot(df$ddAugJul, df$Acs)
plot(df$AugTmax, df$Tvar)
plot(df$Tmin, df$GSP)
plot(df$TOctSep, df$PPT)
plot(df$OctTmin, df$PMarAug)
plot(df$fallTmean, df$Pmean)

par(mfrow=c(2,2))
plot(df$ddAugJul, df$GSP)
plot(df$AugTmax, df$Tvar)
plot(df$Tmin, df$PMarAug)
plot(df$TOctSep, df$PPT)


