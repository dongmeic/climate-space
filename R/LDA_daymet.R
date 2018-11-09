# Created by Dongmei Chen
# run in an interactive mode

library(MASS)
library(Hmisc)
library(dplyr)
library(ggpubr)
library(car)
library(rcompanion)
library(rgdal)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

if(0){
	data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
	head(data)
	d <- dim(data)[1]/20
	df_monthly <- data[,1:22]
	df_monthly <- df_monthly[, -which(names(df_monthly) %in% c("winterTmin"))]
	write.csv(df_monthly, paste0(inpath, "bioclim_vars_monthly_1996_2015_r.csv"), row.names=FALSE)
	dim(df_monthly)
	roi.shp <- readOGR(dsn="/gpfs/projects/gavingrp/dongmeic/beetle/shapefiles", layer = "na10km_roi")
	rows <- roi.shp@data[,"seq_1_leng"]
	allrows <- c()
	for(i in 1:20){allrows <- c(allrows, rows+(i-1)*d)}
	df_monthly_roi <- df_monthly[allrows,]
	write.csv(df_monthly_roi, paste0(inpath, "bioclim_vars_m_roi_1996_2015_r.csv"), row.names=FALSE)
	df_monthly_roi <- read.csv(paste0(inpath, "bioclim_vars_m_roi_1996_2015_r.csv"))
	dim(df_monthly_roi)
	dmClim <- read.csv(paste0(inpath, "daymet_bioclim_1996_2015_r.csv")) # from daily_bioclimate_presence.R
	bioClim <- cbind(df_monthly_roi, dmClim)
	bioClim$hosts <- roi.shp$hosts
	write.csv(bioClim, paste0(inpath, "bioclim_vars_both_1996_2015_r.csv"), row.names=FALSE)
}
bioClim <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_r.csv"))

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

# add 'max.drop'
ignore <- c('Acs', 'Ecs', 'Lcs', 'Ncs','summerT40', 'drop10','drop15', 'drop20', 'drop20plus',
						'Oct20', 'Oct30', 'Oct40', 'Jan20', 'Jan30', 'Jan40', 'Mar20', 'Mar30', 'Mar40',
						'min20', 'min22', 'min24', 'min26', 'min28', 'min30', 'min32',
						'min34', 'min36', 'min38', 'min40', 'beetles', 'hosts', 'year')

SAMPLES <- 500
best.exps <- c()

for (field in names(bioClim)) {
  if (!(field %in% ignore)) {
    min.x <- min(bioClim[, field],
                 na.rm=T)
    best.exp <- get.best.transform.big(
        list(bioClim), field, SAMPLES, plt=T, time=T)
    cat(field, ': ', best.exp, '\n', sep='')
    best.exps[field] <- best.exp
  }
}

# transform and check the distributions before and after the transformation
par(mfrow=c(2, 2))
for (field in names(bioClim)) {
  if (!(field %in% ignore)) {
    hist(bioClim[, field], main=field, col=4)
      
    min.x <- min(bioClim[, field], na.rm=T)
    bioClim[, field] <- (bioClim[, field] + abs(min.x) + 1)^best.exps[field]
    
    hist(bioClim[, field], main=paste(field, "'", sep=''), col=4)
  }
}

head(bioClim) # all NAs in the year column?
#data$year <- unlist(lapply(1996:2015,function(i) rep(i,dim(ndf)[1]/length(1996:2015))))
#bioClim <- bioClim[, -which(names(bioClim) %in% c("Jan40"))]
write.csv(bioClim, paste0(inpath, "bioclim_vars_both_1996_2015_t.csv"), row.names=FALSE)
#bioClim <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_t.csv"))

dat <- bioClim[,!(names(bioClim) %in% ignore)]
png("histograms_trans_both.png", width=14, height=10, units="in", res=300)
par(mfrow=c(5,7))
for(i in 1:dim(dat)[2]){
	plotNormalHistogram(dat[,i], main=colnames(dat)[i])
	print(i)
}
dev.off()

# correlation matrix
#bioClim <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_t.csv"))

my_data <- scale(dat)
res <- cor(my_data, use = "complete.obs")
sink(paste0(inpath,"CorrMatrix_daymet.txt"))
round(res, 2)
sink()
#res2 <- rcorr(my_data)

# rescale the predictors
dt <- as.data.frame(my_data)
dt$beetles <- bioClim$beetles
# Linear Discriminant Analysis
dt.lda <- lda(beetles ~ GSP + summerP2 + winterTmin + Tvar + PPT + OctMin + drop5 + ddAugJul + maxAugT + max.drop + OptTsum + AugMax + MarTmin, data=dt)
#dt.lda <- lda(beetles ~ ., data=dt)
sink(paste0(inpath,"lda_daymet.txt"))
print(dt.lda)
sink()

print("all done")
#load("/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/LDA_daymet.RData")
#save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/LDA_daymet.RData")
