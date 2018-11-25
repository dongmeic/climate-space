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

ignore <- c('Acs', 'Ecs', 'Lcs', 'Ncs','summerT40', 'drop10','drop15', 'drop20', 'drop20plus',
							'Oct20', 'Oct30', 'Oct40', 'Jan20', 'Jan30', 'Jan40', 'Mar20', 'Mar30', 'Mar40',
							'winter20', 'winter30', 'winter40', 'beetles', 'hosts', 'year', 'prs')
							
get.best.exps <- function(df){
	SAMPLES <- 500
	best.exps <- c()
	for (field in names(df)) {
		if (!(field %in% ignore)) {
			min.x <- min(df[, field],
									 na.rm=T)
			best.exp <- get.best.transform.big(
					list(df), field, SAMPLES, plt=T, time=T)
			cat(field, ': ', best.exp, '\n', sep='')
			best.exps[field] <- best.exp
		}
	}
	outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
	write.csv(best.exps, paste0(outpath, "best_exps_transform.csv"), row.names=FALSE)
	return(best.exps)
}

# transform and check the distributions before and after the transformation
get.transformed.dt <- function(df){
	best.exps <- get.best.exps(df)
	outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
	pdf(paste0(outpath, "distributions_transform.pdf"), width=12, height=6)
	par(mfrow=c(1, 2))
	for (field in names(df)) {
		if (!(field %in% ignore)) {
			hist(df[, field], main=field, col=4)
			
			min.x <- min(df[, field], na.rm=T)
			df[, field] <- (df[, field] + abs(min.x) + 1)^best.exps[field]
		
			hist(df[, field], main=paste(field, "'", sep=''), col=4)
		}
	}
	dev.off()
	return(df)
}

# This gets the cumulative mean for a single variable:
get.cumulative.mean <- function(
    variable, dt, q, max.iter=5000, n.samples=5000, threshold=0.01, 
    use.threshold=T) {
  if (use.threshold) {
    v <- dt[, variable]
    allowable.variance <- threshold * (max(v, na.rm=T) - min(v, na.rm=T))
  } else { allowable.variance <- Inf }
  qv <- cum.mean <- rep(NA, max.iter)
  for (i in 1:max.iter) {
    if (i > 100 & i %% 100 == 0) {
      last100 <- cum.mean[(i - 101):(i - 1)]
      if (use.threshold & var(last100) <= allowable.variance) {
        return (cum.mean)
      }
    }
    s1 <- sample(dt[dt$peak == 1, ][, variable], n.samples)
    s2 <- sample(dt[dt$peak == 0, ][, variable], n.samples)
    q1 <- as.numeric(quantile(s1, q))
    q2 <- as.numeric(quantile(s2, q))
    qv[i] <- q1 - q2
    cum.mean[i] <- mean(qv[1:i])
  }
  cum.mean
}
 
# This stores the cum.means for all variables in a matrix (each column is a variable)    	
get.all.cumulative.means <- function(
    vars, dt, q, max.iter=5000, n.samples=5000, threshold=0.01, use.threshold=T) {
  cum.means <- matrix(NA, max.iter, length(vars))
  for (v in 1:length(vars)) {
    cum.means[, v] <- get.cumulative.mean(
      vars[v], dt, q, max.iter, n.samples, threshold, use.threshold)
  }
  colnames(cum.means) <- vars
  cum.means
}

get.index.of.last.finite.value <- function(x) {
  max(which(!is.na(x)))
}

get.sample.sizes.needed <- function(cum.means) {
  apply(cum.means, 2, get.index.of.last.finite.value)
}

get.diff.matrix <- function(dt, var, iter){
	taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
	df1 <- as.data.frame(matrix(,ncol=0,nrow=7))
	df2 <- as.data.frame(matrix(,ncol=0,nrow=7))
	df3 <- as.data.frame(matrix(,ncol=0,nrow=7))
	for(i in 1:iter){
		s1 <- sample(dt[dt$peak==1,][,var],5000)
		s2 <- sample(dt[dt$peak==0,][,var],5000)
		q1 <- as.numeric(quantile(s1, taus))
		q2 <- as.numeric(quantile(s2, taus))
		q3 <- q1 - q2
		df1 <- rbind(df1, q1)
		df2 <- rbind(df2, q2)
		df3 <- rbind(df3, q3)
		print(paste(var, i))
	}
	names(df1) <- paste0("t", taus)
	names(df2) <- paste0("t", taus)
	names(df3) <- paste0("t", taus)
	outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
	write.csv(df1, paste0(outpath, "quantile/", var, "_peak.csv"), row.names=FALSE)
	write.csv(df2, paste0(outpath, "quantile/", var, "_nonpeak.csv"), row.names=FALSE)
	write.csv(df3, paste0(outpath, "quantile/", var, "_diff.csv"), row.names=FALSE)
}

density.plot <- function(var){
	outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
	df <- read.csv(paste0(outpath, "quantile/", var, "_diff.csv"))
  p1 <- density(df[,1])
  p2 <- density(df[,2])
  p3 <- density(df[,3])
  p4 <- density(df[,4])
  p5 <- density(df[,5])
  p6 <- density(df[,6])
  p7 <- density(df[,7])
  rx <- range(df)
  ry <- range(c(p1$y,p2$y,p3$y,p4$y,p5$y,p6$y,p7$y))
  plot(p1, col=cols[1], main=var, xlab="", ylab="", cex.main=1.5, cex.lab=1.5, cex.axis=1.5, lwd=2, xlim=rx, ylim=ry)
  lines(p2, col=cols[2], lwd=2.5)
  lines(p3, col=cols[3], lwd=2.5)
  lines(p4, col=cols[4], lwd=2.5)
  lines(p5, col=cols[5], lwd=2.5)
  lines(p6, col=cols[6], lwd=2.5)
  lines(p7, col=cols[7], lwd=2.5)
  print(paste(var, "is done!"))
}