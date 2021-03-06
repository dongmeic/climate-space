# run in an interactive mode or in srun

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)

CRU = 1
if(1){
	if(CRU){
		data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
	}else{
		data <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_r.csv"))
	}
}
#head(data)
#na10km <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
#data$x <- rep(na10km$x, 20); data$y <- rep(na10km$y, 20)

dt <- data[data$beetles==1,]
if(CRU){
	write.csv(dt, paste0(inpath, "bioclimatic_values_presence.csv"), row.names=FALSE)
	#dt <- read.csv(paste0(inpath, "bioclimatic_values_presence.csv"))
}else{
	#write.csv(dt, paste0(inpath, "bioclim_vars_presence_both.csv"), row.names=FALSE)
	dt <- read.csv(paste0(inpath, "bioclim_vars_presence_both.csv"))
}

peakyears <- 2006:2008
nonpeakyears <- 1996:1998
dt$peak <- ifelse(dt$year %in% peakyears, 1, ifelse(dt$year %in% nonpeakyears, 0, 2))

vars <- c("ddAugJul", "maxAugT", "OptTsum", "winterTmin", "summerP2", "MarTmin",
						 "drop5", "GSP", "PPT", "AugMax", "OctMin", "Tvar")

tau <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

if(0){ # t-tests
	ptm <- proc.time()
	for(i in 1:length(vars)){	
		m1 <- replicate(1000, quantile(sample(dt[dt$peak==1,][,vars[i]],5000), tau))
		write.csv(m1, paste0(inpath, "quantile/peak_", vars[i], "_r.csv"), row.names=FALSE)
		m2 <- replicate(1000, quantile(sample(dt[dt$peak==0,][,vars[i]],5000), tau))
		write.csv(m2, paste0(inpath, "quantile/nonpeak_", vars[i], "_r.csv"), row.names=FALSE)
		print(i)
	}
	proc.time() - ptm

	t.df <- as.data.frame(matrix(,ncol=0,nrow=7))
	p.df <- as.data.frame(matrix(,ncol=0,nrow=7))
	e.df <- as.data.frame(matrix(,ncol=0,nrow=7))
	for(i in 1:length(vars)){
		m1 <- read.csv(paste0(inpath, "quantile/peak_", vars[i], "_r.csv"))
		m2 <- read.csv(paste0(inpath, "quantile/nonpeak_", vars[i], "_r.csv"))
		t <- vector(); p <- vector(); e1 <- vector(); e2 <- vector()
		for(j in 1:length(tau)){
			p1 <- as.numeric(m1[j,]); p2 <- as.numeric(m2[j,])
			t[j] <- median(replicate(100,t.test(sample(as.numeric(m1[j,]),100), sample(as.numeric(m2[j,]),100))$statistic))
			p[j] <- median(replicate(100,t.test(sample(as.numeric(m1[j,]),100), sample(as.numeric(m2[j,]),100))$p.value))
			e1[j] <- as.numeric(t.test(p1, p2)$estimate[1])
			e2[j] <- as.numeric(t.test(p1, p2)$estimate[2])
			est <- replicate(100,t.test(sample(as.numeric(m1[j,]),100), sample(as.numeric(m2[j,]),100))$estimate)
			est.df <- as.data.frame(t(est))
			est.df$diff <- est.df[,1] - est.df[,2]
			write.csv(est.df, paste0(inpath, "quantile/", vars[i], "_", tau[j], "_estimates.csv"), row.names=FALSE)
		}	
		t.df <- cbind(t.df, t)
		p.df <- cbind(p.df, p)
		e3 <- e1 - e2
		e.df <- cbind(e.df, e1, e2, e3)
		names(t.df)[dim(t.df)[2]] <- vars[i]
		names(p.df)[dim(p.df)[2]] <- vars[i]
		names(e.df)[dim(e.df)[2]-2] <- paste0(vars[i], "_x")
		names(e.df)[dim(e.df)[2]-1] <- paste0(vars[i], "_y")
		names(e.df)[dim(e.df)[2]] <- paste0(vars[i], "_z")
		print(vars[i])
	}
	write.csv(t.df, paste0(inpath, "quantile/tvalues_median.csv"), row.names=FALSE)
	write.csv(p.df, paste0(inpath, "quantile/pvalues_median.csv"), row.names=FALSE)
	write.csv(e.df, paste0(inpath, "quantile/estimates.csv"), row.names=FALSE)

	# run without replicates
	e.df <- as.data.frame(matrix(,ncol=0,nrow=8))
	for(i in 1:length(vars)){
		q1 <- quantile(dt[dt$peak==1,][,vars[i]], tau)
		q2 <- quantile(dt[dt$peak==0,][,vars[i]], tau)
		q3 <- q1- q2
		q4 <- q3/abs(q2)
		e.df <- cbind(e.df, q1=as.numeric(q1), q2=as.numeric(q2), q3=as.numeric(q3), q4=as.numeric(q4))
		names(e.df)[dim(e.df)[2]-3] <- paste0(vars[i], "_x")
		names(e.df)[dim(e.df)[2]-2] <- paste0(vars[i], "_y")
		names(e.df)[dim(e.df)[2]-1] <- paste0(vars[i], "_z")
		names(e.df)[dim(e.df)[2]] <- paste0(vars[i], "_p")
		print(vars[i])
	}
	write.csv(e.df, paste0(inpath, "quantile/quantile_diff.csv"), row.names=FALSE)
}

# You can change threshold to make it more or less strict.  The default is 0.01,
# meaning 1% of the range of values in the variable

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

THRESHOLD <- 0.000000001
cum.means <- get.all.cumulative.means(vars, dt, q=0.95, threshold=THRESHOLD, use.threshold=F)
if(CRU){
	write.csv(cum.means, paste0(inpath, "cumulative_means.csv"), row.names=FALSE)
}else{
	write.csv(cum.means, paste0(inpath, "cumulative_means_daymet.csv"), row.names=FALSE)
	#cum.means <- read.csv(paste0(inpath, "cumulative_means_daymet.csv"))
}

# Test one:
#plot(cum.means[, 1], type='l') # or
#plot(cum.means[, 'var.name'], type='l')

# plot all
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
if(CRU){
	png(paste0(out,"cumulative_means.png"), width=12, height=9, units="in", res=300)
}else{
	png(paste0(out,"cumulative_means_daymet.png"), width=12, height=9, units="in", res=300)
}
par(mfrow=c(3,4), mar=c(2.5, 2.5, 3.5, 2))
for(i in 1:12){
	plot(cum.means[, i], type='l', col=i, xlab="", ylab="", main=vars[i], lwd=2)
}
dev.off()

get.index.of.last.finite.value <- function(x) {
  max(which(!is.na(x)))
}

get.sample.sizes.needed <- function(cum.means) {
  apply(cum.means, 2, get.index.of.last.finite.value)
}

#print(paste('Sample sizes needed for threshold', THRESHOLD))
#get.sample.sizes.needed(cum.means)

get.diff.matrix <- function(dt, var, iter){
	tau <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
	df1 <- as.data.frame(matrix(,ncol=0,nrow=7))
	df2 <- as.data.frame(matrix(,ncol=0,nrow=7))
	df3 <- as.data.frame(matrix(,ncol=0,nrow=7))
	for(i in 1:iter){
		s1 <- sample(dt[dt$peak==1,][,var],5000)
		s2 <- sample(dt[dt$peak==0,][,var],5000)
		q1 <- as.numeric(quantile(s1, tau))
		q2 <- as.numeric(quantile(s2, tau))
		q3 <- q1 - q2
		df1 <- rbind(df1, q1)
		df2 <- rbind(df2, q2)
		df3 <- rbind(df3, q3)
		print(paste(var, i))
	}
	names(df1) <- paste0("t", tau)
	names(df2) <- paste0("t", tau)
	names(df3) <- paste0("t", tau)
	write.csv(df1, paste0(inpath, "quantile/", var, "_peak.csv"), row.names=FALSE)
	write.csv(df2, paste0(inpath, "quantile/", var, "_nonpeak.csv"), row.names=FALSE)
	write.csv(df3, paste0(inpath, "quantile/", var, "_diff.csv"), row.names=FALSE)
}

#iters <- c(5000, 1900, 5000, 5000, 2100, 5000, 5000, 1600)
iters <- c(2000, 500, 1000, 2000, 2000, 1000, 1000, 2000, 3000, 2000, 2000, 2000)
for(var in vars){
	get.diff.matrix(dt, var, iters[which(vars==var)])
	print(paste(which(vars==var), var, iters[which(vars==var)]))
}

library(RColorBrewer)
cols <- brewer.pal(7,"Blues")
density.plot <- function(var){
	df <- read.csv(paste0(inpath, "quantile/", var, "_diff.csv"))
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

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
tau <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

if(CRU){
	png(paste0(out,"quant_diff_density_plots.png"), width=12, height=9, units="in", res=300)
}else{
	png(paste0(out,"quant_diff_density_plots_both.png"), width=12, height=9, units="in", res=300)
}
par(mfrow=c(3,4),mar=c(3.5,3.5,3,1))
for (i in 1:12){
  density.plot(vars[i])
  if(i==9){
    legend('topleft', lty=1, lwd=2, col=cols, legend=tau, cex = 1.5, bty='n')
  }
}
dev.off()

print("all done")

save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/bootstrapped.RData")