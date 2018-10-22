# run in an interactive mode

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)

# use transformed data
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
head(data)
na10km <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
data$x <- rep(na10km$x, 20); data$y <- rep(na10km$y, 20)

dt <- data[data$beetles==1,]

#write.csv(dt, paste0(inpath, "bioclimatic_values_presence.csv"), row.names=FALSE)
#dt <- read.csv(paste0(inpath, "bioclimatic_values_presence.csv"))

peakyears <- 2006:2010
nonpeakyears <- 2001:2005
dt$peak <- ifelse(dt$year %in% peakyears, 1, ifelse(dt$year %in% nonpeakyears, 0, 2))

vars <- c("ddAugJul","AugTmax","winterTmin","summerP0","PPT","GSP","summerP1","Tvar")

if(0){
	d <- vector()
	times <- seq(500,1000,by=10)
	for(i in 1:length(times)){
		d[i] <- mean(replicate(times[i], sample(dt$ddAugJul, 5000)))
		print(i)
	}
}

tau <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
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


for(j in 1:length(vars)){
	df1 <- as.data.frame(matrix(,ncol=0,nrow=7))
	df2 <- as.data.frame(matrix(,ncol=0,nrow=7))
	df3 <- as.data.frame(matrix(,ncol=0,nrow=7))
	for(i in 1:1000){
		s1 <- sample(dt[dt$peak==1,][,vars[j]],5000)
		s2 <- sample(dt[dt$peak==0,][,vars[j]],5000)
		q1 <- as.numeric(quantile(s1, tau))
		q2 <- as.numeric(quantile(s2, tau))
		q3 <- q1 - q2
		df1 <- rbind(df1, q1)
		df2 <- rbind(df2, q2)
		df3 <- rbind(df3, q3)
		print(paste(vars[j], i))
	}
	names(df1) <- paste0("t", tau)
	names(df2) <- paste0("t", tau)
	names(df3) <- paste0("t", tau)
	write.csv(df1, paste0(inpath, "quantile/", vars[j], "peak_csv"), row.names=FALSE)
	write.csv(df2, paste0(inpath, "quantile/", vars[j], "nonpeak_csv"), row.names=FALSE)
	write.csv(df3, paste0(inpath, "quantile/", vars[j], "diff_csv"), row.names=FALSE)
}
