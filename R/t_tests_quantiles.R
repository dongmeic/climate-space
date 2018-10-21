# run in an interactive mode

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)

# use transformed data
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
head(data)
dt <- data[data$beetles==1,]
na10km <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/na10km_v2.csv")
#write.csv(dt, paste0(inpath, "bioclimatic_values_presence.csv"), row.names=FALSE)
#dt <- read.csv(paste0(inpath, "bioclimatic_values_presence.csv"))

peakyears <- 2006:2010
dt$peak <- ifelse(dt$year %in% peakyears, 1, 0)

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
for(i in 1:length(vars)){
	m1 <- replicate(1000, quantile(sample(dt[dt$peak==1,][,vars[i]],5000), tau))
	write.csv(m1, paste0(inpath, "quantile/peak_", vars[i], "_r.csv"), row.names=FALSE)
	m2 <- replicate(1000, quantile(sample(dt[dt$peak==0,][,vars[i]],5000), tau))
	write.csv(m2, paste0(inpath, "quantile/nonpeak_", vars[i], "_r.csv"), row.names=FALSE)
	print(i)
}

t.df <- as.data.frame(matrix(,ncol=0,nrow=7))
p.df <- as.data.frame(matrix(,ncol=0,nrow=7))
e.df <- as.data.frame(matrix(,ncol=0,nrow=14))
for(i in 1:length(vars)){
	m1 <- read.csv(paste0(inpath, "quantile/peak_", vars[i], "_r.csv"))
	m2 <- read.csv(paste0(inpath, "quantile/nonpeak_", vars[i], "_r.csv"))
	t <- vector(); p <- vector(); e1 <- vector(); e2 <- vector()
	for(j in 1:length(tau)){
		s1 <- sample(as.numeric(m1[j,]),100); s2 <- sample(as.numeric(m2[j,]),100)
		p1 <- as.numeric(m1[j,]); p2 <- as.numeric(m2[j,])
		t[j] <- median(replicate(10,t.test(s1, s2)$statistic))
		p[j] <- median(replicate(10,t.test(s1, s2)$p.value))
		e1[j] <- as.numeric(t.test(p1, p2)$estimate[1])
		e2[j] <- as.numeric(t.test(p1, p2)$estimate[2])
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
