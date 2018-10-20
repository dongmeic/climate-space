# run in an interactive mode

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)

# use transformed data
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_t.csv"))
head(data)
dt <- data[data$beetles==1,]

peakyears <- 2006:2010
dt$peak <- ifelse(dt$year %in% peakyears, 1, 0)

x <- sample(dt[dt$peak==1,][,'ddAugJul'],5000)
y <- sample(dt[dt$peak==0,][,'ddAugJul'],5000)

d <- vector()
times <- seq(500,1000,by=10)
for(i in 1:length(times)){
	d[i] <- mean(replicate(times[i], sample(dt$ddAugJul, 5000)))
	print(i)
}

tau <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
peak <- data.frame(q1=double(), q2=double(), q3=double(), q4=double(), q5=double(), q6=double(), q7=double())
nonpeak <- data.frame(q1=double(), q2=double(), q3=double(), q4=double(), q5=double(), q6=double(), q7=double())

m <- replicate(1000, quantile(sample(dt[dt$peak==1,][,'ddAugJul'],5000), tau))

ts = replicate(1000,t.test(sample(dt[dt$peak==1,][,'ddAugJul'],5000),sample(dt[dt$peak==0,][,'ddAugJul'],5000))$statistic)
tps = replicate(1000,t.test(sample(dt[dt$peak==1,][,'ddAugJul'],5000),sample(dt[dt$peak==0,][,'ddAugJul'],5000))$p.value)

par(mfrow=c(1, 2))
boxplot(sample(dt[dt$peak==1,][,'ddAugJul'],5000))
boxplot(sample(dt[dt$peak==0,][,'ddAugJul'],5000))

hist(ts, prob=TRUE)
lines(density(ts), col='red')

hist(tps, prob=TRUE)
lines(density(tps), col='red')
