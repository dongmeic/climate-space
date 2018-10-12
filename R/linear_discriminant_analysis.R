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


