# Created by Dongmei Chen
# run in an interactive mode

library(MASS)
library(Hmisc)
install.packages("corrplot", repos='http://cran.us.r-project.org')
library(corrplot)
install.packages("PerformanceAnalytics", repos='http://cran.us.r-project.org')
library(PerformanceAnalytics)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))

d <- dim(data)[2]
# Remove hosts column
dat <- data[,-d]
d <- dim(dat)[2]
# rescale the predictors
dt <- cbind(dat[d], scale(dat[,-d]))
dt$beetles <- as.character(dt$beetles)
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
df <- dt[dt$beetles=="1",]
par(mfrow=c(2,5))
plot(df$Tmean, df$Tvar)
plot(df$TMarAug,df$PPT)
plot(df$fallTmean, df$POctSep)
plot(df$ddAugJul, df$PcumOctSep)
plot(df$ddAugJun, df$Pmean)
plot(df$AugTmean, df$GSP)
plot(df$AugTmax,df$JanTmin)
plot(df$Tmin, df$summerP1)
plot(df$summerTmean, df$summerP0)
plot(df$OctTmin, df$summerP2)
plot(df$TOctSep, df$POctSep)

par(mfrow=c(2,2))
plot(df$Tmean, df$Tvar)
plot(df$TMarAug,df$PPT)
plot(df$AugTmax,df$POctSep)
plot(df$Tmin, df$summerP1)

