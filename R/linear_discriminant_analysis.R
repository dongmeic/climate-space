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
dat <- cbind(dat[d], scale(dat[,-d]))
dat$beetles <- as.character(dat$beetles)
# Linear Discriminant Analysis
dat.lda <- lda(beetles ~ ., data=dat)
sink(paste0(inpath,"lda.txt"))
print(dat.lda)
sink()

# Assess the accuracy of the prediction
# percent correct for each category of "beetles"
fit <- lda(beetles ~ ., data=dat, na.action="na.omit", CV=TRUE)
ct <- table(dat$beetles, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# correlation matrix
my_data <- scale(dat[,-d])
res <- cor(my_data)
round(res, 2)
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

