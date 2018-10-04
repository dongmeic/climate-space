# Created by Dongmei Chen

library(MASS)
inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
dat <- data[,-35]
dat <- cbind(dat[34], scale(dat[,-34]))
dat$beetles <- as.character(dat$beetles)
# Linear Discriminant Analysis with Jacknifed Prediction 
dat.lda <- lda(beetles ~ ., data=dat)

# Assess the accuracy of the prediction
# percent correct for each category of "beetles"
fit <- lda(beetles ~ ., data=dat, na.action="na.omit", CV=TRUE)
ct <- table(dat$beetles, fit$class)
diag(prop.table(ct, 1))
# total percent correct
sum(diag(prop.table(ct)))

# Scatter plot using the 1st two discriminant dimensions
plot(fit) # fit from lda