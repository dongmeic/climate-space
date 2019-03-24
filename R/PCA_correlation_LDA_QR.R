# the script is to run PCA, correlation matrix, LDA and QR
# run in interactive mode

library(MASS)
library(Hmisc)
library(dplyr)
library(ggpubr)
library(car)
library(rcompanion)
library(quantreg)
library(RColorBrewer)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/data_transform.R")

outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
ptm <- proc.time()
indata <- get_data()
proc.time() - ptm
# user  system elapsed
# 682.455  11.609 694.729
#indata$hosts <- ifelse(indata$beetles==1 & indata$hosts==0, 1, indata$hosts)
#write.csv(indata, paste0(outpath, "bioclim_vars_both_na_1996_2015_r.csv"), row.names=FALSE)
a <- dim(indata)[2]
indata.cc <- indata[complete.cases(indata),]
# remove cv.gsp
df <- indata[, -which(names(indata) %in% c('cv.gsp', 'beetles', 'hosts', 'year'))]
#test <- indata.cc[sample(nrow(indata.cc), 500000),]
#write.csv(test, paste0(outpath, "bioclim_vars_na_r_test.csv"), row.names=FALSE)
head(indata)
#df <- indata.cc[,-a:-(a-2)] # remove the last three columns
#dat <- df[,!(names(df) %in% ignore)]
pca <- prcomp(df, scale. = TRUE)
#pca <- princomp(df, cor = TRUE)
sink(paste0(outpath,"pca_summary.txt"))
summary(pca, loadings <- T)
sink()
head(pca$rotation)
#write.csv(x = unclass(loadings(pca)), file = paste0(outpath,"pca_loading.csv"))
write.csv(x = pca$rotation, file = paste0(outpath,"pca_loading.csv"))

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
pca <- princomp(df)
png(paste0(out,"PCA_variable_selection.png"), width=12, height=9, units="in", res=300)
par(mfrow=c(2, 2))
par(mar=c(4, 4, 3, 3))
t = 0.01
for(i in c(1, 3)) {
  plot(pca$scores[indata.cc$hosts == 0, i], 
       pca$scores[indata.cc$hosts == 0, i + 1], 
       col=rgb(1, 0, 0, t), 
       pch=16,
       xlab=paste('PC', i, sep=''),
       ylab=paste('PC', i + 1, sep=''), 
       main='Host Tree')
  points(pca$scores[indata.cc$hosts == 1, i],
         pca$scores[indata.cc$hosts == 1, i + 1],
         pch=16,
         col=rgb(0, 1, 0, t))
  legend('bottomleft', pch=16, col=c(2, 3), legend=c('absent', 'present'))
  plot(pca$scores[indata.cc$beetles == 0, i], 
       pca$scores[indata.cc$beetles == 0, i + 1], 
       col=rgb(1, 0, 1, t), 
       pch=16,
       xlab=paste('PC', i, sep=''),
       ylab='', 
       yaxt='n',
       main='Beetle')
  points(pca$scores[indata.cc$beetles == 1, i],
         pca$scores[indata.cc$beetles == 1, i + 1],
         pch=16,
         col=rgb(0, 1, 1, t))
  legend('bottomleft', pch=16, col=c(6, 5), legend=c('absent', 'present'))

}
dev.off()

ptm <- proc.time()
print("get transformed data...this will take a while...")
df.t <- get.transformed.dt(df)
proc.time() - ptm
#    user   system  elapsed
#3309.279   18.175 6677.268
df.t <- cbind(df.t, indata.cc[,(a-2):a])
write.csv(df.t, paste0(outpath, "bioclim_vars_both_na_1996_2015_t.csv"), row.names=FALSE)

#df.t <- read.csv(paste0(outpath, "bioclim_vars_both_na_1996_2015_t.csv"))
dat <- df.t[,!(names(df.t) %in% ignore)]
png(paste0(out, "histograms_trans_both.png"), width=16, height=10, units="in", res=300)
par(mfrow=c(5,8))
for(i in names(dat)){
	plotNormalHistogram(dat[,i], main=i)
	print(i)
}
dev.off()

dat <- df[,!(names(df) %in% ignore)]
my_data <- scale(dat)
res <- cor(my_data, use = "complete.obs")
sink(paste0(outpath,"CorrMatrix_daymet_r.txt"))
round(res, 2)
sink()

dat <- df.t[,!(names(df.t) %in% ignore)]
my_data <- scale(dat)
res <- cor(my_data, use = "complete.obs")
sink(paste0(outpath,"CorrMatrix_daymet.txt"))
round(res, 2)
sink()

dt <- as.data.frame(my_data)
#dt$beetles <- indata.cc$beetles
dt$prs <- indata.cc$prs
# Linear Discriminant Analysis
#dt.lda <- lda(beetles ~ ., data=dt)
dt.lda <- lda(prs ~ ., data=dt)
sink(paste0(outpath,"lda_daymet_all.txt"))
print(dt.lda)
sink()

dt.lda <- lda(beetles ~ summerP2 + Tmin + Tvar + PPT + drop5 + ddAugJul + AugTmax + wd + mi, data=dt)
sink(paste0(outpath,"lda_daymet.txt"))
print(dt.lda)
sink()

dt <- indata[indata$beetles==1,]
#df <- dt[,c("Tvar", "ddAugJul", "Tmin", "AugTmax", "wd", "mi", "year")]
df <- dt[,c("JanTmin", "MarTmin", "Tvar", "summerP2", "ddAugJul", "AugTmax", "wd", "mi", "year")]
write.csv(df, paste0(outpath, "data_for_QR_both.csv"), row.names=FALSE)
#df <- read.csv(paste0(outpath, "data_for_QR.csv"))
taus <- c(.05,.1,.25,.75,.90,.95)
sink(paste0(outpath,"QR_summary_both.txt"))
fit <- rq(JanTmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(MarTmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(Tvar~year, tau=taus, data=df)
summary(fit)
fit <- rq(summerP2~year, tau=taus, data=df)
summary(fit)
fit <- rq(ddAugJul~year, tau=taus, data=df)
summary(fit)
fit <- rq(AugTmax~year, tau=taus, data=df)
summary(fit)
fit <- rq(wd~year, tau=taus, data=df)
summary(fit)
fit <- rq(mi~year, tau=taus, data=df)
summary(fit)
sink()					 

png(paste0(out,"QR_plots.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4), mar=c(3.5,3.5,3,1))
for (i in 1:8){
	plot(df$year,df[,i],cex=.25, type="n", main=colnames(df)[i], cex.main =1.5, xlab="", ylab="", cex.lab=1.5)
	points(df$year,df[,i],pch=16,cex=.5,col=rgb(0.7,0.7,0.7,0.05))
	abline(rq(df[,i]~df$year,tau=.5),col="black", lwd=1.5)
	abline(lm(df[,i]~df$year),lty=2,col="red", lwd=1.5) #the dreaded ols line
	for(k in 1:length(taus)){
					 abline(rq(df[,i]~df$year,tau=taus[k]),col=rgb(0.5,0.5,0.5))
					 }
	print(i)
}
dev.off()

# select variables from climate space
vars <- c("OctTmin", "JanTmin", "MarTmin", "Tvar", "summerTmean", "AugTmean", "AugTmax", "ddAugJul",
					"summerP0", "summerP2", "wd", "mi")
df <- dt[,c(vars, "year")]

#indata <- get_data()				 
dt <- indata[indata$beetles==1,]
peakyears <- 2006:2008
nonpeakyears <- 1996:1998
dt$peak <- ifelse(dt$year %in% peakyears, 1, ifelse(dt$year %in% nonpeakyears, 0, 2))

#vars <- c("Tvar", "ddAugJul", "Tmin", "AugTmax", "wd", "mi")
vars <- c("JanTmin", "MarTmin", "Tvar", "summerP2", "ddAugJul", "AugTmax", "wd", "mi")

taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)

THRESHOLD <- 0.000000001
#cum.means <- read.csv(paste0(outpath, "cumulative_means_daymet.csv"))
#cum.means$Tmin <- get.cumulative.mean('Tmin', dt, q=0.95, threshold=THRESHOLD, use.threshold=F)
cum.means <- get.all.cumulative.means(vars, dt, q=0.95, threshold=THRESHOLD, use.threshold=F)
write.csv(cum.means, paste0(outpath, "cumulative_means_daymet.csv"), row.names=FALSE)

png(paste0(out,"cumulative_means.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4), mar=c(2.5, 2.5, 3.5, 2))
for(i in 1:8){
	plot(cum.means[, i], type='l', col=i, xlab="", ylab="", main=vars[i], lwd=2)
}
dev.off()

iters <- c(2000, 2000, 2000, 2000, 2000, 2000, 1000, 1000)
for(var in vars){
	get.diff.matrix(dt, var, iters[which(vars==var)])
	print(paste(which(vars==var), var, iters[which(vars==var)]))
}

cols <- brewer.pal(7,"Blues")
png(paste0(out,"quant_diff_density_plots.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4),mar=c(3.5,3.5,3,1))
for (i in 1:8){
  density.plot(vars[i])
  if(i==8){
    legend('topright', lty=1, lwd=2, col=cols, legend=taus, cex = 1.5, bty='n')
  }
}
dev.off()

vars <- c("ddAugJun", "ddAugJul", "Acs", "Ecs", "Lcs", "Ncs","Oct20", "Oct30", "Oct40", "OctMin",
					"Jan20", "Jan30", "Jan40", "JanMin", "Mar20", "Mar30", "Mar40", "MarMin",
					"winter20", "winter30", "winter40", "winterMin", "maxAugT", "OptTsum","summerT40")
					
k1 <- dim(dt[dt$peak==1,])[1]; k2 <- dim(dt[dt$peak==0,])[1]
d1 <- vector(); d2 <- vector()
for(var in vars){
	i <- which(vars==var)
	if(var == "ddAugJun"){
		d1[i] <- sum(dt[dt$peak==1,][,var]>305) / k1		
		d2[i] <- sum(dt[dt$peak==0,][,var]>305) / k2	
	}else if(var=="ddAugJul"){
		d1[i] <- sum(dt[dt$peak==1,][,var]>833)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]>833)/k2	
	}else if(var %in% c("Acs", "Oct20", "Jan20", "Mar20", "winter20")){
		d1[i] <- sum(dt[dt$peak==1,][,var]<=4)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]<=4)/k2		
	}else if(var %in% c("OctMin", "JanMin", "MarMin", "winterMin")){	
		d1[i] <- sum(dt[dt$peak==1,][,var]>-40)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]>-40)/k2
	}else if(var=="maxAugT"){
		d1[i] <- sum(dt[dt$peak==1,][,var]>2)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]>2)/k2
	}else if(var=="summerT40"){
		d1[i] <- sum(dt[dt$peak==1,][,var]==0)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]==0)/k2
	}else if(var=="OptTsum"){
		d1[i] <- sum(dt[dt$peak==1,][,var]>0)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]>0)/k2
	}else{
		d1[i] <- sum(dt[dt$peak==1,][,var]==0)/k1
		d2[i] <- sum(dt[dt$peak==0,][,var]==0)/k2
	}
}
df <- data.frame(peak=d1, nonpeak=d2, var=vars)

write.csv(df, paste0(outpath, "daily_threshold_peak.csv"), row.names=FALSE)
