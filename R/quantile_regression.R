# Created by Dongmei Chen
# run in an interactive mode

library(quantreg)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

# run Line 4 to 24 in PCA_bioclimatic_variables.R if reading data is too slow; data <- ndf
data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
head(data)

# reference: https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
dt <- data[data$beetles==1,]
df <- dt[,c("ddAugJul","AugTmax","winterTmin","summerP0","PPT","GSP","summerP1","Tvar","year")]
write.csv(df, paste0(inpath, "data_for_QR.csv"), row.names=FALSE)
ylabs <- c("Day-degrees above 5.5 °C from Aug to Jul",
						"Maximum temperature in Aug (°C)",
						"Minimum winter temperature (°C)",
						"Summer precipitation in current year (mm)",
						"Cumulative monthly Oct-Aug precipitation (mm)",
						"Growing season precipitation (mm)", 
						"Summer precipitation in previous year (mm)",
						"Seasonal temperature variation (Aug - Jul)")

#df <- read.csv(paste0(inpath, "data_for_QR.csv"))
sink(paste0(inpath,"QR_summary.txt"))
fit <- rq(ddAugJul~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(AugTmax~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(winterTmin~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(summerP0~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(PPT~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(GSP~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(summerP1~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
fit <- rq(Tvar~year, tau=c(.05, .1, .25, .5, .75, .9, .95), data=df)
summary(fit)
sink()

attach(df)
taus <- c(.05,.1,.25,.75,.90,.95)
#png(paste0(out,"test_QR_plot.png"), width=6, height=6, units="in", res=300)
png(paste0(out,"QR_plots.png"), width=12, height=6, units="in", res=300)
#par(mfrow=c(1,1))
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

save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/QR.RData")