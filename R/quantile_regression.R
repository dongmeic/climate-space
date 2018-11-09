# Created by Dongmei Chen
# run in an interactive mode or in srun

library(quantreg)

CRU = 0

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

# run Line 4 to 24 in PCA_bioclimatic_variables.R if reading data is too slow; data <- ndf
if(CRU){
	data <- read.csv(paste0(inpath, "bioclimatic_values_1996_2015_r.csv"))
}else{
	data <- read.csv(paste0(inpath, "bioclim_vars_both_1996_2015_r.csv"))
}
head(data)

# reference: https://cran.r-project.org/web/packages/quantreg/vignettes/rq.pdf
dt <- data[data$beetles==1,]
#df <- dt[,c("ddAugJul", "AugTmax", "winterTmin", "summerP0", "PPT", "GSP", "summerP1", "Tvar", "year")]
df <- dt[,c("ddAugJul", "maxAugT", "OptTsum", "winterTmin", "summerP2", "MarTmin",
						 "drop5", "GSP", "PPT", "AugMax", "OctMin", "Tvar", "year")]
if(CRU){
	write.csv(df, paste0(inpath, "data_for_QR.csv"), row.names=FALSE)
}else{
	write.csv(df, paste0(inpath, "data_for_QR_both.csv"), row.names=FALSE)
}


#df <- read.csv(paste0(inpath, "data_for_QR.csv"))
taus <- c(.05,.1,.25,.75,.90,.95)
if(CRU){
	sink(paste0(inpath,"QR_summary.txt"))
}else{
	sink(paste0(inpath,"QR_summary_both.txt"))
}
fit <- rq(ddAugJul~year, tau=taus, data=df)
summary(fit)
fit <- rq(maxAugT~year, tau=taus, data=df)
summary(fit)
fit <- rq(OptTsum~year, tau=taus, data=df)
summary(fit)
fit <- rq(winterTmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(summerP2~year, tau=taus, data=df)
summary(fit)
fit <- rq(MarTmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(drop5~year, tau=taus, data=df)
summary(fit)
fit <- rq(GSP~year, tau=taus, data=df)
summary(fit)
fit <- rq(PPT~year, tau=taus, data=df)
summary(fit)
fit <- rq(AugMax~year, tau=taus, data=df)
summary(fit)
fit <- rq(OctMin~year, tau=taus, data=df)
summary(fit)
fit <- rq(Tvar~year, tau=taus, data=df)
summary(fit)
sink()

attach(df)
if(CRU){
	png(paste0(out,"QR_plots.png"), width=12, height=6, units="in", res=300)
}else{
	png(paste0(out,"QR_plots_both.png"), width=12, height=9, units="in", res=300)
}
par(mfrow=c(3,4), mar=c(3.5,3.5,3,1))
for (i in 1:12){
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

print("all done")
#save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/QR.RData")
save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/QR_daymet.RData")