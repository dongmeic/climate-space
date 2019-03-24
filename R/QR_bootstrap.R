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
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
ptm <- proc.time()
indata <- get_data()
proc.time() - ptm
# user  system elapsed
# 682.455  11.609 694.729

dt <- indata[indata$beetles==1,]
# select variables from climate space
vars <- c("JanTmin", "MarTmin", "Tmin", "Tvar")
df <- dt[,c(vars, "year")]
write.csv(df, paste0(outpath, "data_for_QR_both.csv"), row.names=FALSE)
taus <- c(.05,.1,.25,.75,.90,.95)
sink(paste0(outpath,"QR_summary_both.txt"))
fit <- rq(JanTmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(MarTmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(Tmin~year, tau=taus, data=df)
summary(fit)
fit <- rq(Tvar~year, tau=taus, data=df)
summary(fit)
sink()	

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlstat <- read.csv(paste0(csvpath, "beetle_presence_statistics.csv"))
btlstat <- btlstat[rows$rows,]

d <- dim(btlstat)[1]
expanded <- which(btlstat$maxabs >= 10)
allrows <- c()
for(i in 1:20){allrows <- c(allrows, expanded+(i-1)*d)}
expanded.clm <- indata[allrows,]
expanded.clm <- expanded.clm[expanded.clm$beetles==1,]

core <- which(btlstat$ngbyrs >= 80)
allrows <- c()
for(i in 1:20){allrows <- c(allrows, core+(i-1)*d)}
core.clm <- indata[allrows,]
core.clm <- core.clm[core.clm$beetles==1,]

expanded.clm$expand <- rep("expanded", dim(expanded.clm)[1])
core.clm$expand <- rep("core", dim(core.clm)[1])
dt1 <- rbind(expanded.clm, core.clm)
cum.means <- get.all.cumulative.means(vars, dt, q=0.95, threshold=THRESHOLD, use.threshold=F)
write.csv(cum.means, paste0(outpath, "cumulative_means_expansion.csv"), row.names=FALSE)

png(paste0(out,"cumulative_means_expansion.png"), width=12, height=3, units="in", res=300)
par(mfrow=c(1,4), mar=c(2.5, 2.5, 3.5, 2))
for(i in 1:4){
	plot(cum.means[, i], type='l', col=i, xlab="", ylab="", main=vars[i], lwd=2)
}
dev.off()

# peak and nonpeak years
peakyears <- 2006:2008
nonpeakyears <- 1996:1998
dt$peak <- ifelse(dt$year %in% peakyears, 1, ifelse(dt$year %in% nonpeakyears, 0, 2))

for(var in vars){
	get.diff.matrix(dt, var, 2000)
	print(paste(which(vars==var), var, 2000))
}

for(var in vars){
	get.diff.matrix(dt1, var, 2000, peak=F)
	print(paste(which(vars==var), var, 2000))
}

taus <- c(0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95)
ptm <- proc.time()
png(paste0(out,"QR_quant_diff_density.png"), width=12, height=12, units="in", res=300)
par(mfrow=c(3,4), mar=c(3.5,3.5,8,1))
for (i in 1:4){
	plot(df$year,df[,i],cex=.25, type="n", main=colnames(df)[i], cex.main =1.5, xlab="", ylab="", cex.lab=1.5)
	points(df$year,df[,i],pch=16,cex=.5,col=rgb(0.7,0.7,0.7,0.05))
	abline(rq(df[,i]~df$year,tau=.5),col="black", lwd=1.5)
	abline(lm(df[,i]~df$year),lty=2,col="red", lwd=1.5) #the dreaded ols line
	for(k in 1:length(taus)){
					 abline(rq(df[,i]~df$year,tau=taus[k]),col=rgb(0.3,0.3,0.3))
					 }
	print(i)
}
mtext('Quantile regression fit of beetle climate space over time', outer = TRUE, cex = 1.5, line=-3)
for (i in 1:4){
  density.plot(vars[i])
  cols <- brewer.pal(7,"Blues")
  if(i==4){
    legend('topright', lty=1, lwd=2, col=cols, legend=taus, cex = 1.5, bty='n')
  }
}
for (i in 1:4){
  density.plot(vars[i], peak=F)
  cols <- brewer.pal(7,"Oranges")
  if(i==3){
    legend('topright', lty=1, lwd=2, col=cols, legend=taus, cex = 1.5, bty='n', ncol=2)
  }
}
dev.off()
proc.time() - ptm

# boxplot
peakyears <- 2006:2008
nonpeakyears <- 1996:1998
expanding1 <- 2000:2002
expanding2 <- 2003:2005
dt$peak <- ifelse(dt$year %in% peakyears, 4, 
					 ifelse(dt$year %in% nonpeakyears, 1, 
					 ifelse(dt$year %in% expanding1,2,
					 ifelse(dt$year %in% expanding2,3,0))))

peak.boxplot <- function(var){
  df <- dt[dt$peak!=0,][,c(var,"peak")]
  colnames(df)[1] <- "bioclm"
  boxplot(bioclm~peak,data=df, main=var, cex=1.5, horizontal = TRUE,col=cols, outcol=rgb(0,0,0,0.4),outcex=0.5,
   xlab="", ylab="")
}

expand.boxplot <- function(var){
  df <- dt[,c(var,"expand")]
  colnames(df)[1] <- "bioclm"
  boxplot(bioclm~expand,data=df, main=var, cex=1.5, horizontal = TRUE,col=cols, outcol=rgb(0,0,0,0.4),outcex=0.5,
   xlab="", ylab="")
}

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
png(paste0(out,"boxplot_years_expansion.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4),mar=c(3.5,3.5,3,1))
cols <- c('#f7f7f7','#cccccc','#969696','#525252')
for (var in vars){
  peak.boxplot(var)
  if(var=="mi"){
    legend('right', fill=cols, ncol=2, legend=c("1:1996-1998","2:2000-2002","3:2003-2005","4:2006-2008"), cex = 0.8, bty='n')
  }
}
cols <- c('#f7f7f7','#525252')
for (var in vars){
  expand.boxplot(var)
  if(var=="mi"){
    legend('right', fill=cols, ncol=2, legend=c("Core","Expanded"), cex = 0.8, bty='n')
  }
}
dev.off()	


 