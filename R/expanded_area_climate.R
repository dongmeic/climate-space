source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/data_transform.R")

library(RColorBrewer)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlstat <- read.csv(paste0(csvpath, "beetle_presence_statistics.csv"))
btlstat <- btlstat[rows$rows,]
indata <- get_data()
indata$hosts <- ifelse(indata$beetles==1 & indata$hosts==0, 1, indata$hosts)

sink(paste0(csvpath,"bioclimate_summary.txt"))
summary(indata[indata$beetles==1,][,1:61])
sink()

d <- dim(btlstat)[1]
expanded <- which(btlstat$maxabs >= 10)
allrows <- c()
for(i in 1:20){allrows <- c(allrows, expanded+(i-1)*d)}
expanded.clm <- indata[allrows,]
expanded.clm <- expanded.clm[expanded.clm$beetles==1,]

sink(paste0(csvpath,"expanded_bioclm.txt"))
summary(expanded.clm)
sink()

core <- which(btlstat$ngbyrs >= 80)
allrows <- c()
for(i in 1:20){allrows <- c(allrows, core+(i-1)*d)}
core.clm <- indata[allrows,]
core.clm <- core.clm[core.clm$beetles==1,]
sink(paste0(csvpath,"core_bioclm.txt"))
summary(core.clm)
sink()

expanded.clm$expand <- rep("expanded", dim(expanded.clm)[1])
core.clm$expand <- rep("core", dim(core.clm)[1])
dt <- rbind(expanded.clm, core.clm)
vars <- c("JanTmin", "MarTmin", "Tvar", "summerP2", "ddAugJul", "AugTmax", "wd", "mi")

expand.boxplot <- function(var){
  df <- dt[,c(var,"expand")]
  colnames(df)[1] <- "bioclm"
  boxplot(bioclm~expand,data=df, main=var, cex=1.5, horizontal = TRUE,col=cols, outcol=rgb(0,0,0,0.4),outcex=0.5,
   xlab="", ylab="")
}

cols <- c('#f7f7f7','#525252')
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
png(paste0(out,"expanded_core_boxplot.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4),mar=c(3.5,3.5,3,1))
for (var in vars){
  expand.boxplot(var)
  if(var=="mi"){
    legend('right', fill=cols, ncol=2, legend=c("Core","Expanded"), cex = 0.8, bty='n')
  }
}
dev.off()

iters <- c(2000, 2000, 2000, 2000, 2000, 2000, 1000, 1000)
for(var in vars){
	get.diff.matrix(dt, var, iters[which(vars==var)], peak=F)
	print(paste(which(vars==var), var, iters[which(vars==var)]))
}

cols <- brewer.pal(7,"Blues")
taus <- c(.05,.1,.25,0.5,.75,.90,.95)
png(paste0(out,"expand_quant_diff_density.png"), width=12, height=6, units="in", res=300)
par(mfrow=c(2,4),mar=c(3.5,3.5,3,1))
for (i in 1:8){
  density.plot(vars[i], peak=F)
  if(i==8){
    legend('topright', lty=1, lwd=2, col=cols, legend=taus, cex = 1.5, bty='n')
  }
}
dev.off()

vars <- c("ddAugJun", "ddAugJul", "Acs", "Ecs", "Lcs", "Ncs","Oct20", "Oct30", "Oct40", "OctMin",
					"Jan20", "Jan30", "Jan40", "JanMin", "Mar20", "Mar30", "Mar40", "MarMin",
					"winter20", "winter30", "winter40", "winterMin", "maxAugT", "OptTsum","summerT40")
					
k1 <- dim(dt[dt$expand=="expanded",])[1]; k2 <- dim(dt[dt$expand!="expanded",])[1]
d1 <- vector(); d2 <- vector()
for(var in vars){
	i <- which(vars==var)
	if(var == "ddAugJun"){
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]>305) / k1		
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]>305) / k2	
	}else if(var=="ddAugJul"){
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]>833)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]>833)/k2	
	}else if(var %in% c("Acs", "Oct20", "Jan20", "Mar20", "winter20")){
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]<=4)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]<=4)/k2		
	}else if(var %in% c("OctMin", "JanMin", "MarMin", "winterMin")){	
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]>-40)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]>-40)/k2
	}else if(var=="maxAugT"){
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]>2)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]>2)/k2
	}else if(var=="summerT40"){
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]==0)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]==0)/k2
	}else if(var=="OptTsum"){
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]>0)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]>0)/k2
	}else{
		d1[i] <- sum(dt[dt$expand=="expanded",][,var]==0)/k1
		d2[i] <- sum(dt[dt$expand!="expanded",][,var]==0)/k2
	}
}
df <- data.frame(expand=d1, core=d2, var=vars)

write.csv(df, paste0(csvpath, "daily_threshold_expand.csv"), row.names=FALSE)

vars <- c("JanTmin", "MarTmin", "Tmin", "summerT40", "AugMaxT", "maxT", "max.drop",
				  "Jan40", "Mar40", "winter40", "JanMin", "MarMin", "winterMin")
dt <- indata[indata$beetles==1,][,vars]
d <- dim(dt)[1]
for(var in vars){
	m1 <- min(dt[,var])
	m2 <- max(dt[,var])
	s1 <- sum(dt[,var]==m1)
	s2 <- sum(dt[,var]==m2)
	print(paste(var, m1, m2, s1, s2))
}