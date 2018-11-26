# Created by Dongmei Chen

#csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
#csvfile <- "bioclimatic_values_1996_2015_r.csv" # from PCA_bioclimatic_variables.R
#ndf <- read.csv(paste0(csvpath,csvfile))
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
ndf <- get_data()
ndf <- ndf[complete.cases(ndf),]
ndf$hosts <- ifelse(ndf$beetles==1 & ndf$hosts==0, 1, ndf$hosts)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots"
setwd(out)

vargrp1 <- c("OctTmin", "JanTmin", "MarTmin", "Tmin", "OctMin", "JanMin", "MarMin",  
							"winterMin", "AugMaxT", "maxT", "TMarAug", "summerTmean", 
							"AugTmean", "fallTmean", "TOctSep", "Tmean", "ddAugJul", "ddAugJun")

vargrp2 <- c("summerP0", "summerP1", "summerP2", "AugTmax", "PcumOctSep", "max.drop", "PPT",
						  "cv.gsp", "Tvar", "minT", "PMarAug", "mi",  "cwd", "pt.coef",
							 "POctSep", "Pmean", "wd", "OptTsum")
							 
vargrp <- c("OctTmin", "JanTmin", "MarTmin", "Tmin", "OctMin", "JanMin", "MarMin",  
							"winterMin", "minT", "Acs", "drop5", "max.drop", "maxAugT", "AugMaxT", "AugTmax", "maxT", 
							"TMarAug", "OptTsum", "summerTmean", "AugTmean", "fallTmean", "TOctSep", "Tmean", "ddAugJul", 
							"ddAugJun", "Tvar", "PMarAug", "summerP0", "summerP1", "summerP2", "POctSep",
						  "PcumOctSep", "Pmean", "PPT", "cv.gsp", "mi", "pt.coef", "vpd", "cwd", "wd")

cols <- c("#A9A9A9", "#1b9e77", "#d95f02")

# density plot
density.plot <- function(i){
  p1 <- density(ndf[,vargrp[i]])
  p2 <- density(ndf[ndf$hosts==1,][,vargrp[i]])
  p3 <- density(ndf[ndf$beetles==1,][,vargrp[i]])
  r <- range(c(p1$y,p2$y,p3$y))
  plot(p3,col=cols[3], main=vargrp[i], xlab="", ylab="", cex.main=2, cex.lab=1.5, cex.axis=1.5, lwd=4, ylim=r)
  lines(p2,col=cols[2], lwd=4)
  lines(p1,col=cols[1], lwd=4)
  polygon(p3, col="#d95f027D", border=cols[3])
  print(paste(vargrp[i], "is done!"))
}

for(i in 1:length(vargrp)){
  p1 <- density(ndf[,vargrp[i]])
  p2 <- density(ndf[ndf$hosts==1,][,vargrp[i]])
  p3 <- density(ndf[ndf$beetles==1,][,vargrp[i]])
  r <- range(c(p1$y,p2$y,p3$y))
  png(paste0("bioclimatic_density_",vargrp[i],".png"), width=9, height=8, units="in", res=300)
  par(mfrow=c(1, 1), mar=c(4.5,4.5,3,1))
  plot(p3,col=cols[3], main=vargrp[i], xlab="Values", cex.main=2, cex.lab=1.5, cex.axis=1.5, lwd=4, ylim=r)
  lines(p2,col=cols[2], lwd=4)
  lines(p1,col=cols[1], lwd=4)
  polygon(p3, col="#7570B37D", border=cols[3]) 
  rug(ndf[ndf$beetles==1,][,vargrp[i]],col=cols[3])
  legend('topright', lty=1, lwd=4, col=cols, legend=c("Continent", "Hosts", "Beetles"), cex = 1.8, bty='n')
  dev.off()
  print(paste(vargrp[i], "is done!"))
}

png("bioclim_density_plots.png", width=15, height=24, units="in", res=300)
par(mfrow=c(8,5),mar=c(3.5,3.5,3,1))
for (i in 1:length(vargrp)){
  density.plot(i)
  if(i==40){
    legend('topright', lty=1, lwd=4, col=cols, legend=c("Continent", "Host", "MPB"), cex = 1.5, bty='n')
  }
}
#plot(0,type='n',axes=FALSE,ann=FALSE)
#legend('center', lty=1, lwd=4, col=cols, legend=c("Continent", "Hosts", "Beetles"), cex = 2, bty='n')
dev.off()

# boxplot

presence.boxplot <- function(i){
  hosts.df <- ndf[,c(vargrp[i],"hosts")]
  colnames(hosts.df)[1] <- "bioclm"
  hosts.df$hosts <- as.character(hosts.df$hosts)
  beetles.df <- ndf[,c(vargrp[i],"beetles")]
  colnames(beetles.df)[1] <- "bioclm"
  beetles.df$beetles <- as.character(beetles.df$beetles)
  boxplot(bioclm~hosts,data=hosts.df, main=vargrp[i], horizontal = TRUE,col="#1B9E777D", outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab="", ylab="") 
  boxplot(bioclm~beetles,data=beetles.df, main=vargrp[i], horizontal = TRUE,col="#7570B37D",outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab="", ylab="")
   print(paste(vargrp[i], "is done!"))
}

for(i in 1:length(vargrp)){
  hosts.df <- ndf[,c(vargrp[i],"hosts")]
  colnames(hosts.df)[1] <- "bioclm"
  hosts.df$hosts <- as.character(hosts.df$hosts)
  beetles.df <- ndf[,c(vargrp[i],"beetles")]
  colnames(beetles.df)[1] <- "bioclm"
  beetles.df$beetles <- as.character(beetles.df$beetles)
  png(paste0("presence_boxplot_",vargrp[i],".png"), width=8, height=6, units="in", res=300)
  par(mfrow=c(2,1),mar=c(4.5,4.5,3,1))
  boxplot(bioclm~hosts,data=hosts.df, main="Hosts", horizontal = TRUE,col="#1B9E777D", outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab="", ylab="Presence") 
  boxplot(bioclm~beetles,data=beetles.df, main="Beetles", horizontal = TRUE,col="#7570B37D",outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab=vargrp[i], ylab="Presence")   
  dev.off() 
  print(paste(vargrp[i], "is done!"))
}

# png("presence_boxplot_bioclm.png", width=18, height=10, units="in", res=300)
# layout(matrix(c(seq(1,11,by=2),seq(2,12,by=2),seq(13,23,by=2),seq(14,24,by=2),
#                 seq(25,35,by=2),seq(26,36,by=2),seq(37,47,by=2),seq(38,48,by=2)),8,6,byrow = TRUE))
# par(mar=c(3,3,3,1))
# for (i in 1:length(vargrp)){
#   presence.boxplot(i)
# }

png("presence_boxplot_bioclm.png", width=12, height=6, units="in", res=300)
layout(matrix(c(seq(1,7,by=2),seq(2,8,by=2),seq(9,15,by=2),seq(10,16,by=2)),4,4,byrow = TRUE))
par(mar=c(3,3,3,1))
for (i in 1:length(vargrp)){
  presence.boxplot(i)
}
# plot(0,type='n',axes=FALSE,ann=FALSE)
# legend('bottomleft',legend="Hosts",fill="#1B9E777D", cex = 2, bty='n')
# plot(0,type='n',axes=FALSE,ann=FALSE)
# legend('topleft',legend= "Beetles",fill="#7570B37D", cex = 2, bty='n')
dev.off()
print("all done!")