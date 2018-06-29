# Created by Dongmei Chen

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
csvfile <- "bioclimatic_values_1996_2015.csv"
ndf <- read.csv(paste0(csvpath,csvfile))
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots"
setwd(out)

vargrp <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
			"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax", 
			 "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean", 
			 "GSP", "POctSep", "PMarAug")

# density plot
density.plot <- function(i){
  p1 <- density(ndf[,vargrp[i]])
  p2 <- density(ndf[ndf$hosts==1,][,vargrp[i]])
  p3 <- density(ndf[ndf$beetles==1,][,vargrp[i]])
  plot(p3,col='red', main=vargrp[i], xlab="", ylab="", cex.main=2, cex.lab=1.5, cex.axis=1.5, lwd=4)
  lines(p2,col='green', lwd=4)
  lines(p1,col='grey', lwd=4)
  polygon(p3, col=rgb(1,0,0,0.2), border='red')
  print(paste(vargrp[i], "is done!"))
}

for(i in 1:length(vargrp)){
  p1 <- density(ndf[,vargrp[i]])
  p2 <- density(ndf[ndf$hosts==1,][,vargrp[i]])
  p3 <- density(ndf[ndf$beetles==1,][,vargrp[i]])
  png(paste0("bioclimatic_density_",vargrp[i],".png"), width=9, height=8, units="in", res=300)
  par(mfrow=c(1, 1), mar=c(4.5,4.5,3,1))
  plot(p3,col='red', main=vargrp[i], xlab="Values", cex.main=2, cex.lab=1.5, cex.axis=1.5, lwd=4)
  lines(p2,col='green', lwd=4)
  lines(p1,col='grey', lwd=4)
  polygon(p3, col=rgb(1,0,0,0.2), border='red') 
  rug(ndf[ndf$beetles==1,][,vargrp[i]],col=rgb(1,0,0,0.05))
  legend('topright', lty=1, lwd=4, col=c("grey", "green", "red"), legend=c("Continent", "Hosts", "Beetles"), cex = 1.8, bty='n')
  dev.off()
  print(paste(vargrp[i], "is done!"))
}

png("bioclimatic_density_plots.png", width=18, height=12, units="in", res=300)
par(mfrow=c(4,6),mar=c(3.5,3.5,3,1))
for (i in 1:length(vargrp)){
  density.plot(i)
}
plot(0,type='n',axes=FALSE,ann=FALSE)
legend('center', lty=1, lwd=4, col=c("grey", "green", "red"), legend=c("Continent", "Hosts", "Beetles"), cex = 2, bty='n')
dev.off()

# boxplot

presence.boxplot <- function(i){
  hosts.df <- ndf[,c(vargrp[i],"hosts")]
  colnames(hosts.df)[1] <- "bioclm"
  hosts.df$hosts <- as.character(hosts.df$hosts)
  beetles.df <- ndf[,c(vargrp[i],"beetles")]
  colnames(beetles.df)[1] <- "bioclm"
  beetles.df$beetles <- as.character(beetles.df$beetles)
  boxplot(bioclm~hosts,data=hosts.df, main=vargrp[i], horizontal = TRUE,col=rgb(0,1,0,0.5), outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab="", ylab="") 
  boxplot(bioclm~beetles,data=beetles.df, main=vargrp[i], horizontal = TRUE,col=rgb(1,0,0,0.5),outcol=rgb(0,0,0,0.1),outcex=0.5,
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
  boxplot(bioclm~hosts,data=hosts.df, main="Hosts", horizontal = TRUE,col=rgb(0,1,0,0.5), outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab="", ylab="Presence") 
  boxplot(bioclm~beetles,data=beetles.df, main="Beetles", horizontal = TRUE,col=rgb(1,0,0,0.5),outcol=rgb(0,0,0,0.1),outcex=0.5,
   xlab=vargrp[i], ylab="Presence")   
  dev.off() 
  print(paste(vargrp[i], "is done!"))
}

png("presence_boxplot_bioclm.png", width=18, height=10, units="in", res=300)
layout(matrix(c(seq(1,11,by=2),seq(2,12,by=2),seq(13,23,by=2),seq(14,24,by=2),
seq(25,35,by=2),seq(26,36,by=2),seq(37,47,by=2),seq(38,48,by=2)),8,6,byrow = TRUE))
par(mar=c(3,3,3,1))
for (i in 1:length(vargrp)){
  presence.boxplot(i)
}
plot(0,type='n',axes=FALSE,ann=FALSE)
legend('bottomleft',legend="Hosts",fill=rgb(0,1,0,0.5), cex = 2, bty='n')
plot(0,type='n',axes=FALSE,ann=FALSE)
legend('topleft',legend= "Beetles",fill=rgb(1,0,0,0.5), cex = 2, bty='n')
dev.off()
print("all done!")
