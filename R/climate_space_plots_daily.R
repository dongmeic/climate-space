# Created by Dongmei Chen

csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
csvfile <- "bioclimatic_values_1996_2015_daily.csv" # from PCA_bioclimatic_variables.R
ndf <- read.csv(paste0(csvpath,csvfile))
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots"
setwd(out)

vargrp <- c("drop0", "drop5", "ddAugJul", "ddAugJun")

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

png("bioclimatic_density_plots_daily.png", width=6, height=6, units="in", res=300)
par(mfrow=c(2,2),mar=c(3.5,3.5,3,1))
for (i in 1:length(vargrp)){
  density.plot(i)
  if(i==length(vargrp)){
   legend('topright', lty=1, lwd=4, col=c("grey", "green", "red"), legend=c("Continent", "Hosts", "Beetles"),bty='n')
  }
}
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

png("presence_boxplot_bioclm_daily.png", width=6, height=6, units="in", res=300)
layout(matrix(c(seq(1,3,by=2),seq(2,4,by=2),seq(5,7,by=2),seq(6,8,by=2)),4,2,byrow = TRUE))
par(mar=c(3,3,3,1))
for (i in 1:length(vargrp)){
  presence.boxplot(i)
}
# plot(0,type='n',axes=FALSE,ann=FALSE)
# legend('bottomleft',legend="Hosts",fill=rgb(0,1,0,0.5), cex = 2, bty='n')
# plot(0,type='n',axes=FALSE,ann=FALSE)
# legend('topleft',legend= "Beetles",fill=rgb(1,0,0,0.5), cex = 2, bty='n')
dev.off()
print("all done!")