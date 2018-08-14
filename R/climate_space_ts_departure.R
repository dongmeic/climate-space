# Created by Dongmei Chen
library(ggplot2)
library(grid)

years <- 1996:2015; nyr <- length(years)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vargrp1 <- c("Tmin", "MarTmin", "TOctSep", "Tmean", "OctTmin", "winterTmin",
							"summerTmean", "PMarAug", "PcumOctSep", "summerP0", "Pmean", "ddAugJun")
							
vargrp2 <- c("TMarAug","AugTmean", "AugTmax", "Tvar", "fallTmean", "JanTmin",
							"PPT", "POctSep", "GSP", "summerP1","summerP2", "ddAugJul")
							
cols <- c("grey70", "#1b9e77", "#7570b3")

l1 <- rep(c(1,2,3,4),5); l2 <- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4))
for (i in 1:length(vargrp1)){
  df.t <- read.csv(paste0(csvpath, vargrp1[i], "_std_", years[1], "_", years[nyr], ".csv"))
  df.p <- read.csv(paste0(csvpath, vargrp2[i], "_std_", years[1], "_", years[nyr], ".csv"))
  length(df.t[,1]); length(df.p[,1]); ml <- min(length(df.t[,1]), length(df.p[,1]))
  df <- cbind(data.frame(v1=tail(df.t[,1],ml)),data.frame(v2=tail(df.p[,1],ml)),data.frame(prs=tail(df.t[,2],ml)),data.frame(yrs=tail(df.t[,3],ml)))
  climate.space <- function(j){
    df.s <- subset(df, yrs==years[j])
    df.ss <- subset(df, yrs==years[j] & prs=="mpb")
    sd1 <- length(na.omit(df.ss[(df.ss$v1 <= 1 & df.ss$v1 > 0) | (df.ss$v1 >= -1 & df.ss$v1 < 0),]$v1))
    sd2 <- length(na.omit(df.ss[(df.ss$v1 <= 2 & df.ss$v1 > 1) | (df.ss$v1 < -1 & df.ss$v1 >= -2),]$v1))
    sd3 <- length(na.omit(df.ss[(df.ss$v1 <= 3 & df.ss$v1 > 2) | (df.ss$v1 < -2 & df.ss$v1 >= -3),]$v1))
    sd4 <- length(na.omit(df.ss[(df.ss$v2 <= 1 & df.ss$v2 > 0) | (df.ss$v2 >= -1 & df.ss$v2 < 0),]$v2))
    sd5 <- length(na.omit(df.ss[(df.ss$v2 <= 2 & df.ss$v2 > 1) | (df.ss$v2 < -1 & df.ss$v2 >= -2),]$v2))
    sd6 <- length(na.omit(df.ss[(df.ss$v2 <= 3 & df.ss$v2 > 2) | (df.ss$v2 < -2 & df.ss$v2 >= -3),]$v2))
    sdsum1 <- length(na.omit(df.ss$v2))
    sdsum <- length(na.omit(df.ss$v1))
    n1 <- round(sd1/sdsum, digits = 1)
    n2 <- round(sd2/sdsum, digits = 1)
    n3 <- round(sd3/sdsum, digits = 1)
    n4 <- round(sd4/sdsum1, digits = 1)
    n5 <- round(sd5/sdsum1, digits = 1)
    n6 <- round(sd6/sdsum1, digits = 1)
  
    p <- qplot(v1, v2, data=df.s, color=factor(prs), alpha=I(0.5), xlab = paste(vargrp1[i], "(SD)"), ylab = paste(vargrp2[i], "(SD)"), main = years[j])+xlim(-5,5)+ylim(-5,5)
    d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
    p <- p + scale_colour_manual(values = cols)
    p <- p + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
    p <- p + theme(title =element_text(size=14, face='bold'), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),legend.position="none")
    return(p) 
  }
  png(paste0("cs_std_",vargrp1[i],"_",vargrp2[i],".png"), width=15, height=12, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(4, 5)))
  for(yr in 1:20){
  	print(climate.space(yr), vp = vplayout(l1[yr], l2[yr]))
  }
  dev.off()
  print(paste("The time-series climate space of departure with variables", vargrp1[i], "and", vargrp2[i], "is done!"))  
}

print("all done!")