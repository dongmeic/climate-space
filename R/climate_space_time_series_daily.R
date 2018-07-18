# Created by Dongmei Chen
library(ggplot2)
library(grid)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1996:2015; nyr <- length(years)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
setwd(out)

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vargrp1 <- c("drop0", "drop5")
vargrp2 <- c("ddAugJul", "ddAugJun")
cols <- c("grey70", "#1b9e77", "#d95f02")

n1 <- rep(c(1,2,3,4),5); n2 <- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4))
foreach(i=1:length(vargrp1))%dopar%{
  df1 <- read.csv(paste0(csvpath, vargrp1[i], "_",years[1], "_",years[nyr], ".csv"))
  df2 <- read.csv(paste0(csvpath, vargrp2[i], "_",years[1], "_",years[nyr], ".csv"))
  df <- cbind(data.frame(drop=df1[,1]),data.frame(dd=df2[,1]),data.frame(prs=df1[,2]),data.frame(yrs=df1[,3]))
  climate.space <- function(j){
    df.ss <- subset(df, yrs==years[j])
    p <- qplot(drop, dd, data=df.ss, color=factor(prs), alpha=I(0.7), xlab = vargrp1[i], ylab = vargrp2[i], main = years[j])
    p <- p + scale_colour_manual(values = cols)
    p <- p + xlim(min(df$drop), max(df$drop)) + ylim(min(df$dd), max(df$dd))
    p <- p + theme(title =element_text(size=14, face='bold'), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),legend.position="none")
    return(p) 
  }
  png(paste0("cs_",vargrp1[i],"_",vargrp2[i],"_ts.png"), width=15, height=12, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(4, 5)))
  for(yr in 1:20){
  	print(climate.space(yr), vp = vplayout(n1[yr], n2[yr]))
  }
  dev.off()
  print(paste("The time-series climate space with variables", vargrp1[i], "and", vargrp2[i], "is done!"))  
}

print("all done!")