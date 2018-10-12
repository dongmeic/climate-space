# Created by Dongmei Chen
# Copy from climate_space_time_series.R

library(ggplot2)
library(grid)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1996:2015; nyr <- length(years)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/ts"
setwd(out)

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vargrp.1 <- c("ddAugJul", "AugTmax", "winterTmin", "summerP1")
							
vargrp.2 <- c("GSP", "summerP0", "PPT", "Tvar")

cols <- c("grey70", "#1b9e77", "#7570b3")

n1 <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5)); n2 <- rep(c(1,2,3,4,5),4) 
foreach(i=1:length(vargrp.1))%dopar%{
  #df <- indata[,c(vargrp.1[i], vargrp.2[i], "prs", "yrs")]
  df.1 <- read.csv(paste0(csvpath, vargrp.1[i], "_",years[1], "_",years[nyr], ".csv")) # climate_space_time_series.R
  df.2 <- read.csv(paste0(csvpath, vargrp.2[i], "_",years[1], "_",years[nyr], ".csv"))
  df <- cbind(data.frame(tmp=df.1[,1]),data.frame(pre=df.2[,1]),data.frame(prs=df.1[,2]),data.frame(yrs=df.1[,3]))
  climate.space <- function(j){
    df.ss <- subset(df, yrs==years[j])
    p <- qplot(tmp, pre, data=df.ss, color=factor(prs), alpha=I(0.5), xlab = vargrp.1[i], ylab = vargrp.2[i], main = years[j])
    p <- p + scale_colour_manual(values = cols)
    p <- p + xlim(min(df$tmp), max(df$tmp)) + ylim(min(df$pre), max(df$pre))
    p <- p + theme(title =element_text(size=14, face='bold'), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),legend.position="none")
    return(p) 
  }
  png(paste0("cs_",vargrp.1[i],"_",vargrp.2[i],"_ts.png"), width=15, height=12, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(4, 5)))
  for(yr in 1:20){
  	print(climate.space(yr), vp = vplayout(n1[yr], n2[yr]))
  }
  dev.off()
  print(paste("The time-series climate space with variables", vargrp.1[i], "and", vargrp.2[i], "is done!"))  
}

print("all done!")