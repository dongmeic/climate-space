# Created by Dongmei Chen
library(ncdf4)
library(ggplot2)
library(grid)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

years <- 1997:2016
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
setwd(out)

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vargrp.t <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
				"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax")
vargrp.p <- c("PcumOctSep", "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean",
				"GSP", "POctSep", "PMarAug", "summerP0", "summerP1", "summerP2")
cols <- c("grey70", "#1b9e77", "#d95f02")

csvfile <- "bioclimatic_variables_1997_2016.csv"
indata <- read.csv(csvfile)
n1 <- rep(c(1,2,3,4),5); n2 <- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4))
foreach(i=1:length(c(vargrp.t, vargrp.p))) %dopar% {
  df <- indata[,c(vargrp.t[i], vargrp.p[i], "prs", "yrs")]
  colnames(df)[1:2] <- c("tmp", "pre") 
  climate.space <- function(j){
    df.ss <- subset(df, yrs==years[j])
    colnames(df.ss)[1:2] <- c("tmp", "pre")
    p <- qplot(tmp, pre, data=df.ss, color=factor(prs), alpha=I(0.7), xlab = vargrp.t[i], ylab = vargrp.p[i], main = years[j])
    p <- p + scale_colour_manual(name="Presencce", labels=c("Continent","Hosts","Beetles"), values = cols)
    p <- p + xlim(min(df$tmp), max(df$tmp)) + ylim(min(df$pre), max(df$pre))
    p <- p + theme(title =element_text(size=14, face='bold'), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),legend.position="none")
    return(p) 
  }
  png(paste0("cs_",vargrp.t[i],"_",vargrp.p[i],".png"), width=15, height=12, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(4, 5)))
  for(yr in 1:20){
  	print(climate.space(yr), vp = vplayout(n1[yr], n2[yr]))
  }
  dev.off()
  print(paste("The time-series climate space with variables", vargrp.t[i], "and", vargrp.p[i], "is done!"))  
}

print("all done!")