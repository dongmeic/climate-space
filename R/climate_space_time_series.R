# Created by Dongmei Chen
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

vargrp.t <- c("Tmin", "MarTmin", "TOctSep", "Tmean", "fallTmean", "OctTmin", "winterTmin",
							"JanTmin", "ddAugJun", "ddAugJul", "TMarAug", "summerTmean")
							
vargrp.p <- c("AugTmean", "AugTmax", "Tvar", "PMarAug", "PcumOctSep", "PPT", "Pmean",
              "POctSep", "summerP2", "GSP", "summerP0", "summerP1")
        
cols <- c("grey70", "#1b9e77", "#7570b3")

#csvfile <- "bioclimatic_variables_1996_2015.csv" # from climate_space_union.R
#indata <- read.csv(csvfile)

climate_space_time_series <- function(i, labs=T){
  df.t <- read.csv(paste0(csvpath, vargrp.t[i], "_",years[1], "_",years[nyr], ".csv")) # climate_space_time_series.R
  df.p <- read.csv(paste0(csvpath, vargrp.p[i], "_",years[1], "_",years[nyr], ".csv"))
  df <- cbind(data.frame(tmp=df.t[,1]),data.frame(pre=df.p[,1]),data.frame(prs=df.t[,2]),data.frame(yrs=df.t[,3]))
  climate.space <- function(j){
    df.ss <- subset(df, yrs==years[j])
    if(labs){
    	p <- qplot(tmp, pre, data=df.ss, color=factor(prs), alpha=I(0.5), xlab = vargrp.t[i], ylab = vargrp.p[i], main = years[j])   
    }else{
    	p <- qplot(tmp, pre, data=df.ss, color=factor(prs), alpha=I(0.5), xlab = "", ylab = "", main = years[j])
    }  
    p <- p + scale_colour_manual(values = cols)
    p <- p + xlim(min(df$tmp), max(df$tmp)) + ylim(min(df$pre), max(df$pre))
    p <- p + theme(title =element_text(size=14, face='bold'), axis.text=element_text(size=10),axis.title=element_text(size=12,face="bold"),legend.position="none")
    return(p) 
  }
  png(paste0("cs_",vargrp.t[i],"_",vargrp.p[i],"_ts.png"), width=15, height=12, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(4, 5)))
  for(yr in 1:20){
  	print(climate.space(yr), vp = vplayout(n1[yr], n2[yr]))
  }
  dev.off()
}

# plot one pair of bioclimate (ddAugJul and GSP)
i <- 10
climate_space_time_series(i, labs=F)

n1 <- rep(c(1,2,3,4),5); n2 <- c(rep(1,4),rep(2,4),rep(3,4),rep(4,4),rep(5,4))
foreach(i=1:length(vargrp.t))%dopar%{
  #df <- indata[,c(vargrp.t[i], vargrp.p[i], "prs", "yrs")]
	climate_space_time_series(i)
  print(paste("The time-series climate space with variables", vargrp.t[i], "and", vargrp.p[i], "is done!"))  
}

print("all done!")