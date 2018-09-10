# Created by Dongmei Chen

library(ggplot2)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(outpath)

# start_year:1901
vargrp.a <- c("JanTmin", "MarTmin", "TMarAug", "summerTmean", 
				"AugTmean", "AugTmax", "GSP", "PMarAug", "summerP0")
# start_year:1902
vargrp.b <- c("OctTmin", "fallTmean", "winterTmin", "Tmin", "Tmean", "Tvar", "TOctSep", "summerP1", "summerP2", "Pmean")
# start_year:1902, daily
vargrp.e <- c("drop0", "drop5", "ddAugJul", "ddAugJun")
# start_year:1903
vargrp.c <- c("POctSep", "PcumOctSep")
# start_year:1907
vargrp.d <- c("PPT")
vargrp <- c(vargrp.a, vargrp.b, vargrp.e, vargrp.c, vargrp.d)

varnms.a <- c("Minimum temperature in Jan",
			  "Minimum temperature in Mar",
			  "Mean temperature from Mar to Aug",
			  "Mean temperature from Jun to Aug",
			  "Mean temperature in Aug",
			  "Maximum temperature in Aug",
			  "Growing season precipitation",
			  "Sum of precipitation from Mar to Aug",
			  "Sum of precipitation from Jun to Aug")

varnms.b <- c("Minimum temperature in Oct",
			  "Mean temperature from Sep to Nov",
			  "Minimum winter temperature", 
			  "Mean minimum temperature from Nov to Mar", 
			  "Mean temperature from Aug to Jul",
			  "Temperature variation from Aug to Jul",
			  "Mean temperature from Oct to Sep",
			  "Precipitation from Jun to Aug in previous year",
			  "Cumulative precipitation from Jun to Aug",
			  "Mean precipitation from Aug to Jul")
			  
varnms.e <- c("No. days of positive temperature change",
            "No. days when a 0-5 °C drop ",
            "Degree days from August to July",
            "Degree days from August to June")
			  			  
varnms.c <- c("Precipitation from Oct and Sep in previous year",
			  "Cumulative precipitation from Oct to Sep")

varnms.d <- c("Cumulative monthly Oct-Aug precipitation")

varnms <- c(varnms.a, varnms.b, varnms.e, varnms.c, varnms.d)

startyrs <- c(rep(1901,9), rep(1902,14), rep(1903, 2), 1907)

units <- c(rep("(°C)",6), rep("(mm)",3), rep("(°C)",5),"","(°C)",rep("(mm)",3),rep("(day)",2),rep("(°C)",2),rep("(mm)",3))

cols <- c("grey70", "#1b9e77", "#7570b3")
rect <- data.frame(xmin=1996, xmax=2015, ymin=-Inf, ymax=Inf)
override.linetype <- c("dashed", "longdash", "solid")

foreach(i=1:length(varnms)) %dopar% {
  indata <- read.csv(paste0(inpath,vargrp[i],"_",startyrs[i],"_1.csv")) # from time_series_boxplot.R; 1- the whole beetle affected area in all years
  df <- aggregate(indata$var, by=list(prs=indata$prs, yrs=indata$yrs), FUN=mean)
  df.s <- subset(df, prs == "mpb")
  test <- cor.test(df.s$yrs, df.s$x)
  g <- ggplot(data=df, aes(x=yrs, y=x,linetype = prs, color= prs)) + 
    geom_line(alpha=0.3) + 
    geom_point(alpha=0.3) + 
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)
  g <- g + scale_linetype_manual(values=override.linetype, guide = FALSE)
  g <- g + guides(colour = guide_legend(override.aes = list(linetype = override.linetype)))
  g <- g + scale_colour_manual(name="", labels=c("Continent","Hosts","Beetles"), values = cols)
  g <- g + labs(title=varnms[i], subtitle=paste0("Correlation with time in the beetle range: r = ", format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)), 
  							x=paste("Years since", startyrs[i]), y = paste(vargrp[i], units[i]))
  g <- g + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)
  g <- g + geom_vline(xintercept = 2008, color = "black", linetype=4)
  ggsave(paste0("time_series_mean_",vargrp[i],"_",startyrs[i],"_1.png"), g, width=10, height=4, units="in", dpi = 300)
  print(paste(vargrp[i], "is done!"))
}

print("all done!")