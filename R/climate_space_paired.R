# Created by Dongmei Chen
library(ncdf4)
library(ggplot2)
library(grid)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

# functions
# functions to return text in a boxplot
max.n <- function(x){
  if (max(x)>0 & max(x)< 10){
    return(c(y = max(x)*1.32, label = round(max(x),1))) 
  } else if (max(x)< 0){
    return(c(y = max(x)*0.8, label = round(max(x),1)))
  } else{
    return(c(y = max(x)*1.12, label = round(max(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
min.n <- function(x){
  if (min(x) < 0) {
    return(c(y = min(x)*1.16, label = round(min(x),1)))
  } else {
    return(c(y = min(x)*0.32, label = round(min(x),1))) 
  }
  # experiment with the multiplier to find the perfect position
}
# function for mean labels
mean.n <- function(x){
  if (mean(x) > -20 & mean(x) < 0) {
    return(c(y = mean(x)*0.88, label = round(mean(x),1)))
  } else if (mean(x) < -20){
    return(c(y = mean(x)*1.08, label = round(mean(x),1)))
  } else{
    return(c(y = mean(x)*1.02, label = round(mean(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

years <- 1997:2016
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

vargrp.t <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
				"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax")
vargrp.p <- c("PcumOctSep", "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean",
				"Pmean", "POctSep", "PMarAug", "summerP0", "summerP1", "summerP2")
				
varnms.t <- c("Minimum temperature in Oct",
			  "Mean temperature from Sep to Nov",
			  "Minimum winter temperature",
			  "Minimum temperature in Jan",
			  "Minimum temperature in Mar",
			  "Mean minimum temperature from Nov to Mar", 
			  "Mean temperature from Aug to Jul",
			  "Temperature variation from Aug to Jul",
			  "Mean temperature from Oct to Sep",
			  "Mean temperature from Mar to Aug",
			  "Mean temperature from Jun to Aug",
			  "Mean temperature in Aug",
			  "Maximum temperature in Aug")

varnms.p <- c("Cumulative precipitation from Oct to Sep",
			  "Cumulative precipitation from Oct to Sep",
			  "Sum of precipitation from Jun to Aug",
			  "Precipitation from Jun to Aug in previous year",
			  "Cumulative precipitation from Jun to Aug",
			  "Cumulative monthly Oct-Aug precipitation",
			  "Mean precipitation from Aug to Jul",
			  "Mean precipitation from Aug to Jul",
			  "Precipitation from Oct and Sep in previous year",
			  "Sum of precipitation from Mar to Aug",
			  "Sum of precipitation from Jun to Aug",
			  "Precipitation from Jun to Aug in previous year",
			  "Cumulative precipitation from Jun to Aug")

cols <- c("grey70", "#1b9e77", "#d95f02")
			  
get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_1997.2016.4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.dataframe <- function(varp,vart,yr){
  data.p <- get.data(varp)
  data.t <- get.data(vart)
  na.p <- data.p[,,1,yr]
  na.p <- na.p[!is.na(na.p)]
  na.t <- data.t[,,1,yr]
  na.t <- na.t[!is.na(na.t)]
  
  vgt.p <- data.p[,,2,yr]
  vgt.p <- vgt.p[!is.na(vgt.p)]
  vgt.t <- data.t[,,2,yr]
  vgt.t <- vgt.t[!is.na(vgt.t)]
  
  btl.p <- data.p[,,3,yr]
  btl.p <- btl.p[!is.na(btl.p)]
  btl.t <- data.t[,,3,yr]
  btl.t <- btl.t[!is.na(btl.t)]
  
  pre <- c(na.p, vgt.p, btl.p)
  tmp <- c(na.t, vgt.t, btl.t)
  prs <- c(rep("continent",length(na.t)),rep("hosts",length(vgt.t)),rep("mpb",length(btl.t)))
  df <- data.frame(tmp, pre, prs)
  return(df)
}

climate.space.paired <- function(yr,i){
  df <- get.dataframe(vargrp.p[i],vargrp.t[i],yr)
  plot1 <- qplot(tmp,pre, data=df, color=factor(prs), alpha=I(0.7), xlab = varnms.t[i], ylab = varnms.p[i], main = paste("MPB climate space in", toString(years[yr])))
  plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
  plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
  plot2 <- ggplot(df, aes(x=prs, y=tmp, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y=varnms.t[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  plot3 <- ggplot(df, aes(x=prs, y=pre, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y=varnms.p[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  
  png(paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],"_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
  print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
  print(plot2, vp = vplayout(1, 3))
  print(plot3, vp = vplayout(1, 4))
  dev.off()
}

ptm <- proc.time()
print("start plotting climate space")
foreach(i=1:length(years)) %dopar%{
  foreach(j=1:length(vargrp.t)) %dopar%{
    climate.space.paired(i,j)
    print(paste("processed year", years[i], "and variable pair", vargrp.t[j], "and", vargrp.p[j]))
  }
}

print("making an animation")
foreach(i=1:length(vargrp.t)) %dopar%{
  im.convert(paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],"_*.png"), output = paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],".gif"))
}
proc.time() - ptm

climate.space.departure <- function(yr, i){
  df <- get.dataframe(paste0(vargrp.p[i], "_std"),paste0(vargrp.t[i],"_std"),yr)
  df.ss <- subset(df, df$prs=="mpb")
  sd1 <- length(na.omit(df.ss[(df.ss$tmp <= 1 & df.ss$tmp > 0) | (df.ss$tmp >= -1 & df.ss$tmp < 0),]$tmp))
  sd2 <- length(na.omit(df.ss[(df.ss$tmp <= 2 & df.ss$tmp > 1) | (df.ss$tmp < -1 & df.ss$tmp >= -2),]$tmp))
  sd3 <- length(na.omit(df.ss[(df.ss$tmp <= 3 & df.ss$tmp > 2) | (df.ss$tmp < -2 & df.ss$tmp >= -3),]$tmp))
  sd4 <- length(na.omit(df.ss[(df.ss$pre <= 1 & df.ss$pre > 0) | (df.ss$pre >= -1 & df.ss$pre < 0),]$pre))
  sd5 <- length(na.omit(df.ss[(df.ss$pre <= 2 & df.ss$pre > 1) | (df.ss$pre < -1 & df.ss$pre >= -2),]$pre))
  sd6 <- length(na.omit(df.ss[(df.ss$pre <= 3 & df.ss$pre > 2) | (df.ss$pre < -2 & df.ss$pre >= -3),]$pre))
  sdsum1 <- length(na.omit(df[df$prs=="mpb",]$pre))
  sdsum <- length(na.omit(df[df$prs=="mpb",]$tmp))
  n1 <- round(sd1/sdsum, digits = 2)
  n2 <- round(sd2/sdsum, digits = 2)
  n3 <- round(sd3/sdsum, digits = 2)
  n4 <- round(sd4/sdsum1, digits = 2)
  n5 <- round(sd5/sdsum1, digits = 2)
  n6 <- round(sd6/sdsum1, digits = 2)
  
  plot1 <- qplot(tmp, pre, data=df, color=factor(prs), alpha=I(0.7), xlab = paste(varnms.t[i], "(SD)"), ylab = paste(varnms.p[i], "(SD)"), main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
  plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Continent","Hosts","Beetles"), values = cols)
  plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
  d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
  plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
  plot2 <- ggplot(df, aes(x=prs, y=tmp,fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y=paste(varnms.t[i], "(SD)"))+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  plot3 <- ggplot(df, aes(x=prs, y=pre,fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y=paste(varnms.p[i], "(SD)"))+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  
  png(paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],"_std_", toString(years[yr]), ".png"), width=12, height=6, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
  print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
  print(plot2, vp = vplayout(1, 3))
  print(plot3, vp = vplayout(1, 4))
  dev.off()		
}

print("start plotting climate space of departure")
foreach(i=1:length(years)) %dopar%{
  foreach(j=1:length(vargrp.t)) %dopar%{
    climate.space.departure(i,j)
    print(paste("processed year", years[i], "and variable pair", vargrp.t[j], "and", vargrp.p[j]))
  }
}

print("making an animation again")
foreach(i=1:length(vargrp.t)) %dopar%{
  im.convert(paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],"_std_*.png"), output = paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],"_std.gif"))
}

print("all done!")