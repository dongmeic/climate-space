# Created by Dongmei Chen
library(ncdf4)
library(ggplot2)
library(grid)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/boxplot_settings.R")
years <- 1996:2015; nyr <- length(years)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)
			
vargrp1 <- c("drop0", "drop5")
vargrp2 <- c("ddAugJul", "ddAugJun")
				
varnms1 <- c("No. days of positive temperature change",
             "No. days when a 0-5 °C drop ")

varnms2 <- c("Degree days from August to July",
             "Degree days from August to June")

cols <- c("grey70", "#1b9e77", "#d95f02")
			  
get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.dataframe <- function(var2,var1,yr){
  data2  <- get.data(var2)
  data1 <- get.data(var1)
  na2  <- data2[,,1,yr]
  na2  <- na2[!is.na(na2)]
  na1 <- data1[,,1,yr]
  na1 <- na1[!is.na(na1)]
  
  vgt2  <- data2[,,2,yr]
  vgt2  <- vgt2[!is.na(vgt2)]
  vgt1 <- data1[,,2,yr]
  vgt1 <- vgt1[!is.na(vgt1)]
  
  btl2  <- data2[,,3,yr]
  btl2  <- btl2[!is.na(btl2)]
  btl1 <- data1[,,3,yr]
  btl1 <- btl1[!is.na(btl1)]
  
  dd <- c(na2, vgt2, btl2)
  drop  <- c(na1, vgt1, btl1)
  prs <- c(rep("continent",length(na1)),rep("hosts",length(vgt1)),rep("mpb",length(btl1)))
  df <- data.frame(drop, dd, prs)
  return(df)
}

climate.space.paired <- function(yr,i){
  df <- get.dataframe(vargrp2[i],vargrp1[i],yr)
  plot1 <- qplot(drop, dd, data=df, color=factor(prs), alpha=I(0.7), xlab = varnms1[i], ylab = varnms2[i], main = paste("MPB climate space in", toString(years[yr])))
  plot1 <- plot1 + xlim(range(get.data(vargrp1[i]), na.rm=T)[1], range(get.data(vargrp1[i]), na.rm=T)[2]) + ylim(range(get.data(vargrp2[i]), na.rm=T)[1], range(get.data(vargrp2[i]), na.rm=T)[2])
  plot1 <- plot1 + scale_colour_manual(name="Presence", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
  plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
  plot2 <- ggplot(df, aes(x=prs, y=drop, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presence", y=varnms1[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  plot3 <- ggplot(df, aes(x=prs, y=dd, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presence", y=varnms2[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  
  png(paste0(out,"cs_",vargrp1[i],"_",vargrp2[i],"_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
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
  foreach(j=1:length(vargrp1)) %dopar%{
    climate.space.paired(i,j)
    print(paste("processed year", years[i], "and variable pair", vargrp1[j], "and", vargrp2[j]))
  }
}

print("making an animation")
foreach(i=1:length(vargrp1)) %dopar%{
  im.convert(paste0(out,"cs_",vargrp1[i],"_",vargrp2[i],"_*.png"), output = paste0(out,"cs_",vargrp1[i],"_",vargrp2[i],".gif"))
}
proc.time() - ptm

print("all done!")