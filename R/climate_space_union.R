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
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

vargrp.t <- c("Tmin", "MarTmin", "TOctSep", "Tmean", "fallTmean", "OctTmin", "winterTmin",
							"JanTmin", "ddAugJun", "ddAugJul", "TMarAug", "summerTmean")
							
vargrp.p <- c("AugTmean", "AugTmax", "Tvar", "PMarAug", "PcumOctSep", "PPT", "Pmean",
              "POctSep", "summerP2", "GSP", "summerP0", "summerP1")

vargrp <- c(vargrp.t, vargrp.p)

svars <- c("GSP", "PMarAug", "summerP0","summerP1", "summerP2", 
           "Pmean","POctSep", "PcumOctSep", "PPT", "ddAugJul", "ddAugJun")

varnms.t <- c("Mean minimum temperature from Nov to Mar",
				"Minimum temperature in Mar",
				"Mean temperature from Oct to Sep",
				"Mean temperature from Aug to Jul",
			  "Mean temperature from Sep to Nov",
			  "Minimum temperature in Oct",
			  "Minimum winter temperature",
			  "Minimum temperature in Jan",
        "Degree days from August to June",
        "Degree days from August to July",
			  "Mean temperature from Mar to Aug",
			  "Mean temperature from Jun to Aug")

varnms.p <- c("Mean temperature in Aug",
				"Maximum temperature in Aug",
				"Temperature variation from Aug to Jul",
				"Sum of precipitation from Mar to Aug",
				"Cumulative precipitation from Oct to Sep",
				"Cumulative monthly Oct-Aug precipitation",
				"Mean precipitation from Aug to Jul",
				"Precipitation from Oct and Sep in previous year",
				"Cumulative precipitation from Jun to Aug",
				"Growing season precipitation",
				"Sum of precipitation from Jun to Aug",
				"Precipitation from Jun to Aug in previous year")

cols <- c("grey70", "#1b9e77", "#7570b3")

get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.dtcol <- function(var){
  ndf <- data.frame(var=double(), prs=character(), yrs=numeric())
  data <- get.data(var)
  for (yr in 1:nyr){
    na <- data[,,1,yr]
    na <- na[!is.na(na)]
    vgt <- data[,,2,yr]
    vgt <- vgt[!is.na(vgt)]
    btl <- data[,,3,yr]
    btl <- btl[!is.na(btl)]
    valcol <- c(na, vgt, btl)
    prs <- c(rep("continent",length(na)),rep("hosts",length(vgt)),rep("mpb",length(btl)))
    yrs <- c(rep(years[yr],length(prs)))
    df <- data.frame(valcol,prs,yrs)
    ndf <- rbind(ndf, df)
  }
  colnames(ndf)[1] <- var
  if(var %in% svars){
  	ndf[,1] <- sqrt(ndf[,1])
  }
  write.csv(ndf, paste0(outcsvpath, var, "_", years[1], "_", years[nyr], ".csv"), row.names = FALSE)
}

foreach(i = 1:length(vargrp))%dopar%{
  get.dtcol(vargrp[i])
}

# reorganize the data table
df <- read.csv(paste0(outcsvpath, vargrp[1], "_", years[1], "_", years[nyr], ".csv"))
df1 <- data.frame(df[,vargrp[1]])
colnames(df1) <- vargrp[1]
df2 <- df[,2:3]
for(i in 2:length(vargrp)){
  df3 <- read.csv(paste0(outcsvpath, vargrp[i], "_", years[1], "_", years[nyr], ".csv"))
  df4 <- data.frame(df3[,vargrp[i]])
  colnames(df4) <- vargrp[i]
  df1 <- cbind(df1, df4)
  print(paste("adding the variable", vargrp[i]))
}
df5 <- cbind(df1, df2)
write.csv(df5, "bioclimatic_variables_1996_2015.csv", row.names = FALSE)

get.abs.data <- function(var, yr){
	data <- get.data(var)
	na <- data[,,1,yr]
	nav <- na[!is.na(na)]
	vgt <- data[,,2,yr]
  vgtv <- vgt[!is.na(vgt)]
  btl <- data[,,3,yr]
  btlv <- btl[!is.na(btl)]
  vgt.abs <- na[is.na(vgt) & !is.na(na)]
  btl.abs <- na[is.na(btl) & !is.na(na)]
  vals <- c(nav, vgtv, vgt.abs, btlv, btl.abs)
  if(var %in% svars){
  	vals <- sqrt(vals)
  }
  prs <- c(rep("continent",length(nav)),rep("hosts",length(vgtv)),rep("hosts-abs",length(vgt.abs)),rep("mpb",length(btlv)),rep("mpb-abs",length(btl.abs)))
  df <- data.frame(vals, prs)
  return(df)
}

cols2 <- c("grey70", "#1b9e77", "#1B9E777D", "#7570b3", "#7570B37D")
climate.space.paired <- function(i){
  df <- df5[,c(vargrp.t[i], vargrp.p[i], "prs")]
  colnames(df)[1:2] <- c("tmp", "pre")
  plot1 <- qplot(tmp, pre, data=df, color=factor(prs), alpha=I(0.5), xlab = varnms.t[i], ylab = varnms.p[i], main = "MPB climate space")
  plot1 <- plot1 + scale_colour_manual(name="Presence", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
  plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
  
  df <- get.abs.data(vargrp.t[i])
  plot2 <- ggplot(df, aes(x=prs, y=vals, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols2)+theme(axis.ticks.x=element_blank())+labs(x="Presence", y=varnms.t[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+ 
    scale_x_discrete(labels=c("continent" = "Continent", "hosts" = "Hosts", "hosts-abs" = "Hosts-abs", "mpb" = "Beetles", "mpb-abs" = "Beetles-abs"))
  
  df <- get.abs.data(vargrp.p[i])
  plot3 <- ggplot(df, aes(x=prs, y=vals, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols2)+theme(axis.ticks.x=element_blank())+labs(x="Presence", y=varnms.p[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+ 
    scale_x_discrete(labels=c("continent" = "Continent", "hosts" = "Hosts", "hosts-abs" = "Hosts-abs", "mpb" = "Beetles", "mpb-abs" = "Beetles-abs"))

  png(paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],".png"), width=14, height=6, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
  print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
  print(plot2, vp = vplayout(1, 3))
  print(plot3, vp = vplayout(1, 4))
  dev.off()
}

foreach(i = 1:length(vargrp.t))%dopar%{
  climate.space.paired(i)
  print(paste("The union climate space with variables", vargrp.t[i], "and", vargrp.p[i], "is done!"))
}

climate.space <- function(i){
  df.t <- read.csv(paste0(outcsvpath, vargrp.t[i], "_", years[1], "_", years[nyr], ".csv"))
  df.p <- read.csv(paste0(outcsvpath, vargrp.p[i], "_", years[1], "_", years[nyr], ".csv"))
  df <- cbind(data.frame(tmp=df.t[,1]),data.frame(pre=df.p[,1]),data.frame(prs=df.t[,2]))
  df.na <- subset(df, prs=="continent")
  plot(df.na$tmp, df.na$pre, pch=16, cex=0.1, col = alpha(cols[1], 0.5), main=paste0(vargrp.t[i],"/",vargrp.p[i]), xlab=vargrp.t[i], ylab=vargrp.p[i], cex.main=2, cex.lab=1.5, cex.axis=1.5)
  df.vgt <- subset(df, prs=="hosts")
  points(df.vgt$tmp, df.vgt$pre, pch=16, cex=0.1, col = alpha(cols[2], 0.5))
  df.btl <- subset(df, prs=="mpb")
  points(df.btl$tmp, df.btl$pre, pch=16, cex=0.1, col = alpha(cols[3], 0.5))  
}

png("cs_var_union.png", width=13, height=10, units="in", res=300)
par(mfrow=c(3,4),mar=c(5,5,3,1))
for(i in 1:length(vargrp.t)){
  climate.space(i)
  if(i==1){
    legend('topleft', pch=16, col=cols, legend=c("Continent", "Hosts", "Beetles"), bty='n')
  }
  print(paste0("plotting climate space with ", vargrp.t[i], " and ", vargrp.p[i]))
}
#plot(0,type='n',axes=FALSE,ann=FALSE)
#legend('center', pch=16, col=cols, legend=c("Continent", "Hosts", "Beetles"), cex = 2, bty='n')
dev.off()

print("all done!")