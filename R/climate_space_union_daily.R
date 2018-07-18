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

vargrp1 <- c("drop0", "drop5")
vargrp2 <- c("ddAugJul", "ddAugJun")
vargrp <- c("drop0", "drop5", "ddAugJul", "ddAugJun")
				
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
  write.csv(ndf, paste0(outcsvpath, var, "_",years[1], "_",years[nyr], ".csv"), row.names = FALSE)
}

foreach(i = 1:length(vargrp))%dopar%{
  get.dtcol(vargrp[i])
}

# reorganize the data table
df <- read.csv(paste0(outcsvpath, vargrp[1], "_",years[1], "_",years[nyr], ".csv"))
df1 <- data.frame(df[,vargrp[1]])
colnames(df1) <- vargrp[1]
df2 <- df[,2:3]
for(i in 2:length(vargrp)){
  df3 <- read.csv(paste0(outcsvpath, vargrp[i], "_",years[1], "_",years[nyr], ".csv"))
  df4 <- data.frame(df3[,vargrp[i]])
  colnames(df4) <- vargrp[i]
  df1 <- cbind(df1, df4)
  print(paste("adding the variable", vargrp[i]))
}
df5 <- cbind(df1, df2)
write.csv(df5, "bioclimatic_variables_1996_2015.csv", row.names = FALSE)

climate.space.paired <- function(i){
  df <- df5[,c(vargrp1[i], vargrp2[i], "prs")]
  colnames(df)[1:2] <- c("drop", "dd")
  plot1 <- qplot(drop, dd, data=df, color=factor(prs), alpha=I(0.7), xlab = varnms1[i], ylab = varnms2[i], main = "MPB climate space")
  plot1 <- plot1 + scale_colour_manual(name="ddsencce", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
  plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
  plot2 <- ggplot(df, aes(x=prs, y=drop, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="ddsencce", y=varnms1[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
  plot3 <- ggplot(df, aes(x=prs, y=dd, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="ddsencce", y=varnms2[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")

  png(paste0(out,"cs_",vargrp1[i],"_",vargrp2[i],".png"), width=12, height=6, units="in", res=300)
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
  print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
  print(plot2, vp = vplayout(1, 3))
  print(plot3, vp = vplayout(1, 4))
  dev.off()
}

foreach(i = 1:length(vargrp1))%dopar%{
  climate.space.paired(i)
  print(paste("The union climate space with variables", vargrp1[i], "and", vargrp2[i], "is done!"))
}

climate.space <- function(i){
  df1 <- read.csv(paste0(outcsvpath, vargrp1[i], "_",years[1], "_",years[nyr], ".csv"))
  df2 <- read.csv(paste0(outcsvpath, vargrp2[i], "_",years[1], "_",years[nyr], ".csv"))
  df <- cbind(data.frame(drop=df1[,1]),data.frame(dd=df2[,1]),data.frame(prs=df1[,2]))
  df.na <- subset(df, prs=="continent")
  plot(df.na$drop, df.na$dd, pch=16, cex=0.1, col = alpha(cols[1], 0.2), main=paste0(vargrp1[i],"/",vargrp2[i]), xlab=vargrp1[i], ylab=vargrp2[i], cex.main=2, cex.lab=1.5, cex.axis=1.5)
  df.vgt <- subset(df, prs=="hosts")
  points(df.vgt$drop, df.vgt$dd, pch=16, cex=0.1, col = alpha(cols[2], 0.2))
  df.btl <- subset(df, prs=="mpb")
  points(df.btl$drop, df.btl$dd, pch=16, cex=0.1, col = alpha(cols[3], 0.2))  
}
png("cs_monthly_var_union.png", width=16, height=10, units="in", res=300)
par(mfrow=c(1,3),mar=c(5,5,3,1))
for(i in 1:length(vargrp1)){
  climate.space(i)
  print(paste("plotting climate space with", vargrp1[i], "and", vargrp2[i]))
}
plot(0,type='n',axes=FALSE,ann=FALSE)
legend('center', pch=16, col=cols, legend=c("Continent", "Hosts", "Beetles"), cex = 2, bty='n')
dev.off()

print("all done!")