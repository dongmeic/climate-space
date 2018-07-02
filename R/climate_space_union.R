# Created by Dongmei Chen
library(ncdf4)
library(ggplot2)
library(grid)
# library(animation)
# library(parallel)
# library(doParallel)
# library(foreach)
# registerDoParallel(cores=28)

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

years <- 1996:2015; nyr <- length(years)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/"
outcsvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

vargrp.t <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
				"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax")
vargrp.p <- c("PcumOctSep", "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean",
				"GSP", "POctSep", "PMarAug", "summerP0", "summerP1", "summerP2")

vargrp <- c("OctTmin", "fallTmean", "winterTmin", "JanTmin", "MarTmin", "Tmin", "Tmean", 
			"Tvar", "TOctSep", "TMarAug", "summerTmean", "AugTmean", "AugTmax", 
			 "PcumOctSep", "summerP0", "summerP1", "summerP2", "PPT", "Pmean", 
			 "GSP", "POctSep", "PMarAug")
			
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
			  "Growing season precipitation",
			  "Precipitation from Oct and Sep in previous year",
			  "Sum of precipitation from Mar to Aug",
			  "Sum of precipitation from Jun to Aug",
			  "Precipitation from Jun to Aug in previous year",
			  "Cumulative precipitation from Jun to Aug")

cols <- c("grey70", "#1b9e77", "#d95f02")

# get.data <- function(var){
#   ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
#   ncin <- nc_open(paste0(ncpath, ncfile))
#   data <- ncvar_get(ncin,var)
#   fillvalue <- ncatt_get(ncin,var,"_FillValue")
#   data[data==fillvalue$value] <- NA
#   return(data)
# }
# 
# get.dtcol <- function(var){
#   ndf <- data.frame(var=double(), prs=character(), yrs=numeric())
#   data <- get.data(var)
#   for (yr in 1:nyr){
#     na <- data[,,1,yr]
#     na <- na[!is.na(na)]
#     vgt <- data[,,2,yr]
#     vgt <- vgt[!is.na(vgt)]
#     btl <- data[,,3,yr]
#     btl <- btl[!is.na(btl)]
#     valcol <- c(na, vgt, btl)
#     prs <- c(rep("continent",length(na)),rep("hosts",length(vgt)),rep("mpb",length(btl)))
#     yrs <- c(rep(years[yr],length(prs)))
#     df <- data.frame(valcol,prs,yrs)
#     ndf <- rbind(ndf, df)
#   }
#   colnames(ndf)[1] <- var
#   write.csv(ndf, paste0(outcsvpath, var, "_",years[1], "_",years[nyr], ".csv"), row.names = FALSE)
# }
# 
# foreach(i = 1:length(vargrp))%dopar%{
#   get.dtcol(vargrp[i])
# }
# 
# # reorganize the data table
# df <- read.csv(paste0(outcsvpath, vargrp[1], "_",years[1], "_",years[nyr], ".csv"))
# df1 <- data.frame(df[,vargrp[1]])
# colnames(df1) <- vargrp[1]
# df2 <- df[,2:3]
# for(i in 2:length(vargrp)){
#   df3 <- read.csv(paste0(outcsvpath, vargrp[i], "_",years[1], "_",years[nyr], ".csv"))
#   df4 <- data.frame(df3[,vargrp[i]])
#   colnames(df4) <- vargrp[i]
#   df1 <- cbind(df1, df4)
#   print(paste("adding the variable", vargrp[i]))
# }
# df5 <- cbind(df1, df2)
# write.csv(df5, "bioclimatic_variables_1996_2015.csv", row.names = FALSE)
# 
# climate.space.paired <- function(i){
#   df <- df5[,c(vargrp.t[i], vargrp.p[i], "prs")]
#   colnames(df)[1:2] <- c("tmp", "pre")
#   plot1 <- qplot(tmp, pre, data=df, color=factor(prs), alpha=I(0.7), xlab = varnms.t[i], ylab = varnms.p[i], main = "MPB climate space")
#   plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
#   plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
#   plot2 <- ggplot(df, aes(x=prs, y=tmp, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y=varnms.t[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
#     stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
#   plot3 <- ggplot(df, aes(x=prs, y=pre, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols)+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y=varnms.p[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
#     stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
#   
#   png(paste0(out,"cs_",vargrp.t[i],"_",vargrp.p[i],".png"), width=12, height=6, units="in", res=300)
#   grid.newpage()
#   par(mar=c(2,2,4,2))
#   pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
#   print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
#   print(plot2, vp = vplayout(1, 3))
#   print(plot3, vp = vplayout(1, 4))
#   dev.off()
# }

# foreach(i = 1:length(vargrp.t))%dopar%{
#   climate.space.paired(i)
#   print(paste("The union climate space with variables", vargrp.t[i], "and", vargrp.p[i], "is done!"))
# }

climate.space <- function(i){
  df.t <- read.csv(paste0(outcsvpath, vargrp.t[i], "_",years[1], "_",years[nyr], ".csv"))
  df.p <- read.csv(paste0(outcsvpath, vargrp.p[i], "_",years[1], "_",years[nyr], ".csv"))
  df <- cbind(data.frame(tmp=df.t[,1]),data.frame(pre=df.p[,1]),data.frame(prs=df.t[,2]))
  df.na <- subset(df, prs=="continent")
  plot(df.na$tmp, df.na$pre, pch=16, cex=0.1, col = alpha(cols[1], 0.2), main=paste0(vargrp.t[i],"/",vargrp.p[i]), xlab=vargrp.t[i], ylab=vargrp.p[i], cex.main=2, cex.lab=1.5, cex.axis=1.5)
  df.vgt <- subset(df, prs=="hosts")
  points(df.vgt$tmp, df.vgt$pre, pch=16, cex=0.1, col = alpha(cols[2], 0.2))
  df.btl <- subset(df, prs=="mpb")
  points(df.btl$tmp, df.btl$pre, pch=16, cex=0.1, col = alpha(cols[3], 0.2))  
}
png("cs_monthly_var_union.png", width=16, height=10, units="in", res=300)
par(mfrow=c(3,5),mar=c(5,5,3,1))
for(i in 1:length(vargrp.t)){
  climate.space(i)
  print(paste0("plotting climate space with ", vargrp.t[i], " and ", vargrp.p[i]))
}
plot(0,type='n',axes=FALSE,ann=FALSE)
legend('center', pch=16, col=cols, legend=c("Continent", "Hosts", "Beetles"), cex = 2, bty='n')
dev.off()

print("all done!")		  