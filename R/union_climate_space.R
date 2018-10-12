# Created by Dongmei Chen
# Copy from climate_space_union.R

library(ncdf4)
library(ggplot2)
library(grid)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/boxplot_settings.R")

years <- 1996:2015; nyr <- length(years)
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

vars1 <- c("ddAugJul", "AugTmax", "Tmin", "TOctSep")
vars2 <- c("GSP", "Tvar", "PMarAug", "PPT")

vargrp <- c(vars1, vars2)

varnms1 <- c("Sqrt(Day-degrees above 5.5 °C from Aug to Jul)",
						 "Maximum temperature in Aug (°C)",
						 "Mean minimum temperature (°C, Nov - Mar)",
						 "Water-year mean temperature (°C, Oct - Sep)")

varnms2 <- c("Sqrt(Growing season precipitation, mm)",
						 "Seasonal temperature variation (Aug - Jul)",
						 "Sqrt(Sum of precipitation from Mar to Aug, mm)",
						 "Sqrt(Cumulative monthly Oct-Aug precipitation, mm)")

varnms <- c(varnms1, varnms2)
cols <- c("grey70", "#1b9e77", "#7570b3")

get.data <- function(var){
  ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

# reorganize the data table
df <- read.csv(paste0(csvpath, vargrp[1], "_", years[1], "_", years[nyr], ".csv"))
df1 <- data.frame(df[,vargrp[1]])
colnames(df1) <- vargrp[1]
df2 <- df[,2:3]
for(i in 2:length(vargrp)){
  df3 <- read.csv(paste0(csvpath, vargrp[i], "_", years[1], "_", years[nyr], ".csv"))
  df4 <- data.frame(df3[,vargrp[i]])
  colnames(df4) <- vargrp[i]
  df1 <- cbind(df1, df4)
  print(paste("adding the variable", vargrp[i]))
}
df5 <- cbind(df1, df2)

climate.space <- function(i){
	df <- df5[,c(vars1[i], vars2[i], "prs")]
  colnames(df)[1:2] <- c("x", "y")
  df <- df[order(df$prs),]
  plot <- qplot(x, y, data=df, color=factor(prs), alpha=I(0.5), xlab = varnms1[i], ylab = varnms2[i])  
	plot <- plot + scale_colour_manual(name="Presence", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
  plot <- plot + theme(axis.text=element_text(size=11),axis.title=element_text(size=11))
	return(plot)
}

png(paste0(out,"union_cs_var.png"), width=10, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,2,2))
pushViewport(viewport(layout = grid.layout(2, 2)))
plot1 <- climate.space(1)
print(plot1, vp = vplayout(1, 1))
plot2 <- climate.space(2)
print(plot2, vp = vplayout(1, 2))
plot3 <- climate.space(3)
print(plot3, vp = vplayout(2, 1))
plot4 <- climate.space(4)
print(plot4, vp = vplayout(2, 2))
dev.off()