# Created by Dongmei Chen
# a revision from exploratory_plots_temporal.R
library(ncdf4)
library(ggplot2)
library(grid)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

# functions to return text in a boxplot
max.n <- function(x){
  if (max(x)< 10){
  return(c(y = max(x)*1.32, label = round(max(x),1))) 
  } else {
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
  if (mean(x) < 0) {
    return(c(y = mean(x)*0.88, label = round(mean(x),1)))
  } else {
    return(c(y = mean(x)*1.02, label = round(mean(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
setwd(out)

# read vegetation and bettle presence data
prs_path <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/prs/"
vgt_ncfile <- "na10km_v2_presence_pines.nc"
ncin_vgt <- nc_open(paste(prs_path,vgt_ncfile,sep=""))
print(ncin_vgt)
vgt.nc <- ncvar_get(ncin_vgt,"navgtprs")
btl.all <- ncvar_get(ncin_vgt, "nabtlprsayr")
btl_ncfile <- "na10km_v2_mpb_presence.nc"
ncin_btl <- nc_open(paste(prs_path,btl_ncfile, sep=""))
print(ncin_btl)
btl.nc <- ncvar_get(ncin_btl,"mpb_prs") # with all hosts
mpb.nc <- ncvar_get(ncin_btl,"chosts_mpb_prs") # with all core hosts
nc_close(ncin_btl)

varsnm <- "Tmin"
varnm <- "Mean minimum temperature from Nov to Mar"
startyr <- 1902

get.data <- function(var, start_yr){
  ncfile <- paste0("na10km_v2_",var, "_", start_yr,".2016.3d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}

get.dataframe <- function(varnm,start_yr){
  years <- start_yr:2016
  nyr <- length(years)
  ndf <- data.frame(var=double(), prs=factor(), yrs=factor())
  data <- get.data(varnm,start_yr)
  for (yr in 1:nyr){
  	print(paste("processing", varnm, "in", years[yr]))
    na <- data[,,yr]
    navls <- na[!is.na(na)]
    
    vgt <- na[which(vgt.nc==1)]
    vgtvls <- vgt[!is.na(vgt)]
    
    if(yr < nyr-20+1){
      btl <- na[which(btl.all==1)]
      btlvls <- btl[!is.na(btl)]
    }else{
      btl <- btl <- na[which(btl.nc[,,yr-nyr+20]==1)]
      btlvls <- btl[!is.na(btl)]
    }
    
    var <- c(navls, vgtvls, btlvls)
    prs <- c(rep("continent",length(navls)),rep("hosts",length(vgtvls)),rep("mpb",length(btlvls)))
    yrs <- rep(toString(years[yr]),length(prs))
    df <- data.frame(var,prs,yrs)
    ndf <- rbind(ndf, df)
  }
  write.csv(ndf, paste0(out, varnm, "_", start_yr, ".csv"), row.names = FALSE)
  return(ndf)
}

ptm <- proc.time()
cols <- c("grey70", "#1b9e77", "#d95f02")
df <- get.dataframe(varsnm, startyr)
print(paste("plotting", varsnm))
df.ss.1 <- subset(df, prs == "continent")
p1 <- ggplot(df.ss.1, aes(x = yrs, y = var)) +geom_boxplot(fill = cols[1], colour = "black", outlier.size = 0.75, 
														 outlier.shape = 1, outlier.alpha = 0.35)+
labs(x="Time", y=varnm)+theme(axis.text.x=element_blank())+
ggtitle("Climatic changes in North America")

df.ss.2 <- subset(df, prs == "hosts")
p2 <- ggplot(df.ss.2, aes(x = yrs, y = var)) +geom_boxplot(fill = cols[2], colour = "black", outlier.size = 0.75, 
														 outlier.shape = 1, outlier.alpha = 0.35)+
labs(x="Time", y=varnm)+theme(axis.text.x=element_blank())+
ggtitle("Climatic changes in areas where core hosts exist")

df.ss.3 <- subset(df, prs == "mpb")
df.ss.3$btl <- ifelse(as.numeric(as.character(df.ss.3$yrs)) > 1996, c('Presence with one year'), c('Presence with all years')) 
p3 <- ggplot(df.ss.3, aes(x = yrs, y = var)) +geom_boxplot(fill = cols[3], colour = "black", outlier.size = 0.75, 
														 outlier.shape = 1, outlier.alpha = 0.35)+
facet_grid(. ~ btl, scales = "free", space = "free")+
labs(x="Time", y=varnm)+theme(axis.text.x=element_blank())+
ggtitle("Climatic changes in areas where mountain pine beetles exist")

png(paste0(out,"plots/temporal_plots_", varsnm, "_", startyr, ".png"), width=16, height=12, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(3, 1))) # 3 rows, 1 column
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(3, 1))
dev.off()
print(paste(varsnm,"done!"))
proc.time() - ptm

print("all done!")