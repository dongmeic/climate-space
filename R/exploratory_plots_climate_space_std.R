# Created by Dongmei Chen
# Exploratory data analysis: climate space

# load libraries
library(ggplot2)
library(grid)
library(animation)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=12)

# functions
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

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
out <- "/home2/dongmeic/beetle/output/climate_space/paired/"
df.t <- read.csv(paste0(path,"climatic_changes_data_tstd_1.csv"));head(df.t)
df.t <- df.t[,-1]
df.p <- read.csv(paste0(path,"climatic_changes_data_pstd_1.csv"));head(df.p)
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])

str(df)
df.s <- subset(df, df_prs=="climate" & df_yr == 2001)[,1:15]
head(df.s)
## check the combinations of two variables in plots
#png(paste0(out,"variables_plots_std.png"), width=12, height=12, units="in", res=300)
#plot(df.s)
#dev.off()

years = 2000:2014; nyr <- length(years)
ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_ntwstd <= 1 & df.ss$df_ntwstd > 0) | (df.ss$df_ntwstd >= -1 & df.ss$df_ntwstd < 0),]$df_ntwstd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_ntwstd <= 2 & df.ss$df_ntwstd > 1) | (df.ss$df_ntwstd < -1 & df.ss$df_ntwstd >= -2),]$df_ntwstd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_ntwstd <= 3 & df.ss$df_ntwstd > 2) | (df.ss$df_ntwstd < -2 & df.ss$df_ntwstd >= -3),]$df_ntwstd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_xtastd <= 1 & df.ss$df_xtastd > 0) | (df.ss$df_xtastd >= -1 & df.ss$df_xtastd < 0),]$df_xtastd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_xtastd <= 2 & df.ss$df_xtastd > 1) | (df.ss$df_xtastd < -1 & df.ss$df_xtastd >= -2),]$df_xtastd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_xtastd <= 3 & df.ss$df_xtastd > 2) | (df.ss$df_xtastd < -2 & df.ss$df_xtastd >= -3),]$df_xtastd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_xtastd))
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_ntwstd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)
	
	plot1 <- qplot(df_ntwstd,df_xtastd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum winter T (SD)", ylab = "Maximum August T (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntwstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum winter T (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_xtastd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Maximum August T (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_ntw_xta.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_ntw_xta.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_ntw_xta.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_matstd <= 1 & df.ss$df_matstd > 0) | (df.ss$df_matstd >= -1 & df.ss$df_matstd < 0),]$df_matstd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_matstd <= 2 & df.ss$df_matstd > 1) | (df.ss$df_matstd < -1 & df.ss$df_matstd >= -2),]$df_matstd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_matstd <= 3 & df.ss$df_matstd > 2) | (df.ss$df_matstd < -2 & df.ss$df_matstd >= -3),]$df_matstd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_mapstd <= 1 & df.ss$df_mapstd > 0) | (df.ss$df_mapstd >= -1 & df.ss$df_mapstd < 0),]$df_mapstd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_mapstd <= 2 & df.ss$df_mapstd > 1) | (df.ss$df_mapstd < -1 & df.ss$df_mapstd >= -2),]$df_mapstd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_mapstd <= 3 & df.ss$df_mapstd > 2) | (df.ss$df_mapstd < -2 & df.ss$df_mapstd >= -3),]$df_mapstd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_mapstd))
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_matstd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)

	plot1 <- qplot(df_matstd,df_mapstd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Previous water year mean T (SD)", ylab = "Previous water year mean P (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_matstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Previous water year mean T (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_mapstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Previous water year mean P (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_mat_map.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_mat_map.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_mat_map.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_mtaastd <= 1 & df.ss$df_mtaastd > 0) | (df.ss$df_mtaastd >= -1 & df.ss$df_mtaastd < 0),]$df_mtaastd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_mtaastd <= 2 & df.ss$df_mtaastd > 1) | (df.ss$df_mtaastd < -1 & df.ss$df_mtaastd >= -2),]$df_mtaastd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_mtaastd <= 3 & df.ss$df_mtaastd > 2) | (df.ss$df_mtaastd < -2 & df.ss$df_mtaastd >= -3),]$df_mtaastd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_cpjastd <= 1 & df.ss$df_cpjastd > 0) | (df.ss$df_cpjastd >= -1 & df.ss$df_cpjastd < 0),]$df_cpjastd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_cpjastd <= 2 & df.ss$df_cpjastd > 1) | (df.ss$df_cpjastd < -1 & df.ss$df_cpjastd >= -2),]$df_cpjastd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_cpjastd <= 3 & df.ss$df_cpjastd > 2) | (df.ss$df_cpjastd < -2 & df.ss$df_cpjastd >= -3),]$df_cpjastd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_cpjastd))	
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_mtaastd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)

	plot1 <- qplot(df_mtaastd,df_cpjastd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Mean T from April to August (SD)", ylab = "Two-year cumulative P from June to August (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mtaastd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Mean T from April to August (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_cpjastd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Two-year cumulative P from June to August (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_mtaa_cpja.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_mtaa_cpja.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_mtaa_cpja.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_mtastd <= 1 & df.ss$df_mtastd > 0) | (df.ss$df_mtastd >= -1 & df.ss$df_mtastd < 0),]$df_mtastd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_mtastd <= 2 & df.ss$df_mtastd > 1) | (df.ss$df_mtastd < -1 & df.ss$df_mtastd >= -2),]$df_mtastd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_mtastd <= 3 & df.ss$df_mtastd > 2) | (df.ss$df_mtastd < -2 & df.ss$df_mtastd >= -3),]$df_mtastd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_pjastd <= 1 & df.ss$df_pjastd > 0) | (df.ss$df_pjastd >= -1 & df.ss$df_pjastd < 0),]$df_pjastd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_pjastd <= 2 & df.ss$df_pjastd > 1) | (df.ss$df_pjastd < -1 & df.ss$df_pjastd >= -2),]$df_pjastd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_pjastd <= 3 & df.ss$df_pjastd > 2) | (df.ss$df_pjastd < -2 & df.ss$df_pjastd >= -3),]$df_pjastd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_pjastd))	
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_mtastd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)

	plot1 <- qplot(df_mtastd,df_pjastd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Mean August T (SD)", ylab = "P from June to August the previous year (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mtastd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Mean August T (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_pjastd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="P from June to August the previous year (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_mta_pja.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_mta_pja.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_mta_pja.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_ntostd <= 1 & df.ss$df_ntostd > 0) | (df.ss$df_ntostd >= -1 & df.ss$df_ntostd < 0),]$df_ntostd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_ntostd <= 2 & df.ss$df_ntostd > 1) | (df.ss$df_ntostd < -1 & df.ss$df_ntostd >= -2),]$df_ntostd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_ntostd <= 3 & df.ss$df_ntostd > 2) | (df.ss$df_ntostd < -2 & df.ss$df_ntostd >= -3),]$df_ntostd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_cposstd <= 1 & df.ss$df_cposstd > 0) | (df.ss$df_cposstd >= -1 & df.ss$df_cposstd < 0),]$df_cposstd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_cposstd <= 2 & df.ss$df_cposstd > 1) | (df.ss$df_cposstd < -1 & df.ss$df_cposstd >= -2),]$df_cposstd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_cposstd <= 3 & df.ss$df_cposstd > 2) | (df.ss$df_cposstd < -2 & df.ss$df_cposstd >= -3),]$df_cposstd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_cposstd))
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_ntostd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)

	plot1 <- qplot(df_ntostd,df_cposstd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum October T (SD)", ylab = "Two-year cumulative P from October to September (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntostd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum October T (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_cposstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Two-year cumulative P from October to September (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_nto_cpos.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_nto_cpos.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_nto_cpos.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_ntmstd <= 1 & df.ss$df_ntmstd > 0) | (df.ss$df_ntmstd >= -1 & df.ss$df_ntmstd < 0),]$df_ntmstd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_ntmstd <= 2 & df.ss$df_ntmstd > 1) | (df.ss$df_ntmstd < -1 & df.ss$df_ntmstd >= -2),]$df_ntmstd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_ntmstd <= 3 & df.ss$df_ntmstd > 2) | (df.ss$df_ntmstd < -2 & df.ss$df_ntmstd >= -3),]$df_ntmstd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_posstd <= 1 & df.ss$df_posstd > 0) | (df.ss$df_posstd >= -1 & df.ss$df_posstd < 0),]$df_posstd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_posstd <= 2 & df.ss$df_posstd > 1) | (df.ss$df_posstd < -1 & df.ss$df_posstd >= -2),]$df_posstd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_posstd <= 3 & df.ss$df_posstd > 2) | (df.ss$df_posstd < -2 & df.ss$df_posstd >= -3),]$df_posstd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_posstd))	
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_ntmstd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)

	plot1 <- qplot(df_ntmstd,df_posstd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum March T (SD)", ylab = "P from October to September the previous year (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntmstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum March T (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_posstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="P from October to September the previous year (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_ntm_pos.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_ntm_pos.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_ntm_pos.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	df.ss <- subset(df, df_yr == years[yr] & df$df_prs=="mpb")
	sd1 <- length(na.omit(df.ss[(df.ss$df_gspstd <= 1 & df.ss$df_gspstd > 0) | (df.ss$df_gspstd >= -1 & df.ss$df_gspstd < 0),]$df_gspstd))
	sd2 <- length(na.omit(df.ss[(df.ss$df_gspstd <= 2 & df.ss$df_gspstd > 1) | (df.ss$df_gspstd < -1 & df.ss$df_gspstd >= -2),]$df_gspstd))
	sd3 <- length(na.omit(df.ss[(df.ss$df_gspstd <= 3 & df.ss$df_gspstd > 2) | (df.ss$df_gspstd < -2 & df.ss$df_gspstd >= -3),]$df_gspstd))
	sd4 <- length(na.omit(df.ss[(df.ss$df_vgpstd <= 1 & df.ss$df_vgpstd > 0) | (df.ss$df_vgpstd >= -1 & df.ss$df_vgpstd < 0),]$df_vgpstd))
	sd5 <- length(na.omit(df.ss[(df.ss$df_vgpstd <= 2 & df.ss$df_vgpstd > 1) | (df.ss$df_vgpstd < -1 & df.ss$df_vgpstd >= -2),]$df_vgpstd))
	sd6 <- length(na.omit(df.ss[(df.ss$df_vgpstd <= 3 & df.ss$df_vgpstd > 2) | (df.ss$df_vgpstd < -2 & df.ss$df_vgpstd >= -3),]$df_vgpstd))
	sdsum1 <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_vgpstd))
	sdsum <- length(na.omit(df.ss[df.ss$df_prs=="mpb",]$df_gspstd))
	n1 <- round(sd1/sdsum, digits = 2)
	n2 <- round(sd2/sdsum, digits = 2)
	n3 <- round(sd3/sdsum, digits = 2)
	n4 <- round(sd4/sdsum1, digits = 2)
	n5 <- round(sd5/sdsum1, digits = 2)
	n6 <- round(sd6/sdsum1, digits = 2)

	plot1 <- qplot(df_gspstd,df_vgpstd, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Growing season precipitation (SD)", ylab = "Variability of growing season precipitation (SD)", main = paste("MPB climate space in", toString(years[yr]))) + xlim(-5,5)+ylim(-5,5)
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	d=data.frame(x1=c(-3,-2,-1), x2=c(3,2,1), y1=c(-3,-2,-1), y2=c(3,2,1),lab=c(paste(n3,",",n6), paste(n2,",",n5), paste(n1,",",n4)))
	plot1 <- plot1 + geom_rect(data=d, aes(xmin=x1, xmax=x2, ymin=y1, ymax=y2, x = NULL,y = NULL), fill=NA, color="black") + geom_text(data=d, aes(x=x2-0.15, y=y1+0.25,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_gspstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Growing season precipitation (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_vgpstd,fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Variability of growing season precipitation (SD)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste(out,"cs_beetles_chosts_std_", toString(years[yr]), "_gsp_vgp.png", sep = ""), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()		
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_std_*_gsp_vgp.png", sep = ""), output = paste(out,"cs_beetles_chosts_std_gsp_vgp.gif", sep=""))

