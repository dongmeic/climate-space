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
df.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(path,"climatic_changes_data_pre_1.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])

str(df)
df.s <- subset(df, df_prs=="climate" & df_yr == 2001)[,1:15]
head(df.s)
## check the combinations of two variables in plots
#png(paste0(out,"variables_plots.png"), width=12, height=12, units="in", res=300)
#plot(df.s)
#dev.off()

years = 2000:2014; nyr <- length(years)
ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	# calculate the points in each "space"
	tp1 <- length(na.omit(df.s[df.s$df_ntw > -5 & df.s$df_mta > 15 & df.s$df_prs=="mpb",]$df_ntw))
	tp2 <- length(na.omit(df.s[df.s$df_ntw > -5 & df.s$df_mta <= 15 & df.s$df_prs=="mpb",]$df_ntw))
	tp3 <- length(na.omit(df.s[df.s$df_ntw <= -5 & df.s$df_mta > 15 & df.s$df_prs=="mpb",]$df_ntw))
	tp4 <- length(na.omit(df.s[df.s$df_ntw <= -5 & df.s$df_mta <= 15 & df.s$df_prs=="mpb",]$df_ntw))
	tpsum <- length(na.omit(df.s[df.s$df_prs=="mpb",]$df_ntw))
	n1 <- round(tp1/tpsum*100, digits = 1)
	n2 <- round(tp2/tpsum*100, digits = 1)
	n3 <- round(tp3/tpsum*100, digits = 1)
	n4 <- round(tp4/tpsum*100, digits = 1)

	plot1 <- qplot(df_ntw,df_mta, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum winter T (°C)", ylab = "Maximum August T (°C)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot1 <- plot1 + geom_vline(xintercept = -40, color = "black", linetype=4)
	plot1 <- plot1 + geom_vline(xintercept = -5, color = "black", linetype=4)
	plot1 <- plot1 + geom_hline(yintercept = 15, color = "black", linetype=4)
	plot1 <- plot1 + xlim(-55, 28) + ylim(-18,39)
	d=data.frame(x1=c(10,10,-20,-20), y1=c(20,10,20,10), lab=c(paste(toString(n1), "%"), paste(toString(n2), "%"), paste(toString(n3), "%"), paste(toString(n4), "%")))
	plot1 <- plot1 + geom_text(data=d, aes(x=x1, y=y1,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntw, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum winter T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_mta, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Maximum August T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_ntw_mta_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_ntw_mta_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_ntw_mta.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	# calculate the points in each "space"
	tp1 <- length(na.omit(df.s[df.s$df_mat < 0 & df.s$df_prs=="mpb",]$df_mat))
	tp2 <- length(na.omit(df.s[df.s$df_mat > 2 & df.s$df_prs=="mpb",]$df_mat))
	tp3 <- length(na.omit(df.s[df.s$df_mat > 0 & df.s$df_mat < 2 & df.s$df_prs=="mpb",]$df_mat))
	tpsum <- length(na.omit(df.s[df.s$df_prs=="mpb",]$df_mat))
	n1 <- round(tp1/tpsum*100, digits = 1)
	n2 <- round(tp2/tpsum*100, digits = 1)
	n3 <- round(tp3/tpsum*100, digits = 1)

	plot1 <- qplot(df_mat,df_map, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Previous water year mean T (°C)", ylab = "Previous water year mean P (mm)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot1 <- plot1 + geom_vline(xintercept = 0, color = "black", linetype=4)
	plot1 <- plot1 + geom_vline(xintercept = 2, color = "black", linetype=4)
	plot1 <- plot1 + xlim(-30, 31) + ylim(0,914)
	d=data.frame(x1=c(-8,10), y1=c(500,500), lab=c(paste(toString(n1), "%"), paste(toString(n2), "%")))
	plot1 <- plot1 + geom_text(data=d, aes(x=x1, y=y1,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mat, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Previous water year mean T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_map, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Previous water year mean P (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_mat_map_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_mat_map_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_mat_map.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	# calculate the points in each "space"
	tp1 <- length(na.omit(df.s[df.s$df_cpja < 200 & df.s$df_prs=="mpb",]$df_cpja))
	tp2 <- length(na.omit(df.s[df.s$df_cpja >= 200 & df.s$df_prs=="mpb",]$df_cpja))
	tpsum <- length(na.omit(df.s[df.s$df_prs=="mpb",]$df_cpja))
	n1 <- round(tp1/tpsum*100, digits = 1)
	n2 <- round(tp2/tpsum*100, digits = 1)

	plot1 <- qplot(df_mtaa,df_cpja, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Mean T from April to August (°C)", ylab = "Two-year cumulative P from June to August (mm)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot1 <- plot1 + geom_hline(yintercept = 200, color = "black", linetype=4)
	plot1 <- plot1 + xlim(-21, 35) + ylim(0,6675)
	d=data.frame(x1=c(12,12), y1=c(10,500), lab=c(paste(toString(n1), "%"), paste(toString(n2), "%")))
	plot1 <- plot1 + geom_text(data=d, aes(x=x1, y=y1,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mtaa, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Mean T from April to August (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_cpja, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Two-year cumulative P from June to August (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_mtaa_cpja_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_mtaa_cpja_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_mtaa_cpja.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])
	# calculate the points in each "space"
	tp1 <- length(na.omit(df.s[df.s$df_pja <= 200 & df.s$df_prs=="mpb",]$df_pja))
	tp2 <- length(na.omit(df.s[df.s$df_pja > 200 & df.s$df_prs=="mpb",]$df_pja))
	tpsum <- length(na.omit(df.s[df.s$df_prs=="mpb",]$df_pja))
	n1 <- round(tp1/tpsum*100, digits = 1)
	n2 <- round(tp2/tpsum*100, digits = 1)

	plot1 <- qplot(df_mta,df_pja, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Mean August T (°C)", ylab = "P from June to August the previous year (mm)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot1 <- plot1 + geom_hline(yintercept = 200, color = "black", linetype=4)
	plot1 <- plot1 + xlim(-18, 39) + ylim(0,3597)
	d=data.frame(x1=c(16,16), y1=c(10,500), lab=c(paste(toString(n1), "%"), paste(toString(n2), "%")))
	plot1 <- plot1 + geom_text(data=d, aes(x=x1, y=y1,label=lab), color="black")
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mta, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Mean August T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_pja, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="P from June to August the previous year (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_mta_pja_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_mta_pja_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_mta_pja.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])

	plot1 <- qplot(df_nto,df_cpos, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum October T (°C)", ylab = "Two-year cumulative P from October to September (mm)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_nto, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum October T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_cpos, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Two-year cumulative P from October to September (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_nto_cpos_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_nto_cpos_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_nto_cpos.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])

	plot1 <- qplot(df_ntm,df_pos, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum March T (°C)", ylab = "P from October to September the previous year (mm)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntm, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Minimum March T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_pos, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="P from October to September the previous year (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_ntm_pos_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_ntm_pos_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_ntm_pos.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])

	plot1 <- qplot(df_gsp,df_vgp, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Growing season precipitation (mm)", ylab = "Variability of growing season precipitation", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+ labs(color="df_prs")
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_gsp, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Growing season precipitation (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_vgp, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = c("dark gray","green","red"))+theme(axis.text.x=element_blank(),axis.ticks.x=element_blank())+labs(x="Presencce", y="Variability of growing season precipitation")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")
	
	png(paste0(out,"cs_beetles_chosts_gsp_vgp_",toString(years[yr]),".png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_gsp_vgp_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_gsp_vgp.gif", sep=""))