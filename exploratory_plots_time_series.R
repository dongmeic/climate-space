# Created by Dongmei Chen
# Exploratory data analysis: time-series plots (based on "temporal" and "key")

library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=12)
library(ggplot2)
library(grid)

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
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/2000-2014/"
df.t <- read.csv(paste0(path,"climatic_changes_data_tmp_2.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(path,"climatic_changes_data_pre_2.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])

ylabs <- c("Previous WY mean T (°C)","Apr to Aug mean T (°C)","Mean Aug T (°C)","Min winter T (°C)","Min Oct T (°C)", "Min Jan T (°C)","Min Mar T (°C)","Max Aug T (°C)",
"Previous WY mean P (mm)","Two-year cumulative P (Jun - Aug)(mm)","Jun to Aug P the previous year (mm)","Two-year cumulative P (Oct - Sep)(mm)", "Oct to Sep P the previous year (mm)",
"Total GS P (mm)","Variability of GS P")

titles <- c("Previous water year mean temperature","April to August mean temperature","Mean August temperature","Minimum winter temperature","Minimum October temperature", "Minimum January temperature","Minimum March temperature","Maximum August temperature",
"Previous water year mean precipitation","Two-year cumulative summer precipitation","Previous year summer precipitation","Two-year cumulative water year precipitation", "Previous water year total precipitation",
"Total growing season precipitation","Variability of growing season precipitation")

units <- c("(°C)", "(°C)", "(°C)", "(°C)","(°C)", "(°C)", "(°C)", "(°C)", "(mm)", "(mm)", "(mm)", "(mm)", "(mm)", "(mm)", "")

tnames <- c("mat","mtaa","mta","ntw","nto","ntj","ntm","xta")
pnames <- c("map","cpja","pja","cpos","pos","gsp","vgp")
names <-c(tnames,pnames)

ptm <- proc.time()
foreach(n=1:15) %dopar% {
	df.ss.1 <- subset(df, df_prs == "climate")
	p1 <- ggplot(df.ss.1, aes(x = as.character(df_yr), y = df.ss.1[,n])) +geom_boxplot(fill = "grey", colour = "black")+labs(x="Year", y=ylabs[n])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+
        ggtitle(paste0(titles[n]," in North America ", units[n]))
  	df.ss.2 <- subset(df, df_prs == "hosts")
	p2 <- ggplot(df.ss.2, aes(x = as.character(df_yr), y = df.ss.2[,n])) +geom_boxplot(fill = "green", colour = "black")+labs(x="Year", y=ylabs[n])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+
        ggtitle(paste0(titles[n]," in areas where core hosts exist ", units[n]))
	df.ss.3 <- subset(df, df_prs == "mpb")
	p3 <- ggplot(df.ss.3, aes(x = as.character(df_yr), y = df.ss.3[,n])) +geom_boxplot(fill = "red", colour = "black")+labs(x="Year", y=ylabs[n])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+
        ggtitle(paste0(titles[n]," in areas where mountain pine beetle exists ", units[n]))
	png(paste0(out,"temporal_plots_", names[n], ".png"), width=14, height=12, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(3, 1))) # 3 rows, 1 column
	print(p1, vp = vplayout(1, 1))
	print(p2, vp = vplayout(2, 1))
	print(p3, vp = vplayout(3, 1))
	dev.off()
	print(paste(names[n],"done!"))	
}
proc.time() - ptm

years = 2000:2014; nyr <- length(years)
df.n <- data.frame(prs=rep(c("climate", "hosts", "mpb"),length=3*nyr))
for (v in 1:15){
  df.v <- data.frame()
  if (v==1){
    time <- vector()
  }
  for (y in 1:nyr){
    df.ss <- subset(df, df_yr == years[y])
	# if (v == 8) {
	#   clm <- max(subset(df.ss, df_prs == "climate")[,v], na.rm = TRUE)
	#   vgt <- max(subset(df.ss, df_prs == "hosts")[,v], na.rm = TRUE)
	#   btl <- max(subset(df.ss, df_prs == "mpb")[,v], na.rm = TRUE)
	#   var <- c(clm, vgt, btl)
	#   df.v <- rbind(df.v, data.frame(var))
	# } else if (v >= 4 && v <=7){
	#   clm <- min(subset(df.ss, df_prs == "climate")[,v], na.rm = TRUE)
	#   vgt <- min(subset(df.ss, df_prs == "hosts")[,v], na.rm = TRUE)
	#   btl <- min(subset(df.ss, df_prs == "mpb")[,v], na.rm = TRUE)
	#   var <- c(clm, vgt, btl)
	#   df.v <- rbind(df.v, data.frame(var))
	# } else{
	  clm <- mean(subset(df.ss, df_prs == "climate")[,v], na.rm = TRUE)
	  vgt <- mean(subset(df.ss, df_prs == "hosts")[,v], na.rm = TRUE)
	  btl <- mean(subset(df.ss, df_prs == "mpb")[,v], na.rm = TRUE)
	  var <- c(clm, vgt, btl)
	  df.v <- rbind(df.v, data.frame(var))
	  if (v==1){
		yr <- rep(years[y],3)
		time <- c(time, yr)
  }
	#    }
    print(years[y])
 }
  df.n <- cbind(df.n, df.v)
  print(paste(names[v],"done!"))
}
colnames(df.n)[2:16] <- names
df.n <- cbind(df.n, yr=time)
write.csv(df.n, paste0(path,"climatic_changes_temporal_stat.csv"))

df.n <- read.csv(paste0(path,"climatic_changes_temporal_stat.csv"))
df.n <- df.n[,-1]

rect <- data.frame(xmin=2005, xmax=2010, ymin=-Inf, ymax=Inf)

p1 <- ggplot(data=df.n, aes(x=yr, y=mat, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Previous water year mean monthly temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p2 <- ggplot(data=df.n, aes(x=yr, y=mtaa, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Mean temperature from April to August (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p3 <- ggplot(data=df.n, aes(x=yr, y=mta, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Mean August temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p4 <- ggplot(data=df.n, aes(x=yr, y=ntw, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum winter temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p5 <- ggplot(data=df.n, aes(x=yr, y=nto, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum October temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p6 <- ggplot(data=df.n, aes(x=yr, y=ntj, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum January temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p7 <- ggplot(data=df.n, aes(x=yr, y=ntm, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum March temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p8 <- ggplot(data=df.n, aes(x=yr, y=xta, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Maximum August temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p9 <- ggplot(data=df.n, aes(x=yr, y=map, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Previous water year mean monthly precipitation (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p10 <- ggplot(data=df.n, aes(x=yr, y=cpja, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Cumulative precipitation from June to August the current and previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p11 <- ggplot(data=df.n, aes(x=yr, y=pja, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Precipitation from June to August the previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p12 <- ggplot(data=df.n, aes(x=yr, y=cpos, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Cumulative precipitation from October to September the current and previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p13 <- ggplot(data=df.n, aes(x=yr, y=pos, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Precipitation from October to September the previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p14 <- ggplot(data=df.n, aes(x=yr, y=gsp, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Growing season precipitation in current year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

p15 <- ggplot(data=df.n, aes(x=yr, y=vgp, colour=interaction(prs),linetype=prs,shape=prs))+geom_line(size=1.2)+geom_point(size=2.5)+
guides(colour = guide_legend(override.aes = list(shape = c(19,17,15),linetype=c(1,2,5))))+ 
scale_shape(guide = FALSE)+ scale_linetype(guide = FALSE)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Variability of growing season precipitation (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_vline(xintercept = 2010, color = "black", linetype=4)+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)

# print two or three plots in one figure
png(paste0(out,"temporal_plots_mat_mtaa.png"), width=8, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_mta_xta.png"), width=8, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p3, vp = vplayout(1, 1))
print(p8, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_ntw_ntj.png"), width=8, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p4, vp = vplayout(1, 1))
print(p6, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_nto_ntm.png"), width=8, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p5, vp = vplayout(1, 1))
print(p7, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_map_cpos_pos.png"), width=8, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(3, 1))) # 3 rows, 1 column
print(p9, vp = vplayout(1, 1))
print(p12, vp = vplayout(2, 1))
print(p13, vp = vplayout(3, 1))
dev.off()

png(paste0(out,"temporal_plots_cpja_pja.png"), width=8, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p10, vp = vplayout(1, 1))
print(p11, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_gsp_vgp.png"), width=8, height=6, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p14, vp = vplayout(1, 1))
print(p15, vp = vplayout(2, 1))
dev.off()
