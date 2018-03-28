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

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
out <- "/home2/dongmeic/beetle/output/climate_space/paired/"
df.btl.t <- read.csv(paste0(path,"climatic_changes_data_tmp_2.csv"))
df.btl.t <- df.btl.t[,-1]
df.btl.p <- read.csv(paste0(path,"climatic_changes_data_pre_2.csv"))
df.btl.p <- df.btl.p[,-1]
df.btl <- cbind(df.btl.t[,1:8],df.btl.p[,1:7],df.btl.t[,9:10])

df.ldp.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_ldp.csv"))
df.ldp.t <- df.ldp.t[,-1]
df.ldp.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_ldp.csv"))
df.ldp.p <- df.ldp.p[,-1]
df.ldp <- cbind(df.ldp.t[,1:8],df.ldp.p[,1:7],df.ldp.t[,9:10])
df.ldp <- df.ldp[df.ldp$ldp_yr >= 2000 & df.ldp$ldp_yr <= 2014,]
colnames(df.ldp) <- colnames(df.btl)

df.jck.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_jck.csv"))
df.jck.t <- df.jck.t[,-1]
df.jck.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_jck.csv"))
df.jck.p <- df.jck.p[,-1]
df.jck <- cbind(df.jck.t[,1:8],df.jck.p[,1:7],df.jck.t[,9:10])
df.jck <- df.jck[df.jck$jck_yr >= 2000 & df.jck$jck_yr <= 2014,]
colnames(df.jck) <- colnames(df.btl)

df <- rbind(df.btl,df.ldp,df.jck)
years = 2000:2014; nyr <- length(years)
legendlabs <- c("Core hosts","Jack","Lodgepole","Beetles")
levellabs <- c("Core hosts", "Jack pine", "Lodgepole pine", "Mountain pine beetle")
cols <- c("#a6d854","#66c2a5","#fc8d62","#e78ac3")
df$df_prs <- as.character(df$df_prs)

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	
	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs(x="Minimum winter T (°C)", y= "Maximum August T (°C)",title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols) + facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntw, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Minimum winter T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+ 
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_mta, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Maximum August T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_ntw_mta_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_ntw_mta_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_ntw_mta_vgt.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{

	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs(x= "Previous water year mean T (°C)", y= "Previous water year mean P (mm)", title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols) + facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mat, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Previous water year mean T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_map, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Previous water year mean P (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_mat_map_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_mat_map_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_mat_map_vgt.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{

	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs( x= "Mean T from April to August (°C)", y = "Two-year cumulative P from June to August (mm)", title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)+ facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mtaa, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Mean T from April to August (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_cpja, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Two-year cumulative P from June to August (mm)")+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_mtaa_cpja_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_mtaa_cpja_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_mtaa_cpja_vgt.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs( x= "Mean August T (°C)", y= "P from June to August the previous year (mm)", title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)+ facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_mta, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Mean August T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_pja, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="P from June to August the previous year (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_mta_pja_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_mta_pja_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_mta_pja_vgt.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	
	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs(x = "Minimum October T (°C)", y = "Two-year cumulative P from October to September (mm)", title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)+ facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_nto, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Minimum October T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_cpos, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Two-year cumulative P from October to September (mm)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_nto_cpos_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_nto_cpos_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_nto_cpos_vgt.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs(x = "Minimum March T (°C)", y = "P from October to September the previous year (mm)", title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)+ facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_ntm, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Minimum March T (°C)")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	 scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles")) 
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_pos, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="P from October to September the previous year (mm)")+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_ntm_pos_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_ntm_pos_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_ntm_pos_vgt.gif", sep=""))

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr] & df_prs != "climate")
	df.s$df_prs <- factor(df.s$df_prs, labels = levellabs)
	plot1 <- ggplot(data=df.s, aes(df_ntw,df_mta, color=df_prs))+ geom_point(data = transform(df.s, df_prs = NULL), colour = "grey85")+ geom_point()+ labs( x= "Growing season precipitation (mm)", y = "Variability of growing season precipitation", title=paste0("MPB climate space in ", years[yr]))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)+ facet_wrap(~df_prs)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot2 <- ggplot(df.s, aes(x=df_prs, y=df_gsp, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Growing season precipitation (mm)")+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	plot3 <- ggplot(df.s, aes(x=df_prs, y=df_vgp, fill=factor(df_prs)))+geom_boxplot()+scale_fill_manual(values = cols)+
	labs(x="Presence", y="Variability of growing season precipitation")+
	geom_point(size=1.5, stat = "summary", fun.y = "mean", color="red")+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
	  stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+
	  scale_x_discrete(labels=c("Core hosts" = "Core hosts", "Jack pine" = "Jack", "Lodgepole pine" = "Lodgepole", "Mountain pine beetle" = "Beetles"))
	
	png(paste0(out,"cs_beetles_chosts_gsp_vgp_",toString(years[yr]),"_vgt.png"), width=12, height=6, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
	print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
	print(plot2, vp = vplayout(1, 3))
	print(plot3, vp = vplayout(1, 4))
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_gsp_vgp_*_vgt.png", sep = ""), output = paste(out,"cs_beetles_chosts_gsp_vgp_vgt.gif", sep=""))
