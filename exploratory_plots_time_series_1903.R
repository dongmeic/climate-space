# Created by Dongmei Chen
# Exploratory data analysis: longt-term time-series plots (based on "time-series")

library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=12)
library(ggplot2)
library(grid)

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/1901-2015/"
df.clm.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_clm.csv"))
df.clm.t <- df.clm.t[,-1]
df.clm.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_clm.csv"))
df.clm.p <- df.clm.p[,-1]
df.clm <- cbind(df.clm.t[,1:8],df.clm.p[,1:7],df.clm.t[,9:10])

df.vgt.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_vgt.csv"))
df.vgt.t <- df.vgt.t[,-1]
df.vgt.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_vgt.csv"))
df.vgt.p <- df.vgt.p[,-1]
df.vgt <- cbind(df.vgt.t[,1:8],df.vgt.p[,1:7],df.vgt.t[,9:10])

df.btl.t <- read.csv(paste0(path,"climatic_changes_data_tmp_1903_btl.csv"))
df.btl.t <- df.btl.t[,-1]
df.btl.p <- read.csv(paste0(path,"climatic_changes_data_pre_1903_btl.csv"))
df.btl.p <- df.btl.p[,-1]
df.btl <- cbind(df.btl.t[,1:8],df.btl.p[,1:7],df.btl.t[,9:10])

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
	p1 <- ggplot(df.clm, aes(x = as.character(clm_yr), y = df.clm[,n])) +geom_boxplot(fill = "grey", colour = "black")+labs(x="Time", y=ylabs[n])+theme(axis.text.x=element_blank())+
        ggtitle(paste0(titles[n]," in North America ", units[n]))
        
	p2 <- ggplot(df.vgt, aes(x = as.character(vgt_yr), y = df.vgt[,n])) +geom_boxplot(fill = "green", colour = "black")+labs(x="Time", y=ylabs[n])+theme(axis.text.x=element_blank())+
        ggtitle(paste0(titles[n]," in areas where core hosts exist ", units[n]))
        
	p3 <- ggplot(df.btl, aes(x = as.character(btl_yr), y = df.btl[,n])) +geom_boxplot(fill = "red", colour = "black")+labs(x="Time", y=ylabs[n])+theme(axis.text.x=element_blank())+
        ggtitle(paste0(titles[n]," in areas where mountain pine beetle exists ", units[n]))
        
	png(paste0(out,"temporal_plots_", names[n], "_1901.png"), width=12, height=10, units="in", res=300)
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