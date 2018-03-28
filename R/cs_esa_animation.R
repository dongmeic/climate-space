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

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
out <- "/home2/dongmeic/beetle/output/climate_space/paired/btl/"
df.t <- read.csv(paste0(path,"climatic_changes_data_tmp_2.csv"))
df.t <- df.t[,-1]
df.p <- read.csv(paste0(path,"climatic_changes_data_pre_2.csv"))
df.p <- df.p[,-1]
df <- cbind(df.t[,1:8],df.p[,1:7],df.t[,9:10])

years = 2000:2014; nyr <- length(years)
legendlabs <- c("Land","Trees","Beetles")
cols <- c("grey70", "#1b9e77", "#d95f02")

ptm <- proc.time()
foreach(yr=1:nyr) %dopar%{
	df.s <- subset(df, df_yr == years[yr])

	plot1 <- qplot(df_ntw,df_pja, data=df.s, color=factor(df_prs), alpha=I(0.7), xlab = "Minimum winter T (°C)", ylab = "Summer P the previous year (mm)", main = paste("MPB climate space in", toString(years[yr])))
	plot1 <- plot1 + scale_colour_manual(name="Presence", labels=legendlabs, values = cols)
	plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"),plot.title = element_text(hjust = 0.5))
	plot1 <- plot1 + xlim(-55, 28) + ylim(0,3000)
	
	png(paste0(out,"cs_beetles_chosts_ntw_pja_p1_",toString(years[yr]),".png"), width=8, height=6, units="in", res=300)
	print(plot1)
	dev.off()
}
proc.time() - ptm
im.convert(paste(out,"cs_beetles_chosts_ntw_pja_p1_*.png", sep = ""), output = paste(out,"cs_beetles_chosts_ntw_mta_p1.gif", sep=""), clean = TRUE)
