library(ggplot2)
library(grid)
library(MASS)
library(parallel)
library(doParallel)
library(foreach)
registerDoParallel(cores=28)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired/ts"
setwd(out)

vargrp1 <- c("OctTmin", "JanTmin", "MarTmin", "Tmin", "OctMin", "JanMin", "MarMin",  
							"winterMin", "AugMaxT", "maxT", "TMarAug", "summerTmean", 
							"AugTmean", "fallTmean", "TOctSep", "Tmean", "ddAugJul", "ddAugJun")

vargrp2 <- c("summerP0", "summerP1", "summerP2", "AugTmax", "PcumOctSep", "max.drop", "PPT",
						  "cv.gsp", "Tvar", "minT", "PMarAug", "mi",  "cwd", "pt.coef",
							 "POctSep", "Pmean", "wd", "OptTsum")

bioClim <- get_data()
bioClim <- bioClim[complete.cases(bioClim),]
bioClim$hosts <- ifelse(bioClim$beetles==1 & bioClim$hosts==0, 1, bioClim$hosts)
bioClim.ss <- subset(bioClim, hosts==1)

n1 <- rep(c(1,2,3,4, 5),4); n2 <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
years <- 1996:2015
ts_cs_plot <- function(var1, var2){
	png(paste0("cs_",var1,"_",var2,"_ts.png"), width=15, height=12, units="in", res=300)
	grid.newpage()
	par(mar=c(2,2,4,2))
	pushViewport(viewport(layout = grid.layout(4, 5)))
	for(yr in years){
		i <- which(years==yr)
		df <- subset(bioClim, year==yr & hosts==1)
		df <- df[,c(var1, var2, "beetles")]
		names(df)[1:2] <- c("var1", "var2")
		p <- ggplot(df, aes(x=var1, y=var2, z = beetles))
		p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
										panel.background = element_blank(), axis.line = element_line(colour = "black"))
		p <- p + theme(legend.position="none")
		#p <- p + geom_point(color = rgb(0.75,0.75,0.75,0.4), size=0.8)
		p <- p + stat_density_2d(geom = "point", aes(alpha=..density..,size = ..density..), n=10, contour = FALSE)
		#p <- p + geom_density_2d(colour=rgb(0.3,0.3,0.3))
		p <- p + stat_summary_hex(bins=30,colour=rgb(1,1,1,0),fun=function(x) sum(x)/length(x))
		#p <- p + scale_fill_gradient(low="lightgray", high="darkred", limits = c(0, 1))
		p <- p + scale_fill_gradient(low=rgb(0.8,0.8,0.8,0.8), high=rgb(0.5,0,0,0.8), limits = c(0, 1))
		p <- p + xlim(min(bioClim.ss[,var1]), max(bioClim.ss[,var1])) + ylim(min(bioClim.ss[,var2]), max(bioClim.ss[,var2]))
# 		if(i == 1){
# 			p <- p + labs(x=var1, y=var2, title =yr)
# 		}else{
# 			p <- p + labs(x="", y="", title =yr)
# 		}		
		p <- p + labs(x=var1, y=var2, title =yr)
		print(p, vp = vplayout(n2[i], n1[i]))
	}
	dev.off()
}

foreach(j=1:length(vargrp1))%dopar%{
	ts_cs_plot(vargrp1[j], vargrp2[j])
  print(paste("The time-series climate space with variables", vargrp1[j], "and", vargrp2[j], "is done!"))  
}

ts_cs_plot("Tvar", "summerP2")

save.image(file="/gpfs/projects/gavingrp/dongmeic/beetle/output/RData/climate_space.RData")
print("all done")
