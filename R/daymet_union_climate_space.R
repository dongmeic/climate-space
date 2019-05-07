library(ggplot2)
library(grid)
library(gridExtra)
#install.packages("ggExtra", repos='http://cran.us.r-project.org')
#library(ggExtra)
library(MASS)

source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/paired"
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

# test: var1 <- "ddAugJul"; var2 <- "summerP2"
u_cs_plot <- function(var1, var2, legend=F){
	df <- bioClim[,c(var1, var2, "beetles", "hosts")]
	names(df)[1:2] <- c("var1", "var2")
	df$prs <- df$beetles + df$hosts
	df <- df[order(df$prs),]
	#df2 <- df[sample(nrow(df), 500),]
	p <- ggplot(df, aes(x=var1, y=var2, z = beetles))
	p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
									panel.background = element_blank(), axis.line = element_line(colour = "black"))
	p <- p + geom_point(aes(color=factor(prs, labels=c("Continent", "Host", "MPB"))), size=0.5)+
					 scale_colour_manual(name="",values = c("#d3d3d380", "#1b9e7780", "#d95f0280"))
	if(legend){
		p <- p + theme(legend.position=c(0.3,0.8),legend.text=element_text(size=12)) +
				 	guides(colour = guide_legend(override.aes = list(size=2)))
	}else{
		p <- p + theme(legend.position="none")
	}
	p <- p + geom_density_2d(colour=rgb(0.3,0.3,0.3,0.7))
	#p <- p + stat_density_2d(geom = "point", aes(alpha=0.2,size = ..density..), n=10, contour = FALSE)
	p <- p + labs(x=var1, y=var2)
	return(p)
}

ucs_plot <- function(var1, var2){
	df <- bioClim[,c(var1, var2, "hosts", "beetles")]
	df <- df[df$hosts == 1,]
	names(df)[1:2] <- c("var1", "var2")
	#df2 <- df[sample(nrow(df), 500),]
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
		p <- p + labs(x=var1, y=var2)
	return(p)
}

n1 <- rep(c(1,2,3,4,5,6),3); n2 <- c(rep(1,6),rep(2,6),rep(3,6))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
png("cs_union_hexbin.png", width=18, height=9, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(3, 6)))
for(i in 1:length(vargrp1)){
	p <- ucs_plot(vargrp1[i], vargrp2[i])
	print(p, vp = vplayout(n2[i], n1[i]))
}
dev.off()
print("all done")
