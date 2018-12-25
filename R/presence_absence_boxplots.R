# boxplot for host and beetle presence and absence
library(ggplot2)
library(grid)
source("/gpfs/projects/gavingrp/dongmeic/climate-space/R/combine_CRU_Daymet.R")
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(out)
indata <- get_data()
indata$hosts <- ifelse(indata$beetles==1 & indata$hosts==0, 1, indata$hosts)

vargrp <- c("OctTmin", "JanTmin", "MarTmin", "Tmin", "OctMin", "JanMin", "MarMin",  
							"winterMin", "minT", "Acs", "drop5", "max.drop", "maxAugT", "AugMaxT", "AugTmax", "maxT", 
							"TMarAug", "OptTsum", "summerTmean", "AugTmean", "fallTmean", "TOctSep", "Tmean", "ddAugJul", 
							"ddAugJun", "Tvar", "PMarAug", "summerP0", "summerP1", "summerP2", "POctSep",
						  "PcumOctSep", "Pmean", "PPT", "cv.gsp", "mi", "pt.coef", "vpd", "cwd", "wd")

presence.boxplot <- function(i, legend=F){
 df <- indata[,c(vargrp[i],"prs")]
 colnames(df)[1] <- "var"
 #df2 <- df[sample(nrow(df), 500),]
 p <- ggplot(df, aes(as.character(prs), var))
 p <- p + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
									panel.background = element_blank(), axis.line = element_line(colour = "black"))
 p <- p + geom_boxplot(outlier.size=0.1, aes(color=factor(prs, labels=c("Host-abs", "MPB-abs", "MPB")))) + coord_flip()
 p <- p + scale_colour_manual(name="",values = c("darkgrey", "#1b9e77", "#d95f02"))
 if(legend){
 	p <- p + theme(legend.position=c(0.8,0.8),legend.text=element_text(size=11)) +
				 	guides(colour = guide_legend(override.aes = list(size=0.8)))
 }else{
 	p <- p + theme(legend.position="none")
 }
 p <- p + labs(x="", y=vargrp[i])
 return(p)
}

n1 <- rep(c(1:5),8); n2 <- c(rep(1,5),rep(2,5),rep(3,5),rep(4,5),rep(5,5),rep(6,5),rep(7,5),rep(8,5))
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
png("cs_boxplot.png", width=15, height=24, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(8, 5)))
for(i in 1:length(vargrp)){
  if(i==40){
		p <- presence.boxplot(i,legend=T)
	}else{
		p <- presence.boxplot(i)
	}
	print(p, vp = vplayout(n2[i], n1[i]))
}
dev.off()

print("all done!")