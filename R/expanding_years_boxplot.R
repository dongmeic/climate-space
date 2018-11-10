# run in an interactive mode

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
setwd(inpath)

dt <- read.csv(paste0(inpath, "bioclim_vars_presence_both.csv")) # t_tests_quantiles.R

peakyears <- 2006:2008
nonpeakyears <- 1996:1998
expanding1 <- 2000:2002
expanding2 <- 2003:2005
dt$peak <- ifelse(dt$year %in% peakyears, 4, 
					 ifelse(dt$year %in% nonpeakyears, 1, 
					 ifelse(dt$year %in% expanding1,2,
					 ifelse(dt$year %in% expanding2,3,0))))

vars <- c("ddAugJul", "maxAugT", "OptTsum", "winterTmin", "summerP2", "MarTmin",
						 "drop5", "GSP", "PPT", "AugMax", "OctMin", "Tvar")

peak.boxplot <- function(var){
  df <- dt[dt$peak!=0,][,c(var,"peak")]
  colnames(df)[1] <- "bioclm"
  boxplot(bioclm~peak,data=df, main=var, cex=1.5, horizontal = TRUE,col=cols, outcol=rgb(0,0,0,0.4),outcex=0.5,
   xlab="", ylab="")
}

#cols <- rev(c('#e41a1c','#377eb8','#4daf4a','#984ea3'))
cols <- c('#f7f7f7','#cccccc','#969696','#525252')
out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
png(paste0(out,"expanding_years_boxplot_both.png"), width=12, height=9, units="in", res=300)
par(mfrow=c(3,4),mar=c(3.5,3.5,3,1))
for (var in vars){
  peak.boxplot(var)
  if(var=="PPT"){
    legend('right', fill=cols, ncol=2, legend=c("1:1996-1998","2:2000-2002","3:2003-2005","4:2006-2008"), cex = 0.8, bty='n')
  }
}
dev.off()