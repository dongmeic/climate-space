# Created by Dongmei Chen
# run in an interactive mode

library(ggplot2)
library(grid)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(outpath)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vars <- c("JanTmin", "AugTmax", "Tmean", "Tvar", "summerP2", "PPT")
varnms <- c("Monthly average of daily minimum temperature in Jan",
						"Monthly average of daily maximum temperature in Aug",
						"Annual mean temperature from Aug to Jul",
					  "Seasonal temperature variation from Aug to Jul",
					  "Two-year cumulative summer precipitation",
					  "Six-year cumulative Oct-Aug monthly precipitation")

startyrs <- c(1901, 1901, 1902, 1902, 1902, 1907)
units <- c("(°C)", "(°C)", "(°C)", "", "(mm)", "(mm)")

cols <- c("#A9A9A9", "#1b9e77", "#d95f02")
rect <- data.frame(xmin=1996, xmax=2015, ymin=-Inf, ymax=Inf)
override.linetype <- c("dashed", "longdash", "solid")

get.df <- function(vars){
	df.v <- data.frame()
	for (i in 1:length(vars)){
		indata <- read.csv(paste0(inpath,vars[i],"_",startyrs[i],"_1.csv"))
		# from time_series_boxplot.R; 1- the whole beetle affected area in all years
		df <- aggregate(indata$var, by=list(prs=indata$prs, yrs=indata$yrs), FUN=mean)
		df.s <- subset(df, prs == "mpb"); df.s$var <- rep(vars[i], dim(df.s)[1])
		df.v <- rbind(df.v, df.s)
		print(i)
	}
	df.v[,-1]	
}
df <- get.df(vars)
#write.csv(df, "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/longterm_bioclim_mean.csv", row.names=FALSE)
#df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/longterm_bioclim_mean.csv")

g <- function(i){
	indata <- read.csv(paste0(inpath,vars[i],"_",startyrs[i],"_1.csv"))
	df <- aggregate(indata$var, by=list(prs=indata$prs, yrs=indata$yrs), FUN=mean)
	df.s <- subset(df, prs == "mpb")
	test <- cor.test(df.s$yrs, df.s$x)
	g <- ggplot(data=df, aes(x=yrs, y=x,linetype = prs, color= prs)) + 
    geom_line(alpha=0.3) + 
    geom_point(alpha=0.3) + 
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)
  g <- g + scale_linetype_manual(values=override.linetype, guide = FALSE)
  g <- g + guides(colour = guide_legend(override.aes = list(linetype = override.linetype)))
  g <- g + scale_colour_manual(name="", labels=c("Continent","Host","MPB"), values = cols)
  g <- g + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position="none",
								 panel.background = element_blank(), axis.line = element_line(colour = "black"))
  #g <- g + labs(title=varnms[i], subtitle=paste0("Correlation with time in the beetle range: r = ", format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)), 
  #							x=paste("Years since", startyrs[i]), y = paste(vars[i], units[i]))
  g <- g + labs(title=paste(varnms[i],units[i]), x="", y ="")
  g <- g + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="grey",alpha=0.2,inherit.aes = FALSE)
  g <- g + geom_vline(xintercept = 2008, color = "black", linetype=4)
  return(g)
}

plot1 <- g(1)
plot2 <- g(2)
plot3 <- g(3)
plot4 <- g(4)
plot5 <- g(5)
plot6 <- g(6)
png(paste0(outpath,"longterm_climatic_changes.png"), width=12, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,1,3,2))
pushViewport(viewport(layout = grid.layout(3, 2)))
print(plot1, vp = vplayout(1, 1))
print(plot2, vp = vplayout(1, 2))
print(plot3, vp = vplayout(2, 1))
print(plot4, vp = vplayout(2, 2))
print(plot5, vp = vplayout(3, 1))
print(plot6, vp = vplayout(3, 2))
dev.off()

print("all done")
