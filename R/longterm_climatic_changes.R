library(ggplot2)
library(grid)

inpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/climate_space/times_series/"
outpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
setwd(outpath)

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

vars <- c("winterTmin", "Tvar", "GSP")
varnms <- c("Minimum winter temperature", "Temperature variation from Aug to Jul", "Growing season precipitation")

startyrs <- c(1902, 1902, 1901)

units <- c("(°C)", "", "(mm)")

cols <- c("grey70", "#1b9e77", "#7570b3")
rect <- data.frame(xmin=1996, xmax=2015, ymin=-Inf, ymax=Inf)
override.linetype <- c("dashed", "longdash", "solid")

g <- function(i){
	indata <- read.csv(paste0(inpath,vars[i],"_",startyrs[i],"_1.csv")) # from time_series_boxplot.R; 1- the whole beetle affected area in all years
	df <- aggregate(indata$var, by=list(prs=indata$prs, yrs=indata$yrs), FUN=mean)
	df.s <- subset(df, prs == "mpb")
	test <- cor.test(df.s$yrs, df.s$x)
	g <- ggplot(data=df, aes(x=yrs, y=x,linetype = prs, color= prs)) + 
    geom_line(alpha=0.3) + 
    geom_point(alpha=0.3) + 
    geom_smooth(method = "lm", formula = y ~ splines::bs(x, 3), se = FALSE)
  g <- g + scale_linetype_manual(values=override.linetype, guide = FALSE)
  g <- g + guides(colour = guide_legend(override.aes = list(linetype = override.linetype)))
  g <- g + scale_colour_manual(name="", labels=c("Continent","Hosts","Beetles"), values = cols)
  g <- g + labs(title=varnms[i], subtitle=paste0("Correlation with time in the beetle range: r = ", format(as.numeric(test$estimate), digits = 2),", p-value = ",format(as.numeric(test$p.value), digits = 2)), 
  							x=paste("Years since", startyrs[i]), y = paste(vars[i], units[i]))
  g <- g + geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)
  g <- g + geom_vline(xintercept = 2008, color = "black", linetype=4)
  return(g)
}

png(paste0(outpath,"longterm_climatic_changes.png"), width=8, height=9, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(3, 1)))
plot1 <- g(1)
print(plot1, vp = vplayout(1, 1))
plot2 <- g(2)
print(plot2, vp = vplayout(2, 1))
plot3 <- g(3)
print(plot3, vp = vplayout(3, 1))
dev.off()
