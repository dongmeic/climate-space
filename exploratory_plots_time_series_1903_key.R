# Created by Dongmei Chen
# Exploratory data analysis: long-term time-series plots

library(ggplot2)
library(grid)

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
out <- "/home2/dongmeic/beetle/output/climate_space/times_series/1901-2015/"
climate <- read.csv(paste0(path,"climatic_changes_temporal_stat_1903_clm.csv"))
hosts <- read.csv(paste0(path,"climatic_changes_temporal_stat_1903_vgt.csv"))
mpb <- read.csv(paste0(path,"climatic_changes_temporal_stat_1903_btl.csv"))

df <- rbind(climate, hosts, mpb)
df <- df[,-1]

rect <- data.frame(xmin=2000, xmax=2014, ymin=-Inf, ymax=Inf)

p1 <- ggplot(data=df, aes(x=yr, y=mat, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Previous water year mean monthly temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p2 <- ggplot(data=df, aes(x=yr, y=mtaa, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Mean temperature from April to August (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p3 <- ggplot(data=df, aes(x=yr, y=mta, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Mean August temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p4 <- ggplot(data=df, aes(x=yr, y=ntw, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum winter temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p5 <- ggplot(data=df, aes(x=yr, y=nto, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum October temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p6 <- ggplot(data=df, aes(x=yr, y=ntj, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum January temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p7 <- ggplot(data=df, aes(x=yr, y=ntm, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum March temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p8 <- ggplot(data=df, aes(x=yr, y=xta, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Maximum August temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p9 <- ggplot(data=df, aes(x=yr, y=map, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Previous water year mean monthly precipitation (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p10 <- ggplot(data=df, aes(x=yr, y=cpja, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Cumulative precipitation from June to August the current and previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p11 <- ggplot(data=df, aes(x=yr, y=pja, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Precipitation from June to August the previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p12 <- ggplot(data=df, aes(x=yr, y=cpos, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Cumulative precipitation from October to September the current and previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p13 <- ggplot(data=df, aes(x=yr, y=pos, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Precipitation from October to September the previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p14 <- ggplot(data=df, aes(x=yr, y=gsp, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Growing season precipitation in current year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

p15 <- ggplot(data=df, aes(x=yr, y=vgp, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Variability of growing season precipitation (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())+
geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax),color="pink",alpha=0.2,inherit.aes = FALSE)+
geom_vline(xintercept = 2004, color = "black", linetype=4)+
geom_vline(xintercept = 2010, color = "black", linetype=4)

# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

# print two or three plots in one figure
png(paste0(out,"temporal_plots_mat_mtaa_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_mta_xta_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p3, vp = vplayout(1, 1))
print(p8, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_ntw_ntj_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p4, vp = vplayout(1, 1))
print(p6, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_nto_ntm_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p5, vp = vplayout(1, 1))
print(p7, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_map_cpos_pos_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(3, 1))) # 3 rows, 1 column
print(p9, vp = vplayout(1, 1))
print(p12, vp = vplayout(2, 1))
print(p13, vp = vplayout(3, 1))
dev.off()

png(paste0(out,"temporal_plots_cpja_pja_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p10, vp = vplayout(1, 1))
print(p11, vp = vplayout(2, 1))
dev.off()

png(paste0(out,"temporal_plots_gsp_vgp_1901.png"), width=12, height=10, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p14, vp = vplayout(1, 1))
print(p15, vp = vplayout(2, 1))
dev.off()
