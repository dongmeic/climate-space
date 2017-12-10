# Created by Dongmei Chen
# Exploratory data analysis: temporal plots

# 1. tmp --- annual mean monthly temperature (mean)
# 2. tmx --- maximum August temperature (max)
# 3. tmn --- minimum winter temperature (min)
# 4. map --- annual mean monthly precipitation (mean)
# 5. tpp --- total monthly precipitation in previous year (mean)
# 6. gsp --- total growing season precipitation in current year (mean)
# 7. cum --- accumulative monthly precipitation in current and previpous years (mean)
# 8. cvp --- mean variability of growing season precipitation (mean)

library(ggplot2)
library(grid)

path <- "/home2/dongmeic/beetle/output/climate_space/times_series/"
df <- read.csv(paste0(path,"climatic_changes_temporal_plots.csv"))
str(df)
df <- df[,-1]
years = 2001:2015; nyr <- length(years)
df.n <- data.frame(prs=rep(c("climate", "hosts", "mpb"),length=3*nyr))
names <- c("tmp","tmx","tmn","map","tpp","gsp","cum","cvp")
for (v in 1:8){
	df.v <- data.frame()
	if (v==1){
		time <- vector()
	}
	for (y in 1:nyr){
		df.ss <- subset(df, df_yr == years[y])
		if (v == 2) {
			clm <- max(subset(df.ss, df_prs == "climate")[,v], na.rm = TRUE)
			vgt <- max(subset(df.ss, df_prs == "hosts")[,v], na.rm = TRUE)
			btl <- max(subset(df.ss, df_prs == "mpb")[,v], na.rm = TRUE)
			var <- c(clm, vgt, btl)
			df.v <- rbind(df.v, data.frame(var)) 	
		} else if (v == 3){
			clm <- min(subset(df.ss, df_prs == "climate")[,v], na.rm = TRUE)
			vgt <- min(subset(df.ss, df_prs == "hosts")[,v], na.rm = TRUE)
			btl <- min(subset(df.ss, df_prs == "mpb")[,v], na.rm = TRUE)
			var <- c(clm, vgt, btl)
			df.v <- rbind(df.v, data.frame(var))		
		} else{
			clm <- mean(subset(df.ss, df_prs == "climate")[,v], na.rm = TRUE)
			vgt <- mean(subset(df.ss, df_prs == "hosts")[,v], na.rm = TRUE)
			btl <- mean(subset(df.ss, df_prs == "mpb")[,v], na.rm = TRUE)					
			var <- c(clm, vgt, btl)
			df.v <- rbind(df.v, data.frame(var))
			if (v==1){
				yr <- rep(years[y],3)
				time <- c(time, yr)
			}
		}
		print(years[y])
	}
	df.n <- cbind(df.n, df.v)
	print(paste(names[v],"done!"))
}
colnames(df.n)[2:9] <- names
df.n <- cbind(df.n, yr=time)
write.csv(df.n, paste0(path,"climatic_changes_temporal_stat.csv"))
ylabs <- c("Water year mean monthly T (°C)","Maximum August T (°C)","Minimum winter T (°C)",
"Mean monthly P (mm)","Total P from previous year (mm)","Total growing season P (mm)",
"Cumulative P in two years (mm)","Variability of growing season P")
p1 <- ggplot(data=df.n, aes(x=yr, y=tmp, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Annual mean monthly temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p2 <- ggplot(data=df.n, aes(x=yr, y=tmx, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Maximum August temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p3 <- ggplot(data=df.n, aes(x=yr, y=tmn, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Minimum winter temperature (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p4 <- ggplot(data=df.n, aes(x=yr, y=map, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Annual mean monthly precipitation (°C)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p5 <- ggplot(data=df.n, aes(x=yr, y=tpp, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Total monthly precipitation in previous year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p6 <- ggplot(data=df.n, aes(x=yr, y=gsp, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Total growing season precipitation in current year (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p7 <- ggplot(data=df.n, aes(x=yr, y=cum, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Accumulative monthly precipitation in two years (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
p8 <- ggplot(data=df.n, aes(x=yr, y=cvp, colour=factor(prs)))+geom_line(size=1.2)+geom_point(size=2)+
scale_colour_manual(name="Presencce", labels=c("Land","Tree","Beetle"), values = c("dark gray","green","red"))+
ggtitle("Mean variability of growing season precipitation (mm)")+
theme(axis.title.x=element_blank(),axis.title.y=element_blank(),plot.background=element_blank())
# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# print plots
png(paste0(path,"temporal_plots_stat.png"), width=20, height=20, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(8, 1))) # 8 rows, 1 column
print(p1, vp = vplayout(1, 1))
print(p2, vp = vplayout(2, 1))
print(p3, vp = vplayout(3, 1))
print(p4, vp = vplayout(4, 1))
print(p5, vp = vplayout(5, 1))
print(p6, vp = vplayout(6, 1))
print(p7, vp = vplayout(7, 1))
print(p8, vp = vplayout(8, 1))
dev.off()

# print two plots in one figure
png(paste0(path,"temporal_plots_tmp_map.png"), width=15, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p1, vp = vplayout(1, 1))
print(p4, vp = vplayout(2, 1))
dev.off()
png(paste0(path,"temporal_plots_tmx_tmn.png"), width=15, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p2, vp = vplayout(1, 1))
print(p3, vp = vplayout(2, 1))
dev.off()
png(paste0(path,"temporal_plots_gsp_cvp.png"), width=15, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p6, vp = vplayout(1, 1))
print(p8, vp = vplayout(2, 1))
dev.off()
png(paste0(path,"temporal_plots_tpp_cum.png"), width=15, height=8, units="in", res=300)
grid.newpage()
par(mar=c(2,2,4,2))
pushViewport(viewport(layout = grid.layout(2, 1))) # 2 rows, 1 column
print(p5, vp = vplayout(1, 1))
print(p7, vp = vplayout(2, 1))
dev.off()