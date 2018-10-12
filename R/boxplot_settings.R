# functions
# functions to return text in a boxplot
max.n <- function(x){
  if (max(x)>0 & max(x)< 10){
    return(c(y = max(x)*1.32, label = round(max(x),1))) 
  } else if (max(x)< 0){
    return(c(y = max(x)*0.8, label = round(max(x),1)))
  } else{
    return(c(y = max(x)*1.12, label = round(max(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
min.n <- function(x){
  if (min(x) < 0) {
    return(c(y = min(x)*1.16, label = round(min(x),1)))
  } else {
    return(c(y = min(x)*0.32, label = round(min(x),1))) 
  }
  # experiment with the multiplier to find the perfect position
}
# function for mean labels
mean.n <- function(x){
  if (mean(x) > -20 & mean(x) < 0) {
    return(c(y = mean(x)*0.88, label = round(mean(x),1)))
  } else if (mean(x) < -20){
    return(c(y = mean(x)*1.08, label = round(mean(x),1)))
  } else{
    return(c(y = mean(x)*1.02, label = round(mean(x),1))) 
  } 
  # experiment with the multiplier to find the perfect position
}
# function for layout
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
# function for absence in boxplot
svars <- c("GSP", "PMarAug", "summerP0","summerP1", "summerP2", 
           "Pmean","POctSep", "PcumOctSep", "PPT", "ddAugJul", "ddAugJun")
         
get.data <- function(var){
	years <- 1996:2015; nyr <- length(years)
	ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/ts/var/"
  ncfile <- paste0("na10km_v2_",var, "_",years[1],".",years[nyr],".4d.nc")
  ncin <- nc_open(paste0(ncpath, ncfile))
  data <- ncvar_get(ncin,var)
  fillvalue <- ncatt_get(ncin,var,"_FillValue")
  data[data==fillvalue$value] <- NA
  return(data)
}
get.abs.data <- function(var, yr){
	data <- get.data(var)
	na <- data[,,1,yr]
	nav <- na[!is.na(na)]
	vgt <- data[,,2,yr]
  vgtv <- vgt[!is.na(vgt)]
  btl <- data[,,3,yr]
  btlv <- btl[!is.na(btl)]
  vgt.abs <- na[is.na(vgt) & !is.na(na)]
  btl.abs <- na[is.na(btl) & !is.na(na)]
  vals <- c(nav, vgtv, vgt.abs, btlv, btl.abs)
  if(var %in% svars){
  	vals <- sqrt(vals)
  }
  prs <- c(rep("continent",length(nav)),rep("hosts",length(vgtv)),rep("hosts-abs",length(vgt.abs)),rep("mpb",length(btlv)),rep("mpb-abs",length(btl.abs)))
  df <- data.frame(vals, prs)
  return(df)
}

climate.space.paired <- function(i){
	cols2 <- c("grey70", "#1b9e77", "#1B9E777D", "#7570b3", "#7570B37D")
	vars1 <- c("ddAugJul", "AugTmax", "winterTmin", "summerP1")
	vars2 <- c("GSP", "summerP0", "PPT", "Tvar")
  df <- df5[,c(vars1[i], vars2[i], "prs")]
  colnames(df)[1:2] <- c("tmp", "pre")
  df <- df[order(df$prs),]  
  plot1 <- qplot(tmp, pre, data=df, color=factor(prs), alpha=I(0.5), xlab = varnms.t[i], ylab = varnms.p[i], main = "MPB climate space")
  plot1 <- plot1 + scale_colour_manual(name="Presence", labels=c("Continent","Hosts","Beetles"), values = cols)+ labs(color="prs")
  plot1 <- plot1 + theme(axis.text=element_text(size=12),axis.title=element_text(size=14,face="bold"))
  
  df <- get.abs.data(vars1[i])
  plot2 <- ggplot(df, aes(x=prs, y=vals, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols2)+theme(axis.ticks.x=element_blank())+labs(x="Presence", y=varnms.t[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+ 
    scale_x_discrete(labels=c("continent" = "Continent", "hosts" = "Hosts", "hosts-abs" = "Hosts-abs", "mpb" = "Beetles", "mpb-abs" = "Beetles-abs"))
  
  df <- get.abs.data(vars2[i])
  plot3 <- ggplot(df, aes(x=prs, y=vals, fill=factor(prs)))+geom_boxplot()+scale_fill_manual(values = cols2)+theme(axis.ticks.x=element_blank())+labs(x="Presence", y=varnms.p[i])+stat_summary(fun.data = max.n, geom = "text", fun.y = max)+
    stat_summary(fun.data = min.n, geom = "text", fun.y = min)+stat_summary(fun.data = mean.n, geom = "text", fun.y = mean, col="white")+theme(legend.position="none")+ 
    scale_x_discrete(labels=c("continent" = "Continent", "hosts" = "Hosts", "hosts-abs" = "Hosts-abs", "mpb" = "Beetles", "mpb-abs" = "Beetles-abs"))
  grid.newpage()
  par(mar=c(2,2,4,2))
  pushViewport(viewport(layout = grid.layout(1, 4))) # 1 rows, 4 columns
  print(plot1, vp = vplayout(1, 1:2))  # the big plot covers rows 1 and cols 1:2
  print(plot2, vp = vplayout(1, 3))
  print(plot3, vp = vplayout(1, 4))
}