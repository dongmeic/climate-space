# Created by Dongmei Chen

out <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/plots/"
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
years <- 1996:2015; nyr <- length(years)
setwd(out)

vargrp.t <- c("Tmin", "MarTmin", "TOctSep", "Tmean", "fallTmean", "OctTmin", "winterTmin",
							"JanTmin", "ddAugJun", "ddAugJul", "TMarAug", "summerTmean")
							
vargrp.p <- c("AugTmean", "AugTmax", "Tvar", "PMarAug", "PcumOctSep", "PPT", "Pmean",
              "POctSep", "summerP2", "GSP", "summerP0", "summerP1")

vargrp <- c(vargrp.t, vargrp.p) 
cols <- c("grey70", "#1b9e77", "#7570b3")

std.histogram <- function(i){
  df <- read.csv(paste0(csvpath, vargrp[i], "_std_", years[1], "_", years[nyr], ".csv"))
  df[,1] <- ceiling(df[,1])
  na <- subset(df, prs=="continent")[,1]
  vgt <- subset(df, prs=="hosts")[,1]
  btl <- subset(df, prs=="mpb")[,1]
  a.bars <- table(na)
  b.bars <- table(vgt)
  c.bars <- table(btl)
  a <- as.numeric(names(a.bars))
  b <- as.numeric(names(b.bars))
  c <- as.numeric(names(c.bars))
  n1 <- which(a==min(b))
  if (n1 > 1){
    b.bars <- c(rep(NA, n1-1), b.bars)
  }
  n2 <- which(a==min(c))
  if (n2 > 1){
    c.bars <- c(rep(NA, n2-1), c.bars)
  }
  r <- range(c(na, vgt, btl))  
  mn <- r[1]
  mx <- r[2]
  bands <- length(seq(mn, mx, by=1))
  all.bars <- c()
  for (j in 1:bands) {
    all.bars <- c(all.bars, a.bars[j], b.bars[j], c.bars[j])
  }
  #png("barplot_std_test.png", width=8, height=6, units="in", res=300)
  barplot(log(all.bars), main=vargrp[i], col=cols, space=0, xaxt='n')
  axis(1, at=seq(1.5, 3*(mx - mn)+2, 3), labels=mn:mx, tick=F)
  #dev.off()
}
png("std_histogram_union.png", width=20, height=12, units="in", res=300)
par(mfrow=c(4,6),mar=c(5,5,3,1))
for(i in 1:length(vargrp)){
  std.histogram(i)
  print(paste0("barplot for ", vargrp[i]))
}
#plot(0,type='n',axes=FALSE,ann=FALSE)
#legend('center', fill=cols, legend=c("Continent", "Hosts", "Beetles"), cex = 2, bty='n')
dev.off()

print("all done!")		  
