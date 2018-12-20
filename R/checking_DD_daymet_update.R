
inpath <- "/gpfs/projects/gavingrp/dongmeic/daymet/bioclm_na/"
years <- 1996:2015; nyr <- length(years)
# read beetle data
na10km_btl_df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")
na10km_btl_df <- na10km_btl_df[rows$rows,]
dd <- read.csv(paste0(inpath, "dm_bioclm_DD_", years[1], "_na.csv"))
dd <- cbind(dd, na10km_btl_df[,c(paste0("prs_",(years[1]+1)),"vegetation")])
b <- dim(dd)[2]; a <- b - 1
colnames(dd)[a:b] <- c("beetles","hosts")
for(i in 2:20){
	df <- read.csv(paste0(inpath, "dm_bioclm_DD_", years[i], "_na.csv"))
	df <- cbind(df, na10km_btl_df[,c(paste0("prs_",(years[i]+1)),"vegetation")])
	b <- dim(df)[2]; a <- b - 1
	colnames(df)[a:b] <- c("beetles","hosts")
	dd <- rbind(dd, df)
	print(years[i])
}
df <- cbind(dd, year=unlist(lapply(years,function(i) rep(i,dim(dd)[1]/nyr))))
df.btl <- df[df$beetles==1,]

d1 <- vector(); d2 <- vector()
vars <- c("ddAugJul", "ddAugJun")
t <- dim(df.btl)[1]
for(var in vars){
	i <- which(vars==var)
	if(var == "ddAugJul"){
		d1[i] <- sum(df.btl[,var]>305)/t		
	}else{
		d1[i] <- sum(df.btl[,var]>833)/t	
	}
	d2[i] <- 1 - d1[i]	
}
