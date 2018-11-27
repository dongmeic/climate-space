# the script is to combine both CRU and Daymet data

years <- 1996:2015; nyr <- length(years)
cru.path <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
daymet.path <- "/gpfs/projects/gavingrp/dongmeic/daymet/bioclm_na/"

# read beetle data
na10km_btl_df <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/beetle_presence.csv")
rows <- read.csv("/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/daymet_na10km.csv")
na10km_btl_df <- na10km_btl_df[rows$rows,]

read_CRU <- function(yr){
	df <- read.csv(paste0(cru.path, "bioclimatic_values_", years[yr],".csv"))
	df <- df[, -which(names(df) %in% c("winterTmin"))]
	df[rows$rows,]
}

read_Daymet <- function(yr){
	df1 <- read.csv(paste0(daymet.path, "dm_bioclm_var_",years[yr],"_na.csv"))
  df2 <- read.csv(paste0(daymet.path, "dm_bioclm_var_",years[yr],"_na_wd.csv"))
  df3 <- read.csv(paste0(daymet.path, "dm_bioclm_DD_",years[yr],"_na.csv"))
  df1$ddAugJul <- df3$ddAugJul; df1$ddAugJun <- df3$ddAugJun;
	cbind(df1, df2)
}

combine_CRU_Daymet <- function(yr){
	df1 <- read_CRU(yr)
	df2 <- read_Daymet(yr)
	df <- cbind(df1, df2, na10km_btl_df[,c(paste0("prs_",(years[yr]+1)),"vegetation")])
	b <- dim(df)[2]; a <- b - 1
	colnames(df)[a:b] <- c("beetles","hosts")
	return(df)
}

get_data <- function(){
	df <- combine_CRU_Daymet(1)
	print(paste("reading data from", years[1]))
	for(i in 2:nyr){
		ndf <- combine_CRU_Daymet(i)
		df <- rbind(df, ndf)
		print(paste("reading data from", years[i]))
	}
	cbind(df, year=unlist(lapply(years,function(i) rep(i,dim(df)[1]/nyr))))
}