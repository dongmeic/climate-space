# Created by Dongmei Chen
# generate time-series beetle presence statistics

library(ncdf4)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlprs <- read.csv(paste0(csvpath, "beetle_presence.csv"))

btlprs$key <- seq(1,dim(btlprs)[1])
target_columns <- colnames(btlprs[,grepl("prs_", colnames(btlprs))])
df <- btlprs[,target_columns]
yr.runs <- seq(2,10,2) # time-series neighboring cell sums of years
years <- 1997:2016

# 0-0, 0-1, 1-0, 1-1 alteration every year
alteration <- function(yr){
  tar_cols <- c(paste0("prs_", years[yr]), paste0("prs_", years[yr+1])
  df.ss <- df[,tar_cols]
  colnm <- paste0("alt",years[yr+1])
  df.ss[,colnm]<- paste0(df.ss[,1],df.ss[,2])
  df.ss[,colnm] <- ifelse(df.ss[,colnm]=="00", 0, 
                    ifelse(df.ss[,colnm]=="01", 1,
                      ifelse(df.ss[,colnm]=="10", 2,3)))
  df[,colnm] <- df.ss[,colnm]
}

n <- length(years) - 1
for (i in 1:n){
  alteration(i)
}

d < dim(df)[1]
# calculate neighboring cell sums of years
neighboring.sum <- function(df, k, yr, m){
  tar_cols <- c(paste0("prs_",years[yr+1:k-1]))
  df <- df[,tar_cols]
  df$sumprs <- rowSums(df)
  df$sumprs[df$sumprs==0] <- NA
  x <- df$x[i]; y <- df$y[i]
  total <- 0
  for(i in c(-10000, 0, 10000)){
    for(j in c(-10000, 0, 10000)){
      cell.value <- df[df$x == x + i & df$y == y + j, 'sumprs']
      if (length(cell.value) && !is.na(cell.value)) {
	    total <- total + cell.value
	  }
    }
  }
  colnm <- paste0("alt",years[yr+k])
  df[,colnm][m] <- ifelse(length(total), total, 0)
}

for(k in yr.runs){
  for(yr in 1:(n+1)){
    for( m in 1:d){
      neighboring.sum(df, k, yr, m)
    }
  }
}

write.csv(df, paste0(csvpath, "ts_beetle_presence_statistics.csv"), row.names=FALSE)