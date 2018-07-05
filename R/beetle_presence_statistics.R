# Created by Dongmei Chen
# generate beetle presence statistics

library(ncdf4)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlprs <- read.csv(paste0(csvpath, "beetle_presence.csv"))
btlprs$key <- seq(1,dim(btlprs)[1])
target_columns <- colnames(btlprs[,grepl("prs_", colnames(btlprs))])
btlprs$sumprs <- rowSums(btlprs[,target_columns])
btlprs$sumprs[btlprs$sumprs==0] <- NA
#summary(btlprs$sumprs[btlprs$sumprs>=1])
# testing
#i <- which(btlprs$sumprs==9)[1]
prs.df <- btlprs[!is.na(btlprs$sumprs),]
# the function to get the consecutive statistics
get.stat <- function(df, i){
# select the row
  target_row <- df[i,target_columns]
# calculate the consecutive repeats
  runs <- rle(target_row)
# longest continuous presence (maxprs)
  maxprs <- max(runs$lengths[which(as.numeric(runs$values[1,])==1)])
# longest continuous absence (maxabs)
  maxabs <- max(runs$lengths[which(as.numeric(runs$values[1,])==0)])
  substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
  }
# frequence of the longest continuous presence (nomaxprs)
  nomaxprs <- length(which(runs$lengths==maxprs))
# frequence of the longest continuous absence (nomaxabs)
  nomaxabs <- length(which(runs$lengths==maxabs))
# the year of presence after the first longest absence (prsyr)  
  prsyr <- substrRight(colnames(runs$values)[(which(runs$lengths==maxabs)[1]+1)],4)
# the year of absence after the first longest presence (absyr)
  absyr <- substrRight(colnames(runs$values)[(which(runs$lengths==maxprs)[1]+1)],4)
# average length of continuous presence (meanprs)
  meanprs <- mean(runs$lengths[which(as.numeric(runs$values[1,])==1)])
# number of transitions (0-1, 1-0; noalter)
  alter01 <- sum(diff(as.numeric(runs$values[1,])) == -1)
  alter10 <- sum(diff(as.numeric(runs$values[1,])) == 1)
  alter <- alter01 + alter10
  return(list(maxprs,maxabs,nomaxprs,nomaxabs,prsyr,absyr,meanprs,alter01,alter10,alter))
}

# the function to select the neighboring cells
get.neighbor.sum <- function(df,i){
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
  ifelse(length(total), total, 0)
}

ptm <- proc.time()
for (i in 1:dim(prs.df)[1]){  
  if(prs.df$sumprs[i]==20){
    prs.df$maxprs[i] <- 20
    prs.df$maxabs[i] <- 0
    prs.df$nomaxprs[i] <- 1
    prs.df$nomaxabs[i] <- 0   
    prs.df$prsyr[i] <- NA
    prs.df$absyr[i] <- NA
    prs.df$meanprs[i] <- 20
    prs.df$alter01 <- NA
    prs.df$alter10 <- NA
    prs.df$alter <- 0
  }else{
    stats <- get.stat(prs.df,i)
    prs.df$maxprs[i] <- stats[[1]]
    prs.df$maxabs[i] <- stats[[2]]
    prs.df$nomaxprs[i] <- stats[[3]]
    prs.df$nomaxabs[i] <- stats[[4]]
    prs.df$prsyr[i] <- stats[[5]]
    prs.df$absyr[i] <- stats[[6]]
    prs.df$meanprs[i] <- stats[[7]]
    prs.df$alter01 <- stats[[8]]
    prs.df$alter10 <- stats[[9]]
    prs.df$alter <- stats[[10]]
  }
  prs.df$ngbyrs[i] <- get.neighbor.sum(prs.df,i)
  # comment below line if the script is run in bash
  #print(paste("row", rownames(prs.df)[i]))	
}
proc.time() - ptm
print("looping done!")

# get the complete data frame
abs.df <- btlprs[is.na(btlprs$sumprs),]
abs.df$maxprs <- rep(NA, dim(abs.df)[1])
abs.df$maxabs <- rep(NA, dim(abs.df)[1])
abs.df$nomaxprs <- rep(NA, dim(abs.df)[1])
abs.df$nomaxabs <- rep(NA, dim(abs.df)[1])
abs.df$prsyr <- rep(NA, dim(abs.df)[1])
abs.df$absyr <- rep(NA, dim(abs.df)[1])
abs.df$meanprs <- rep(NA, dim(abs.df)[1])
abs.df$alter01 <- rep(NA, dim(abs.df)[1])
abs.df$alter10 <- rep(NA, dim(abs.df)[1])
abs.df$alter <- rep(NA, dim(abs.df)[1])
abs.df$ngbyrs <- rep(NA, dim(abs.df)[1])
df <- rbind(abs.df, prs.df)

# double check the possible neighboring presence
# make a bounding box
Xmin <- min(prs.df$x) - 10000;
Xmax <- max(prs.df$x) + 10000;
Ymin <- min(prs.df$y) - 10000;
Ymax <- max(prs.df$y) + 10000;
abs.df.ss <- subset(df, x >= Xmin & x <= Xmax & y >= Ymin & y <= Ymax)
ptm <- proc.time()
for (i in 1:dim(abs.df.ss)[1]){
  if (is.na(abs.df.ss$sumprs[i])){
    abs.df.ss$ngbyrs[i] <- get.neighbor.sum(abs.df.ss,i)
  }
  # comment below line if the script is run in bash
  #print(paste("row", rownames(abs.df.ss)[i]))	
}
proc.time() - ptm
print("looping done!")
summary(abs.df.ss$ngbyrs)
abs.df.ss$ngbyrs[abs.df.ss$ngbyrs==0] <- NA
summary(abs.df.ss$ngbyrs[!is.na(abs.df.ss$ngbyrs)])
# update the dataframe
ptm <- proc.time()
t=0
for(i in 1:length(abs.df.ss$key)){
  print(paste("the original value is", df$ngbyrs[which(df$key == abs.df.ss$key[i])],
  "and the replacement is", abs.df.ss$ngbyrs[i]))
  if (is.na(df$ngbyrs[which(df$key == abs.df.ss$key[i])]) && is.na(abs.df.ss$ngbyrs[i])){
  	#print("keeping NA value")
  	t=t+0
  }else if(!is.na(df$ngbyrs[which(df$key == abs.df.ss$key[i])]) && !is.na(abs.df.ss$ngbyrs[i]) 
           && df$ngbyrs[which(df$key == abs.df.ss$key[i])] == abs.df.ss$ngbyrs[i]){
    #print("keeping the same value")
    t=t+0
  }else{
    #print("updating the value")
    df$ngbyrs[which(df$key == abs.df.ss$key[i])] <- abs.df.ss$ngbyrs[i]
    t=t+1
  } 
}
proc.time() - ptm
print(paste("...totally", t, "grid cells updated..."))
df <- df[order(df$key),]
df <- df[,-which(names(df) %in% c("key"))]
print(paste("dim(df) is", dim(df)))
head(df)
write.csv(df, paste0(csvpath, "beetle_presence_statistics.csv"), row.names=FALSE)

# write the beetle presence statistics into a netCDF file
# open points netCDF file to get dimensions, etc.
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/csvfiles/"
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncin <- nc_open(paste(ncpath,"na10km_v2.nc",sep=""))
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y)
lon <- ncvar_get(ncin, varid="lon")
lat <- ncvar_get(ncin, varid="lat")
nc_close(ncin)

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- "lambert_azimuthal_equal_area"
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")

# replace NA with fillvalue
df[is.na(df)] <- fillvalue

# presence variable definitions
vardefs <- read.csv(paste(csvpath,"beetle_presence_statistics_def.csv",sep=""), as.is=TRUE)
str(vardefs)

# presence variables
ncfname <- paste0(ncpath, "prs/na10km_v2_presence_beetle_statistics.nc")
nvars <- 12

# reshape data
j2 <- sapply(df$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(df$y, function(xy) which.min(abs(y-xy)))
head(cbind(df$x,df$y,j2,k2))

nobs <- dim(df)[1]
m <- rep(1:nvars,each=nobs)
temp_array <- array(fillvalue, dim=c(nx,ny,nvars))
temp_array[cbind(j2,k2,m)] <- as.matrix(df[1:nobs,(dim(df)[2]-11):ncol(df)])

# create netCDF file and put data

ncout <- nc_create(ncfname,list(lon_def,lat_def,proj_def),force_v4=TRUE, verbose=FALSE)

# put additional attributes into dimension and data variables
ncatt_put(ncout,"x","axis","X")
ncatt_put(ncout,"x","standard_name","projection_x_coordinate")
ncatt_put(ncout,"x","grid_spacing","10000 m")
ncatt_put(ncout,"x","_CoordinateAxisType","GeoX")
ncatt_put(ncout,"y","axis","Y")
ncatt_put(ncout,"y","standard_name","projection_y_coordinate")
ncatt_put(ncout,"y","grid_spacing","10000 m")
ncatt_put(ncout,"y","_CoordinateAxisType","GeoY")


ncatt_put(ncout,projname,"name",projname)
ncatt_put(ncout,projname,"long_name",projname)
ncatt_put(ncout,projname,"grid_mapping_name",projname)
ncatt_put(ncout,projname,"longitude_of_projection_origin",-100.0)
ncatt_put(ncout,projname,"latitude_of_projection_origin",50.0)
ncatt_put(ncout,projname,"_CoordinateTransformType","Projection")
ncatt_put(ncout,projname,"_CoordinateAxisTypes","GeoX GeoY")
na10km_projstr <- "+proj=laea +lon_0=-100 +lat_0=50 +x_0=0 +y_0=0 +ellps=WGS84 +datum=WGS84 +units=m +no_defs"
ncatt_put(ncout,projname,"CRS.PROJ.4",na10km_projstr)

# put variables
ncvar_put(ncout,lon_def,lon)
ncvar_put(ncout,lat_def,lat)

# add global attributes
ncatt_put(ncout,0,"title","beetle presence statistics on the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
history <- paste("D.Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
#ncatt_put(ncout,0,"Conventions","CF-1_6")

# add variables
for (i in 1:nvars){
  var_def <- ncvar_def(vardefs$data_name[i],vardefs$units[i],list(xdim,ydim),fillvalue,vardefs$long_name[i],prec="float")  
  ncout <- ncvar_add(ncout, var_def)
  ncvar_put(ncout, var_def, temp_array[,,i])
  ncatt_put(ncout,vardefs$data_name[i],"base_period","1997-2016")
}
# close the file, writing data to disk
nc_close(ncout)
print("all done!")
