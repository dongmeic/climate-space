# Created by Dongmei Chen
# generate time-series beetle presence statistics

library(ncdf4)
csvpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/"
btlprs <- read.csv(paste0(csvpath, "beetle_presence.csv"))

target_columns <- c("x", "y", colnames(btlprs[,grepl("prs_", colnames(btlprs))]))
ndf <- btlprs[,target_columns]
yr.runs <- seq(2,10,2) # time-series neighboring cell sums of years
years <- 1997:2016

# 0-0, 0-1, 1-0, 1-1 alteration every year
alteration <- function(df, yr){
  tar_cols <- c(paste0("prs_", years[yr]), paste0("prs_", years[yr+1]))
  df.ss <- df[,tar_cols]
  colnm <- paste0("alt",years[yr+1])
  df.ss[,colnm]<- paste0(df.ss[,1],df.ss[,2])
  df.ss[,colnm] <- ifelse(df.ss[,colnm]=="00", 0, 
                    ifelse(df.ss[,colnm]=="01", 1,
                      ifelse(df.ss[,colnm]=="10", 2,3)))
  return(df.ss[,colnm])
}

n <- length(years) - 1
print("...writing alteration data...")
for (i in 1:n){
  colnm <- paste0("alt",years[i+1])
  ndf[,colnm] <- alteration(ndf, i)
  print(paste("got", years[i+1]))
}
print("...finished writing alteration...")

d <- dim(ndf)[1]
# calculate neighboring cell sums of years
neighboring.sum <- function(k, yr, m){
  tar_cols <- c("x", "y", paste0("prs_",years[yr+1:k-1]))
  df <- ndf[,tar_cols]
  df$sumprs <- rowSums(df[,-2:-1])
  df$sumprs[df$sumprs==0] <- NA
  
  x <- df$x[m]; y <- df$y[m]
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

print("writing neighboring cell years")
for(k in yr.runs){
  print(paste("running k", k))
  for(yr in 1:(n-k+2)){
    colnm <- paste0("ngb",k,years[yr+k-1])
    v <- vector()
    for(m in 1:d){
      v[m] <- neighboring.sum(k, yr, m)
      # comment below line if the script is run in bash
      print(paste("row", rownames(ndf)[m]))	
    }
    ndf[,colnm] <- v
    print(paste("got", years[yr+k-1]))
  }
  print(paste("finished running k", k))  
}

write.csv(ndf, paste0(csvpath, "ts_beetle_presence_statistics.csv"), row.names=FALSE)
print("finished CSV writing")

# open points netCDF file to get dimensions, etc.
ncpath <- "/gpfs/projects/gavingrp/dongmeic/beetle/ncfiles/na10km_v2/"
ncin <- nc_open(paste0(ncpath,"na10km_v2.nc"))
x <- ncvar_get(ncin, varid="x"); nx <- length(x)
y <- ncvar_get(ncin, varid="y"); ny <- length(y
lon <- ncvar_get(ncin, varid="lon")
lat <- ncvar_get(ncin, varid="lat")
nc_close(ncin)

# time
year <- 1998:2016
nt <- length(year)
tunits <- "year"

# define dimensions
xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
tdim <- ncdim_def("year",units=tunits,longname="year",as.integer(year))

# define common variables
fillvalue <- 1e32
dlname <- "Longitude of cell center"
lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
dlname <- "Latitude of cell center"
lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
projname <- "lambert_azimuthal_equal_area"
proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")
dlname <- "year"
time_def <- ncvar_def(dlname,tunits,list(tdim),NULL,dlname,prec="integer")

# mpb
ncfname <- paste(ncpath,"prs/time_series_presence_statistics.nc", sep="")
csvfile <- "ts_beetle_presence_statistics.csv"
dname <- "prs_alt"
dlname <- "Mountain pine beetle presence-absence alteration"
dunits <- ""

# read and reshape
print("read time series beetle presence statistics")
indata <- read.csv(paste(csvpath, csvfile, sep=""))
str(indata)
print("done!")

print("set j and k index")
j2 <- sapply(indata$x, function(xy) which.min(abs(x-xy)))
k2 <- sapply(indata$y, function(xy) which.min(abs(y-xy)))
head(cbind(indata$x,indata$y,j2,k2))
print("done!")

print("create temporary array")
nobs <- dim(indata)[1]
m <- rep(1:nt,each=nobs)
temp_array <- array(fillvalue, dim=c(nx,ny,nt))
temp_array[cbind(j2,k2,m)] <- as.matrix(indata[1:nobs,(2+20+1):(2+20+19)])
print("done!")

# create netCDF file and put data
print("define the netCDF file")
var_def <- ncvar_def(dname,dunits,list(xdim,ydim,tdim),fillvalue,dlname,prec="float")
ncout <- nc_create(ncfname,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)
print("done!")

# put additional attributes into dimension and data variables
print("writing output")
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
ncvar_put(ncout,time_def,year)
ncvar_put(ncout,var_def,temp_array)

# add global attributes
ncatt_put(ncout,0,"title","beetle presence statistics onto the na10km_v2 10-km Grid")
ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
ncatt_put(ncout,0,"source","generated by time_series_statistics.R")
history <- paste("D.Chen", date(), sep=", ")
ncatt_put(ncout,0,"history",history)
ncatt_put(ncout,0,"base_period","1998-2016")
print("done!")

# add more beetle presence statistics
print("add more beetle presence statistics data")
dname <- "ngbyrs_2"
dlname <- "number of years of presence in the nearest eight cells every 2 years"
var_def <- ncvar_def(dname,dunits,list(xdim,ydim,tdim),fillvalue,dlname,prec="float")
ncout <- ncvar_add(ncout, var_def)
temp_array <- array(fillvalue, dim = c(nx,ny,nt))
temp_array[cbind(j2,k2,m)] <- as.matrix(indata[1:nobs,(2+20+19+1):(2+20+19+19)]) 
ncvar_put(ncout, var_def, temp_array)
nc_close(ncout)
print("done!")


print("writing more 3D netCDF files...")
t <- 2
p <- 41
for (i in yr.runs[-1]){
  year <- (1997+t-1):2016
  nt <- length(year)
  tunits <- "year"
  # define dimensions
  xdim <- ncdim_def("x",units="m",longname="x coordinate of projection",as.double(x))
  ydim <- ncdim_def("y",units="m",longname="y coordinate of projection",as.double(y))
  tdim <- ncdim_def("year",units=tunits,longname="year",as.integer(year))
  # define common variables
  fillvalue <- 1e32
  dlname <- "Longitude of cell center"
  lon_def <- ncvar_def("lon","degrees_east",list(xdim,ydim),NULL,dlname,prec="double")
  dlname <- "Latitude of cell center"
  lat_def <- ncvar_def("lat","degrees_north",list(xdim,ydim),NULL,dlname,prec="double")
  projname <- "lambert_azimuthal_equal_area"
  proj_def <- ncvar_def(projname,"1",NULL,NULL,longname=dlname,prec="char")
  dlname <- "year"
  time_def <- ncvar_def(dlname,tunits,list(tdim),NULL,dlname,prec="integer")
  
  varnm = paste0("ngbyrs_",i)
  print(paste("start writing", varnm))
  varlnm = paste("number of years of presence in the nearest eight cells every", i, "years")
  ncfname <- paste0(ncpath,"prs/time_series_presence_statistics_",i,".nc")
  var_def <- ncvar_def(varnm, dunits, list(xdim, ydim, tdim), fillvalue,varlnm, prec = "float")
  ncout <- nc_create(ncfname,list(lon_def,lat_def,var_def,proj_def),force_v4=TRUE, verbose=FALSE)  
  p <- p + nt
  temp_array <- array(fillvalue, dim = c(nx,ny,nt))
  temp_array[cbind(j2,k2,m)] <- as.matrix(indata[1:nobs,(p+1):(p+length((1997+i-1):2016))])
  t <- i
  # put variables
  ncvar_put(ncout,lon_def,lon)
  ncvar_put(ncout,lat_def,lat)
  ncvar_put(ncout,time_def,year)
  ncvar_put(ncout, var_def, temp_array)
  
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
  ncatt_put(ncout,0,"title","beetle presence statistics onto the na10km_v2 10-km Grid")
  ncatt_put(ncout,0,"institution","Dept. Geography; Univ_ Oregon")
  ncatt_put(ncout,0,"source","generated by time_series_statistics.R")
  history <- paste("D.Chen", date(), sep=", ")
  ncatt_put(ncout,0,"history",history)
  ncatt_put(ncout,0,"base_period",paste0((1997+i-1),"-2016"))
  # close the file, writing data to disk
  nc_close(ncout)
  print(paste("wrote", varnm, "into a netCDF file"))
}
print("all done!")