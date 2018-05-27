# climate-space
A repository for the climate space project

### Getting data to infer monthly min/max temperatures from monthly means

Get **DAILY** weather data samples found via:
https://www.ncdc.noaa.gov/cdo-web/search?datasetid=GHCND >

Select Daily Summaries; Location;  Year range (e.g., 2007-01-01 - 2015-12-31); Units: Standard
Select the following options:
+ Station Name, Geog Location, Include Data Flags
+ Precipitation
  + Precipitation (PRCP)
+ Air Temp
  + Observerd (TOBS) [or TAVG if TOBS not available]
  + Max (TMAX)
  + Min (TMIN)

Save the file as "location_startyear_stopyear.csv" (e.g. "eugeneOR_2007_2015.csv")
