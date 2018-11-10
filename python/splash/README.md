# Splash
Run as:
```
> chmod u+x start.py # grant execution permission
> ./start.py [YEARS]
```

`YEARS` can be any of the following:
* a single year, e.g.: 1996
* a comma-separated list with no spaces, e.g.: 1996,1998,2003
* a range separated with a dash, e.g.: 1996-2001
* the word "all" without quotes

Within this directory, you must add a `data` directory, organized as:
```
splash/ # current directory
  data/
    1996/
      s0001_LAT_ELEV_1996.csv
      s0002_LAT_ELEV_1996.csv
      ...
    1997/
      s0001_LAT_ELEV_1997.csv
      s0002_LAT_ELEV_1997.csv
      ...
    ...
```

Where 0001, 0002, etc. are the cell identifiers, and LAT and ELEV are the latitude and elevation values for that cell.

Individual files must be formatted as:
```
sf,tair,pn
0.375,12.492242,2.388596
0.375,13.038599,6.419028
0.375,13.142999,13.840825
0.375,11.259424,0
0.375,13.004846,20.105391
0.375,14.797937,3.995099
0.375,9.93783,0
0.375,9.753656,0
0.375,10.229944,0
...
```
where the headers are sf (sunshine fraction), tair (air temperature), pn (precipitation), and each row is one day of data for an entire year.

