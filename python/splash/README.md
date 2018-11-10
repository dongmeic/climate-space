# Splash
Run as:
```
> python3 main.py [latitude] [elev]
```

With data stored in `./data/file_name.csv` and formatted as:
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
