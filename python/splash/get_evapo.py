import os
import main

dir = '/gpfs/projects/gavingrp/dongmeic/beetle/output/tables/SPLASH'
files = os.listdir(dir)

# s129279_47.7471601367167_406_1996.csv
def parse(file_name):
	identifier, latitude, elevation, year = file_name.split('_')
	identifier = identifer[1:]
	return identifier, latitude, elevation, year
	
for file in files:
  print('Processing %s...' % file)
	options=parse(file)
	main(options)