#! /usr/bin/env python3

# start.py
# Usage
#   ./start.py [YEARS]
#
# YEARS can be either:
#   - a single year, e.g.: 1996
#   - a comma-separated list with no spaces, e.g.: 1996,1998,2003
#   - a range separated with a dash, e.g.: 1996-2001
#   - the word "all" without quotes

import os
import sys

import splash_main as splash


#DATA_DIR = './data/input'
DATA_DIR = '/gpfs/projects/gavingrp/dongmei/daymet/evapo_na/input'

if len(sys.argv) < 2:
    print('Missing argument.\nUsage:\n  ./start.py [YEARS]')
    sys.exit(1)
YEARS = sys.argv[1]


def main():
    years = get_years(YEARS)
    print('Getting evapotranspiration data for the following years:', years)
    for year in years:
        files = [f for f in os.listdir('%s/%d' % (DATA_DIR, int(year)))
                 if f.startswith('s')]
        for f in files:
            inpath = '%s/%d/%s' % (DATA_DIR, int(year), f)
            splash.main(inpath)
    

def get_years(years):
    if years == 'all':
        return list(range(1996, 2016))
    if len(years) == 4:
        return [years]
    if '-' in years:
        start, stop = years.split('-')
        return list(range(int(start), int(stop) + 1))
    if ',' in years:
        return years.split(',')
    print('YEARS not properly formatted:\nUsage:\n'
          '  ./start.py [YEARS]\n\n'
          'YEARS can be either:\n'
          '  - a single year, e.g.: 1996\n'
          '  - a comma-separated list with no spaces, e.g.: 1996,1998,2003\n'
          '  - a range separated with a dash, e.g.: 1996-2001\n'
          '  - the word "all" without quotes\n')


    
if __name__ == '__main__':
    main()
