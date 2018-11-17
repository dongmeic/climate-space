#! /usr/bin/env python3
# -*- coding: utf-8 -*-
#
# main.py
#
# VERSION: 1.0
# LAST UPDATED: 2016-02-19
#
# ~~~~~~~~
# license:
# ~~~~~~~~
# Copyright (C) 2016 Prentice Lab
#
# This file is part of the SPLASH model.
#
# SPLASH is free software: you can redistribute it and/or modify it under
# the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 2.1 of the License, or
# (at your option) any later version.
#
# SPLASH is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU Lesser General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with SPLASH.  If not, see <http://www.gnu.org/licenses/>.
#
# ~~~~~~~~~
# citation:
# ~~~~~~~~~
# T. W. Davis, I. C. Prentice, B. D. Stocker, R. J. Whitley, H. Wang, B. J.
# Evans, A. V. Gallego-Sala, M. T. Sykes, and W. Cramer, Simple process-
# led algorithms for simulating habitats (SPLASH): Robust indices of radiation,
# evapotranspiration and plant-available moisture, Geoscientific Model
# Development, 2016 (in progress)
#
# ~~~~~~~~~~~~~~~~~
# changelog omitted
# ~~~~~~~~~~~~~~~~~
#
# ~~~~~~~~~~~~~
# modifications
# ~~~~~~~~~~~~~
# Further modifications made by Damian Satterthwaite-Phillips
# 2018-07-17

import logging
import os
import sys

from data import Data
from splash import Splash


def parse_path(path):
    path_parts = path.split('/')
    infile = path_parts[-1]
    outfile = 'ET_' + infile
    outdir = ('/'.join(path_parts[:-1])).replace('input', 'output')
    year = path_parts[-2]
    identifier, latitude, elevation, end = infile.split('_')
    assert end.replace('.csv', '') == year, \
        'Year in file name does not match directory'
    return float(latitude), float(elevation), outdir, outfile, int(year)


def write_data(data, splash, outdir, outfile, year):
    values = 'eet', 'pet', 'aet', 'wn'
    with open('%s/%s' % (outdir, outfile), 'w') as f:
        f.write('equilET,potentialET,actualET,soilMoisture\n')
        for i in range(data.num_lines):
            splash.run_one_day(i + 1,
                               year,
                               splash.wn_vec[i],
                               data.sf_vec[i],
                               data.tair_vec[i],
                               data.pn_vec[i])
            vals = ','.join(splash.get_vals(values)) + '\n'
            f.write(vals)
    print('Writing done.')
                                                                        

def main(path=None):
    latitude, elevation, outdir, outfile, year = parse_path(path)
    if os.path.exists('%s/%s' % (outdir, outfile)):
        print('File %s exists, continuing...' % outfile)
        return
    # Create a root logger:
    root_logger = logging.getLogger()
    root_logger.setLevel(logging.INFO)

    # Instantiating logging handler and record format:
    root_handler = logging.FileHandler("main.log")
    rec_format = "%(asctime)s:%(levelname)s:%(name)s:%(funcName)s:%(message)s"
    formatter = logging.Formatter(rec_format, datefmt="%Y-%m-%d %H:%M:%S")
    root_handler.setFormatter(formatter)

    # Send logging handler to root logger:
    root_logger.addHandler(root_handler)
    my_data = Data()
    my_data.read_csv(path, y=year)
    print('Computing Splash Evapotranspiration with:\n'
          '  lat: %.2f\n  elevation: %.2f\n'
          'and writing to %s/%s...\n' % (latitude, elevation, outdir, outfile))
    my_class = Splash(latitude, elevation)
    my_class.spin_up(my_data)
    if not os.path.isdir(outdir):
         os.makedirs(outdir)
    write_data(my_data, my_class, outdir, outfile, year)
    del my_data
    del my_class
    
    
if __name__ == '__main__':
    INPATH = sys.argv[1]
    main(INPATH)
