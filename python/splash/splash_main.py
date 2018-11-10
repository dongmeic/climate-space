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
import sys

from data import Data
from splash import Splash


def parse_path(path):
    path_parts = path.split('/')
    infile = path_parts[-1]
    outfile = 'ET_' + infile
    outfile = '/'.join(path_parts[:-1]) + '/' + outfile
    year = path_parts[-2]
    identifier, latitude, elevation, end = infile.split('_')
    assert end.replace('.csv', '') == year, \
        'Year in file name does not match directory'
    return float(latitude), float(elevation), outfile


def main(path=None):
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
    my_data.read_csv(path)
    latitude, elevation, outfile = parse_path(path)
    print('Computing Splash Evapotranspiration with:\n'
          '  lat: %.2f\n  elevation: %.2f\n'
          'and writing to %s...\n' % (latitude, elevation, outfile))
    my_class = Splash(latitude, elevation)
    my_class.spin_up(my_data)
    #my_class.print_daily_sm()
    my_class.write_daily_aet(outfile)

    
if __name__ == '__main__':
    main()
