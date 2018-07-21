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


def main():
    if len(sys.argv) < 3:
        print('Usage: main.py lat elv')
        sys.exit(1)
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
    example = 1
    my_data = Data()
    if example == 1:
        # Example 1: read CSV file:
        my_file = 'data/example_data.csv'
        my_data.read_csv(my_file)
    elif example == 2:
        # Example 2: read TXT files:
        my_sf_file = 'daily_sf_2000_cruts.txt'
        my_pn_file = 'daily_pn_2000_wfdei.txt'
        my_tair_file = 'daily_tair_2000_wfdei.txt'
        my_data.read_txt(my_sf_file, 'sf')
        my_data.read_txt(my_pn_file, 'pn')
        my_data.read_txt(my_tair_file, 'tair')

    # Consistency Test #4: Spin-Up
    #my_lat = 37.7
    #my_elv = 142.
    my_lat = float(sys.argv[1])
    my_elv = float(sys.argv[2])
    my_class = Splash(my_lat, my_elv)
    my_class.spin_up(my_data)
    #my_class.print_daily_sm()
    my_class.print_daily_aet()

    
if __name__ == '__main__':
    main()
