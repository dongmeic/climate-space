#! /usr/bin/env python3
#
# splash_data.py
#
# VERSION: 1.0-r1
# LAST UPDATED: 2016-09-11
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
# Development, 2015 (in progress)
#
# ~~~~~~~~~~~~
# description:
# ~~~~~~~~~~~~
# The script creates a CSV of the necessary daily meteorological variables
# for running the SPLASH code, including: fraction of bright sunshine,
# Sf (unitless), air temperature, Tair (deg. C), and precipitation, Pn (mm).
# The script creates a CSV of the necessary daily meteorological variables
# for running the SPLASH code, including: fraction of bright sunshine,
# Sf (unitless), air temperature, Tair (deg. C), and precipitation, Pn (mm).
#
# Supported input sources currently include:
#   Sf:
#    * CRU TS3.2 Cld (monthly cloudiness fraction)
#   Tair:
#    * CRU TS3.2x Tmn (monthly min daily air temperature)
#    * CRU TS3.2x Tmp (monthly mean daily air temperature)
#    * CRU TS3.2x Tmx (monthly max daily dair temperature)
#    * WATCH daily Tair (daily mean air temperature)
#   Pn
#    * CRU TS3.2 Pre (monthly total precipitation)
#    * WATCH daily Rainf (daily mean mass flow rate of rainfall)
#
# ~~~~~~~~~~~~~
# modification:
# ~~~~~~~~~~~~~
# Additional modifications made by Damian Satterthwaite-Phillips
# 2018-07-19

import datetime
import glob
import logging
import os.path

import numpy
from scipy.io import netcdf

from const import kPo, kTo, kL, kMa, kG, kR

class SplashData:
    """
    Name:     SplashData
    Features: Processes daily data for the SPLASH model
    History:  Version 1.0
              - created SPLASH_DATA class [15.01.26]
              - fixed mean daily CRU air temperature (tmp not tmn) [15.01.27]
              - renaming (addresses issue #3) [15.08.23]
              - references const.py for SPLASH constants [15.08.23]
              - addressed Python 2/3 compatibility [16.02.17]
              - fixed netCDF RuntimeWarning [16.02.17]
              - added logging [16.02.17]
    """
