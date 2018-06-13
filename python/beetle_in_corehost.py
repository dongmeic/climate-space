#-------------------------------------------------------------------------------
# Name:        beetle_in_corehost.py
# Purpose:     get the beetle data that overlapps in the core host range
#
# Author:      dongmeic
#
# Created:     06/05/2018
# Copyright:   (c) dongmeic 2018
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()
arcpy.env.workspace = "H:/beetle/output/climate_space/presence/"
outfolder = "H:/beetle/output/climate_space/presence/"

infc = "na_presence_beetle_vegetation.shp"

for year in range(1997,2017):
    fieldList = arcpy.ListFields(infc)
    field_names = [f.name for f in fieldList]
    newfield = 'chost_' + str(year)
    if newfield in field_names:
        pass
    else:
        arcpy.AddField_management(infc, newfield, "SHORT", "", "", 10)

    with arcpy.da.UpdateCursor(infc, ['prs_{0}'.format(year), 'vegetation', 'chost_{0}'.format(year)]) as cursor:
        for row in cursor:
            if row[0] == 1 and row[1] == 1:
                row[2] = 1
            else:
                row[2] = 0
            cursor.updateRow(row)
    print("beetle in core hosts in year {0} updated".format(year))
    del row
    del cursor
    print(arcpy.GetMessages(0))

stop = timeit.default_timer()
print stop - start
