#-------------------------------------------------------------------------------
# Name:        beetle_in_corehost.py
# Purpose:     get the beetle data that overlapps in the core host range
#
# Author:      dongmeic
#
# Created:     04/29/2017
# Copyright:   (c) dongmeic 2017
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()
arcpy.env.workspace = "H:/beetle/output/2017/climate_space/"
outfolder = "H:/beetle/output/2017/climate_space/"

infc = "H:/beetle/output/2017/climate_space/na_presence_beetle_vegetation.shp"


try:

    fieldList = arcpy.ListFields(infc)
    for year in range(2001,2016):
        for field in fieldList:
            if field == 'chost_' + str(year):
                break
            else:
                newfield = 'chost_' + str(year)
                arcpy.AddField_management(infc, newfield, "SHORT", "", "", 10)

        with arcpy.da.UpdateCursor(infc, ['prs_{0}'.format(year), 'vegetation', 'chost_{0}'.format(year)]) as cursor:
            for row in cursor:
                if row[0] == 1 and row[1] == 1:
                    row[2] = 1
                    cursor.updateRow(row)
                else:
                    row[2] = 0
                    cursor.updateRow(row)
        print("beetle in core hosts in year {0} updated".format(year))
        del row
        del cursor
        print(arcpy.GetMessages(0))

except:
   print arcpy.GetMessages()

stop = timeit.default_timer()
print stop - start
