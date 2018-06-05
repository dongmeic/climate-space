#-------------------------------------------------------------------------------
# Name:        beetle_in_pine.py
# Purpose:     get the beetle data that overlaps with the pine data
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
arcpy.env.workspace = "H:/beetle/data/vector/vegetation/corehost"
outfolder = "H:/beetle/output/climate_space/presence/"

infc = outfolder+"na_presence_beetle_vegetation.shp"

pineList = arcpy.ListFeatureClasses()
pineNameList = []
for pine in pineList:
    pineDesc = arcpy.Describe(pine)
    pineName = pineDesc.basename
    pineNameList.append(pineName)

for vegname in pineNameList:
    for year in range(1997,2017):
        fieldList = arcpy.ListFields(infc)
        field_names = [f.name for f in fieldList]
        newfield = vegname[:5] + '_' + str(year)
        if newfield in field_names:
            pass
        else:              
            arcpy.AddField_management(infc, newfield, "SHORT", "", "", 10)
        with arcpy.da.UpdateCursor(infc, ['prs_{0}'.format(year), '{0}'.format(vegname), '{0}_{1}'.format(vegname[:5],year)]) as cursor:
            for row in cursor:
                if row[0] == 1 and row[1] == 1:
                    row[2] = 1
                else:
                    row[2] = 0
                cursor.updateRow(row)
        del row
        del cursor
        print(arcpy.GetMessages(0))
        print('{0} in {1} update finished'.format(vegname, year))
stop = timeit.default_timer()
print stop - start
