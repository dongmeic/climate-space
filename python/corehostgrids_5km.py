#-------------------------------------------------------------------------------
# Name:        corehostgrids.py
# Purpose:     show all core host species on a 10 km grid
# Updates:     change the distance to 5km
# Author:      dongmeic
#
# Created:     12/20/2018
# Copyright:   (c) dongmeic 2018
# Licence:     <your licence>
#-------------------------------------------------------------------------------


import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()

arcpy.env.workspace = "H:/beetle/data/vector/vegetation/corehost"
outfolder = "H:/beetle/output/climate_space/presence/"

fclist=arcpy.ListFeatureClasses()

mask = "H:/beetle/data/vector/na10km_v2/reproject/na10km_mask_pts.shp"
arcpy.MakeFeatureLayer_management(mask,"mask")
dist = "5000 Meters"
fieldList = arcpy.ListFields("mask")
field_names = [f.name for f in fieldList]
if "vegetation" in field_names:
    pass
else:
    arcpy.AddField_management("mask", "vegetation", "SHORT", "", "", 10)
for fc in fclist:
    dsc_fc = arcpy.Describe(fc)
    arcpy.MakeFeatureLayer_management(fc,"{0}".format(dsc_fc.basename))
    arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "{0}".format(dsc_fc.basename), dist, "ADD_TO_SELECTION")
with arcpy.da.UpdateCursor("mask", ['vegetation']) as cursor:
    for row in cursor:
        row[0] = 1
        cursor.updateRow(row)
del row
del cursor
print('vegetation update finished!')
arcpy.SelectLayerByAttribute_management("mask", "SWITCH_SELECTION")
with arcpy.da.UpdateCursor("mask", ['vegetation']) as cursor:
    for row in cursor:
        row[0] = 0
        cursor.updateRow(row)
del row
del cursor
arcpy.SelectLayerByAttribute_management("mask", "CLEAR_SELECTION")
arcpy.CopyFeatures_management("mask", outfolder+"na_presence_beetle_cohosts_5km.shp")
layer = "mask"
del layer

stop = timeit.default_timer()
print stop - start

