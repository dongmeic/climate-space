#-------------------------------------------------------------------------------
# Name:        pinegrids.py
# Purpose:     show core host species on a 10 km grid
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

fclist=arcpy.ListFeatureClasses()

mask = "H:/beetle/data/vector/na10km_v2/reproject/na10km_mask_pts.shp"
arcpy.MakeFeatureLayer_management(mask,"mask")
dist = "10000 Meters"
for fc in fclist:
    dsc_fc = arcpy.Describe(fc)
    arcpy.MakeFeatureLayer_management(fc,"{0}".format(dsc_fc.basename))
    fieldList = arcpy.ListFields("mask")
    field_names = [f.name for f in fieldList]
    # by pine species
    if dsc_fc.basename in field_names:
        pass
    else:
        arcpy.AddField_management("mask", "{0}".format(dsc_fc.basename), "SHORT", "", "", 10)

    arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "{0}".format(dsc_fc.basename), dist, "NEW_SELECTION")
    with arcpy.da.UpdateCursor("mask", ["{0}".format(dsc_fc.basename)]) as cursor:
        for row in cursor:
            row[0] = 1
            cursor.updateRow(row)
    del row
    del cursor
    print('{0} update finished'.format(fc))
    arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "{0}".format(dsc_fc.basename), dist, "SWITCH_SELECTION")
    with arcpy.da.UpdateCursor("mask", ["{0}".format(dsc_fc.basename)]) as cursor:
        for row in cursor:
            row[0] = 0
            cursor.updateRow(row)
    del row
    del cursor
    print('non-{0} update finished'.format(fc))
    arcpy.SelectLayerByAttribute_management("mask", "CLEAR_SELECTION")
arcpy.CopyFeatures_management("mask", outfolder+"na_presence_beetle_vegetation.shp")
layer = "mask"
del layer
print('all finished!')

stop = timeit.default_timer()
print stop - start
