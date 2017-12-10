#-------------------------------------------------------------------------------
# Name:        pinegrids.py
# Purpose:     show core host species on a 10 km grid
#
# Author:      dongmeic
#
# Created:     04/29/2017
# Copyright:   (c) dongmeic 2016
# Licence:     <your licence>
#-------------------------------------------------------------------------------


import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()

arcpy.env.workspace = "H:/beetle/data/vector/vegetation/corehost"
outfolder = "H:/beetle/output/2017/climate_space/"

fclist=arcpy.ListFeatureClasses()

try:
    mask = "H:/beetle/data/vector/na10km_v2/reproject/na10km_mask_pts.shp"
    dsc_mask = arcpy.Describe(mask)
    arcpy.MakeFeatureLayer_management(mask,"mask")
    dist = "10000 Meters"
    for fc in fclist:
        dsc_fc = arcpy.Describe(fc)
        arcpy.MakeFeatureLayer_management(fc,"{0}".format(dsc_fc.basename))
        fieldList = arcpy.ListFields("mask")
        # by pine species
        for field in fieldList:
            if field == dsc_fc.basename:
                break
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
except:
   print arcpy.GetMessages()

stop = timeit.default_timer()
print stop - start
