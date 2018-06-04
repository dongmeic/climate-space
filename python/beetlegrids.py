#-------------------------------------------------------------------------------
# Name:        beetlegrids.py
# Purpose:     beetle presence from 1997 to 2016 on a 10 km grid in North America
# Updates:     Added year 2016 and polygons from BC
# Author:      dongmeic
#
# Created:     06/04/2018
# Copyright:   (c) dongmeic 2018
# Licence:     <your licence>
#-------------------------------------------------------------------------------

import arcpy, timeit
from arcpy import env
env.overwriteOutput=True

start = timeit.default_timer()

arcpy.env.workspace = "H:/beetle/data/vector/mpbdata/reprojected"
outfolder = "H:/beetle/output/climate_space/presence/"

try:
    mask = "H:/beetle/data/vector/na10km_v2/reproject/na10km_mask_pts.shp"
    dsc_mask = arcpy.Describe(mask)
    arcpy.MakeFeatureLayer_management(mask,"mask")
    dist = "10000 Meters"
        
    for year in range(1997,2001):
        bcfc = "bc_mpb_points_" + str(year) + ".shp"
        bcpoly = "bc_mpb_poly_" + str(year) + ".shp"
        usfc = "us_mpb_" + str(year)+".shp"
        arcpy.MakeFeatureLayer_management(bcfc,"bcfc_{0}".format(year))
        arcpy.MakeFeatureLayer_management(bcpoly,"bcpoly_{0}".format(year))
        arcpy.MakeFeatureLayer_management(usfc,"usfc_{0}".format(year))
        fieldList = arcpy.ListFields("mask")
        for field in fieldList:
            if field == "prs_"+ str(year):
                break
            else:
                arcpy.AddField_management("mask", "prs_"+ str(year), "SHORT", "", "", 10)
            try:
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "bcfc_{0}".format(year), dist, "NEW_SELECTION")
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "bcpoly_{0}".format(year), dist, "ADD_TO_SELECTION")
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "usfc_{0}".format(year), dist, "ADD_TO_SELECTION")
                with arcpy.da.UpdateCursor("mask", ['prs_{0}'.format(year)]) as cursor:
                    for row in cursor:
                        row[0] = 1
                        cursor.updateRow(row)
                del row
                del cursor
                arcpy.SelectLayerByLocation_management("mask", "SWITCH_SELECTION")
                with arcpy.da.UpdateCursor("mask", ['prs_{0}'.format(year)]) as cursor:
                    for row in cursor:
                        row[0] = 0
                        cursor.updateRow(row)
                del row
                del cursor
                arcpy.SelectLayerByAttribute_management("mask", "CLEAR_SELECTION")
                print(arcpy.GetMessages(0))
            except arcpy.ExecuteError:
                print(arcpy.GetMessages(2))

            except Exception as ex:
                print(ex.args[0])
        print('{0} year update finished'.format(year))
        
    for year in range(2001,2017):
        abfc = "ab_mpb_points_" + str(year) + ".shp"
        bcfc = "bc_mpb_points_" + str(year) + ".shp"
        bcpoly = "bc_mpb_poly_" + str(year) + ".shp"
        usfc = "us_mpb_" + str(year)+".shp"
        arcpy.MakeFeatureLayer_management(abfc,"abfc_{0}".format(year))
        arcpy.MakeFeatureLayer_management(bcfc,"bcfc_{0}".format(year))
        arcpy.MakeFeatureLayer_management(bcpoly,"bcpoly_{0}".format(year))
        arcpy.MakeFeatureLayer_management(usfc,"usfc_{0}".format(year))
        fieldList = arcpy.ListFields("mask")
        for field in fieldList:
            if field == "prs_"+ str(year):
                break
            else:
                arcpy.AddField_management("mask", "prs_"+ str(year), "SHORT", "", "", 10)
            try:
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "abfc_{0}".format(year), dist, "NEW_SELECTION")
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "bcfc_{0}".format(year), dist, "ADD_TO_SELECTION")
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "bcpoly_{0}".format(year), dist, "ADD_TO_SELECTION")
                arcpy.SelectLayerByLocation_management("mask", "WITHIN_A_DISTANCE", "usfc_{0}".format(year), dist, "ADD_TO_SELECTION")
                with arcpy.da.UpdateCursor("mask", ['prs_{0}'.format(year)]) as cursor:
                    for row in cursor:
                        row[0] = 1
                        cursor.updateRow(row)
                del row
                del cursor
                arcpy.SelectLayerByLocation_management("mask", "SWITCH_SELECTION")
                with arcpy.da.UpdateCursor("mask", ['prs_{0}'.format(year)]) as cursor:
                    for row in cursor:
                        row[0] = 0
                        cursor.updateRow(row)
                del row
                del cursor
                arcpy.SelectLayerByAttribute_management("mask", "CLEAR_SELECTION")
                print(arcpy.GetMessages(0))
            except arcpy.ExecuteError:
                print(arcpy.GetMessages(2))

            except Exception as ex:
                print(ex.args[0])
        print('{0} year update finished'.format(year))

    arcpy.CopyFeatures_management("mask", outfolder+"na_beetle_presence.shp")
    layer = "mask"
    del layer
except:
   print arcpy.GetMessages()

stop = timeit.default_timer()
print stop - start
