#!C:\Python\Python310\python.exe

import sys

from osgeo.gdal import UseExceptions, deprecation_warn

# import osgeo_utils.gdal_pansharpen as a convenience to use as a script
from osgeo_utils.gdal_pansharpen import *  # noqa
from osgeo_utils.gdal_pansharpen import main

UseExceptions()

deprecation_warn("gdal_pansharpen")
sys.exit(main(sys.argv))
