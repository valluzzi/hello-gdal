#!C:\Python\Python310\python.exe

import sys

from osgeo.gdal import UseExceptions, deprecation_warn

# import osgeo_utils.gdalattachpct as a convenience to use as a script
from osgeo_utils.gdalattachpct import *  # noqa
from osgeo_utils.gdalattachpct import main

UseExceptions()

deprecation_warn("gdalattachpct")
sys.exit(main(sys.argv))
