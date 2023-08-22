program TestOpen
    use gdal
    implicit none

    TYPE(gdaldriverh) :: driver
    TYPE(gdaldataseth) :: ds
    
    call GDALAllRegister()

    ds = Open('test.tif', 0)

    print *, gdalgetrasterysize(ds) , gdalgetrasterxsize(ds)

    call close(ds)

   
    call GDALDestroyDriverManager()

    print *, "========================================================="
end program TestOpen
