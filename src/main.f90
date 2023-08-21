program HelloWorld
    use gdal
    implicit none

    TYPE(gdaldriverh) :: driver
    TYPE(gdaldataseth) :: ds1, ds2
    character(len=16), dimension(:), allocatable ::CO

    
    CO = (/ "COMPRESS=LZW" /)

    call GDALAllRegister()

    ds1 = Open('test.tif', 0)

    print *, gdalgetrasterysize(ds1) , gdalgetrasterxsize(ds1)

    call close(ds1)

    ds2 = GTiff2COG('test.tif', 'test2.tif')
    call close(ds2)


    deallocate(CO)
    call GDALDestroyDriverManager()

    ! This program prints "Hello, World!" to the screen.
    print *, "Hello, my old World!"
    print *, "========================================================="
end program HelloWorld
