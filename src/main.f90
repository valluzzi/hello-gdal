program HelloWorld
    use gdal
    implicit none

    TYPE(gdaldriverh) :: driver
    TYPE(gdaldataseth) :: ds1, ds2
    character(len=16), dimension(:), allocatable ::CO
    real :: data(4,4) = reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/))
    integer :: err    


    CO = (/ "COMPRESS=LZW" /)

    call GDALAllRegister()

    ds1 = Open('test.tif', 0)

    print *, gdalgetrasterysize(ds1) , gdalgetrasterxsize(ds1)

    call close(ds1)

    !Numpy2GDAL_float32(data, gt, prj, 'test.tif', CO)

    deallocate(CO)
    call GDALDestroyDriverManager()

    ! This program prints "Hello, World!" to the screen.
    print *, "Hello, my old World!"
    print *, "========================================================="
end program HelloWorld
