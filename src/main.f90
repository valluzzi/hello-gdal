program HelloWorld
    use gdal
    use gdal_numpy
    implicit none

    TYPE(gdaldriverh) :: driver
    TYPE(gdaldataseth) :: ds
    integer :: m, n
    character(len=16), dimension(:), allocatable ::CO
    !real :: data(4,4) = reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/))
    real(kind = c_double), dimension(:,:), allocatable :: data
    character(:), allocatable :: wkt
    real(kind=c_double)::gt(6)
    
    integer :: err    
    character(:), allocatable :: filename

    CO = (/ "COMPRESS=LZW" /)

    call AllRegister()
    

    filename = "test_float64.tif"


    ds = Open(filename, 0)

    print *, "nodata value: ", GetNodata(ds)

    err = GDAL2Numpy(filename, data, prj=wkt,gt=gt, load_nodata_as=real(-999, kind=c_double))
    print *, "Shape of matrix: ", shape(data), kind(data(1,1)), GetDataType(ds)
    print *, "max value: ", maxval(data), "min value: ", minval(data)
    print *, "min:", GetRasterMinimum(ds), "max:", GetRasterMaximum(ds)
    
    
    print *, wkt
    print *, gt
    call close(ds)
    print *, "========================================================="
    
    !data, gt, prj, filename, save_nodata_as, frmt) result(err)
    err =Numpy2GDAL(data, gt, wkt, "test_float64_2.tif",  -999.0 )




    deallocate(CO)
    call GDALDestroyDriverManager()

    print *, "========================================================="
end program HelloWorld
