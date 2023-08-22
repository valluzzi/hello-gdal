program HelloWorld
    use gdal
    use gdal_utils
    implicit none

    TYPE(gdaldriverh) :: driver
    TYPE(gdaldataseth) :: ds
    integer :: m, n
    character(len=16), dimension(:), allocatable ::CO
    real :: data(4,4) = reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/))
    !real, dimension(:,:), allocatable :: matrix
    class(Raster), allocatable :: r
    integer :: err    
    character(:), allocatable :: filename

    CO = (/ "COMPRESS=LZW" /)

    call AllRegister()

    filename = "test_float32.tif"

    ds = Open(filename, 0)

    print *, "nodata value: ", GetNodata(ds)


    r = GDAL2Numpy(filename, 1, 1.5) 

    print *, r
    !print *, "Shape of matrix: ", shape(r%data), kind(r%data(1,1))
    !print *, "max value: ", maxval(r%data), "min value: ", minval(r%data)
    call close(ds)

    deallocate(CO)
    call GDALDestroyDriverManager()

    print *, "========================================================="
end program HelloWorld
