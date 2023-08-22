program HelloWorld
    use gdal
    use gdal_utils
    implicit none

    TYPE(gdaldriverh) :: driver
    TYPE(gdaldataseth) :: ds
    integer :: m, n
    character(len=16), dimension(:), allocatable ::CO
    real :: data(4,4) = reshape((/1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16/), (/4,4/))
    real, dimension(:,:), allocatable :: matrix
    integer :: err    


    CO = (/ "COMPRESS=LZW" /)

    call GDALAllRegister()

    ds = Open('test.tif', 0)

    print *, "nodata value: ", GetNodata(ds)

    m = GetRasterYSize(ds)
    n = GetRasterXSize(ds)
    allocate(matrix(n,m))
    
    print *, m,n
 
    matrix = GDAL2Numpy("test.tif", 1, 100.0) 
    
    print *, matrix

    print *, GetDataType(ds), GetDataTypeName(ds)

    call close(ds)

    deallocate(CO)
    call GDALDestroyDriverManager()

    ! This program prints "Hello, World!" to the screen.
    print *, "Hello, my old World!"
    print *, "========================================================="
end program HelloWorld
