module gdal_utils

    use gdal
    use gdal_numpy
    implicit none

    interface Numpy2GDAL
        !module procedure Numpy2GDAL_int16
        module procedure Numpy2GDAL_Float32
    end interface

    contains

    !--------------------------------------------------------------
    ! Function:   GTiff2COG
    ! Purpose:    Create a COG from a GTiff
    ! Inputs:     filesrc - source file name
    !             fileout - output file name
    ! Returns:    GTiff2COG - GDAL dataset handle
    !--------------------------------------------------------------
    function GTiff2COG(filesrc, fileout) 
        implicit none
        character(*), intent(in) :: filesrc
        character(*), intent(in) :: fileout
        type(gdaldataseth) :: ds
        type(gdaldataseth) :: GTiff2COG
        character(len=16), dimension(:), allocatable :: CO

        ds = Open(filesrc, GA_ReadOnly)
        CO = (/'COMPRESS=DEFLATE'/)

        GTiff2COG = GDALCreateCopy( & 
                        GetDriverByName("COG"), &
                        fileout//char(0), &
                        ds, &
                        0, &
                        c_ptr_ptr_getobject(c_ptr_ptr_new(CO)), &
                        C_NULL_PTR, &
                        C_NULL_PTR)
        
        deallocate(CO)

    end function


   


    function Numpy2GDAL_Float32(data, gt, prj, filename, save_nodata_as, frmt) result(err)
        implicit none
        real(kind=c_float), dimension(:,:), intent(in) :: data
        real(kind=c_float), dimension(6), intent(in) :: gt
        character(*), intent(in) :: prj
        character(*), intent(in) :: filename
        real(kind=c_double), intent(in) :: save_nodata_as
        character(*), intent(in) :: frmt

        integer(kind=c_int) :: err ! CPLErr
        type(gdaldataseth) :: ds
        integer(kind=c_int) :: m, n
        m = size(data,1)
        n = size(data,2)

        ds = Create(filename, n, m, 1, GDT_Float32)
        err = SetGeoTransform(ds, gt)
        err = SetProjection(ds, prj)
        err = SetNodata(ds, save_nodata_as)
        err = WriteArray(ds, data)
        call Close(ds)
    
    end function

end module