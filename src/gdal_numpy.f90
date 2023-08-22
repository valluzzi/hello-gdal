module gdal_numpy

    use gdal
    implicit none

    interface GDAL2Numpy
        
        module procedure GDAL2Numpy_Float32
    end interface

    contains

    !--------------------------------------------------------------
    ! Function:   GDAL2Numpy_Float32
    ! Purpose:    Read a GDAL dataset into a numpy array
    ! Inputs:     filename - source file name
    !             band - band number to read
    !             load_nodata_as - value to load as nodata
    ! Returns:    GDAL2Numpy_Float32 - numpy array
    !--------------------------------------------------------------
    function GDAL2Numpy_Float32(filename, band, load_nodata_as) result(data)
        implicit none
        character(*), intent(in) :: filename
        integer(kind=c_int), optional, intent(in) :: band
        real(kind=c_float), optional, intent(in) :: load_nodata_as
        real(kind=c_float), allocatable :: data(:,:)
        real(kind=c_float) :: nodata
        type(gdaldataseth) :: ds
        integer(kind=c_int) :: err

        ds = Open(filename, GA_ReadOnly)
        nodata = GetNodata(ds)
        
        ! Read the data
        err = ReadArray(ds, band, data)

        ! Replace nodata values with user-defined value
        if (present(load_nodata_as).and.load_nodata_as.ne.nodata) then
            where(data.eq.nodata) data = load_nodata_as
        end if
        
        call Close(ds)

    end function




end module