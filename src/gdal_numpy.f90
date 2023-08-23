module gdal_numpy

    use iso_c_binding
    use fortranc
    use gdal
    implicit none

    interface GDAL2Numpy
        module procedure    GDAL2Numpy_Float32, &
                            GDAL2Numpy_Float64
    end interface

    contains


    !--------------------------------------------------------------
    ! Function:   GDAL2Numpy_Float32
    ! Purpose:    Read a GDAL dataset into a numpy array
    ! Inputs:     filename - source file name
    !             band - band number to read
    !             load_nodata_as - value to load as nodata
    ! Returns:    numpy array
    !--------------------------------------------------------------
    function GDAL2Numpy_Float32(filename, data, gt, prj, band, load_nodata_as) result(err)
        implicit none
        integer(kind = c_int), parameter :: KIND = c_float
        
        character(*), intent(in) :: filename
        real(kind = c_double), optional, intent(out) :: gt(6)
        character(:), allocatable, optional, intent(out) :: prj
        integer(kind = c_int), optional, intent(in) :: band
        real(kind = KIND),  optional, intent(in) :: load_nodata_as
        real(kind = KIND),  allocatable, intent(inout) :: data(:,:)
        
        real(kind = KIND) :: nodata
        type(gdaldataseth) :: ds
        integer(kind=c_int) :: err

        print *, "GDAL2Numpy_Float32"
        ds = Open(filename, GA_ReadOnly)
        if (.true.) then      
            err = ReadArray(gdalgetrasterband(ds, iif(present(band),band,1)), data, GetRasterXSize(ds), GetRasterYSize(ds))

            !Replace nodata values with user-defined value
            nodata = real(GetNodata(ds), kind = KIND)

            !Get the geotransform
            if (present(gt)) then
                gt = GetGeoTransform(ds)
            end if

            !get the projection
            if (present(prj)) then
                prj = GetProjection(ds)
            end if
            
            if (err==0.and.present(load_nodata_as).and.load_nodata_as/=nodata) then
                where(data.eq.nodata) data = load_nodata_as
            end if
            call Close(ds)
        endif
    end function


    !--------------------------------------------------------------
    ! Function:   GDAL2Numpy_Float64
    ! Purpose:    Read a GDAL dataset into a numpy array
    ! Inputs:     filename - source file name
    !             band - band number to read
    !             load_nodata_as - value to load as nodata
    ! Returns:    numpy array
    !--------------------------------------------------------------
    function GDAL2Numpy_Float64(filename, data, gt, prj, band, load_nodata_as) result(err)
        implicit none
        integer(kind = c_int), parameter :: KIND = c_double
        
        character(*), intent(in) :: filename
        real(kind = c_double), optional, intent(out) :: gt(6)
        character(:), allocatable, optional, intent(out) :: prj
        integer(kind = c_int), optional, intent(in) :: band
        real(kind = KIND),  optional, intent(in) :: load_nodata_as
        real(kind = KIND),  allocatable, intent(inout) :: data(:,:)
        
        real(kind = KIND) :: nodata
        type(gdaldataseth) :: ds
        integer(kind=c_int) :: err

        print *, "GDAL2Numpy_Float64"

        ds = Open(filename, GA_ReadOnly)
        if (.true.) then      
            err = ReadArray(gdalgetrasterband(ds, iif(present(band),band,1)), data, GetRasterXSize(ds), GetRasterYSize(ds))

            !Replace nodata values with user-defined value
            nodata = real(GetNodata(ds), kind = KIND)

            !Get the geotransform
            if (present(gt)) then
                gt = GetGeoTransform(ds)
            end if

            !get the projection
            if (present(prj)) then
                prj = GetProjection(ds)
            end if
            
            if (err==0.and.present(load_nodata_as).and.load_nodata_as/=nodata) then
                where(data.eq.nodata) data = load_nodata_as
            end if
            call Close(ds)
        endif
    end function

    

    


    





end module