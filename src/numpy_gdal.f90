module numpy_gdal

    use iso_c_binding
    use fortranc
    use strings
    use gdal
    implicit none

    interface Numpy2GDAL
        module procedure Numpy2GDAL_Float32
        
    end interface

    contains


    function Numpy2GDAL_Float32(data, gt, prj, filename, save_nodata_as, metadata, frmt) result(err)
        implicit none
        integer(kind=c_int), parameter :: KIND = c_float 
        real(kind=KIND), dimension(:,:), intent(in) :: data
        real(kind=c_double), dimension(6),   intent(in) :: gt
        character(*), intent(in) :: prj
        character(*), intent(in) :: filename
        real(kind=KIND),  optional, intent(in) :: save_nodata_as
        character(*), optional, intent(in) :: frmt
        !metadata is a list of key-value pairs
        character(*), optional, intent(in) :: metadata(:)
        character(:), allocatable :: key, value, item
        character(:), allocatable :: arr(:)

        integer(kind=c_int) :: err ! CPLErr
        type(gdaldataseth) :: ds
        integer(kind=c_int) :: m, n, j
        m = size(data,1)
        n = size(data,2)

        ds = Create(filename, n, m, 1, GDT_Float32)
        err = SetGeoTransform(ds, gt)
        err = SetProjection(ds, prj)
        if (present(save_nodata_as)) then
            err = SetNodata(ds, save_nodata_as)
        end if

        !set metadata
        !loop over metadata and set each item
        if (present(metadata)) then
            do j = 1, size(metadata)
                item = metadata(j)
                print *, item
                !split the string into key and valueB
                arr = split( item , "=", .true., .false.)
                err = SetMetadataItem(ds, arr(1), arr(2))
            end do
        end if

        err = WriteArray(ds, data)
        call Close(ds)
    
    end function


end module