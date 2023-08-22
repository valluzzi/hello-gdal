module gdal
    use strings
    use gdal_c_bind
    implicit none

    type, abstract :: Raster
        real(kind=c_double) :: gt(6)
        character(:), allocatable :: proj
    end type


    type, extends(Raster) :: ByteRaster
        integer(kind=1), dimension(:,:), allocatable :: data
    end type

    type, extends(Raster) :: Float32Raster
        real(kind=c_float), dimension(:,:), allocatable :: data
    end type

    type, extends(Raster) :: Float64Raster
        real(kind=c_double), dimension(:,:), allocatable :: data
    end type


    interface SetGeoTransform
        module procedure SetGeoTransform_f64, SetGeoTransform_f32
    end interface

    ! interface ReadArray
    !     module procedure ReadArray_Float32
    ! end interface

    contains

    !--------------------------------------------------------------
    ! Subroutine: AllRegister
    ! Purpose:    Register all GDAL drivers
    !--------------------------------------------------------------    
    subroutine AllRegister()
        call GDALAllRegister()
    end subroutine

    !--------------------------------------------------------------
    ! Function:   GetDriverByName
    ! Purpose:    Get a GDAL driver by name
    ! Inputs:     name - name of the driver
    ! Returns:    GetDriverByName - GDAL driver handle
    !--------------------------------------------------------------
    function GetDriverByName(name)
        implicit none
        character(*), intent(in) :: name
        type(gdaldriverh) :: GetDriverByName
        GetDriverByName = GDALGetDriverByName(name//char(0))
    end function GetDriverByName

    !--------------------------------------------------------------
    ! Function:   GetDriverByExt
    ! Purpose:    Get a GDAL driver by file extension
    ! Inputs:     filename - name of the file
    ! Returns:    GetDriverByExt - GDAL driver handle
    !--------------------------------------------------------------
    function GetDriverByExt(filename)
        implicit none
        character(*), intent(in) :: filename
        character(:), allocatable :: name
        character(:), allocatable :: ext
        type(gdaldriverh) :: GetDriverByExt

        integer :: idx
        idx = index(filename,'.') +1
        ext = trim(filename(idx:))
        
        if (ext.eq."tif") then
            name = "GTiff"
        else 
            name = ext
        end if

        GetDriverByExt = GDALGetDriverByName(name//char(0))
        deallocate(name)
        deallocate(ext)
    end function GetDriverByExt

    !--------------------------------------------------------------
    ! Function:   Open
    ! Purpose:    Open a GDAL dataset
    ! Inputs:     filename - name of the file to open
    !             access - access type
    ! Returns:    Open - GDAL dataset handle
    !--------------------------------------------------------------
    function Open(filename, access)
        implicit none
        character(*), intent(in) :: filename
        integer, intent(in) :: access
        type(gdaldataseth) :: Open
        Open = gdalopen(filename//char(0), access)
    end function Open


    !--------------------------------------------------------------
    ! Function:   Create
    ! Purpose:    Create a new dataset
    ! Inputs:     filename - name of the file to create
    !             n - number of rows
    !             m - number of columns
    !             nbands - number of bands
    !             bandtype - data type of the bands
    !             options - creation options
    ! Returns:    Create - GDAL dataset handle
    !--------------------------------------------------------------
    function Create(filename, n, m, nbands, bandtype, options)
        !!
        !! Create a new dataset FORTRAN
        !!
        implicit none
        character(*), intent(in) :: filename
        integer, intent(in) :: n, m
        integer, optional :: bandtype
        integer, optional :: nbands
        character(len=16), dimension(:), optional, intent(in) :: options
        integer :: dtype, nb
        character(len=16), dimension(:), allocatable :: CO
        type(gdaldriverh) :: driver
        type(gdaldataseth) :: Create

        if (.not.present(bandtype)) then
            dtype = GDT_Float32
        else 
            dtype = bandtype
        end if
        
        if (.not.present(nbands)) then
            nb = 1
        else
            nb = nbands
        end if

        if (.not.present(options)) then
            CO = (/'BIGTIFF=YES     ','TILED=YES       ', 'BLOCKXSIZE=256  ', 'BLOCKYSIZE=256  '/)
        else 
            CO = options
        end if

        ! default driver by file extension
        driver = GetDriverByExt(filename)
        Create  = GDALCreate(driver, filename//char(0), n, m, nb, dtype, c_ptr_ptr_getobject(c_ptr_ptr_new(CO)))
        deallocate(CO)
    end function Create

    !--------------------------------------------------------------
    ! Function:   GetRasterXSize
    ! Purpose:    Get the number of columns of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    ! Returns:    GetRasterXSize - number of columns
    !--------------------------------------------------------------
    function GetRasterXSize(ds)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        integer :: GetRasterXSize
        GetRasterXSize = GDALGetRasterXSize(ds)
    end function

    !--------------------------------------------------------------
    ! Function:   GetRasterYSize
    ! Purpose:    Get the number of rows of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    ! Returns:    GetRasterXSize - number of rows
    !--------------------------------------------------------------
    function GetRasterYSize(ds)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        integer :: GetRasterYSize
        GetRasterYSize = GDALGetRasterYSize(ds)
    end function


    !--------------------------------------------------------------
    ! Function:   GetRasterMinimum
    ! Purpose:    Get the minimum value of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             band - band number   
    ! Returns:    GetRasterMinimum - minimum value
    !--------------------------------------------------------------
    function GetRasterMinimum(ds, band)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        integer(kind=c_int), optional, intent(in) :: band
        integer(kind=c_int) :: pbsuccess
        real(kind=c_double) :: GetRasterMinimum 
        integer(kind=c_int) :: b
        
        b = iif(.not.present(band), 1, band)

        GetRasterMinimum = GDALGetRasterMinimum(gdalgetrasterband(ds, b), pbsuccess)
    end function


    !--------------------------------------------------------------
    ! Function:   GetRasterMaximum
    ! Purpose:    Get the maximum value of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             band - band number   
    ! Returns:    GetRasterMaximum - maximum value
    !--------------------------------------------------------------
    function GetRasterMaximum(ds, band)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        integer(kind=c_int), optional, intent(in) :: band
        integer(kind=c_int) :: pbsuccess
        real(kind=c_double) :: GetRasterMaximum 
        integer(kind=c_int) :: b
        
        b = iif(.not.present(band), 1, band)
        
        GetRasterMaximum = GDALGetRasterMaximum(gdalgetrasterband(ds, b), pbsuccess)
    end function



    !--------------------------------------------------------------
    ! Function:   Close
    ! Purpose:    Close a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !--------------------------------------------------------------
    subroutine Close(ds)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        call gdalclose(ds)
    end subroutine 

    !--------------------------------------------------------------
    ! Function:   CreateCopy
    ! Purpose:    Create a copy of a GDAL dataset
    ! Inputs:     driver - GDAL driver handle
    !             filesrc - source file name
    !             fileout - output file name
    !             options - creation options
    ! Returns:    Copy - GDAL dataset handle
    !--------------------------------------------------------------
    function Copy(filesrc, fileout, options) 
        implicit none
        character(*), intent(in) :: filesrc
        character(*), intent(in) :: fileout
        character(len=16), dimension(:), optional, intent(in) :: options
        character(len=16), dimension(:), allocatable :: CO
        type(gdaldriverh) :: driver
        type(gdaldataseth) :: ds
        type(gdaldataseth) :: Copy

        ds = Open(filesrc, GA_ReadOnly)

        driver = GetDriverByExt(fileout)
        if (.not.present(options)) then
            CO = (/'BIGTIFF=YES     ','TILED=YES       ', 'BLOCKXSIZE=256  ', 'BLOCKYSIZE=256  '/)
        else 
            CO = options
        end if
        Copy = GDALCreateCopy(driver, fileout//char(0), ds, 0, c_ptr_ptr_getobject(c_ptr_ptr_new(CO)), C_NULL_PTR, C_NULL_PTR)

    end function

    !--------------------------------------------------------------
    ! Function:   GetProjection
    ! Purpose:    Get the projection of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    ! Returns:    GetProjection - projection string
    !--------------------------------------------------------------
    function GetDataType(ds, band) result(dtype)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        integer(kind=c_int), optional, intent(in) :: band
        integer :: dtype
        type(gdalrasterbandh) :: b
        if (.not.present(band)) then
            b = gdalgetrasterband(ds, 1)
        else 
            b = gdalgetrasterband(ds, band)
        end if  
        dtype = int(GDALGetRasterDataType(b))
    end function

    !--------------------------------------------------------------
    ! Function:   GetDataTypeName
    ! Purpose:    Get the data type name of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    ! Returns:    GetDataTypeName - data type name
    !--------------------------------------------------------------
    function GetDataTypeName(ds, band)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        integer(kind=c_int), optional, intent(in) :: band
        integer(kind=c_int) :: dtype
        character(len=16) :: GetDataTypeName

        dtype = GetDataType(ds, band)
        select case (dtype)
            case (GDT_Byte)
                GetDataTypeName = "Byte"
            case (GDT_UInt16)
                GetDataTypeName = "UInt16"
            case (GDT_Int16)
                GetDataTypeName = "Int16"
            case (GDT_UInt32)
                GetDataTypeName = "UInt32"
            case (GDT_Int32)
                GetDataTypeName = "Int32"
            case (GDT_Float32)
                GetDataTypeName = "Float32"
            case (GDT_Float64)
                GetDataTypeName = "Float64"
            case (GDT_CInt16)
                GetDataTypeName = "CInt16"
            case (GDT_CInt32)
                GetDataTypeName = "CInt32"
            case (GDT_CFloat32)
                GetDataTypeName = "CFloat32"
            case (GDT_CFloat64)
                GetDataTypeName = "CFloat64"
            case default
                GetDataTypeName = "Unknown"
        end select
    end function

    !--------------------------------------------------------------
    ! Function:   GetNodata
    ! Purpose:    Get the nodata value of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    ! Returns:    GetNodata - nodata value
    !--------------------------------------------------------------
    function GetNodata(ds) result(nodata)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        real(kind=c_double) :: nodata
        type(gdalrasterbandh) :: band
        integer(kind=c_int) :: pbsuccess
        band = gdalgetrasterband(ds, 1)
        nodata = GDALGetRasterNoDataValue(band, pbsuccess)
    end function

    !--------------------------------------------------------------
    ! Function:   SetNodata
    ! Purpose:    Set the nodata value of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             nodata - nodata value
    ! Returns:    err - error code
    !--------------------------------------------------------------
    function SetNodata(ds, nodata) result(err)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        real(kind=c_double), intent(in) :: nodata
        integer :: err ! CPLErr
        type(gdalrasterbandh) :: band
        band = gdalgetrasterband(ds, 1)
        err = GDALSetRasterNoDataValue(band, nodata)
    end function

    !--------------------------------------------------------------
    ! Function:   SetGeoTransform
    ! Purpose:    Set the geotransform of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             gt - geotransform array
    ! Returns:    err - error code
    !--------------------------------------------------------------
    function SetGeoTransform_f64(ds, gt) result(err)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        real(kind=c_double), dimension(6), intent(in) :: gt
        integer(kind=c_int) :: err
        err = GDALSetGeoTransform(ds, gt)
    end function

    function SetGeoTransform_f32(ds, gt) result(err)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        real(kind=c_float), dimension(6), intent(in) :: gt
        integer(kind=c_int) :: err, j
        real(kind=c_double), dimension(6) :: gt_dbl
        do j = 1, 6
            gt_dbl(j) = real(gt(j), kind=c_double)
        end do
        err = GDALSetGeoTransform(ds, gt_dbl)
    end function

    !--------------------------------------------------------------
    ! Function:   SetProjection
    ! Purpose:    Set the projection of a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             proj - projection string
    ! Returns:    err - error code
    !--------------------------------------------------------------
    function SetProjection(ds, proj) result(err)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        character(*), intent(in) :: proj
        integer(kind=c_int) :: err
        err = GDALSetProjection(ds, proj//char(0))
    end function


   

    !--------------------------------------------------------------
    ! Function:   ReadArray_Float32
    ! Purpose:    Read a 2D array of data values from a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             band - band number
    ! Returns:    data - 2D array of data values
    !--------------------------------------------------------------
    function ReadArray_Float32(ds, band) result(data)
        implicit none
        type(gdaldataseth),value :: ds  
        integer(kind=c_int), optional, intent(in) :: band
        real(kind=c_float),  allocatable:: data(:,:)
        integer(kind=c_int) :: m, n, b, dtype
        integer(kind=c_int) :: err ! CPLErr

        if (.not.present(band)) then
            b = 1
        else 
            b = band
        end if

        m = GDALGetRasterYSize(ds)
        n = GDALGetRasterXSize(ds)
        
        dtype = GetDataType(ds, b)
        
        if (.not.allocated(data)) then
            allocate(data(n,m))
        end if
        err = gdalrasterio( gdalgetrasterband(ds, b) , GF_Read, 0, 0, n, m, c_loc(data), n, m, dtype, 0,0)           
        
    end function

    function ReadArray(ds, band) result(res)
        implicit none
        type(gdaldataseth),value :: ds
        integer(kind=c_int), optional,intent(in) :: band
        class(Raster), allocatable :: res
        type(Float32Raster) :: res_f32
        type(Float64Raster) :: res_f64
        integer(kind=c_int) :: m, n, b, dtype, err

        m = GDALGetRasterYSize(ds)
        n = GDALGetRasterXSize(ds)
        
        b = 1
        dtype = GetDataType(ds, b)


        select case (dtype)
            case (GDT_Float32)
                allocate(res_f32%data(n,m))
                err = gdalrasterio( gdalgetrasterband(ds, b) , GF_Read, 0, 0, n, m, c_loc(res_f32%data), n, m, dtype, 0,0)           
                res = res_f32
            case (GDT_Float64)
                allocate(res_f64%data(n,m))
                err = gdalrasterio( gdalgetrasterband(ds, b) , GF_Read, 0, 0, n, m, c_loc(res_f64%data), n, m, dtype, 0,0)           
                res = res_f64
        end select            
        

    end function
  
    

    !--------------------------------------------------------------
    ! Function:   WriteArray
    ! Purpose:    Write a 2D array of data values to a GDAL dataset
    ! Inputs:     ds - GDAL dataset handle
    !             data - 2D array of data values
    !             m - number of rows
    !             n - number of columns
    ! Returns:    err - error code
    !--------------------------------------------------------------
    function WriteArray(ds, data) result(err)
        implicit none
        type(gdaldataseth),value :: ds
        real(kind=c_float), dimension(:,:), intent(in) :: data
        integer(kind=c_int) :: m, n
        integer(kind=c_int) :: err ! CPLErr

        m = size(data,1)
        n = size(data,2)
        !gdaldatasetrasterio(hds, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
        !   nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) bind(C, name='GDALDatasetRasterIO')
        err = gdaldatasetrasterio(ds, GF_Write, 0, 0, n, m, c_loc(data), n, m, GDT_Float32, 1, [1], 0, 0, 0)
    end function


end module