module gdal
    use,intrinsic :: iso_c_binding
    use fortranc
    implicit none

    ! Hand made symbolic constant definitions
    ! GDALDatatype
    integer(kind=c_int),parameter :: GDT_Unknown = 0 !< constant defining the native data type of a dataset data: unknown
    integer(kind=c_int),parameter :: GDT_Byte = 1 !< byte, in Fortran it can be declared as \a integer(kind=C_INT_8_T)
    integer(kind=c_int),parameter :: GDT_UInt16 = 2 !< unsigned 16 bit integer, it should be avoided in Fortran and translated into a signed type
    integer(kind=c_int),parameter :: GDT_Int16 = 3 !< signed 16 bit integer, in Fortran it can be declared as \a integer(kind=C_INT_16_T)
    integer(kind=c_int),parameter :: GDT_UInt32 = 4 !< unsigned 32 bit integer, it should be avoided in Fortran and translated into a signed type
    integer(kind=c_int),parameter :: GDT_Int32 = 5  !< signed 32 bit integer, in Fortran it can be declared as \a integer(kind=C_INT)
    integer(kind=c_int),parameter :: GDT_Float32 = 6 !< 32 bit floating point real, in Fortran it can be declared as \a REAL(kind=C_FLOAT)
    integer(kind=c_int),parameter :: GDT_Float64 = 7 !< 64 bit floating point real, in Fortran it can be declared as \a REAL(kind=C_DOUBLE)
    integer(kind=c_int),parameter :: GDT_CInt16 = 8 !< 16 bit integer complex, it should be avoided in Fortran and translated into a floating point type
    integer(kind=c_int),parameter :: GDT_CInt32 = 9 !< 32 bit integer complex, it should be avoided in Fortran and translated into a floating point type
    integer(kind=c_int),parameter :: GDT_CFloat32 = 10 !< 32 bit (*2) floating point complex, in Fortran it can be declared as \a COMPLEX(kind=C_FLOAT_COMPLEX)
    integer(kind=c_int),parameter :: GDT_CFloat64 = 11 !< 64 bit (*2) floating point complex, in Fortran it can be declared as \a COMPLEX(kind=C_DOUBLE_COMPLEX)
    integer(kind=c_int),parameter :: GDT_typeCount = 12

    ! GDALAccess
    integer(kind=c_int),parameter :: GA_ReadOnly = 0 !< access type for opening a file: read only
    integer(kind=c_int),parameter :: GA_Update = 1 !< update access type for opening a file: read and write

    ! GDALRWFlag
    INTEGER(kind=c_int),PARAMETER :: GF_Read = 0 !< operation to be performed on a dataset: read
    INTEGER(kind=c_int),PARAMETER :: GF_Write = 1 !< write

    interface
        subroutine GDALAllRegister() bind(C, name='GDALAllRegister')
        import
        !!DEC$ ATTRIBUTES STDCALL :: GDALAllRegister
        end subroutine 
    end interface

    interface
        subroutine GDALDestroyDriverManager() bind(C, name='GDALDestroyDriverManager')
        import
        !!DEC$ ATTRIBUTES STDCALL :: GDALDestroyDriverManager
        end subroutine GDALDestroyDriverManager
    end interface

    type, bind(C) :: GDALDatasetH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type GDALDatasetH

    type, bind(C) :: GDALMajorObjectH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type GDALMajorObjectH

    type, bind(C) :: GDALDriverH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type GDALDriverH

    type, bind(C) :: GDALRasterAttributeTableH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type GDALRasterAttributeTableH

    type, bind(C) :: GDALColorTableH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type GDALColorTableH

    type, bind(C) :: GDALRasterBandH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type GDALRasterBandH 

    interface
        function GDALGetDriverByName(pszname) bind(C, name='GDALGetDriverByName')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetDriverByName
            character(kind=c_char), intent(in) :: pszname(*)
            type(gdaldriverh) :: GDALGetDriverByName
        end function 
    end interface


    interface
        function GDALOpen(pszfilename, eaccess) bind(C,name='GDALOpen')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALOpen
            character(kind=c_char),intent(in) :: pszfilename(*)
            integer(kind=c_int),value :: eaccess ! GDALAccess
            type(gdaldataseth) :: GDALOpen
        end function
    end interface

    interface
        function GDALGetRasterBand(hds, nbandid) BIND(C,name='GDALGetRasterBand')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterBand
            type(gdaldataseth),VALUE :: hds
            integer(kind=c_int),VALUE :: nbandid
            type(gdalrasterbandh) :: gdalgetrasterband
        end function 
    end interface

    interface 
        subroutine GDALClose(hds) bind(C, name='GDALClose')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALClose
            type(gdaldataseth),value :: hds
        end subroutine
    end interface

    interface
        function GDALCreate(hdriver, pszfilename, nxsize, nysize, nbands, ebandtype, papszoptions) bind(C, name='GDALCreate')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALCreate
            type(gdaldriverh),value :: hdriver
            CHARACTER(kind=c_char),intent(in) :: pszfilename(*)
            integer(kind=c_int),value :: nxsize
            integer(kind=c_int),value :: nysize
            integer(kind=c_int),value :: nbands
            integer(kind=c_int),value :: ebandtype ! GDALDatatype
            type(c_ptr),value :: papszoptions ! type(c_ptr_ptr)
            type(gdaldataseth) :: GDALCreate
        end function GDALCreate
    end interface

    interface
        function GDALGetRasterXSize(hdataset) bind(C, name='GDALGetRasterXSize')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterXSize
            type(gdaldataseth),value :: hdataset
            integer(kind=c_int) :: GDALGetRasterXSize
        end function 
    end interface

    interface
        function GDALGetRasterYSize(hdataset) bind(C,name='GDALGetRasterYSize')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterYSize
            type(gdaldataseth),value :: hdataset
            integer(kind=c_int) :: GDALGetRasterYSize
        end function 
    end interface

    interface
        function GDALCreateCopy(hdriver, pszfilename, hsrcds, bstrict, papszoptions, pfnprogress, pprogressdata) bind(C, name='GDALCreateCopy')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALCreateCopy
            type(gdaldriverh),value :: hdriver
            character(kind=c_char),intent(in) :: pszfilename(*)
            type(gdaldataseth),value :: hsrcds
            integer(kind=c_int),value :: bstrict
            type(c_ptr),value :: papszoptions ! type(c_ptr_ptr)
            type(c_ptr),value :: pfnprogress
            type(c_ptr),value :: pprogressdata ! void*
            type(gdaldataseth) :: GDALCreateCopy
        end function 
    end interface

    interface
        function GDALDatasetRasterIO(hds, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
            nbandcount, panbandcount, npixelspace, nlinespace, nbandspace) bind(C, name='GDALDatasetRasterIO')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALDatasetRasterIO
            type(gdaldataseth),value :: hds
            integer(kind=c_int),value :: erwflag ! GDALRWFlag
            integer(kind=c_int),value :: ndsxoff
            integer(kind=c_int),value :: ndsyoff
            integer(kind=c_int),value :: ndsxsize
            integer(kind=c_int),value :: ndsysize
            type(c_ptr),value :: pbuffer ! void*
            integer(kind=c_int),value :: nbxsize
            integer(kind=c_int),value :: nbysize
            integer(kind=c_int),value :: ebdatatype ! GDALDatatype
            integer(kind=c_int),value :: nbandcount
            integer(kind=c_int) :: panbandcount(*)
            integer(kind=c_int),value :: npixelspace
            integer(kind=c_int),value :: nlinespace
            integer(kind=c_int),value :: nbandspace
            integer(kind=c_int) :: GDALDatasetRasterIO ! CPLErr
        end function
    end interface

    INTERFACE
        FUNCTION gdalgetrasternodatavalue(hband, pbsuccess) BIND(C,name='GDALGetRasterNoDataValue')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterNoDataValue
            TYPE(gdalrasterbandh),VALUE :: hband
            INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
            REAL(kind=c_double) :: gdalgetrasternodatavalue
        END FUNCTION gdalgetrasternodatavalue
    END INTERFACE

    INTERFACE
        FUNCTION gdalsetrasternodatavalue(hband, dfvalue) BIND(C,name='GDALSetRasterNoDataValue')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALSetRasterNoDataValue
            TYPE(gdalrasterbandh),VALUE :: hband
            REAL(kind=c_double),VALUE :: dfvalue
            INTEGER(kind=c_int) :: gdalsetrasternodatavalue ! CPLErr
        END FUNCTION gdalsetrasternodatavalue
    END INTERFACE

    INTERFACE
        FUNCTION gdalsetprojection(hds, pszprojection) BIND(C,name='GDALSetProjection')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALSetProjection
            TYPE(gdaldataseth),VALUE :: hds
            CHARACTER(kind=c_char),INTENT(in) :: pszprojection(*)
            INTEGER(kind=c_int) :: gdalsetprojection ! CPLErr
        END FUNCTION gdalsetprojection
    END INTERFACE

    INTERFACE
        FUNCTION gdalgetgeotransform(hds, padftransform) BIND(C,name='GDALGetGeoTransform')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetGeoTransform
            TYPE(gdaldataseth),VALUE :: hds
            REAL(kind=c_double) :: padftransform(*)
            INTEGER(kind=c_int) :: gdalgetgeotransform ! CPLErr
        END FUNCTION gdalgetgeotransform
    END INTERFACE

    INTERFACE
        FUNCTION gdalsetgeotransform(hds, padftransform) BIND(C,name='GDALSetGeoTransform')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALSetGeoTransform
            TYPE(gdaldataseth),VALUE :: hds
            REAL(kind=c_double) :: padftransform(*)
            INTEGER(kind=c_int) :: gdalsetgeotransform ! CPLErr
        END FUNCTION gdalsetgeotransform
    END INTERFACE

    interface SetGeoTransform
        module procedure SetGeoTransform_f64, SetGeoTransform_f32
    end interface

    contains

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


    function Numpy2GDAL_float32(data, gt, prj, filename, save_nodata_as, frmt) result(err)
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