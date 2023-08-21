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

    interface
        subroutine GDALAllRegister() bind(C, name='GDALAllRegister')
        import
        !!DEC$ ATTRIBUTES STDCALL :: GDALAllRegister
        end subroutine 
    end interface

    interface
        subroutine GDALDestroyDriverManager() bind(C, name='GDALDestroyDriverManager')
        IMPORT
        !!DEC$ ATTRIBUTES STDCALL :: GDALDestroyDriverManager
        end subroutine GDALDestroyDriverManager
    end interface

    type,bind(C) :: GDALDatasetH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type

    type,bind(C) :: GDALDriverH
        private
        type(c_ptr) :: ptr = C_NULL_PTR
    end type 

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
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALOpen
            CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
            integer(kind=c_int),VALUE :: eaccess ! GDALAccess
            type(gdaldataseth) :: GDALOpen
        end function
    end interface

    interface 
        SUBROUTINE GDALClose(hds) bind(C, name='GDALClose')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALClose
            type(gdaldataseth),VALUE :: hds
        end SUBROUTINE
    end interface

    interface
        function GDALCreate(hdriver, pszfilename, nxsize, nysize, nbands, ebandtype, papszoptions) bind(C, name='GDALCreate')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALCreate
            type(gdaldriverh),VALUE :: hdriver
            CHARACTER(kind=c_char),INTENT(in) :: pszfilename(*)
            integer(kind=c_int),VALUE :: nxsize
            integer(kind=c_int),VALUE :: nysize
            integer(kind=c_int),VALUE :: nbands
            integer(kind=c_int),VALUE :: ebandtype ! GDALDatatype
            type(c_ptr),VALUE :: papszoptions ! type(c_ptr_ptr)
            type(gdaldataseth) :: GDALCreate
        end function GDALCreate
    end interface

    interface
        function GDALGetRasterXSize(hdataset) bind(C, name='GDALGetRasterXSize')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterXSize
            type(gdaldataseth),VALUE :: hdataset
            integer(kind=c_int) :: GDALGetRasterXSize
        end function 
    end interface

    interface
        function GDALGetRasterYSize(hdataset) bind(C,name='GDALGetRasterYSize')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterYSize
            type(gdaldataseth),VALUE :: hdataset
            integer(kind=c_int) :: GDALGetRasterYSize
        end function 
    end interface

    interface
        function GDALCreateCopy(hdriver, pszfilename, hsrcds, bstrict, papszoptions, pfnprogress, pprogressdata) bind(C, name='GDALCreateCopy')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALCreateCopy
            type(gdaldriverh),VALUE :: hdriver
            character(kind=c_char),intent(in) :: pszfilename(*)
            type(gdaldataseth),VALUE :: hsrcds
            integer(kind=c_int),VALUE :: bstrict
            type(c_ptr),VALUE :: papszoptions ! type(c_ptr_ptr)
            type(c_ptr),VALUE :: pfnprogress
            type(c_ptr),VALUE :: pprogressdata ! void*
            type(gdaldataseth) :: GDALCreateCopy
        end function 
    end interface

    contains

    function GetDriverByName(name)
        implicit none
        character(*), intent(in) :: name
        type(gdaldriverh) :: GetDriverByName
        GetDriverByName = GDALGetDriverByName(name//char(0))
    end function GetDriverByName


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


    function Open(filename, access)
        implicit none
        character(*), intent(in) :: filename
        integer, intent(in) :: access
        type(gdaldataseth) :: Open
        Open = gdalopen(filename//char(0), access)
    end function Open


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

    subroutine Close(ds)
        implicit none
        type(gdaldataseth), intent(in) :: ds
        call gdalclose(ds)
    end subroutine 

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


    function GTiff2COG(filesrc, fileout) 
        implicit none
        character(*), intent(in) :: filesrc
        character(*), intent(in) :: fileout
        type(gdaldataseth) :: ds
        type(gdaldataseth) :: GTiff2COG

        ds = Open(filesrc, GA_ReadOnly)

        GTiff2COG = GDALCreateCopy(
            GetDriverByName("COG"), 
            fileout//char(0), 
            ds, 
            0, 
            c_ptr_ptr_getobject(c_ptr_ptr_new((/'COMPRESS=DEFLATE'/))), 
            C_NULL_PTR, 
            C_NULL_PTR)

    end function



end module