module gdal_c_bind
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

    INTERFACE
        FUNCTION gdalrasterio(hrband, erwflag, ndsxoff, ndsyoff, ndsxsize, ndsysize, pbuffer, nbxsize, nbysize, ebdatatype, &
        npixelspace, nlinespace) BIND(C,name='GDALRasterIO')
            !!DEC$ ATTRIBUTES STDCALL :: GDALRasterIO
            IMPORT

            TYPE(gdalrasterbandh),VALUE :: hrband
            INTEGER(kind=c_int),VALUE :: erwflag ! GDALRWFlag
            INTEGER(kind=c_int),VALUE :: ndsxoff
            INTEGER(kind=c_int),VALUE :: ndsyoff
            INTEGER(kind=c_int),VALUE :: ndsxsize
            INTEGER(kind=c_int),VALUE :: ndsysize
            TYPE(c_ptr),VALUE :: pbuffer ! void*
            INTEGER(kind=c_int),VALUE :: nbxsize
            INTEGER(kind=c_int),VALUE :: nbysize
            INTEGER(kind=c_int),VALUE :: ebdatatype ! GDALDataType
            INTEGER(kind=c_int),VALUE :: npixelspace
            INTEGER(kind=c_int),VALUE :: nlinespace
            INTEGER(kind=c_int) :: gdalrasterio ! CPLErr
        END FUNCTION gdalrasterio
    END INTERFACE

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
        FUNCTION GDALGetRasterDataType(hband) BIND(C,name='GDALGetRasterDataType')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterDataType
            TYPE(gdalrasterbandh),VALUE :: hband
            INTEGER(kind=c_int) :: gdalgetrasterdatatype ! GDALDataType
        END FUNCTION GDALGetRasterDataType
    END INTERFACE

    INTERFACE
        FUNCTION GDALGetRasterNoDataValue(hband, pbsuccess) BIND(C,name='GDALGetRasterNoDataValue')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterNoDataValue
            TYPE(gdalrasterbandh),VALUE :: hband
            INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
            REAL(kind=c_double) :: GDALGetRasterNoDataValue
        END FUNCTION GDALGetRasterNoDataValue
    END INTERFACE

    INTERFACE
        FUNCTION GDALSetRasterNoDataValue(hband, dfvalue) BIND(C,name='GDALSetRasterNoDataValue')
            import
            !!DEC$ ATTRIBUTES STDCALL :: GDALSetRasterNoDataValue
            TYPE(gdalrasterbandh),VALUE :: hband
            REAL(kind=c_double),VALUE :: dfvalue
            INTEGER(kind=c_int) :: GDALSetRasterNoDataValue ! CPLErr
        END FUNCTION GDALSetRasterNoDataValue
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

    INTERFACE
        FUNCTION gdalgetrasterminimum(hband, pbsuccess) BIND(C,name='GDALGetRasterMinimum')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterMinimum
            TYPE(gdalrasterbandh),VALUE :: hband
            INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
            REAL(kind=c_double) :: gdalgetrasterminimum
        END FUNCTION gdalgetrasterminimum
    END INTERFACE

    INTERFACE
        FUNCTION gdalgetrastermaximum(hband, pbsuccess) BIND(C,name='GDALGetRasterMaximum')
            IMPORT
            !!DEC$ ATTRIBUTES STDCALL :: GDALGetRasterMaximum
            TYPE(gdalrasterbandh),VALUE :: hband
            INTEGER(kind=c_int),INTENT(inout) :: pbsuccess
            REAL(kind=c_double) :: gdalgetrastermaximum
        END FUNCTION gdalgetrastermaximum
    END INTERFACE

end module