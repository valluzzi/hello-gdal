module gdal
    use,intrinsic :: iso_c_binding
    implicit none
    interface
        subroutine GDALAllRegister() bind(C,name='GDALAllRegister')
        IMPORT
        !!DEC$ ATTRIBUTES STDCALL :: GDALAllRegister
        end subroutine GDALAllRegister
    end interface
end module