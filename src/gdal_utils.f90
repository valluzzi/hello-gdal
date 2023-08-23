module gdal_utils

    use gdal
    use gdal_numpy
    implicit none

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


   


    

end module