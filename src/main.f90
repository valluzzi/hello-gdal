program HelloWorld
    use gdal
    implicit none

    call GDALAllRegister()

    ! This program prints "Hello, World!" to the screen.
    write(*,*) "Hello, my old World!"

end program HelloWorld
