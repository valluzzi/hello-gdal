module strings

    implicit none

    interface iif
        module procedure iif_logical, iif_integer, iif_float, iif_string
    end interface

    contains


    !---------------------------------------------------------------------------
    !  iif_logical
    !
    !  Returns a logical value based on the value of the first argument.
    !  If the first argument is true, the second argument is returned.
    !---------------------------------------------------------------------------
    function iif_logical(cond, a, b) result(res)
        logical, intent(in)::cond
        logical, intent(in)::a
        logical, intent(in)::b
        logical::res
        if (cond) then
            res = a 
        else 
            res = b 
        end if
    end function

    !---------------------------------------------------------------------------
    !  iif_integer
    !
    !  Returns a logical value based on the value of the first argument.
    !  If the first argument is true, the second argument is returned.
    !---------------------------------------------------------------------------
    function iif_integer(cond, a, b) result(res)
        logical, intent(in)::cond
        integer, intent(in)::a
        integer, intent(in)::b
        integer::res
        if (cond) then
            res = a 
        else 
            res = b 
        end if
    end function


    !---------------------------------------------------------------------------
    !  iif_float
    !  Returns a logical value based on the value of the first argument.
    !  If the first argument is true, the second argument is returned.
    !---------------------------------------------------------------------------
    function iif_float(cond, a, b) result(res)
        logical, intent(in)::cond
        real, intent(in)::a
        real, intent(in)::b
        real::res
        if (cond) then
            res = a 
        else 
            res = b 
        end if
    end function

    !---------------------------------------------------------------------------
    !  iif_string
    !
    !  Returns a logical value based on the value of the first argument.
    !  If the first argument is true, the second argument is returned.
    !---------------------------------------------------------------------------
    function iif_string(cond, a, b) result(res)
        logical, intent(in)::cond
        character(*), intent(in)::a
        character(*), intent(in)::b
        character(:), allocatable::res
        if (cond) then
            res = a 
        else 
            res = b 
        end if
    end function

end module