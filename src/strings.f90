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


    !!
    !! triml 
    !!
    function triml(text, prefix) result(res)
        character(*),intent(in) :: text
        character(*),intent(in), optional :: prefix
        character(:),allocatable::c,res 
        integer::j,b,e,l
        c = ' '
        if(present(prefix)) c = prefix
        e = len(text)
        l = len(c)
        b = 1

        do j=1,e-l+1,1
            if (text(j:j+l-1)==c) then
               b = b + l
            else
               exit
            end if
        end do
        res = text(b:e)
    end function
    
    !!
    !! trimr 
    !!
    function trimr(text, suffix) result(res)
        character(*),intent(in) :: text
        character(*),intent(in), optional :: suffix
        character(:),allocatable::c,res 
        integer::j,e,l
        c = ' '
        if(present(suffix)) c=suffix 
        e = len(text)
        l = len(c)

        do j=e-l+1,1,-1
            if (text(j:j+l-1)==c) then
               e = e - l
            else
               exit
            end if
        end do
        res = text(1:e)
    end function
    !!
    !! strip 
    !!
    function strip(text, prefix, suffix) result(res)
        character(*),intent(in) :: text
        character(*),intent(in), optional :: prefix,suffix
        character(:),allocatable::c,s,res 
        if(present(prefix)) then 
            s = prefix
        else 
            s = ' ' 
        end if 
        if(present(suffix)) then
            c = suffix
        else 
            c = s
        end if 
        res = triml(trimr(text,c),s)
    end function

     !!
    !!  mcount - count max length of occurrence
    !!
    function mcount(text, c) result(res)
        character(*),intent(in) :: text
        character(*),intent(in) :: c
        integer::res(2)
        integer::j, ccount, wmax, wcount
        ccount=0;wcount=0; wmax=0
        
        do j = 1,len(text) 
            if (text(j:j+len(c)-1)==c)then
                wmax = max(wcount, wmax)
                wcount=0
                ccount = ccount+1
            else
                wcount = wcount+1
                wmax = max(wcount, wmax)
            endif
        end do
        
        res = (/ccount, wmax/)
    end function

    !!
    !!  split
    !!
    function split(text, sep, trim, remove_empty) result(res)
        character(*),intent(in) :: text
        character(*),intent(in) :: sep
        logical,optional,intent(in) :: trim, remove_empty
        logical::trim_item = .false., remove_empty_item = .false.
        integer::arr(2)
        integer::i, j, n, w, lentxt, lenc, m=0
        character(:),allocatable::word 
        character(:),allocatable::res(:), temp(:)
        
        if (present(trim)) trim_item = trim
        if (present(remove_empty)) remove_empty_item = remove_empty
        i=1;arr=mcount(text,sep);lenc = len(sep);lentxt=len(text)
        n = arr(1)+1;w = arr(2)
        allocate(character(w)::res(n))
        word = ""
        do j=1,lentxt
            if (text(j:j+lenc-1)==sep .or. j==lentxt) then
                if (j==lentxt.and.text(j:j+lenc-1).ne.sep ) word = word//text(j:j)
                if (trim_item) word = strip(word)
                res(i) = word
                i=i+1
                m = m + iif(remove_empty_item.and.len(word)==0,0,1)
                word=""
            else
                word = word//text(j:j)
            endif
        end do
        
        if (m<n) then
            allocate(character(w)::temp(n))
            temp(:) = res(:)
            deallocate(res)
            allocate(character(w)::res(m))
            j=0
            do j=1,n
                if (len(temp(j))>0) then
                    res(i)= temp(j)
                    i=i+1
                end if
            end do
            deallocate(temp)
        endif
    end function

end module