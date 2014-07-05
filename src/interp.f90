module interp
    use constants
    contains

    function linterp(x,y,xval)
        ! Linear interpolation of a function defined as a set of (x_i,y_i) points
        ! evaluated at x=xval.


        implicit none 
        real(dp_real), intent(in) :: x(:), y(:), xval
        real(dp_real) :: linterp
        integer :: i 
        real(dp_real) :: a,b

        if ( xval < x(1) ) then
            write(stderr,*) "ERROR: (linterp) requested value at x=",xval," however, &
                & this is below the minimum value of x=",x(1)
            stop 
        else if ( xval > x(size(x)) ) then 
            write(stderr,*) "ERROR: (linterp) requested value at x=",xval," however, &
                & this is above the maximum value of x=",x(size(x))
            stop 
        end if 


        i=1
        do while( (x(i) < xval) )
            i = i + 1
        end do 

    
        a = xval - x(i-1)
        b = x(i) - x(i-1)

        linterp = y(i-1) + (a/b)*( y(i) - y(i-1) )


    end function linterp

end module interp