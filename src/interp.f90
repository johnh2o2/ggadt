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

        i = int((xval - x(1))*size(x)/(x(size(x)) - x(1)))

        if ( i < 1 ) then
            write(stderr,*) "ERROR: (linterp) requested value at x=",xval," however, &
                & this is below the minimum value of x=",x(1)
            stop 1
        else if ( i > size(x) ) then 
            write(stderr,*) "ERROR: (linterp) requested value at x=",xval," however, &
                & this is above the maximum value of x=",x(size(x))
            stop 1
        end if 
    
        a = xval - x(i-1)
        b = x(i) - x(i-1)

        linterp = y(i-1) + (a/b)*( y(i) - y(i-1) )


    end function linterp

    function linterp2d(x, y, z, xval, yval)
        ! (2d) Linear interpolation of a function defined as a set of (x_i, x_j, y_i, y_j) points
        ! evaluated at x=xval.


        implicit none 
        real(dp_real), intent(in) :: x(:), y(:), z(:,:), xval, yval
        real(dp_real) :: linterp2d
        integer :: i,j
        real(dp_real) :: x1, x2, y1, y2, f11, f12, f21, f22

        i = int((xval - x(1))*size(x)/(x(size(x)) - x(1)))
        j = int((yval - y(1))*size(y)/(y(size(y)) - y(1)))

        if ( i < 1 ) then
            write(stderr,*) "ERROR: (linterp2d) requested value at x=",xval," however, &
                & this is below the minimum value of x=",x(1)
            stop 1
        else if ( i > size(x) ) then 
            write(stderr,*) "ERROR: (linterp2d) requested value at x=",xval," however, &
                & this is above the maximum value of x=",x( size(x))
            stop 1
        else if ( j < 1 ) then 
            write(stderr,*) "ERROR: (linterp2d) requested value at y=",yval," however, &
                & this is below the minimum value of y=",y(1)
            stop 1
        else if ( j > size(y) ) then 
            write(stderr,*) "ERROR: (linterp2d) requested value at y=",yval," however, &
                & this is above the maximum value of y=",y(size(y))
            stop 1
        end if

        x1 = x(i-1)
        x2 = x(i)
        y1 = x(j-1)
        y2 = x(j)
        f11 = z(i-1, j-1)
        f12 = z(i-1, j)
        f22 = z(i, j)
        f21 = z(i, j-1)

        linterp2d = (1./((x2 - x1)*(y2 - y1))) * ( f11 * (x2 - xval) * (y2 - yval) + &
                                                   f21 * (xval - x1) * (y2 - yval) + &
                                                   f12 * (x2 - xval) * (yval - y1) + &
                                                   f22 * (xval - x1) * (yval - y1) )


    end function linterp

end module interp