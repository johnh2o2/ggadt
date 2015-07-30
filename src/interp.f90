module interp
    use constants
    contains

    subroutine get_index(x, xval, i)
        implicit none

        real(dp_real), intent(in) :: x(:), xval
        integer, intent(out)      :: i

        i = int((xval - x(1))*(size(x) - 1)/(x(size(x)) - x(1))) + 1
        
        if (i .eq. 0) then
            i = i + 1
        else if (i .eq. (size(x) - 1)) then
            i = size(x) - 1
        end if 

        if ( i < 1 ) then
            write(stderr,*) "ERROR: (get_index) requested value at x=",xval," however, &
                & this is below the minimum value of x=",x(1)," (i=", i,") out of ",size(x)
            stop 1
        else if ( i > (size(x) - 1) ) then 
            write(stderr,*) "ERROR: (get_index) requested value at x=",xval," however, &
                & this is above the maximum value of x=",x(size(x))," (i=", i,") out of ",size(x)
            stop 1
        end if 
    
    end subroutine get_index


    function linterp(x,y,xval)
        ! Linear interpolation of a function defined as a set of (x_i,y_i) points
        ! evaluated at x=xval.

        implicit none 
        real(dp_real), intent(in) :: x(:), y(:), xval
        real(dp_real) :: linterp
        integer :: i 

        call get_index(x, xval, i)

        linterp = y(i) + ((xval - x(i))/(x(i+1) - x(i))) *( y(i+1) - y(i) )

    end function linterp

    function l2dval(x, y, z, i, j, xval, yval)

        implicit none
        real(dp_real), intent(in)   :: x(:), y(:), z(:,:), xval, yval
        integer, intent(in)         :: i,j
        real(dp_real)               :: l2dval 
        real(dp_real)               :: x1, x2, y1, y2, f11, f12, f21, f22

        x1 = x(i)
        x2 = x(i+1)
        y1 = y(j)
        y2 = y(j+1)
        f11 = z(i, j)
        f12 = z(i, j+1)
        f22 = z(i+1, j+1)
        f21 = z(i+1, j)

        l2dval = (1./((x2 - x1)*(y2 - y1))) * ( f11 * (x2 - xval) * (y2 - yval) + &
                                                   f21 * (xval - x1) * (y2 - yval) + &
                                                   f12 * (x2 - xval) * (yval - y1) + &
                                                   f22 * (xval - x1) * (yval - y1) )
    end function l2dval

    function linterp2d(x, y, z, xval, yval)
        ! (2d) Linear interpolation of a function defined as a set of (x_i, x_j, y_i, y_j) points
        ! evaluated at (x, y)=(xval, yval).

        implicit none 
        real(dp_real), intent(in)   :: x(:), y(:), z(:,:), xval, yval
        real(dp_real)               :: linterp2d
        integer                     :: i,j
        

        call get_index(x, xval, i)
        call get_index(y, yval, j)

        linterp2d = l2dval(x, y, z, i, j, xval, yval)

    end function linterp2d
    function linterp2d_complex(x, y, z, xval, yval)
        ! (2d) Linear interpolation of a (complex) function defined as a set of (x_i, x_j, y_i, y_j) points
        ! evaluated at (x, y)=(xval, yval).

        implicit none 
        real(dp_real), intent(in)       :: x(:), y(:), xval, yval
        complex(dp_complex), intent(in) :: z(:,:)

        real(dp_real), dimension(size(x),size(y))  &
                                        :: zr, zi

        complex(dp_complex)             :: linterp2d_complex
        integer                         :: i,j

        do i=1,size(x)
            do j=1, size(y)
                zr(i,j) = REAL(z(i,j))
                zi(i,j) = IMAG(z(i,j))
            end do
        end do

        linterp2d_complex = CMPLX( linterp2d(x, y, zr, xval, yval), linterp2d(x, y, zi, xval, yval) )

    end function linterp2d_complex

end module interp