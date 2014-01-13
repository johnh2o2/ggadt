module ellipsoid

    use, intrinsic :: iso_c_binding
    use common_mod

    implicit none
    
    contains

    function chord_ellipsoid(x,y,r)
        implicit none
        real :: chord_ellipsoid
        real :: d, temp
        real, dimension(3) :: c
        real, dimension(2) :: pos
        real, intent(in), dimension(3,3) :: r
        real, intent(in) :: x, y
        integer :: i,j,k,m
        pos(1) = x
        pos(2) = y
        c(1) = 0
        c(2) = 0
        c(3) = 0
        do i=1,3
            temp = grain_a(i)**(-2.0)
            c(1) = c(1) + temp*r(i,3)**2
            do j=1,2
                c(2) = c(2) + 2*temp*r(i,3)*r(i,j)*pos(j)
                do k=1,2
                    c(3) = c(3) + temp*r(i,j)*r(i,k)*pos(j)*pos(k)
                end do
            end do
        end do 

        
        c(3) = c(3) - 1 
        d    = c(2)*c(2)-4*c(1)*c(3)
    

        if ((d .lt. 0.0) .or. (c(1) .eq. 0.0)) then
            chord_ellipsoid = 0.0
        else
            chord_ellipsoid = sqrt(d)/c(1)
        end if
    end function chord_ellipsoid

    function phi_ellipsoid(x,y,k,r)
        implicit none
        real, intent(in) :: x,y,k
        real, dimension(3,3), intent(in) :: r
        complex(c_double_complex) :: phi_ellipsoid
        phi_ellipsoid = k*delm*chord_ellipsoid(x,y,r)
        
    end function phi_ellipsoid

    function shadow_ellipsoid(x,y,k,r)
        implicit none
        real, intent(in) :: x,y,k
        real, dimension(3,3), intent(in) :: r
        complex(c_double_complex) :: shadow_ellipsoid
        shadow_ellipsoid = 1.0-exp( (0.0,1.0)*phi_ellipsoid(x,y,k,r) )
        
    end function shadow_ellipsoid

end module ellipsoid