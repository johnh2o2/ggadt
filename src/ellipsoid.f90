module ellipsoid

    !use, intrinsic :: iso_c_binding

    ! TODO: add rotation matrix speedup; for each orientation, set the inverse rotation matrix in here so
    ! you don't have to recalculate it all the time

    ! Also, hardcode the inverse rotation matrix in terms of the rotation angles -- that should be faster to compute.

    ! Reduce the computations by storing constants that are common to a single orientation (there should be 4)
    use constants
    implicit none
    
    real(kind=dp_real), dimension(4) :: q

    contains

    subroutine set_constants(beta,theta,phi,grain_a)
        implicit none 
        real(kind=dp_real), dimension(6) :: cc
        real(kind=dp_real) :: aa
        real(kind=dp_real), dimension(3,3) :: rinv
        real(kind=dp_real), dimension(3), intent(in) :: grain_a
        real(kind=dp_real), intent(in) :: beta,theta,phi
        integer :: i 



        do i=1,6
            cc(i) = 0.0D0
        end do 

        rinv(1,1) = cos(theta)
        rinv(1,2) = cos(phi)*sin(theta)
        rinv(1,3) = sin(phi)*sin(theta)

        rinv(2,1) = -cos(beta)*sin(theta)
        rinv(2,2) = cos(beta)*cos(phi)*cos(theta) - sin(beta)*sin(phi)
        rinv(2,3) = cos(phi)*sin(beta) + cos(beta)*cos(theta)*sin(phi)

        rinv(3,1) = sin(beta)*sin(theta)
        rinv(3,2) = -cos(phi)*cos(theta)*sin(beta) - cos(beta)*sin(phi)
        rinv(3,3) = cos(beta)*cos(phi) - cos(theta)*sin(beta)*sin(phi)

        ! TODO: validate the above formula (and use it to validate the rotation matrix function)

        do i=1,3
            aa = grain_a(i)**2
            cc(1) = cc(1) + rinv(i,3)**2/aa
            cc(2) = cc(2) + rinv(i,3)*rinv(i,1)/aa 
            cc(3) = cc(3) + rinv(i,3)*rinv(i,2)/aa 
            cc(4) = cc(4) + rinv(i,1)**2/aa
            cc(5) = cc(5) + rinv(i,1)*rinv(i,2)/aa 
            cc(6) = cc(6) + rinv(i,2)**2/aa

        end do 
        cc(2) = 2*cc(2)
        cc(3) = 2*cc(3)
        cc(5) = 2*cc(5)


        q(1) = 4.0D0/cc(1)
        q(2) = (cc(2)/cc(1))**2 - 4*cc(4)/cc(1)
        q(3) = (cc(3)/cc(1))**2 - 4*cc(6)/cc(1)
        q(4) = 2*cc(2)*cc(3)/(cc(1)**2) - 4*cc(5)/cc(1)

    end subroutine set_constants


    function chord_ellipsoid(x,y,grain_a)
        implicit none
        real(kind=dp_real) :: chord_ellipsoid
        real(kind=dp_real) :: d
        real(kind=dp_real),  dimension(3), intent(in) :: grain_a
        real(kind=dp_real),  intent(in) :: x, y
        

        d    = q(1) + q(2)*x*x + q(3)*y*y + q(4)*x*y
    

        if (d .lt. 0.0) then
            chord_ellipsoid = 0.0D0
        else
            chord_ellipsoid = sqrt(d)
        end if
    end function chord_ellipsoid

    function phi_ellipsoid(x,y,k,grain_a,delm)
        implicit none
        real(kind=dp_real),  intent(in) :: x,y,k
        complex(kind=dp_complex), intent(in) :: delm
        real(kind=dp_real),  dimension(3), intent(in) :: grain_a

        
        
        complex(kind=dp_complex) :: phi_ellipsoid

        phi_ellipsoid = k*delm*chord_ellipsoid(x,y,grain_a)
        
    end function phi_ellipsoid

    function shadow_ellipsoid(x,y,k,grain_a,delm)
        implicit none
        real(kind=dp_real),  intent(in) :: x,y,k
        
        complex(kind=dp_complex), intent(in) :: delm
        real(kind=dp_real),  dimension(3), intent(in) :: grain_a
        complex(kind=dp_complex) :: shadow_ellipsoid

        shadow_ellipsoid = 1.0D0 -exp( (0.0D0,1.0D0)*phi_ellipsoid(x,y,k,grain_a,delm) )
        
    end function shadow_ellipsoid

end module ellipsoid