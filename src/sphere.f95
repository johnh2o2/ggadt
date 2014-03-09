module sphere

    !use, intrinsic :: iso_c_binding
    use constants

    contains

    function chord_sphere(pos,pos_o,r)
        implicit none
        real(kind=dp_real), dimension(3), intent(in) :: pos, pos_o
        real(kind=dp_real), intent(in) :: r 
        real(kind=dp_real) :: l
        real(kind=dp_real) :: chord_sphere
        l = r*r - (pos(1)-pos_o(1))**2  - (pos(2)-pos_o(2))**2
        if ((l .lt. 0) .or. (l .eq. 0)) then
            chord_sphere = 0.0
        else
            chord_sphere = 2*sqrt(l)
        end if 
    end function chord_sphere

    function phi_sphere(x,y,k,gr_a,delta_m)
        implicit none
        real(kind=dp_real), intent(in) :: x,y,k,gr_a
        complex(kind=dp_complex), intent(in) :: delta_m
        real(kind=dp_real) :: r, l
        complex(kind=dp_complex) :: phi_sphere
        !gr_a = sqrt(sum(grain_a*grain_a))
        r = sqrt(x*x + y*y)
        l = sqrt(gr_a*gr_a - r*r)
        phi_sphere = (0,0)

        if (r < gr_a) then 
                phi_sphere = k*delta_m*(2*l)
        end if
    end function phi_sphere

    function shadow_sphere(x,y,k,gr_a,delta_m)
        implicit none
        real(kind=dp_real), intent(in) :: x,y,k,gr_a
        complex(kind=dp_complex), intent(in) :: delta_m
        complex(kind=dp_complex) :: shadow_sphere
        shadow_sphere = 1-exp( (0.0,1.0)*phi_sphere(x,y,k,gr_a,delta_m) )
        
    end function shadow_sphere

end module sphere