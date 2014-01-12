module sphere

	use, intrinsic :: iso_c_binding
	use common_mod
	

	contains

	function chord_sphere(pos,pos_o,r)
		implicit none
		real, dimension(3), intent(in) :: pos, pos_o
		real, intent(in) :: r 
		real :: l
		real :: chord_sphere
		l = r*r - (pos(1)-pos_o(1))**2  - (pos(2)-pos_o(2))**2
		if ((l .lt. 0) .or. (l .eq. 0)) then
			chord_sphere = 0
		else
			chord_sphere = 2*sqrt(l)
		end if 
	end function chord_sphere

	function phi_sphere(x,y,z,k)
		implicit none
		real, intent(in) :: x,y,z,k
		real :: r, l, gr_a
		complex(c_double_complex) :: phi_sphere
		gr_a = sqrt(sum(grain_a*grain_a))
		r = sqrt(x*x + y*y)
		l = sqrt(gr_a*gr_a - r*r)
		phi_sphere = (0,0)

		if (r < gr_a) then 
			if (z < l .and. z > -l) then 
				phi_sphere = k*delm*(z+l)
			else if (z > l) then
				phi_sphere = k*delm*(2*l)
			else
				phi_sphere = (0,0)
			end if
		end if
	end function phi_sphere

	function shadow_sphere(x,y,z,k)
		implicit none
		real, intent(in) :: x,y,z,k
		complex(c_double_complex) :: shadow_sphere
		shadow_sphere = 1-exp( (0.0,1.0)*phi_sphere(x,y,z,k) )
		
	end function shadow_sphere

end module sphere