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

    function func1(x,rho)
        implicit none
        real(kind=dp_real), intent(in) :: x
        complex(kind=dp_complex), intent(in) :: rho
        real(kind=dp_real) :: func1, umin, umax, du, u
        integer :: i 

        umin = 0
        umax = PI/2.0
        du = (umax - umin)/nu_sphere

        func1 = 0.0

        do i=0,(nu_sphere-1)
            u = umin + i*du
            func1 = func1 + du*(1 - exp(-(0.0,1.0)*rho*sin(u)))*BesJ0(x*cos(u))*sin(u)*cos(u)
        end do

    end function func1

    function scatter_sphere(k,aeff,theta,delta_m)
        implicit none
        real(kind=dp_real), intent(in) :: k,aeff, theta
        complex(kind=dp_complex), intent(in) :: delta_m

        complex(kind=dp_complex) :: scatter_sphere, rho
        real(kind=dp_real) :: x

        rho = 2*k*aeff*delta_m
        x = k*aeff*theta
        scatter_sphere = k*k*aeff*aeff*func1(x,rho)
        

    end function scatter_sphere

    subroutine q_sphere(k,aeff,delta_m,qabs,qscat,qext)
        implicit none
        real(kind=dp_real), intent(in) :: k,aeff
        real(kind=dp_real), intent(out) :: qabs,qscat,qext
        complex(kind=dp_complex), intent(in) :: delta_m

        real(kind=dp_real) :: x, rho1, rho2, beta, arho
        complex(kind=dp_complex) :: rho

        rho = 2*k*aeff*delta_m
        arho = REAL(ABS(rho),kind=dp_real)
        rho1 = REAL(rho,kind=dp_real)
        if (rho1 == arho) then
            rho2 = 0
        else
            rho2 = sqrt(arho*arho - rho1*rho1)
        end if
        beta = atan(rho2/rho1)

        qext = 2 + (4./(arho**2))*(cos(2*beta) - exp(-rho2)*(cos(rho1 - 2*beta) - arho*sin(rho1 - beta)))
        qabs = 1 + exp(-2*rho2)/rho2 + (exp(-2*rho2) - 1)/(2*rho2*rho2)
        qscat = qext - qabs

    end subroutine q_sphere




end module sphere