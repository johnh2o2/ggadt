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
        ! Performs the integral necessary to calculate the scattering matrix
        ! for a spherical grain in ADT (see Draine & Allaf-Akbari 2006)
        implicit none
        real(kind=dp_real), intent(in) :: x
        complex(kind=dp_complex), intent(in) :: rho
        complex(kind=dp_complex) :: func1, val1, val2
        real(kind=dp_real) :: umin, umax, du, u
        integer :: i 

        umin = 0.0
        umax = PI/2.0
        du = (umax - umin)/nu_sphere

        func1 = 0.0
        val1 = 0.0
        val2 = 0.0

        do i=0,(nu_sphere-1)
            u = umin + i*du
            val1 = val1 + du*exp(-(0.0,1.0)*rho*sin(u))*BesJ0(x*cos(u))*sin(u)*cos(u)
            val2 = val2 + du*BesJ0(x*cos(u))*sin(u)*cos(u)
        end do

        func1 = val2 - val1

    end function func1

    function scatter_sphere(k,aeff,theta,delta_m)
        ! Calculates the differential scattering amplitude for a spherical grain
        !   inputs:
        !       k        wavenumber of incident photon (in 1/microns)
        !       aeff     radius of grain
        !       theta    scattering angle at which to perform the calculation
        !       delta_m  m-1 where m is the index of refraction of the material
        !   outputs:
        !       dsigma_scat/dOmega(theta)
        implicit none
        real(kind=dp_real), intent(in) :: k,aeff, theta
        complex(kind=dp_complex), intent(in) :: delta_m

        complex(kind=dp_complex) :: sc, rho
        real(kind=dp_real) :: x, scatter_sphere

        rho = 2*k*aeff*delta_m
        x = k*aeff*theta
        sc = k*k*aeff*aeff*func1(x,rho)

        scatter_sphere = REAL(ABS(sc),kind=dp_real)*REAL(ABS(sc),kind=dp_real)/(k*k)
        

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