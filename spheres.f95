! 11/11/2013--John Hoffman
!=========================
! This will be:
! (1) a test of the programming, since we can directly compare to Mie Theory
! (2) a future module that will be used for collections of spheres.

program spheres
	implicit none
	print "Hello world!"

end program spheres


function index_of_refraction(x,y,z)
implicit none
 	real :: r, index_of_refraction, m, R
 	m = (1.00001,0.00001)
 	R = 1
 	r = sqrt(x**2 + y**2)
 	if (r > R) then
 		index_of_refraction = 0
 	else 
 		L = 2*sqrt(R**2 - r**2)
 		if ( (z < -(L/2)) .OR. (z > (L/2))) then
 			index_of_refraction = 0
 		else
 			index_of_refraction = m
 		end if 
 	end if
end function index_of_refraction

function phi(x,y,z,zmin)
	implicit none
	integer :: N, i
	real :: Z, phi, dZ
	phi = 0
	N = 100
	dZ = (z - zmin)/N
	do i=0,N
		Z = zmin + i*dZ
		phi = phi + dZ*index_of_refraction(x,y,Z)
	end do 
end function phi

function shadow(x,y,z,zmin)
	implicit none
	real :: shadow 
	shadow = 1-exp((0.0,1.0)*phi(x,y,z,zmin))
end function shadow

function get_k(x)
	implicit none
	integer :: n, i, j 
	nx = SIZE(x,1)
	real :: L
	real, dimension(n) :: get_k
    L = x(n) - x(1)
    do i=1,n
    	get_k(i) = (PI*i)/L
    end do
end function get_k

function FFT(f,x,y)
	implicit none
	use, intrinsic :: iso_c_binding
       include 'fftw3.f03'

    type(C_PTR) :: plan
    integer :: nx, ny 
    nx = SIZE(x,1)
    ny = SIZE(y,1)

    complex(C_DOUBLE_COMPLEX), dimension(nx,ny) :: FFT

    plan = fftw_plan_dft_2d(ny, nx, f ,FFT, FFTW_FORWARD,FFTW_ESTIMATE)
    call fftw_execute_dft(plan, f, FFT)
    call fftw_destroy_plan(plan)
end function FFT

