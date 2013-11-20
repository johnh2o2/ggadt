! 11/11/2013--John Hoffman
!=========================
! This will be:
! (1) a test of the programming, since we can directly compare to Mie Theory
! (2) a future module that will be used for collections of spheres.

program spheres
	use, intrinsic :: iso_c_binding
	real :: xmin, xmax, ymin, ymax, zmin, zmax, dx, dy, dz, k, Ephot, adust, PI
	real, dimension(2048) :: x, y, z, kx, ky, thetax, thetay, scatterx, scattery
	double complex, dimension(2048,2048) :: sh, ftsh
	integer :: i, j

	adust = 0.2 ! micrometers
	Ephot = 1.0 ! keV

	PI = 3.14159
	k = (2*PI/1.239842)*1000*Ephot*adust 

	xmin = -50*adust
	ymin = -50*adust
	zmin = -50*adust
	xmax = 50*adust
	ymax = 50*adust
	zmax = 50*adust
	! set x, y, and z arrays
	dx = (xmax-xmin)/(SIZE(x)-1)
	dy = (ymax-ymin)/(SIZE(y)-1)
	dz = (zmax-zmin)/(SIZE(z)-1)

	do i=1,SIZE(x)
		x(i) = xmin + (i-1)*dx
	end do
	do i=1,SIZE(y)
		y(i) = ymin + (i-1)*dy
	end do
	do i=1,SIZE(z)
		z(i) = zmin + (i-1)*dz
	end do


	! set shadow function
	do i=1,size(x,1)
		do j=1,size(y,1)
			sh(i,j) = shadow(x(i),y(j),zmax,zmin)
		end do
	end do

	ftsh = fft(sh,x,y)
	!print *,ftsh
	kx = get_k(x)
	ky = get_k(y)

	do i=1,SIZE(kx)
		thetax(i) = ASIN(kx(i)/k)
		thetay(i) = ASIN(ky(i)/k)
	end do

	! Now output results from phi = 0 (using only nhat = cos(theta)zhat + sin(theta)xhat)
	print *,"#theta (radians)   [dscat/domega (x-axis)] [dscat/domega (y-axis)]"
	do i=1,size(x)
		scatterx(i) = (abs(ftsh(i,1))**2)/(k**2)
		scattery(i) = (abs(ftsh(1,i))**2)/(k**2)
		print *,thetax(i),' ',scatterx(i),' ',scattery(i)
	end do
contains

function index_of_refraction(x,y,z)
implicit none
 	complex(C_DOUBLE_COMPLEX) :: m, index_of_refraction
 	real :: r,r_sph,l, mreal, mimag
 	real, intent(in) :: x,y,z

 	mreal = 1-7.152*(10**(-4))
 	mimag = 1.887*(10**(-4))

 	m = CMPLX(mreal, mimag)
 	r_sph = 1.0*adust
 	r = sqrt(x**2 + y**2)
 	index_of_refraction = 0
 	if (r > r_sph) then
 		index_of_refraction = 0
 	else 
 		l = 2*sqrt(r_sph**2 - r**2)
 		if ( (z < -(l/2)) .OR. (z > (l/2))) then
 			index_of_refraction = 0
 		else
 			index_of_refraction = m
 		end if 
 	end if
end function index_of_refraction

function phi(x,y,z,zmin)
	implicit none
	integer :: N, i
	real, intent(in) :: x,y,z,zmin
	real :: dZ, zt
	complex(C_DOUBLE_COMPLEX) :: phi
	phi = (0,0)
	N = 300
	dZ = (z - zmin)/N
	do i=0,N
		zt = zmin + i*dZ
		phi = phi + dZ*index_of_refraction(x,y,zt)
	end do 
end function phi

function shadow(x,y,z,zmin)
	implicit none
	real, intent(in) :: x,y,z,zmin
	complex(C_DOUBLE_COMPLEX) :: shadow 
	shadow = 1-exp((0.0,1.0)*phi(x,y,z,zmin))
end function shadow

function get_k(x)
	implicit none
	real, intent(in) :: x(:)
	integer :: n, i, j 
	
	real :: L, PI
	real, dimension(SIZE(x)) :: get_k

	PI = 3.14159
	n = SIZE(x)
    L = x(n) - x(1)
    do i=1,n
    	get_k(i) = (PI*i)/L
    end do
end function get_k

function FFT(f,x,y)
	use, intrinsic :: iso_c_binding
       include 'fftw3.f03'
    real, intent(in) :: x(:), y(:)
    complex(C_DOUBLE_COMPLEX), intent(inout) :: f(:,:)
    type(C_PTR) :: plan
    complex(C_DOUBLE_COMPLEX), dimension(SIZE(x),SIZE(y)) :: FFT
    integer :: nx, ny, i, j

    nx = SIZE(x)
    ny = SIZE(y)
    plan = fftw_plan_dft_2d(ny, nx, f ,FFT, FFTW_FORWARD,FFTW_ESTIMATE)
    call fftw_execute_dft(plan, f, FFT)
    call fftw_destroy_plan(plan)
end function FFT
end program spheres

