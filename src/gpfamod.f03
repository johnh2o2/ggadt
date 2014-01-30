module gpfa
	use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env

    use gpfa_raw
    use common_mod


contains

function fft(f,x,y)

	real, intent(in) :: x(:), y(:)
    complex(c_double_complex), intent(in) :: f(:,:)
    complex(c_double_complex), dimension(size(x),size(y)) :: fft
    integer :: i, j, Nx, Ny, JUMP, LOT, INC, ISIGN
    !parameter (ntrig = 16384)
    integer :: npoints
    real, dimension(2*size(x)) :: trigs_x
    real, dimension(2*size(y)) :: trigs_y
    real :: f_gpfa(2*size(x)*size(y))
    
    ISIGN = 1
  
    Nx = size(x)
    Ny = size(y)


    call SETGPFA(trigs_x, Nx)
    call SETGPFA(trigs_y, Ny)

    !stop

    do i=1,Nx
    	do j=1,Ny
    		f_gpfa(1+2*((i-1)+(j-1)*Ny)) = REAL(f(i,j))
    		f_gpfa(2+2*((i-1)+(j-1)*Ny)) = IMAG(f(i,j))
    	end do 
    end do

    !write(0,*) "GPFA: About to do rows"
    ! Do rows
    INC=2
    JUMP=2*Nx
    LOT=Ny

    call GPFA_FFT(f_gpfa(1),f_gpfa(2),trigs_y,INC,JUMP,Ny,LOT,ISIGN)
    
    !write(0,*) "GPFA: About to do columns"
    ! Do columns
    INC=2*Nx
    JUMP=2
    LOT=Nx

    call GPFA_FFT(f_gpfa(1),f_gpfa(2),trigs_x,INC,JUMP,Nx,LOT,ISIGN)

    !write(0,*) "GPFA: About to output the data"
    ! write results to output vector.
    
    !npoints = 0
    do i=1,Nx
    	do j=1,Ny
    		fft(i,j) = CMPLX(f_gpfa(1+2*((i-1)+(j-1)*Ny)),f_gpfa(2+2*((i-1)+(j-1)*Ny)))
            !if (abs(fft(i,j)) > 0) npoints = 1 + npoints
    	end do
    end do

    !write(0,*) "All done here. (npoints = ",npoints," of ",Nx*Ny,")"

end function fft 

end module gpfa