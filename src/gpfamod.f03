module gpfa
	use, intrinsic :: iso_c_binding
    use gpfa_raw
contains

function fft(f,x,y)

	real, intent(in) :: x(:), y(:)
    complex(c_double_complex), intent(inout) :: f(:,:)
    complex(c_double_complex), dimension(size(x),size(y)) :: fft
    integer :: i, j, Nx, Ny, JUMP, LOT, INC, N, ISIGN
    real, dimension(size(x)) :: trigs_x
    real, dimension(size(y)) :: trigs_y
    real, dimension(2*size(x)*size(y)) :: f_gpfa

    ISIGN = +1
  
    Nx = size(x)
    Ny = size(y)

    call SETGPFA(trigs_x, Nx)
    call SETGPFA(trigs_y, Ny)

    do i=1,N
    	do j=1,N
    		f_gpfa(1+2*((i-1)+(j-1)*Ny)) = REAL(f(i,j))
    		f_gpfa(2+2*((i-1)+(j-1)*Ny)) = IMAG(f(i,j))
    	end do 
    end do

    write(0,*) "About to do rows"
    ! Do rows
    INC=2
    JUMP=2*Nx
    LOT=Ny

    call GPFA_FFT(f_gpfa(1),f_gpfa(2),trigs_y,INC,JUMP,Ny,LOT,ISIGN)
    
    write(0,*) "About to do columns"
    ! Do columns
    INC=2*Nx
    JUMP=2
    LOT=Nx

    call GPFA_FFT(f_gpfa(1),f_gpfa(2),trigs_x,INC,JUMP,Nx,LOT,ISIGN)

    write(0,*) "About to output the data"
    ! write results to output vector.
    do i=1,Nx
    	do j=1,Ny
    		fft(i,j) = CMPLX(f_gpfa(1+2*((i-1)+(j-1)*Ny)),f_gpfa(2+2*((i-1)+(j-1)*Ny)))
    	end do 
    end do

    write(0,*) "All done here."

end function fft 

end module gpfa