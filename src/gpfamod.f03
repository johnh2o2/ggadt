module gpfa
	use, intrinsic :: iso_c_binding
    use gpfa_raw
contains

function fft(f,x,y)

	real, intent(in) :: x(:), y(:)
    complex(c_double_complex), intent(inout) :: f(:,:)
    complex(c_double_complex), dimension(size(x),size(y)) :: fft
    integer :: i, j, N
    real, dimension(size(x)) :: trigs_x
    real, dimension(size(y)) :: trigs_y
    real, dimension(size(x),size(y)) :: f_gpfa_re, f_gpfa_im

  
    call SETGPFA(trigs_x, size(x))
    call SETGPFA(trigs_y, size(y))

    do i=1,N
    	do j=1,N
    		f_gpfa_re(i,j) = REAL(f(i,j))
    		f_gpfa_im(i,j) = IMAG(f(i,j))
    	end do 
    end do

    do i=1,size(x)
    	call GPFA_FFT(f_gpfa_re(i,:),f_gpfa_im(i,:),trigs_y,1,1,1,1,1)
    end do 
    do i=1,size(x)
    	call GPFA_FFT(f_gpfa_re(:,i),f_gpfa_im(:,i),trigs_y,1,1,1,1,1)
    end do 

    do i=1,N
    	do j=1,N
    		fft(i,j) = CMPLX(f_gpfa_re(i,j),f_gpfa_im(i,j))
    	end do 
    end do

end function fft 

end module gpfa