module gpfa
	use, intrinsic :: iso_c_binding
       include 'gpfa.f90'
contains
function fft(f,x,y)
	real, intent(in) :: x(:), y(:)
    complex(c_double_complex), intent(inout) :: f(:,:)
    complex(c_double_complex), dimension(size(x),size(y)) :: fft

    

end function fft 

end module gpfa