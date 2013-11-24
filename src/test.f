program test
	use, intrinsic :: iso_c_binding

	complex(C_DOUBLE_COMPLEX) :: x, y
	real :: z

	z = 1
	x = (0,1)

	y = (1,1)

	print *, x+y+z
	print *, x*y*z
end program test