program foo
use omp_lib
include '/opt/local/include/fftw3.f'
integer ok, i
integer*8 plan
double complex data(1024)
do i=1,size(data)
	data(i) = exp(-(3*real(i)/real(size(data)))*cos(cos(real(i))))
end do
call dfftw_init_threads(ok)
call dfftw_plan_with_nthreads(4)
call dfftw_plan_dft_1d(plan, 1024, data, data, -1, 0)
call dfftw_execute(plan)
call dfftw_destroy_plan(plan)
call dfftw_cleanup_threads
end program foo