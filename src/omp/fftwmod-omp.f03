module fftw
    use omp_lib
    use, intrinsic :: iso_c_binding
       include '/opt/local/include//fftw3.f03'
    logical :: first_time = .true.
    type(c_ptr) :: plan
    integer ::  mode = fftw_estimate
    character(len=100) :: mode_name
    character(len=100) :: plan_filename

contains

    subroutine set_optimization_mode(mode_name_in)
        character(len=100), intent(in) :: mode_name_in

        mode_name = mode_name_in

        select case (mode_name)
        case('fftw_estimate')
            mode = fftw_estimate
        case('fftw_patient')
            mode = fftw_patient
        case('fftw_exhaustive')
            mode = fftw_exhaustive
        case('fftw_measure')
            mode = fftw_measure
        case default
            mode = fftw_estimate 
        end select
    end subroutine set_optimization_mode

    function fft(f,x,y)
        
        real, intent(in) :: x(:), y(:)
        complex(c_double_complex), intent(inout) :: f(:,:)
        complex(c_double_complex), dimension(size(x),size(y)) :: fft
        integer :: nx, ny, i, j, threaderror, numthreads
        integer :: error


        nx = size(x)
        ny = size(y)
	threaderror = fftw_init_threads()
	if (threaderror == 0) then
		write (0,*) "------------------------------"
		write (0,*) "Error initializing multiple threads. Program will attempt to proceed"
		write (0,*) "using 1 thread."
		write (0,*) "------------------------------"
		numthreads = 1
	else
		numthreads = omp_get_max_threads()
	endif
	!write(0,*) "Number of threads: ",numthreads
	call fftw_plan_with_nthreads(numthreads)
        if (first_time) then
            write(0,*) new_line('a')//"        /"
            write(0,*) " FFTW: | Finding best fft algorithm to use..."
            write(plan_filename,'(a,i0.4,a,i0.4,a,i0.3,a,a)') "/Users/jah5/.ggadt/plans/&
            &plan_nx",nx,"_ny",ny,"_fftw_mode",mode,".plan",char(0)
            error = fftw_import_wisdom_from_filename(trim(adjustl(plan_filename)))
            if (error == 0) then
                write (0,*) "   --> | No previous wisdom detected:"
                write (0,*) "       |"
                write (0,*) "       |  FFTW will search for fastest FFT algorithm (using",trim(adjustl(mode_name)),")."
                write (0,*) "       |  this may take several minutes, depending on your grid size."
                write (0,*) "       |"

                plan = fftw_plan_dft_2d(ny, nx, f ,fft, fftw_backward,mode)
                error = fftw_export_wisdom_to_filename(plan_filename)
                if (error == 0) then
                    write (0,*) "  ***FFTW ERROR: couldn't save plan to ",trim(adjustl(plan_filename))
                else
                    write (0,*) "       |+ Successfully saved plan to ",trim(adjustl(plan_filename))
                endif
            else
                write (0,*) "       | + Found and loaded previous wisdom from '",trim(adjustl(plan_filename)),"'"
                plan = fftw_plan_dft_2d(ny, nx, f ,fft, fftw_backward,mode)
            end if
            write (0,*) "       | Done."
            write (0,*) "       \"
            first_time = .false.
        end if
        !write (0,*) "about to do fft"
        !plan = fftw_plan_dft_2d(ny, nx, f ,fft, fftw_backward,fftw_patient)
        call fftw_execute_dft(plan, f, fft)
    end function fft

end module fftw
