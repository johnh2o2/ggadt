module fftw
    
    use, intrinsic :: iso_c_binding
       include '/fftw3.f03'
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
        case('estimate')
            mode = fftw_estimate
        case('patient')
            mode = fftw_patient
        case('exhaustive')
            mode = fftw_exhaustive
        case('measure')
            mode = fftw_measure
        case default
            mode = fftw_estimate 
        end select
    end subroutine set_optimization_mode



    function fft(f,x,y)
        
        real, intent(in) :: x(:), y(:)
        complex(c_double_complex), intent(inout) :: f(:,:)
        complex(c_double_complex), dimension(size(x),size(y)) :: fft
        integer :: nx, ny, i, j
        integer :: error


        nx = size(x)
        ny = size(y)
        ! OMP threaderror = fftw_init_threads()
        ! OMP if (threaderror == 0) then
        ! OMP     write (0,*) "------------------------------"
        ! OMP     write (0,*) "Error initializing multiple threads. Program will attempt to proceed"
        ! OMP     write (0,*) "using 1 thread."
        ! OMP     write (0,*) "------------------------------"
        ! OMP     numthreads = 1
        ! OMP else
        ! OMP     numthreads = omp_get_max_threads()
        ! OMP endif
        ! OMP call fftw_plan_with_nthreads(numthreads)
        if (first_time) then
            error = fftw_import_wisdom_from_filename(trim(adjustl(plan_filename)))
            if (error == 0) then
                plan = fftw_plan_dft_2d(ny, nx, f ,fft, fftw_backward,mode)
                error = fftw_export_wisdom_to_filename(plan_filename)
            else
                plan = fftw_plan_dft_2d(ny, nx, f ,fft, fftw_backward,mode)
            end if
            first_time = .false.
        end if
        !write (0,*) "about to do fft"
        !plan = fftw_plan_dft_2d(ny, nx, f ,fft, fftw_backward,fftw_patient)
        call fftw_execute_dft(plan, f, fft)
    end function fft

end module fftw
