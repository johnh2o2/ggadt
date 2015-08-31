module fftw
    use omp_lib

    ! SEVERAL NOTES!
    ! ==============
    ! Note: WISDOM is not implemented here. Need to add --enable-fftw-wisdom flag in the configure.ac file, and
    !       test that the f77_wisdom.f works.
    !
    ! Note: There are several annoying hurdles for implementing openmp version of FFTW3 library.
    !   1) lfftw3_omp is NOT installed with standard port install fftw3.
    !   2) using lfftw3_threads and incluing -pthreads in the FCFLAGS generates a failure
    !       when the dfftw_init_threads() is called. No idea why this is. In addition, even if
    !       we can get fftw3 to cooperate with openmp/pthreads, there are different FCFLAGS 
    !       commands for pthreads depending on the compiler, and the only m4 script I can find
    !       for this targets C compilers. Maybe that's sufficient? I don't know.
    !   If may make sense to either 
    !           a) abandon FFTW altogether, 
    !           b) only install openmp FFTW under very stringent conditions (i.e. no macports, only with lfftw3_omp)
    !           c) Just have a serial version of FFTW...Maybe one that implements wisdom for some sort of advantage?
    !    

    use constants
    use, intrinsic :: iso_c_binding
       include '/fftw3.f'
    logical :: first_time_1d = .true., first_time_2d = .true., use_wisdom = .true., quiet=.true.
    integer(8) :: plan
    integer ::  mode = fftw_estimate
    integer ::  NTHREADS = 8
    character(len=10) :: mode_name = 'estimate'
    

    
contains
    include './f95_wisdom.f90'

    subroutine set_optimization_mode(mode_name_in)
        character(len=10), intent(in) :: mode_name_in
        if (.not. quiet) then
            write(stderr, *) "    fftw: setting optimization mode to ", mode_name_in
        end if 
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

    function fft1d(f)
        
        complex(c_double_complex), intent(inout) :: f(:)
        complex(c_double_complex), dimension(size(f)) :: fft1d
        integer :: n, threaderror, error, numthreads
        character(len=5) :: nval
        character(len=400) :: plan_filename

        n = size(f)
        write(nval, '(I0.5)') n


        plan_filename = "/fftw_plan_cob"//&
            &trim(adjustl(nval))//"_"//trim(adjustl(mode_name))//".plan"
        
        if (first_time_1d) then
            call dfftw_init_threads(threaderror)
            if (threaderror == 0) then
                write (0,*) "------------------------------"
                write (0,*) "Error initializing multiple threads. Program will attempt to proceed"
                write (0,*) "using 1 thread."
                write (0,*) "------------------------------"
                numthreads = 1
            else
                numthreads = NTHREADS
                call omp_set_num_threads(numthreads)
                !write(stderr, *) numthreads, OMP_NUM_THREADS, omp_get_num_threads(),  omp_get_max_threads()
            end if
        endif
        call dfftw_plan_with_nthreads(numthreads)
        !WISDOM ----
        if (use_wisdom) then
            if (first_time_1d) then       
                error= import_wisdom_from_filename( trim(adjustl(plan_filename)))
                if (error == 0) then
                    if (.not. quiet) then
                        write(stderr, *) "    fftw: did not successfully import wisdom; planning now."
                    end if 
                    call dfftw_plan_dft_1d(plan, n, f ,fft1d, fftw_backward,mode)
                    call export_wisdom_to_filename(trim(adjustl(plan_filename)))
                else
                    if (.not. quiet) then
                        write(stderr, *) "    fftw: LOADED wisdom!!"
                    end if 

                    call dfftw_plan_dft_1d(plan, n, f ,fft1d, fftw_backward,mode)
                end if
                first_time_1d = .false.
            end if
        else
            if (first_time_1d) then    
                call dfftw_plan_dft_1d(plan, n, f ,fft1d, fftw_backward,mode)
                first_time_1d = .false.
            end if
        end if 
        ! ---- WISDOM
        call dfftw_execute_dft(plan, f, fft1d)
        !call dfftw_cleanup_threads()
    end function fft1d



    function fft2d(f)

        
        !real(kind=dp_real),  intent(in) :: x(:), y(:)
        complex(c_double_complex), intent(inout) :: f(:,:)
        complex(c_double_complex), dimension(size(f(:,1)),size(f(1,:))) :: fft2d
        integer :: nx, ny, threaderror, error, numthreads
        character(len=10) :: nval
        character(len=400) :: plan_filename

        nx = size(f(:,1))
        ny = size(f(1,:))

        ! I'm being sloppy here -- most of the code assumes nx = ny.
        write(nval, '(I0.5)') nx

        plan_filename = "/fftw_plan_cob"//trim(adjustl(nval))&
            &//"x"//trim(adjustl(nval))//"_"//trim(adjustl(mode_name))//".plan"

        !if (.not. quiet) then
        !    write(stderr, *) "   fftw: plan name = '",trim(adjustl(plan_filename)),"'"
        !end if
        
        if (first_time_2d) then
            call dfftw_init_threads(threaderror)
            if (threaderror == 0) then
                write (0,*) "------------------------------"
                write (0,*) "Error initializing multiple threads. Program will attempt to proceed"
                write (0,*) "using 1 thread."
                write (0,*) "------------------------------"
                numthreads = 1
            else
                numthreads = NTHREADS
                call omp_set_num_threads(numthreads)
                !write(stderr, *) numthreads, OMP_NUM_THREADS, omp_get_num_threads(),  omp_get_max_threads()
            end if
        endif
        call dfftw_plan_with_nthreads(numthreads)
        if (use_wisdom) then
            if (first_time_2d) then
                error = import_wisdom_from_filename(trim(adjustl(plan_filename)))
                if (error == 0) then
                    if (.not. quiet) then
                        write(stderr, *) "    fftw: did not successfully import wisdom; planning now."
                    end if 
                    call dfftw_plan_dft_2d(plan, ny, nx, f ,fft2d, fftw_backward,mode)
                    call export_wisdom_to_filename( trim(adjustl(plan_filename)))
                else
                    if (.not. quiet) then
                        write(stderr, *) "    fftw: LOADED wisdom!!"
                    end if 

                    call dfftw_plan_dft_2d(plan, ny, nx, f ,fft2d, fftw_backward,mode)
                end if
                first_time_2d = .false.
            end if
        else 
            if (first_time_2d) then
                call dfftw_plan_dft_2d(plan, ny, nx, f ,fft2d, fftw_backward,mode)
                first_time_2d = .false.
            end if 
        end if
        call dfftw_execute_dft(plan, f, fft2d)
        !call dfftw_cleanup_threads()
    end function fft2d


end module fftw