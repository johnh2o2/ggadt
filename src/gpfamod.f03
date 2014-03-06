module gpfa
	use, intrinsic :: iso_c_binding
    use, intrinsic :: iso_fortran_env

    use gpfa_raw
    use common_mod
    use constants


contains

    function fft(f)
        complex(c_double_complex), intent(in) :: f(:,:)
        complex(c_double_complex), dimension(size(f(:,1)),size(f(1,:))) :: fft
        integer :: i, j, Nx, Ny, JUMP, LOT, INC
        !parameter (ntrig = 16384)
        integer :: npoints
        real, dimension(2*size(f(:,1))) :: trigs_x
        real, dimension(2*size(f(1,:))) :: trigs_y
        real :: f_gpfa(2*size(f(:,1))*size(f(1,:)))
      
        Nx = size(f(:,1))
        Ny = size(f(1,:))


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
    function fft_firstk(f,K)
        ! How this calculation works:
        !   Say I have 15 data points, in one dimension, for which I only care about the first 5 values of its FFT.
        !
        !   We can calculate these first five k values by doing 15/5 = 3 FFT's of length 5.
        !
        !       Original data: dddddddddddddddd
        !
        !       FFT1 = FFT( d--d--d--d--d--d )
        !       FFT2 = FFT( -d--d--d--d--d-- )
        !       FFT3 = FFT( --d--d--d--d--d- )
        !
        !       FFT(orig)(k) = sum_i(FFTi(k))*twid_factor(k)
        ! 

        integer, intent(in) :: K
        integer :: lentwids, keff, allocation_status
        complex(c_double_complex), intent(inout) :: f(:,:)
        complex(c_double_complex), dimension(K,K) :: fft_firstk 
        complex(c_double_complex), allocatable :: small_fft(:,:), twids(:,:), fft_firstk_working(:,:)

        integer  :: N, NFFT, i,j,l,m 

        keff = k
        N = size(f(:,1))
        NFFT = N/Keff

        ! Make sure N is divisible by K.
        !    if not, we take the first K+n values of the FFT 
        !    and throw away n of them (n is the smallest integer such that K+n | N)
        do while ((.not. keff*NFFT .eq. N) .and. keff < N )
            keff = keff + 1
            NFFT = N/Keff 
        end do 

        lentwids = N

        ! allocate twids, small_fft and fft_firstk_working arrays
        !   twids               -- contain twiddle factors needed to combine the different fft calculations
        !   small_fft           -- the array in which each fft is temporarily stored
        !   fft_firstk_working  --

        allocate(twids(lentwids, lentwids), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) " **ERROR! Cannot allocate twids array (fftwmod)"
            stop
        end if 
        allocate(small_fft(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) " **ERROR! Cannot allocate small_fft array (fftwmod)"
            stop
        end if 
        allocate(fft_firstk_working(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) " **ERROR! Cannot allocate fft_firstk_working array (fftwmod)"
            stop
        end if 

        do i=1,keff
            do j=1,keff
                fft_firstk_working(i,j) = 0.0D0
            end do 
        end do 

        if (Keff*NFFT .ne. N) then
            write(0,*) "ERROR: NFFT = ",NFFT," and K = ",K,", but NFFT*K = ",NFFT*K," != ",N
            stop 
        endif

        

        ! Calculate twiddle factors

        do i=0,(keff-1)
            do j=0,(keff-1)
                do l=0,(NFFT-1)
                    do m=0,(NFFT-1)
                        twids(1 + i + l*Keff, 1 + j + m*Keff ) &
                            & = exp(CMPLX(0,1)*ISIGN*twopi*(real(i*l+j*m))/real(N))
                    end do 
                end do 
            end do 
        end do

        do l=1,NFFT
            do m=1,NFFT
                

                ! execute plan
                small_fft = fft(f(l:N:NFFT,m:N:NFFT))

                ! transfer results to master array
                do i=1,Keff
                    do j=1,Keff 
                        fft_firstk_working(i,j) = fft_firstk_working(i,j) + small_fft(i,j)*twids( i+(l-1)*Keff , j+(m-1)*Keff )
                    end do 
                end do 

            end do 
        end do
        
        ! only get the K values that we care about
        fft_firstk = fft_firstk_working(:K,:K)

        deallocate(twids)
        deallocate(small_fft)
        deallocate(fft_firstk_working)
    end function fft_firstk

    function kspace_shift(f,shiftx,shifty)
        real(kind=dp_real),  intent(in) :: shiftx,shifty
        complex(c_double_complex), intent(in) :: f(:,:)
        complex(c_double_complex), dimension(size(f(:,1)),size(f(1,:))) :: kspace_shift
        integer :: i, j, n

        n = size(f(:,1))

        do i=1,n
            do j=1,n
                kspace_shift(i,j) = f(i,j)*exp(ISIGN*CMPLX(0,1)*twopi*(shiftx*(i-1)+shifty*(j-1))/real(n))
            end do
        end do
    end function kspace_shift


    function fft_center(f)
        complex(c_double_complex), intent(in) :: f(:,:)
        complex(c_double_complex), dimension(size(f(:,1)),size(f(1,:))) :: fft_center
        integer :: i, j, n

        n = size(f(:,1))
        do i=1,n
            do j=1,n
                fft_center(i,j) = f(i,j)*(-1)**(i+j)
            end do
        end do 
    end function fft_center



    function experimental_fft(f,kmin,kmax,enhancement)
        
        integer, intent(in) :: kmin, kmax, enhancement
        
        complex(c_double_complex), intent(inout) :: f(:,:)
        complex(c_double_complex), allocatable :: experimental_fft(:,:)
        complex(c_double_complex), allocatable :: working_fft(:,:)
        complex(c_double_complex), dimension(size(f(:,1)), size(f(1,:))) :: new_grid
        integer :: i,j,a,b,norig
        real(kind=dp_real) :: deltax, deltay

        f = kspace_shift(f,real(kmin,kind=dp_real),real(kmin,kind=dp_real)) ! shift the input function so that the k-space origin is at (kmin,kmin)
        norig = kmax - kmin 

        !write (0,*) "norig=",norig,"kmin=",kmin,"kmax=",kmax,"enhancement=",enhancement

        ! allocate memory
        allocate(working_fft(norig,norig))
        allocate(experimental_fft(norig*enhancement+1, norig*enhancement+1))
        

        do i=1,enhancement+1
            deltax = real(i-1)/real(enhancement)
         
            do j=1,enhancement+1
                deltay = real(j-1)/real(enhancement)
    
                new_grid = kspace_shift(f,deltax,deltay)
                working_fft = fft_firstk(new_grid,norig)
                !do a=1,norig
                !    do b=1,norig
                !        if (abs(working_fft(a,b)) > HUGE_NUMBER) then
                !            write(0,*) "IN FFT -- working_fft = ",working_fft(a,b)
                !        end if 
                !    end do
                !end do 
                do a=1,norig
                    do b=1,norig
                        if ((i + (a-1)*enhancement <= size(experimental_fft(:,1))) .and. &
                            & (j + (b-1)*enhancement <= size(experimental_fft(1,:)))) then
                            experimental_fft(i + (a-1)*enhancement,j + (b-1)*enhancement) = working_fft(a,b)
                        end if 
                    end do
                end do 

            end do
        end do 
        f = kspace_shift(f,real(-kmin,kind=dp_real),real(-kmin,kind=dp_real)) ! shift the input function back.
        
    end function experimental_fft
end module gpfa