module gpfa
	!use, intrinsic :: iso_c_binding
    !use, intrinsic :: iso_fortran_env

    use gpfa_raw
    use constants


contains

    function fft(f)
        ! May 2015 -- JAH -- this might only work for square f(:,:) ( i.e. size(f(1,:)) = size(f(:,1)) )
        complex(kind=dp_complex), intent(in)                            :: f(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)),size(f(1,:)))  :: fft
        integer                                                         :: i, j, Nx, Ny, JUMP, LOT, INC
        !parameter (ntrig = 16384)
        integer                                                         :: npoints
        real, dimension(2*size(f(:,1)))                                 :: trigs_x
        real, dimension(2*size(f(1,:)))                                 :: trigs_y
        real                                                            :: f_gpfa(2*size(f(:,1))*size(f(1,:)))
        
        Nx = size(f(:,1))
        Ny = size(f(1,:))


        call SETGPFA(trigs_x, Nx)
        call SETGPFA(trigs_y, Ny)
        
        do i=1,Nx
        	do j=1,Ny
        		f_gpfa(1+2*((i-1)+(j-1)*Ny)) = REAL(f(i,j))  
        		f_gpfa(2+2*((i-1)+(j-1)*Ny)) = IMAG(f(i,j))  
        	end do 
        end do

        ! Do rows
        INC=2
        JUMP=2*Nx
        LOT=Ny
        
        call GPFA_FFT(f_gpfa(1),f_gpfa(2),trigs_y,INC,JUMP,Ny,LOT,ISIGN)
        
        ! Do columns
        INC=2*Nx
        JUMP=2
        LOT=Nx
        
        call GPFA_FFT(f_gpfa(1),f_gpfa(2),trigs_x,INC,JUMP,Nx,LOT,ISIGN)
        
        ! write results to output vector.
        
        !npoints = 0
        do i=1,Nx
        	do j=1,Ny
        		fft(i,j) = CMPLX(f_gpfa(1+2*((i-1)+(j-1)*Ny)),f_gpfa(2+2*((i-1)+(j-1)*Ny)))
        	end do
        end do

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

        integer, intent(in)                                 :: K
        integer                                             :: lentwids, keff, allocation_status
        complex(kind=dp_complex), intent(inout)             :: f(:,:)
        complex(kind=dp_complex), dimension(K,K)            :: fft_firstk 
        complex(kind=dp_complex), allocatable               :: small_fft(:,:), twids(:,:), fft_firstk_working(:,:)

        integer                                             :: N, NFFT, i,j,l,m 

        keff    = k
        N       = size(f(:,1))
        NFFT    = N/Keff

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
            write (0,*) "ERROR: Cannot allocate twids array (fftwmod)"
            stop
        end if 
        allocate(small_fft(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate small_fft array (fftwmod)"
            stop
        end if 
        allocate(fft_firstk_working(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate fft_firstk_working array (fftwmod)"
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
                            & = zexp(CMPLX(0,1)*ISIGN*twopi*(real(i*l+j*m))/real(N))
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

end module gpfa