module gpfa
	!use, intrinsic :: iso_c_binding
    !use, intrinsic :: iso_fortran_env

    use gpfa_raw
    use constants


contains

    function fft1d(f)
        complex(kind=dp_complex), intent(in)                            :: f(:)
        complex(kind=dp_complex), dimension(size(f))                    :: fft1d
        integer                                                         :: i, j, N, JUMP, LOT, INC
        !parameter (ntrig = 16384)
        integer                                                         :: npoints
        real, dimension(2*size(f))                                      :: trigs
        real                                                            :: f_gpfa(2*size(f(:)))
        
        N = size(f)

        call SETGPFA(trigs, N)
        
        
        do i=1,N
            f_gpfa(1+2*(i-1)) = REAL(f(i))  
            f_gpfa(2+2*(i-1)) = IMAG(f(i))  
        end do

        ! Do rows
        INC=2
        JUMP=2*N
        LOT=1
        
        call GPFA_fft(f_gpfa(1),f_gpfa(2),trigs,INC,JUMP,N,LOT,ISIGN)
        
        
        ! write results to output vector.
        
        !npoints = 0
        do i=1,N
            fft1d(i) = CMPLX(f_gpfa(1+2*(i-1)),f_gpfa(2+2*(i-1)))
        end do

    end function fft1d 
    function fft1d_firstk(f,K)
        ! How this calculation works:
        !   Say I have 15 data points, in one dimension, for which I only care about the first 5 values of its fft1d.
        !
        !   We can calculate these first five k values by doing 15/5 = 3 fft1d's of length 5.
        !
        !       Original data: dddddddddddddddd
        !
        !       fft1d1 = fft1d( d--d--d--d--d--d )
        !       fft1d2 = fft1d( -d--d--d--d--d-- )
        !       fft1d3 = fft1d( --d--d--d--d--d- )
        !
        !       fft1d(orig)(k) = sum_i(fft1di(k))*twid_factor(k)
        ! 

        integer, intent(in)                                 :: K
        integer                                             :: lentwids, keff, allocation_status
        complex(kind=dp_complex), intent(inout)             :: f(:)
        complex(kind=dp_complex), dimension(K)              :: fft1d_firstk 
        complex(kind=dp_complex), allocatable               :: small_fft1d(:), twids(:), fft1d_firstk_working(:)

        integer                                             :: N, Nfft1d, i,l

        keff    = k
        N       = size(f)
        Nfft1d    = N/Keff

        ! Make sure N is divisible by K.
        !    if not, we take the first K+n values of the fft1d 
        !    and throw away n of them (n is the smallest integer such that K+n | N)
        do while ((.not. keff*Nfft1d .eq. N) .and. keff < N )
            keff = keff + 1
            Nfft1d = N/Keff 
        end do 

        lentwids = N

        ! allocate twids, small_fft1d and fft1d_firstk_working arrays
        !   twids               -- contain twiddle factors needed to combine the different fft1d calculations
        !   small_fft1d           -- the array in which each fft1d is temporarily stored
        !   fft1d_firstk_working  --

        allocate(twids(lentwids), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate twids array (fft1dwmod)"
            stop
        end if 
        allocate(small_fft1d(keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate small_fft1d array (fft1dwmod)"
            stop
        end if 
        allocate(fft1d_firstk_working(keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate fft1d_firstk_working array (fft1dwmod)"
            stop
        end if 

        do i=1,keff
            fft1d_firstk_working(i) = 0.0D0 
        end do 

        if (Keff*Nfft1d .ne. N) then
            write(0,*) "ERROR: Nfft1d = ",Nfft1d," and K = ",K,", but Nfft1d*K = ",Nfft1d*K," != ",N
            stop 
        endif

        ! Calculate twiddle factors
        do i=0,(keff-1)
            do l=0,(Nfft1d-1)    
                twids(1 + i + l*Keff) = zexp(CMPLX(0,1)*ISIGN*twopi*(real(i*l))/real(N))
            end do 
        end do

        do l=1,Nfft1d
            ! execute plan
            ! write(stderr, *) "about to do fft1d() call.", l, N, Nfft1d
            small_fft1d = fft1d(f(l:N:Nfft1d))
            ! transfer results to master array
            do i=1,Keff
                fft1d_firstk_working(i) = fft1d_firstk_working(i) + small_fft1d(i)*twids( i+(l-1)*Keff )
            end do 
        end do
        
        ! only get the K values that we care about
        fft1d_firstk = fft1d_firstk_working(:K)

        deallocate(twids)
        deallocate(small_fft1d)
        deallocate(fft1d_firstk_working)
    end function fft1d_firstk


    function fft2d(f)
        ! May 2015 -- JAH -- this might only work for square f(:,:) ( i.e. size(f(1,:)) = size(f(:,1)) )
        complex(kind=dp_complex), intent(in)                            :: f(:,:)
        complex(kind=dp_complex), dimension(size(f(:,1)),size(f(1,:)))  :: fft2d
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
        
        call GPFA_fft(f_gpfa(1),f_gpfa(2),trigs_y,INC,JUMP,Ny,LOT,ISIGN)
        
        ! Do columns
        INC=2*Nx
        JUMP=2
        LOT=Nx
        
        call GPFA_fft(f_gpfa(1),f_gpfa(2),trigs_x,INC,JUMP,Nx,LOT,ISIGN)
        
        ! write results to output vector.
        
        !npoints = 0
        do i=1,Nx
        	do j=1,Ny
        		fft2d(i,j) = CMPLX(f_gpfa(1+2*((i-1)+(j-1)*Ny)),f_gpfa(2+2*((i-1)+(j-1)*Ny)))
        	end do
        end do

    end function fft2d 
    function fft2d_firstk(f,K)
        ! How this calculation works:
        !   Say I have 15 data points, in one dimension, for which I only care about the first 5 values of its fft2d.
        !
        !   We can calculate these first five k values by doing 15/5 = 3 fft2d's of length 5.
        !
        !       Original data: dddddddddddddddd
        !
        !       fft2d1 = fft2d( d--d--d--d--d--d )
        !       fft2d2 = fft2d( -d--d--d--d--d-- )
        !       fft2d3 = fft2d( --d--d--d--d--d- )
        !
        !       fft2d(orig)(k) = sum_i(fft2di(k))*twid_factor(k)
        ! 

        integer, intent(in)                                 :: K
        integer                                             :: lentwids, keff, allocation_status
        complex(kind=dp_complex), intent(inout)             :: f(:,:)
        complex(kind=dp_complex), dimension(K,K)            :: fft2d_firstk 
        complex(kind=dp_complex), allocatable               :: small_fft2d(:,:), twids(:,:), fft2d_firstk_working(:,:)

        integer                                             :: N, Nfft2d, i,j,l,m 

        keff    = k
        N       = size(f(:,1))
        Nfft2d    = N/Keff

        ! Make sure N is divisible by K.
        !    if not, we take the first K+n values of the fft2d 
        !    and throw away n of them (n is the smallest integer such that K+n | N)
        do while ((.not. keff*Nfft2d .eq. N) .and. keff < N )
            keff = keff + 1
            Nfft2d = N/Keff 
        end do 

        lentwids = N

        ! allocate twids, small_fft2d and fft2d_firstk_working arrays
        !   twids               -- contain twiddle factors needed to combine the different fft2d calculations
        !   small_fft2d           -- the array in which each fft2d is temporarily stored
        !   fft2d_firstk_working  --

        allocate(twids(lentwids, lentwids), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate twids array (fft2dwmod)"
            stop
        end if 
        allocate(small_fft2d(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate small_fft2d array (fft2dwmod)"
            stop
        end if 
        allocate(fft2d_firstk_working(keff, keff), stat = allocation_status)
        if (allocation_status/=0) then
            write (0,*) "ERROR: Cannot allocate fft2d_firstk_working array (fft2dwmod)"
            stop
        end if 

        do i=1,keff
            do j=1,keff
                fft2d_firstk_working(i,j) = 0.0D0
            end do 
        end do 

        if (Keff*Nfft2d .ne. N) then
            write(0,*) "ERROR: Nfft2d = ",Nfft2d," and K = ",K,", but Nfft2d*K = ",Nfft2d*K," != ",N
            stop 
        endif

        

        ! Calculate twiddle factors

        do i=0,(keff-1)
            do j=0,(keff-1)
                do l=0,(Nfft2d-1)
                    do m=0,(Nfft2d-1)
                        twids(1 + i + l*Keff, 1 + j + m*Keff ) &
                            & = zexp(CMPLX(0,1)*ISIGN*twopi*(real(i*l+j*m))/real(N))
                    end do 
                end do 
            end do 
        end do

        do l=1,Nfft2d
            do m=1,Nfft2d
                

                ! execute plan
                small_fft2d = fft2d(f(l:N:Nfft2d,m:N:Nfft2d))

                ! transfer results to master array
                do i=1,Keff
                    do j=1,Keff 
                        fft2d_firstk_working(i,j) = fft2d_firstk_working(i,j) &
                                            & + small_fft2d(i,j)*twids( i+(l-1)*Keff , j+(m-1)*Keff )
                    end do 
                end do 

            end do 
        end do
        
        ! only get the K values that we care about
        fft2d_firstk = fft2d_firstk_working(:K,:K)

        deallocate(twids)
        deallocate(small_fft2d)
        deallocate(fft2d_firstk_working)
    end function fft2d_firstk

end module gpfa