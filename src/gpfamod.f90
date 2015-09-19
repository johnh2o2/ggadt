module gpfa

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
    

end module gpfa
