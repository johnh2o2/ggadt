      Subroutine CXFFT3_MKL(CX,MX,MY,MZ,ISIGN)
      USE DDPRECISION,ONLY : WP 
! Library:

      use mkl_dfti

      IMPLICIT NONE

! Arguments:

      INTEGER :: ISIGN,MX,MY,MZ
      COMPLEX(WP) :: CX(*)


! Local Variables

      type(DFTI_DESCRIPTOR), POINTER :: Desc_Handle
      save Desc_Handle
      integer   status
!BTD 080605 start
      real(WP):: Scale
!BTD 080605 end
      integer   lengths(3)
      integer   strides_in(4)

        
      integer i,j,k

!BTD 080605 start
      INTEGER MXOLD,MYOLD,MZOLD
      SAVE MXOLD,MYOLD,MZOLD
      DATA MXOLD,MYOLD,MZOLD/0,0,0/
!BTD 080605 end

!=======================================================================
! Subroutine CXFFT3_MKL
! Purpose: to use Intel Math Kernel Library to calculate 3d FFTs
! input:
!    MX,MY,MZ = dimensioning information
!    ISIGN = +1 for forward FFT
!            -1 for backward FFT
!    CX(I,J,K)=complex arry of complex scalars at locations (I,J,K) with
!                I=1,...,MX
!                J=1,...,MY
!                K=1,...,MZ
! returns:
!
!             Mx-1 My-1 Mz-1
! CX(I,J,K) = sum  sum  sum  CXin(u,v,w)*exp(ISIGN*2*pi*i*(u*I/Mx+v*J/My+w*K/Mz)
!             u=0  v=0  w=0

! MKL routines used:
!    DftiCreateDescriptor
!    DftiErrorClass
!    DftiSetValue
!    DftiCommitDescriptor
!    DftiComputeBackward
!    DftiComputeForward
! Written by Art S. Lazanoff, NASA Ames Research Center, June 2008
! Modified by B.T. Draine, Princeton University
! History
! 080601 (ASL) first written
! 080605 (BTD) modified
!              * IMPLICIT NONE 
!              * initialize trig tables only when called for
!                new values of MX or MY or MZ
! end history
!========================================================================
!Art debug       write(61,'(a,3(1x,i5),z16)')'mx, my, mz = ',mx,my,mz,loc(cx)
!Art debug       write(61,*)cx
        lengths(1) = mx
        lengths(2) = my
        lengths(3) = mz
        strides_in(1) = 0
        strides_in(2) = 1
        strides_in(3) = mx
        strides_in(4) = mx*my


!BTD 080605 start
!       if( isign .eq. 0) then

      IF(MX/=MXOLD.OR.MY/=MYOLD.OR.MZ/=MZOLD)THEN
!BTD 080605 end

!.......Compute trig table

!Art      write(0,*)'MKL DFT initialization'

         if(WP == 8) then
         Status = DftiCreateDescriptor( Desc_Handle, DFTI_DOUBLE, DFTI_COMPLEX, 3, lengths)
         else
         Status = DftiCreateDescriptor( Desc_Handle, DFTI_SINGLE, DFTI_COMPLEX, 3, lengths)
         endif

         if(.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
            write(0,*)'DftiCreateDescriptor, status = ',status
            stop'cannot create DftiDescriptor'
         endif

         Status = DftiSetValue(Desc_Handle, DFTI_INPUT_STRIDES, strides_in)
         if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
            write(0,*)'Dfti_input_strides, status = ',status
            stop'cannot create Dfti_input_strides'
         endif

         Status = DftiSetValue(Desc_Handle, DFTI_OUTPUT_STRIDES, strides_in)
         if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
            write(0,*)'Dfti_output_strides, status = ',status
            stop'cannot create Dfti_output_strides'
         endif

         Scale = 1._WP
!Art         scale = 1./real((mx*my*mz),WP)
         Status = DftiSetValue(Desc_Handle, DFTI_FORWARD_SCALE, Scale)
         if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
            write(0,*)'DFTI_FORWARD_SCALE status = ',Status
            stop'DFTI_FORWARD_SCALE'
         endif

!Art         scale = 1./real((mx*my*mz),WP)
         Scale = 1._WP
         Status = DftiSetValue(Desc_Handle, DFTI_BACKWARD_SCALE, Scale)
         if (.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
            write(0,*)'DFTI_BACKWARD_SCALE status = ',Status
            stop'DFTI_BACKWARD_SCALE'
         endif


         Status = DftiCommitDescriptor( Desc_Handle )
         if(.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then
            write(0,*)'DftiCommitDescriptor, status = ',status
            stop'cannot DftiCommitDescriptor'
         endif

         MXOLD = MX; MYOLD = MY; MZOLD = MZ

!Art      write(0,*)'Dfti initialization done'
      endif

!.......Compute 3d FFT 

      if (isign .eq. -1 ) then
!Art     write(0,*)'DFTI_Backward_SCALE'

         Status = DftiComputeBackward( Desc_Handle, cx)
         if (.not. DftiErrorClass(Status, DFTI_NO_ERROR) ) then
            write(0,*)'DftiComputeBackward = ',Status
            stop'DftiComputeBackward'
         endif
!Art debug        write(62,*)'mx, my, mz = ',mx,my,mz
!Art debug        write(62,*)cx
!Art debug        stop'scsl debug'

      else
        
!Art        write(0,*)'DftiComputeForward'

         Status = DftiComputeForward( Desc_Handle, cx)
         if(.not. DftiErrorClass(Status, DFTI_NO_ERROR)) then 
            write(0,*)'DftiComputeForward, status = ',status
            stop'cannot DftiComputeForward'
         endif
      endif   !isign test

       
    end subroutine cxfft3_mkl
