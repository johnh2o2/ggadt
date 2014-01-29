    SUBROUTINE CXFFT3N(C,MX,MY,MZ,ISIGN)
      USE DDPRECISION,ONLY : WP

! Parameters:

      INTEGER :: MXTRIG
!      PARAMETER(MXTRIG=1000)
      PARAMETER(MXTRIG=16384)

! Arguments:

      INTEGER :: ISIGN,MX,MY,MZ
      REAL(WP) :: C(*)

! Local scalars:

      INTEGER :: INC,IND,JUMP,K,LOT,MXOLD,MYOLD,MZOLD

! Local arrays:

      REAL(WP) ::       &
         TRIG1(MXTRIG), &
         TRIG2(MXTRIG), &
         TRIG3(MXTRIG)
      SAVE TRIG1,TRIG2,TRIG3,MXOLD,MYOLD,MZOLD
!***********************************************************************
! Subroutine CXFFT3N

! Interface routine for "Generalized Prime Factor Algorithm" FFT code
! of Temperton for computation of 3 dimensional FFTs

! Calling program should have call of form

!    CALL CXFFT3N(CX,MX,MY,MZ,ISIGN)

! where
!    CX(I,J,K)= complex array of 3-vectors at locations (I,J,K), with
!               I = 1,...,MX
!               J = 1,...,MY
!               K = 1,...,MZ

! Upon return,
!
!            Mx-1 My-1 Mz-1
! CX(I,J,K)= sum  sum  sum CXin(u,v,w)*exp[ISIGN*2*pi*i*(u*I/Mx+v*J/My+w*K/Mz)
!            u=0  v=0  w=0

! Note that here C is defined as a real array C(*)
! C(1) = Re[CX(1,1,1)]
! C(2) = Im[CX(1,1,1)]
! C(3) = Re[CX(2,1,1)]
! C(4) = Im[CX(2,1,1)]
! ...

! or, in general:

! C(1+2*((I-1)+(J-1)*MY+(K-1)*MY*MZ) = Re[CX(I,J,K)]
! C(2+2*((I-1)+(J-1)*MY+(K-1)*MY*MZ) = Im[CX(I,J,K)]


! Interface written by P.J.Flatau

! History:
! 07.06.30 (BTD) Increased MXTRIG from 1000 to 16384=4*4096
!                (since current version of EXTEND has NF235 up to 4096).
! 08.06.05 (BTD) corrected comments
! end history
!***********************************************************************
      DATA MXOLD,MYOLD,MZOLD/0,0,0/

!*** diagnostic
!      write(0,*)'cxfft3n ckpt 1'
!***
      IF(MX/=MXOLD.OR.MY/=MYOLD.OR.MZ/=MZOLD)THEN
         MXOLD=MX
         MYOLD=MY
         MZOLD=MZ
!*** diagnostic
!        write(0,*)'cxfft3n ckpt 2'
!***
         CALL SETGPFA(TRIG1,MX)
         CALL SETGPFA(TRIG2,MY)
         CALL SETGPFA(TRIG3,MZ)
!*** diagnostic
!        write(0,*)'cxfft3n ckpt 3'
!***
      ENDIF
! --- first dimension
      INC=2
      JUMP=2*MX
      LOT=MY*MZ

!*** diagnostic
!      write(0,*)'cxfft3n ckpt 4'
!***
      CALL GPFA(C(1),C(2),TRIG1,INC,JUMP,MX,LOT,ISIGN)
!*** diagnostic
!      write(0,*)'cxfft3n ckpt 5'
!***
      INC=MX*2
      JUMP=2
      LOT=MX
! --- one plane at a time
      DO K=1,MZ
         IND=1+2*MX*MY*(K-1)
!*** diagnostic
!        write(0,*)'cxfft3n ckpt 250, about to call GPFA for k=',k
!***
        CALL GPFA(C(IND),C(IND+1),TRIG2,INC,JUMP,MY,LOT,ISIGN)
!*** diagnostic
!        write(0,*)'returned to cxfft3n from gpfa'
!***
      ENDDO

! --- third dimension
      INC=MX*MY*2
      JUMP=2
      LOT=MX*MY
!*** diagnostic
!        write(0,*)'cxfft3n ckpt 300, about to call GPFA for k=',k
!***
      CALL GPFA(C(1),C(2),TRIG3,INC,JUMP,MZ,LOT,ISIGN)
!*** diagnostic
!      write(0,*)'returned to cxfft3n from GPFA'
!***
      RETURN
    END SUBROUTINE CXFFT3N
