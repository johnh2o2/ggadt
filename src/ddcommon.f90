! ------------------ ddcommon_v2 ---------------------------------------------
! history:
! 12.08.11 (BTD) added DIPINT to DDCOMMON_0
! 13.01.04 (BTD) added AK2OLD_B,AK3OLD_B,WOLD_B to DDCOMMON_0
! end history
!-----------------------------------------------------------------------------
    MODULE DDCOMMON_0
    USE DDPRECISION,ONLY: WP
    INTEGER,SAVE:: IBSELF,IDIPINT,NGRID
    REAL(WP),SAVE:: AK2OLD,AK2OLD_B,AK3OLD,AK3OLD_B,WOLD,WOLD_B
    CHARACTER*60:: CFLPAR

! purpose of this module is to 
! 1. Supply CFLPAR to subroutine DDSCAT
! 2. Reset values of AK2OLD,AK3OLD,WOLD between calls to DDSCAT
!    to force recalculation of A_ij by subroutine ESELF
!    (If not done, earlier A_ij values might be inadvertently reused).
! 3. Communicate NGRID calculated in ESELF to other routines
! 4. Communicate DIPINT (specifying dipole interaction) to other routines.
! 5. Hold values of AK2OLD_B,AK3OLD_B,WOLD_B 
!    (analogs of AK2OLD,AK3OLD,WOLD used by ESELF)

    END MODULE DDCOMMON_0
!-----------------------------------------------------------------------------
    MODULE DDCOMMON_1
      USE DDPRECISION,ONLY: WP
      REAL(WP) :: &
         AK_TF(3),  &
         DX(3)
    END MODULE DDCOMMON_1
!------------------------------------------------------------------------------
    MODULE DDCOMMON_2
      USE DDPRECISION,ONLY: WP
      COMPLEX(WP),ALLOCATABLE :: CXADIA(:)
    END MODULE DDCOMMON_2
!------------------------------------------------------------------------------
    MODULE DDCOMMON_3
      USE DDPRECISION,ONLY: WP
      COMPLEX(WP),ALLOCATABLE :: CXZC(:,:,:,:)
    END MODULE DDCOMMON_3
!------------------------------------------------------------------------------
    MODULE DDCOMMON_4
      USE DDPRECISION,ONLY: WP
      COMPLEX(WP),ALLOCATABLE :: CXZW(:,:,:,:)
    END MODULE DDCOMMON_4
!------------------------------------------------------------------------------
    MODULE DDCOMMON_5
      INTEGER*2,ALLOCATABLE :: IOCC(:)
    END MODULE DDCOMMON_5
!------------------------------------------------------------------------------
    MODULE DDCOMMON_6
      USE DDPRECISION,ONLY: WP
      INTEGER :: MXNATF,MXNXF,MXNYF,MXNZF,NAT,NAT3,NAT0,NX,NY,NZ,MXN3F, &
                 IDVOUT,IPBC
      REAL(WP) :: GAMMA,PYD,PZD
    END MODULE DDCOMMON_6
!------------------------------------------------------------------------------
    MODULE DDCOMMON_7
      USE DDPRECISION,ONLY: WP
      COMPLEX(WP),ALLOCATABLE :: CXAOFF(:,:)
    END MODULE DDCOMMON_7
!------------------------------------------------------------------------------
    MODULE DDCOMMON_8
      CHARACTER(6) :: CMDFFT
    END MODULE DDCOMMON_8
!------------------------------------------------------------------------------
    MODULE DDCOMMON_9
      USE DDPRECISION,ONLY: WP
      INTEGER :: IDVOUT2,ITERMX,ITERN
      REAL(WP) :: ERRSCAL
    END MODULE DDCOMMON_9
!------------------------------------------------------------------------------
    MODULE DDCOMMON_10
      INTEGER :: MYID
    END MODULE DDCOMMON_10
!------------------------------------------------------------------------------
