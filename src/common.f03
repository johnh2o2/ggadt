MODULE COMMON_MOD
  USE, INTRINSIC :: ISO_C_BINDING
  implicit none
  SAVE
  REAL, PARAMETER               ::  PI          = 3.14159
  COMPLEX(C_DOUBLE_COMPLEX)     :: DELM 
  ! Control file variables
  REAL                          ::  A_EFF     
  REAL                          ::  EPHOT     
  CHARACTER(LEN=30)             ::  GEOMETRY  
  CHARACTER(LEN=30)             ::  EULER_ANGLE_MODE 

  INTEGER                       ::  NGRID 
  INTEGER                       ::  NANGLE 

  REAL                         ::  BOX_WIDTH  
  REAL                         ::  IOR_IM        
  REAL                         ::  IOR_RE     
  REAL, dimension(3)           ::  GRAIN_A 
  LOGICAL                      ::  MPI_MODE   
  CHARACTER(LEN=10)            ::  FFT_TYPE  
  CHARACTER(len=100)           ::  SPHLIST_FNAME
  
  ! ios is negative if an end of record condition is encountered or if
  ! an endfile condition was detected.  It is positive if an error was
  ! detected.  ios is zero otherwise.

  contains
  subroutine read_param_file(fname)
    implicit none
    character(len=*), intent(in)::fname
    character(len=300) :: buffer, label
    integer :: pos,pos_end,k
    integer, parameter :: fh = 15
    integer :: ios = 0
    integer :: line = 0
    print *,"#=-=-=-=-=-=-GGADT version 0.15-=-=-=-=-=-="
    print *,"#                parameters                "
    print *,"#/----------------------------------------\"
    do while (ios == 0)


    ! Input related variables
   
      open(fh, file=fname)

      read(fh, '(A)', iostat=ios) buffer
      if (ios == 0) then
        line = line + 1

        ! Find the first instance of whitespace.  Split label and data.
        pos = scan(buffer, '    ')
        label = buffer(1:pos)
        buffer = buffer(pos+1:)
        pos_end = scan(buffer,';')
        buffer = buffer(1:pos_end-1)
        
        select case (label)
        case ('NGRID')
           read(buffer, *, iostat=ios) NGRID
           print '(A8,I27)', '# NGRID',NGRID
        case ('NANGLE')
           read(buffer, *, iostat=ios) NANGLE
           print '(A9,I25)', '# NANGLE',NANGLE
        case ('BOX_WIDTH')
           read(buffer, *, iostat=ios) BOX_WIDTH
           print '(A12,F25.3,A6)', '# BOX_WIDTH',BOX_WIDTH,'*A_EFF'
        case ('IOR_RE')
           read(buffer, *, iostat=ios) IOR_RE
           print '(A9,E31.3)', '# IOR_RE',IOR_RE
        case ('IOR_IM')
           read(buffer, *, iostat=ios) IOR_IM
           print '(A10,E30.3)', '# IOR_IM ',IOR_IM
        case ('GRAIN_A')
           read(buffer, *, iostat=ios) GRAIN_A
           print '(A11,A18,3F7.3)', '# GRAIN_A ',' ',GRAIN_A
        case ('EPHOT')
           read(buffer, *, iostat=ios) EPHOT
           print '(A9,F27.3,A4)', '# EPHOT ',EPHOT,'keV'
        case ('A_EFF')
           read(buffer, *, iostat=ios) A_EFF
          print '(A9,F27.3,A12)', '# A_EFF ',A_EFF,' micrometers'
        case ('GEOMETRY')
           read(buffer, *, iostat=ios) GEOMETRY
           print '(A12,A49)', '# GEOMETRY ',GEOMETRY
        case ('EULER_ANGLE_MODE')
           read(buffer, *, iostat=ios) EULER_ANGLE_MODE
           print '(A20,A17)','# EULER_ANGLE_MODE ',trim(adjustl(EULER_ANGLE_MODE))
        case ('MPI_MODE')
           read(buffer, *, iostat=ios) MPI_MODE
           if (MPI_MODE) then
              print '(A12,A25)', '# MPI_MODE ','TRUE'
           else 
              print '(A12,A24)', '# MPI_MODE ','FALSE'
           end if
        case ('FFT_TYPE')
           read(buffer, *, iostat=ios) FFT_TYPE
           print '(A12,A29)', '# FFT_TYPE ',FFT_TYPE
        case ('SPHLIST_FNAME')
           read(buffer, *, iostat=ios) SPHLIST_FNAME
            print '(A17,A34)','# SPHLIST_FNAME ',trim(adjustl(SPHLIST_FNAME))
        case default
           print *, '#**[ERROR]** Skipping (',trim(adjustl(buffer)),'), line', line
        end select
      end if
    end do
    print *,"#\---------------------------------------/"
    print *,"  "
    DELM = CMPLX(IOR_RE, IOR_IM)
    close(fh)
  end subroutine read_param_file

  FUNCTION ROT_MATRIX(EUL_ANG)
    IMPLICIT NONE
    REAL, DIMENSION(3), INTENT(IN) :: EUL_ANG
    REAL, DIMENSION(3,3) :: ROT_MATRIX
    ROT_MATRIX(1,1) =  COS(EUL_ANG(2))*COS(EUL_ANG(3))
    ROT_MATRIX(1,2) = -COS(EUL_ANG(1))*SIN(EUL_ANG(3))-SIN(EUL_ANG(1))*SIN(EUL_ANG(2))*COS(EUL_ANG(3))
    ROT_MATRIX(1,3) =  SIN(EUL_ANG(1))*SIN(EUL_ANG(3))-COS(EUL_ANG(1))*SIN(EUL_ANG(2))*SIN(EUL_ANG(3))

    ROT_MATRIX(2,1) =  COS(EUL_ANG(2))*SIN(EUL_ANG(3))
    ROT_MATRIX(2,2) =  COS(EUL_ANG(1))*COS(EUL_ANG(3))-SIN(EUL_ANG(1))*SIN(EUL_ANG(2))*SIN(EUL_ANG(3))
    ROT_MATRIX(2,3) = -SIN(EUL_ANG(1))*COS(EUL_ANG(3))-COS(EUL_ANG(1))*SIN(EUL_ANG(2))*SIN(EUL_ANG(3))

    ROT_MATRIX(3,1) =  SIN(EUL_ANG(2))
    ROT_MATRIX(3,2) =  SIN(EUL_ANG(1))*COS(EUL_ANG(2))
    ROT_MATRIX(3,3) =  COS(EUL_ANG(1))*COS(EUL_ANG(2))

  END FUNCTION ROT_MATRIX
  FUNCTION ROT_X(EUL_ANG)
    IMPLICIT NONE
    REAL, INTENT(IN) :: EUL_ANG
    REAL, DIMENSION(3,3) :: ROT_X
    ROT_X(1,1) = 1.0
    ROT_X(1,2) = 0.0
    ROT_X(1,3) = 0.0

    ROT_X(2,1) =  0.0
    ROT_X(2,2) =  COS(EUL_ANG)
    ROT_X(2,3) = -SIN(EUL_ANG)

    ROT_X(3,1) =  0.0
    ROT_X(3,2) =  SIN(EUL_ANG)
    ROT_X(3,3) =  COS(EUL_ANG)

  END FUNCTION ROT_X 
  FUNCTION ROT_Y(EUL_ANG)
    IMPLICIT NONE
    REAL, INTENT(IN) :: EUL_ANG
    REAL, DIMENSION(3,3) :: ROT_Y
    ROT_Y(1,1) = COS(EUL_ANG)
    ROT_Y(1,2) = 0.0
    ROT_Y(1,3) = SIN(EUL_ANG)

    ROT_Y(2,1) =  0.0
    ROT_Y(2,2) =  1.0
    ROT_Y(2,3) =  0.0

    ROT_Y(3,1) = -SIN(EUL_ANG)
    ROT_Y(3,2) =  0.0
    ROT_Y(3,3) =  COS(EUL_ANG)

  END FUNCTION ROT_Y
  FUNCTION ROT_Z(EUL_ANG)
    IMPLICIT NONE
    REAL, INTENT(IN) :: EUL_ANG
    REAL, DIMENSION(3,3) :: ROT_Z
    ROT_Z(1,1) = COS(EUL_ANG)
    ROT_Z(1,2) = SIN(EUL_ANG)
    ROT_Z(1,3) = 0.0

    ROT_Z(2,1) = -SIN(EUL_ANG)
    ROT_Z(2,2) =  COS(EUL_ANG)
    ROT_Z(2,3) =  0.0

    ROT_Z(3,1) = 0.0
    ROT_Z(3,2) = 0.0
    ROT_Z(3,3) = 1.0

  END FUNCTION ROT_Z

end module COMMON_MOD