MODULE SPHERES

	USE, INTRINSIC :: ISO_C_BINDING
	USE PARAMS
	USE SPHERE
	SAVE
	INTEGER :: NSPHERES, NROTS, OK
	REAL, ALLOCATABLE :: RADII(:), IOR_R(:), IOR_I(:)
	REAL, ALLOCATABLE :: POS(:,:), POS_ROT(:,:)
	REAL, DIMENSION(3) :: EULER_ANGLES = (/ 0.0, 0.0, 0.0 /)
	REAL :: dxt, dyt
	!COMPLEX(C_DOUBLE_COMPLEX) :: DELM = CMPLX(IOR_RE, IOR_IM) 
	CHARACTER(len=20) :: SPHLIST_FNAME = "list_of_spheres.dat"

	
	CONTAINS

	SUBROUTINE READ_SPHERES()
		IMPLICIT NONE
	
		INTEGER  :: I, AllocateStatus
		open(unit=1,file=SPHLIST_FNAME)
		read (1,*) NSPHERES
		!print *,"NSPHERES = ", NSPHERES 

		!print *,"ALLOCATING..."
		ALLOCATE(POS(NSPHERES,3),STAT = AllocateStatus)
  		IF (AllocateStatus /= 0) STOP "*** Not enough memory (POS) ***"
		ALLOCATE(POS_ROT(NSPHERES,3),STAT = AllocateStatus)
  		IF (AllocateStatus /= 0) STOP "*** Not enough memory (POS_ROT) ***"
		ALLOCATE(RADII(NSPHERES),STAT = AllocateStatus)
  		IF (AllocateStatus /= 0) STOP "*** Not enough memory (RADII) ***"
		ALLOCATE(IOR_R(NSPHERES),STAT = AllocateStatus)
  		IF (AllocateStatus /= 0) STOP "*** Not enough memory (IOR_R) ***"
		ALLOCATE(IOR_I(NSPHERES),STAT = AllocateStatus)
  		IF (AllocateStatus /= 0) STOP "*** Not enough memory (IOR_I) ***"
		!print *, "ALLOCATED!"

		DO I=1,NSPHERES
			read(1, *) POS(I,1), POS(I,2), POS(I,3), RADII(I), IOR_R(I), IOR_I(I)
		!	print *,"Read sphere: ", POS(I,1), POS(I,2), POS(I,3), RADII(I), IOR_R(I), IOR_I(I)
		END DO 
		!print *, "Done!"
		!call exit()
		close(1)

	END SUBROUTINE READ_SPHERES

	FUNCTION PHI_SPHERES(X,Y,K)
		IMPLICIT NONE
		REAL, DIMENSION(NGRID), INTENT(IN) :: X,Y
		REAL, DIMENSION(3) :: CURRENT_POS
		REAL, INTENT(IN) :: K
		REAL :: R, L, M
		COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(NGRID,NGRID) :: PHI_SPHERES
		INTEGER :: I, J, N ,xi, xf, yi,yf

		dxt = X(2) - X(1)
		dyt = Y(2) - Y(1)

		DO I=1, NSPHERES
			!print *,"Getting phi grid...sphere ",I
			xi = INT((POS_ROT(I,1) - RADII(I) - X(1))/dxt) + 1
			xf = INT((POS_ROT(I,1) + RADII(I) - X(1))/dxt) + 1
			yi = INT((POS_ROT(I,2) - RADII(I) - X(1))/dyt) + 1
			yf = INT((POS_ROT(I,2) + RADII(I) - X(1))/dyt) + 1

			M = CMPLX(IOR_R(I), IOR_I(I))
			!print *,"set indices and M"
			do J=xi,xf
				do N=yi,yf
					!print *,J,N 
					CURRENT_POS = (/ X(J), Y(N), 0.0 /)
					PHI_SPHERES(J,N) = PHI_SPHERES(J,N)+ K*M*CHORD_SPHERE(CURRENT_POS, POS_ROT(I,:), RADII(I))
				end do
			end do 
		END DO 
		
	END FUNCTION PHI_SPHERES

	FUNCTION SHADOW_SPHERES(X,Y,K)
		IMPLICIT NONE
		REAL, DIMENSION(NGRID), INTENT(IN) :: X,Y 
		REAL, INTENT(IN) :: K 
		COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(NGRID,NGRID) :: SHADOW_SPHERES, PHI
		INTEGER :: I, J

		DO I=1,NSPHERES
			POS_ROT(I,:) = MATMUL(ROT_MATRIX(EULER_ANGLES), POS(I,:))
		END DO

		PHI = PHI_SPHERES(X,Y,K)
		DO I=1,NGRID
			DO J=1,NGRID 
				SHADOW_SPHERES(I,J) = 1-EXP( (0.0,1.0)*PHI(I,J) )
			END DO
		END DO 
		
	END FUNCTION SHADOW_SPHERES

END MODULE SPHERES 