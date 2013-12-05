MODULE SPHERES

	USE, INTRINSIC :: ISO_C_BINDING
	USE PARAMS
	USE SPHERE
	SAVE
	INTEGER :: NSPHERES, NROTS, OK, N
	REAL, ALLOCATABLE :: RADII(:), IOR_R(:), IOR_I(:)
	REAL, ALLOCATABLE :: POS(:,:), POS_ROT(:,:)
	
	REAL :: dxt, dyt, A_EFF_TEMP, CONV 
	CHARACTER(len=100) :: SPHLIST_FNAME = "bruces_spheres_BA1024.1.targ"
	CHARACTER(len=1000) :: JUNK
	
	CONTAINS

	SUBROUTINE READ_SPHERES()
		IMPLICIT NONE
		REAL :: V = 0.0
		INTEGER  :: I, AllocateStatus

		open(unit=1,file=SPHLIST_FNAME)	! open sphere file.

		DO I=1,5
			read(1,'(A)') JUNK		! Ignore the header for now.
		END DO 

		! This section will read the entire header in the future.
		NSPHERES = 1024

		! Allocate necessary memory
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

		! Read in spheres
		DO I=1,NSPHERES
			read(1, *) N, POS(I,1), POS(I,2), POS(I,3), RADII(I) 
			IOR_R(I) = IOR_RE
			IOR_I(I) = IOR_IM
			RADII(I) = RADII(I)/2
			V = V+ (4.0*PI/3.0)*(RADII(I))**3
		END DO 

		close(1)

		! Now normalize
		A_EFF_TEMP = ((3*V)/(4*PI))**(1.0/3.0)
		CONV = A_EFF/A_EFF_TEMP

		DO I=1,NSPHERES
			POS(I,1) = POS(I,1)*CONV
			POS(I,2) = POS(I,2)*CONV 
			POS(I,3) = POS(I,3)*CONV
			POS_ROT(I,1) = POS(I,1)
			POS_ROT(I,2) = POS(I,2)
			POS_ROT(I,3) = POS(I,3)
			RADII(I) = RADII(I)*CONV		
		END DO
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
			! Only modify the relevant section of the phi grid
			! indices: i = [xi,xj], j = [yi,yj]
			xi = INT((POS_ROT(I,1) - RADII(I) - X(1))/dxt) + 1
			xf = INT((POS_ROT(I,1) + RADII(I) - X(1))/dxt) + 1
			yi = INT((POS_ROT(I,2) - RADII(I) - X(1))/dyt) + 1
			yf = INT((POS_ROT(I,2) + RADII(I) - X(1))/dyt) + 1

			M = CMPLX(IOR_R(I), IOR_I(I)) ! IOR - 1
			
			do J=xi,xf
				do N=yi,yf
					CURRENT_POS = (/ X(J), Y(N), 0.0 /)
					PHI_SPHERES(J,N) = PHI_SPHERES(J,N)+ K*M*CHORD_SPHERE(CURRENT_POS, POS_ROT(I,:), RADII(I))
				end do
			end do 
		END DO 
		
	END FUNCTION PHI_SPHERES

	FUNCTION SHADOW_SPHERES(X,Y,K,EULER_ANGLES)
		IMPLICIT NONE
		REAL, DIMENSION(NGRID), INTENT(IN) :: X,Y 
		REAL, DIMENSION(3), INTENT(IN) :: EULER_ANGLES
		REAL, INTENT(IN) :: K 
		COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(NGRID,NGRID) :: SHADOW_SPHERES, PHI
		INTEGER :: I, J


		! Rotate sphere positions by the specified Euler angles
		DO I=1,NSPHERES
			POS_ROT(I,:) = MATMUL(ROT_MATRIX(EULER_ANGLES), POS(I,:))
		END DO

		PHI = PHI_SPHERES(X,Y,K) ! Paint phi grid
		DO I=1,NGRID
			DO J=1,NGRID 
				! Convert PHI grid to shadow grid*(-1)^(I+J) so the FFT is centered
				SHADOW_SPHERES(I,J) = (-1.0)**(I+J+1)*(1-EXP( (0.0,1.0)*PHI(I,J) ))
			END DO
		END DO 
		
	END FUNCTION SHADOW_SPHERES

END MODULE SPHERES 