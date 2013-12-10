! 11/11/2013--JOHN HOFFMAN
!=========================
! THIS WILL BE:
! (1) A TEST OF THE PROGRAMMING, SINCE WE CAN DIRECTLY COMPARE TO MIE THEORY
! (2) A FUTURE MODULE THAT WILL BE USED FOR COLLECTIONS OF SPHERES.

PROGRAM GGADT
	USE, INTRINSIC :: ISO_C_BINDING
	USE SPHERE
	USE SPHERES
	USE ELLIPSOID
	USE PARAMS
	USE FFTW
	IMPLICIT NONE

	REAL :: XMIN, XMAX, YMIN, YMAX, ZMIN, ZMAX, DX, DY, DZ, K, L
	REAL, DIMENSION(NGRID) :: X, Y, Z, KX, KY, THETAX, THETAY
	DOUBLE COMPLEX, DIMENSION(NGRID,NGRID) :: SH, FTSH
	REAL, DIMENSION(NGRID,NGRID) :: CHRD
	REAL, DIMENSION(NGRID,NGRID) :: SCATTER, SCATTER_TEMP
	REAL, DIMENSION(3) :: EUL_ANG
	REAL, DIMENSION(3,3) :: RM 
	INTEGER :: I, J, EULX, EULY, EULZ
	CHARACTER(len=32) :: arg

	call set_parameters()
	If ((GEOMETRY == 'SPHERE') .or. (GEOMETRY == 'SPHERES')) THEN 
		IF (GEOMETRY == 'SPHERES') THEN
			call READ_SPHERES()
		END IF 
  		GRAIN_A(1) = A_EFF 
  		GRAIN_A(2) = A_EFF
  		GRAIN_A(3) = A_EFF
	ELSE If (GEOMETRY == 'ELLIPSOID') THEN	
		GRAIN_A(1) = 1.0
  		GRAIN_A(2) = 1.0/SQRT(2.0)
  		!GRAIN_A(2) = 1.0
  		GRAIN_A(3) = 1.0
  		L = A_EFF/((GRAIN_A(1)*GRAIN_A(2)*GRAIN_A(3))**(1.0/3.0))
  		GRAIN_A(1) = GRAIN_A(1)*L
  		GRAIN_A(2) = GRAIN_A(2)*L
  		GRAIN_A(3) = GRAIN_A(3)*L
  	ELSE
		PRINT *,"Cannot understand ", GEOMETRY
		CALL EXIT()
	END IF 
	
	!I = 0
	!DO
	!	CALL GET_COMMAND_ARGUMENT(I, ARG)
	!	IF (LEN_TRIM(ARG) == 0) EXIT
	!	WRITE (*,*) TRIM(ARG)
	!	I = I+1
	!END DO

	K = (2*PI/1.239842)*1000*EPHOT

	XMIN = -BOX_WIDTH*A_EFF/2.0
	YMIN = -BOX_WIDTH*A_EFF/2.0
	ZMIN = -2*A_EFF
	XMAX = BOX_WIDTH*A_EFF/2.0
	YMAX = BOX_WIDTH*A_EFF/2.0
	ZMAX = 2*A_EFF

	DX = (XMAX-XMIN)/(SIZE(X)-1)
	DY = (YMAX-YMIN)/(SIZE(Y)-1)
	DZ = (ZMAX-ZMIN)/(SIZE(Z)-1)

	

	DO I=1,SIZE(X)
		X(I) = XMIN + (I-1)*DX
	END DO
	DO I=1,SIZE(Y)
		Y(I) = YMIN + (I-1)*DY
	END DO
	DO I=1,SIZE(Z)
		Z(I) = ZMIN + (I-1)*DZ
	END DO
	KX = GET_K(X)
	KY = GET_K(Y)
	DO I=1,SIZE(KX)
		THETAX(I) = ASIN(KX(I)/K)
		THETAY(I) = ASIN(KY(I)/K)
	END DO
	
	if (EULER_ANGLE_MODE .eq. 'SEQUENTIAL') THEN 
		NANGLE = INT(NANGLE**(1.0/3.0))
	ELSE IF (EULER_ANGLE_MODE .eq. 'RANDOM') THEN
		call init_random_seed()
	END IF 

	IF (GEOMETRY .EQ. "SPHERE") THEN
		DO I=1,SIZE(X)
			DO J=1,SIZE(Y)
				SH(I,J) = SHADOW_SPHERE(X(I),Y(J),ZMAX, K )*(-1.0)**(I+J+1)
			END DO
		END DO
		FTSH = FFT(SH,X,Y)
		DO I=1,SIZE(X)
			DO J=1,SIZE(Y)
				SCATTER(I,J) = SCATTER(I,J) + ABS((K*FTSH(I,J)*DX*DY))**2/(4*PI*(PI*A_EFF)**2)
			END DO
		END DO
	ELSE
		if (EULER_ANGLE_MODE .eq. 'SEQUENTIAL') THEN 
			DO EULX=1,NANGLE
				EUL_ANG(1) = 2*PI*(EULX-1)/NANGLE
				DO EULY=1,NANGLE
					EUL_ANG(2) = 2*PI*(EULY-1)/NANGLE
					DO EULZ=1,NANGLE
						EUL_ANG(3) = 2*PI*(EULZ-1)/NANGLE
						!RM = ROT_MATRIX(EUL_ANG)
						RM = MATMUL(MATMUL(ROT_X(EUL_ANG(1)), ROT_Y(EUL_ANG(2))),ROT_Z(EUL_ANG(3)))
						write (0,*) "EUL_ANG[",(EULX-1)*NANGLE*NANGLE+EULY*NANGLE+EULZ,"/",NANGLE**3,"]: ",EUL_ANG
						IF (GEOMETRY .EQ. 'ELLIPSOID') THEN 
							
							DO I=1,SIZE(X)
								DO J=1,SIZE(Y)
									SH(I,J) = SHADOW_ELLIPSOID(X(I),Y(J), K, RM )*(-1.0)**(I+J+1) 
								!	CHRD(I,J) = CHORD_ELLIPSOID(X(I),Y(J),RM)
								!	WRITE (0,*) "JUST READ: ", CHRD(I,J)
								END DO
							END DO
							FTSH = FFT(SH,X,Y)
						END IF 
						IF (GEOMETRY .eq. 'SPHERES')	THEN
							SH = SHADOW_SPHERES(X,Y,K,RM)
							FTSH = FFT(SH,X,Y)
						END IF 
						DO I=1,SIZE(X)
							DO J=1,SIZE(Y)
								SCATTER(I,J) = SCATTER(I,J) + LOG10(ABS((K*FTSH(I,J)*DX*DY))**2/(4*PI*(PI*A_EFF)**2))
							END DO
						END DO
					END DO
				END DO
			END DO 
		else if (EULER_ANGLE_MODE .eq. 'RANDOM') THEN 
			DO EULX=1,NANGLE
				write(0,FMT="(A1,A,t21,F6.2,A)",ADVANCE="NO") achar(13), &
					& " Percent Complete: ", (real(EULX-1)/real(NANGLE))*100.0, "%"
				call RANDOM_NUMBER(EUL_ANG)
				EUL_ANG(1) = 2*PI*EUL_ANG(1) 
				EUL_ANG(2) = 2*PI*EUL_ANG(2) 
				EUL_ANG(3) = 2*PI*EUL_ANG(3) 
				!RM = ROT_MATRIX(EUL_ANG)
				RM = MATMUL(MATMUL(ROT_X(EUL_ANG(1)), ROT_Y(EUL_ANG(2))),ROT_Z(EUL_ANG(3)))
				!write (0,*) "EUL_ANG[",EULX,"/",NANGLE,"]: ",EUL_ANG
				IF (GEOMETRY .EQ. "ELLIPSOID") THEN 
					
					DO I=1,SIZE(X)
						DO J=1,SIZE(Y)
							SH(I,J) = SHADOW_ELLIPSOID(X(I),Y(J), K, RM )*(-1.0)**(I+J+1) 
							!CHRD(I,J) = CHORD_ELLIPSOID(X(I),Y(J),RM)
							!WRITE (0,*) "JUST READ: ", CHRD(I,J)
						END DO
					END DO
					FTSH = FFT(SH,X,Y)
				END IF 
				IF (GEOMETRY .eq. 'SPHERES')	THEN
					!SH = PHI_SPHERES(X,Y,K)
					!DO I=1,SIZE(X)
				    !	DO J=1,SIZE(Y)
					!		PRINT *, X(I), Y(J), ABS(SH(I,J))
					!	END DO
					!END DO
					!call exit()
					SH = SHADOW_SPHERES(X,Y,K,RM)
					FTSH = FFT(SH,X,Y)
				END IF 
				DO I=1,SIZE(X)
					DO J=1,SIZE(Y)
						SCATTER(I,J) = SCATTER(I,J) + ABS((K*FTSH(I,J)*DX*DY))**2/(4*PI*(PI*A_EFF)**2)
					END DO
				END DO
				
			END DO
		else
			print *,"Do not understand EULER_ANGLE_MODE=", EULER_ANGLE_MODE
			call exit()
		end if
		IF (GEOMETRY .eq. 'SPHERES')	THEN
			DEALLOCATE(POS)
			DEALLOCATE(POS_ROT)
			DEALLOCATE(RADII)
			DEALLOCATE(IOR_R)
			DEALLOCATE(IOR_I)
		END IF 
	END IF 
	IF (GEOMETRY /= 'SPHERE') THEN
		DO I=1,SIZE(X)
			DO J=1,SIZE(Y)
				IF  (EULER_ANGLE_MODE .eq. 'RANDOM') THEN 
					SCATTER(I,J) = SCATTER(I,J)/REAL(NANGLE)
				ELSE 
					SCATTER(I,J) = SCATTER(I,J)/REAL(NANGLE**3)
				END IF

			END DO
		END DO
	END IF 
	DO I=1,SIZE(X)
		DO J=1,SIZE(Y)
		!	print *,THETAX(I),THETAY(J),SCATTER(I,J)
			!print *,X(I),Y(J),CHRD(I,J)
		END DO
	END DO

	!DO I=1,SIZE(X)
	!	print *,THETAX(I),SCATTER(I,1),SCATTER(1,I)
	!END DO


CONTAINS

FUNCTION GET_K(X)
	IMPLICIT NONE
	REAL, INTENT(IN) :: X(:)
	INTEGER :: N, I, J 
	REAL :: L
	REAL, DIMENSION(SIZE(X)) :: GET_K
	N = SIZE(X)
    L = X(N) - X(1)
    DO I=1,N
    	GET_K(I) = (2*PI/L)*(I-0.5*(N+1)-0.5)
    END DO
END FUNCTION GET_K

 subroutine set_parameters()


 end subroutine set_parameters

 subroutine init_random_seed()
	implicit none
	integer, allocatable :: seed(:)
	integer :: i, n, un, istat, dt(8), pid, t(2), s
	integer(8) :: count, tms

	call random_seed(size = n)
	allocate(seed(n))
	! First try if the OS provides a random number generator
	open(newunit=un, file="/dev/urandom", access="stream", &
	     form="unformatted", action="read", status="old", iostat=istat)
	if (istat == 0) then
	   read(un) seed
	   close(un)
	else
	   ! Fallback to XOR:ing the current time and pid. The PID is
	   ! useful in case one launches multiple instances of the same
	   ! program in parallel.
	   call system_clock(count)
	   if (count /= 0) then
	      t = transfer(count, t)
	   else
	      call date_and_time(values=dt)
	      tms = (dt(1) - 1970) * 365_8 * 24 * 60 * 60 * 1000 &
	           + dt(2) * 31_8 * 24 * 60 * 60 * 1000 &
	           + dt(3) * 24 * 60 * 60 * 60 * 1000 &
	           + dt(5) * 60 * 60 * 1000 &
	           + dt(6) * 60 * 1000 + dt(7) * 1000 &
	           + dt(8)
	      t = transfer(tms, t)
	   end if
	   s = ieor(t(1), t(2))
	   pid = getpid() + 1099279 ! Add a prime
	   s = ieor(s, pid)
	   if (n >= 3) then
	      seed(1) = t(1) + 36269
	      seed(2) = t(2) + 72551
	      seed(3) = pid
	      if (n > 3) then
	         seed(4:) = s + 37 * (/ (i, i = 0, n - 4) /)
	      end if
	   else
	      seed = s + 37 * (/ (i, i = 0, n - 1 ) /)
	   end if
	end if
	call random_seed(put=seed)
	end subroutine init_random_seed


END PROGRAM GGADT

