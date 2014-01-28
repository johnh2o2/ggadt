!*********************************************************************
!                                                                    *
!     GPFAPACK - FORTRAN IMPLEMENTATION OF THE SELF-SORTING          *
!     IN-PLACE GENERALIZED PRIME FACTOR (COMPLEX) FFT [GPFA]         *
!                                                                    *
!     WRITTEN BY CLIVE TEMPERTON                                     *
!     RECHERCHE EN PREVISION NUMERIQUE / ECMWF                       *
!                                                                    *
!     THE PACKAGE CONSISTS OF THE SETUP ROUTINE SETGPFA, TOGETHER    *
!     WITH THE ROUTINES GPFA, GPFA2F, GPFA3F, GPFA5F                 *
!                                                                    *
!*********************************************************************

!        SUBROUTINE 'SETGPFA'
!        SETUP ROUTINE FOR SELF-SORTING IN-PLACE
!            GENERALIZED PRIME FACTOR (COMPLEX) FFT [GPFA]

!        CALL SETGPFA(TRIGS,N)

!        INPUT :
!        -----
!        N IS THE LENGTH OF THE TRANSFORMS. N MUST BE OF THE FORM:
!          -----------------------------------
!            N = (2**IP) * (3**IQ) * (5**IR)
!          -----------------------------------

!        OUTPUT:
!        ------
!        TRIGS IS A TABLE OF TWIDDLE FACTORS,
!          OF LENGTH 2*IPQR (REAL) WORDS, WHERE:
!          --------------------------------------
!            IPQR = (2**IP) + (3**IQ) + (5**IR)
!          --------------------------------------

!        WRITTEN BY CLIVE TEMPERTON 1990

!----------------------------------------------------------------------

    SUBROUTINE SETGPFA(TRIGS,N)
      USE DDPRECISION,ONLY : WP
      REAL(WP) :: ANGLE, DEL, TRIGS, TWOPI

      DIMENSION TRIGS(*)
      DIMENSION NJ(3)

!     DECOMPOSE N INTO FACTORS 2,3,5
!     ------------------------------
      NN = N
      IFAC = 2

      DO LL = 1, 3
        KK = 0
10      CONTINUE
        IF (MOD(NN,IFAC)/=0) GO TO 20
        KK = KK + 1
        NN = NN/IFAC
        GO TO 10
20      CONTINUE
        NJ(LL) = KK
        IFAC = IFAC + LL
      END DO

      IF (NN/=1) THEN
        WRITE (6,40) N
40      FORMAT (' *** WARNING!!!',I10,' IS NOT A LEGAL VALUE OF N ***')
        RETURN
      END IF

      IP = NJ(1)
      IQ = NJ(2)
      IR = NJ(3)

!     COMPUTE LIST OF ROTATED TWIDDLE FACTORS
!     ---------------------------------------
      NJ(1) = 2**IP
      NJ(2) = 3**IQ
      NJ(3) = 5**IR

      TWOPI = 4.0_WP*ASIN(1.0_WP)
      I = 1

      DO LL = 1, 3
        NI = NJ(LL)
        IF (NI==1) GO TO 60

        DEL = TWOPI/REAL(NI,KIND=WP)
        IROT = N/NI
        KINK = MOD(IROT,NI)
        KK = 0

        DO K = 1, NI
          ANGLE = REAL(KK,KIND=WP)*DEL
          TRIGS(I) = COS(ANGLE)
          TRIGS(I+1) = SIN(ANGLE)
          I = I + 2
          KK = KK + KINK
          IF (KK>NI) KK = KK - NI
        END DO
60    END DO

      RETURN
    END SUBROUTINE SETGPFA

!        SUBROUTINE 'GPFA'
!        SELF-SORTING IN-PLACE GENERALIZED PRIME FACTOR (COMPLEX) FFT

!        *** THIS IS THE ALL-FORTRAN VERSION ***
!            -------------------------------

!        CALL GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN)

!        A IS FIRST REAL INPUT/OUTPUT VECTOR
!        B IS FIRST IMAGINARY INPUT/OUTPUT VECTOR
!        TRIGS IS A TABLE OF TWIDDLE FACTORS, PRECALCULATED
!              BY CALLING SUBROUTINE 'SETGPFA'
!        INC IS THE INCREMENT WITHIN EACH DATA VECTOR
!        JUMP IS THE INCREMENT BETWEEN DATA VECTORS
!        N IS THE LENGTH OF THE TRANSFORMS:
!          -----------------------------------
!            N = (2**IP) * (3**IQ) * (5**IR)
!          -----------------------------------
!        LOT IS THE NUMBER OF TRANSFORMS
!        ISIGN = +1 FOR FORWARD TRANSFORM
!              = -1 FOR INVERSE TRANSFORM

!        WRITTEN BY CLIVE TEMPERTON
!        RECHERCHE EN PREVISION NUMERIQUE
!        ATMOSPHERIC ENVIRONMENT SERVICE, CANADA

!----------------------------------------------------------------------

!        DEFINITION OF TRANSFORM
!        -----------------------

!        X(J) = SUM(K=0,...,N-1)(C(K)*EXP(ISIGN*2*I*J*K*PI/N))

!---------------------------------------------------------------------

!        FOR A MATHEMATICAL DEVELOPMENT OF THE ALGORITHM USED,
!        SEE:

!        C TEMPERTON : "A GENERALIZED PRIME FACTOR FFT ALGORITHM
!          FOR ANY N = (2**P)(3**Q)(5**R)",
!          SIAM J. SCI. STAT. COMP., MAY 1992.

!----------------------------------------------------------------------

    SUBROUTINE GPFA(A,B,TRIGS,INC,JUMP,N,LOT,ISIGN)
      USE DDPRECISION,ONLY : WP
      REAL(WP) :: TRIGS, B, A

      DIMENSION A(*), B(*), TRIGS(*)
      DIMENSION NJ(3)

!*** diagnostic
!      write(0,*)'gpfa ckpt 1'
!***

!     DECOMPOSE N INTO FACTORS 2,3,5
!     ------------------------------
      NN = N
      IFAC = 2

      DO LL = 1, 3
        KK = 0
10      CONTINUE
        IF (MOD(NN,IFAC)/=0) GO TO 20
        KK = KK + 1
        NN = NN/IFAC
        GO TO 10
20      CONTINUE
        NJ(LL) = KK
        IFAC = IFAC + LL
      END DO

      IF (NN/=1) THEN
        WRITE (6,40) N
40      FORMAT (' *** WARNING!!!',I10,' IS NOT A LEGAL VALUE OF N ***')
        RETURN
      END IF

      IP = NJ(1)
      IQ = NJ(2)
      IR = NJ(3)

!     COMPUTE THE TRANSFORM
!     ---------------------
      I = 1
      IF (IP>0) THEN
!*** diagnostic
!         write(0,*)'in gpfa, about to call gpfa2f'
!***
        CALL GPFA2F(A,B,TRIGS,INC,JUMP,N,IP,LOT,ISIGN)
!*** diagnostic
!        write(0,*)'returned from gpfa2f'
!***
        I = I + 2*(2**IP)
      END IF
      IF (IQ>0) THEN
!*** diagnostic
!         write(0,*)'in gpfa,about to call gpfa3f'
!***
        CALL GPFA3F(A,B,TRIGS(I),INC,JUMP,N,IQ,LOT,ISIGN)
!*** diagnostic
!        write(0,*)'returned from gpfa3f'
!***
        I = I + 2*(3**IQ)
      END IF
      IF (IR>0) THEN
!*** diagnostic
!         write(0,*)'in gpfa, about to call gpfa5f'
!***
        CALL GPFA5F(A,B,TRIGS(I),INC,JUMP,N,IR,LOT,ISIGN)
!*** diagnostic
!        write(0,*)'returned from gpfa5f'
!***
      END IF

      RETURN
    END SUBROUTINE GPFA
!     fortran version of *gpfa2* -
!     radix-2 section of self-sorting, in-place, generalized pfa
!     central radix-2 and radix-8 passes included
!      so that transform length can be any power of 2

!-------------------------------------------------------------------

    SUBROUTINE GPFA2F(A,B,TRIGS,INC,JUMP,N,MM,LOT,ISIGN)
      USE DDPRECISION, ONLY : WP
      REAL (WP) :: SI7, CO7, SI6, CO6, SI5, CO5, SI4, CO4, C3, C2, C1, SI3, &
        CO3, SI2, CO2, SI1, CO1, U3, U1, U2, U0, T3, T1, T2, T0, SS, S, B, A, &
        TRIGS
      DIMENSION A(*), B(*), TRIGS(*)
      DATA LVR/64/

!     ***************************************************************
!     *                                                             *
!     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. *
!     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE *
!     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      *
!     *                                                             *
!     ***************************************************************

      N2 = 2**MM
      INQ = N/N2
      JSTEPX = (N2-N)*INC
      NINC = N*INC
      INK = INC*INQ

      M2 = 0
      M8 = 0
      IF (MOD(MM,2)==0) THEN
        M = MM/2
      ELSE IF (MOD(MM,4)==1) THEN
        M = (MM-1)/2
        M2 = 1
      ELSE IF (MOD(MM,4)==3) THEN
        M = (MM-3)/2
        M8 = 1
      END IF
      MH = (M+1)/2

      NBLOX = 1 + (LOT-1)/LVR
      LEFT = LOT
      S = REAL(ISIGN,KIND=WP)
      ISTART = 1

!  loop on blocks of lvr transforms
!  --------------------------------
      DO NB = 1, NBLOX

        IF (LEFT<=LVR) THEN
          NVEX = LEFT
        ELSE IF (LEFT<(2*LVR)) THEN
          NVEX = LEFT/2
          NVEX = NVEX + MOD(NVEX,2)
        ELSE
          NVEX = LVR
        END IF
        LEFT = LEFT - NVEX

        LA = 1

!  loop on type I radix-4 passes
!  -----------------------------
        MU = MOD(INQ,4)
        IF (ISIGN==-1) MU = 4 - MU
        SS = 1.0_WP
        IF (MU==3) SS = -1.0_WP

        IF (MH==0) GO TO 200

        DO IPASS = 1, MH
          JSTEP = (N*INC)/(4*LA)
          JSTEPL = JSTEP - NINC

!  k = 0 loop (no twiddle factors)
!  -------------------------------
          DO JJJ = 0, (N-1)*INC, 4*JSTEP
            JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
            DO NU = 1, INQ
              JB = JA + JSTEPL
              IF (JB<ISTART) JB = JB + NINC
              JC = JB + JSTEPL
              IF (JC<ISTART) JC = JC + NINC
              JD = JC + JSTEPL
              IF (JD<ISTART) JD = JD + NINC
              J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
              DO L = 1, NVEX
                T0 = A(JA+J) + A(JC+J)
                T2 = A(JA+J) - A(JC+J)
                T1 = A(JB+J) + A(JD+J)
                T3 = SS*(A(JB+J)-A(JD+J))
                U0 = B(JA+J) + B(JC+J)
                U2 = B(JA+J) - B(JC+J)
                U1 = B(JB+J) + B(JD+J)
                U3 = SS*(B(JB+J)-B(JD+J))
                A(JA+J) = T0 + T1
                A(JC+J) = T0 - T1
                B(JA+J) = U0 + U1
                B(JC+J) = U0 - U1
                A(JB+J) = T2 - U3
                A(JD+J) = T2 + U3
                B(JB+J) = U2 + T3
                B(JD+J) = U2 - T3
                J = J + JUMP
              END DO
              JA = JA + JSTEPX
              IF (JA<ISTART) JA = JA + NINC
            END DO
          END DO

!  finished if n2 = 4
!  ------------------
          IF (N2==4) GO TO 490
          KK = 2*LA

!  loop on nonzero k
!  -----------------
          DO K = INK, JSTEP - INK, INK
            CO1 = TRIGS(KK+1)
            SI1 = S*TRIGS(KK+2)
            CO2 = TRIGS(2*KK+1)
            SI2 = S*TRIGS(2*KK+2)
            CO3 = TRIGS(3*KK+1)
            SI3 = S*TRIGS(3*KK+2)

!  loop along transform
!  --------------------
            DO JJJ = K, (N-1)*INC, 4*JSTEP
              JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
              DO NU = 1, INQ
                JB = JA + JSTEPL
                IF (JB<ISTART) JB = JB + NINC
                JC = JB + JSTEPL
                IF (JC<ISTART) JC = JC + NINC
                JD = JC + JSTEPL
                IF (JD<ISTART) JD = JD + NINC
                J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep,shortloop
                DO L = 1, NVEX
                  T0 = A(JA+J) + A(JC+J)
                  T2 = A(JA+J) - A(JC+J)
                  T1 = A(JB+J) + A(JD+J)
                  T3 = SS*(A(JB+J)-A(JD+J))
                  U0 = B(JA+J) + B(JC+J)
                  U2 = B(JA+J) - B(JC+J)
                  U1 = B(JB+J) + B(JD+J)
                  U3 = SS*(B(JB+J)-B(JD+J))
                  A(JA+J) = T0 + T1
                  B(JA+J) = U0 + U1
                  A(JB+J) = CO1*(T2-U3) - SI1*(U2+T3)
                  B(JB+J) = SI1*(T2-U3) + CO1*(U2+T3)
                  A(JC+J) = CO2*(T0-T1) - SI2*(U0-U1)
                  B(JC+J) = SI2*(T0-T1) + CO2*(U0-U1)
                  A(JD+J) = CO3*(T2+U3) - SI3*(U2-T3)
                  B(JD+J) = SI3*(T2+U3) + CO3*(U2-T3)
                  J = J + JUMP
                END DO
!-----( end of loop across transforms )
                JA = JA + JSTEPX
                IF (JA<ISTART) JA = JA + NINC
              END DO
            END DO
!-----( end of loop along transforms )
            KK = KK + 2*LA
          END DO
!-----( end of loop on nonzero k )
          LA = 4*LA
        END DO
!-----( end of loop on type I radix-4 passes)

!  central radix-2 pass
!  --------------------
200     CONTINUE
        IF (M2==0) GO TO 300

        JSTEP = (N*INC)/(2*LA)
        JSTEPL = JSTEP - NINC

!  k=0 loop (no twiddle factors)
!  -----------------------------
        DO JJJ = 0, (N-1)*INC, 2*JSTEP
          JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
          DO NU = 1, INQ
            JB = JA + JSTEPL
            IF (JB<ISTART) JB = JB + NINC
            J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
            DO L = 1, NVEX
              T0 = A(JA+J) - A(JB+J)
              A(JA+J) = A(JA+J) + A(JB+J)
              A(JB+J) = T0
              U0 = B(JA+J) - B(JB+J)
              B(JA+J) = B(JA+J) + B(JB+J)
              B(JB+J) = U0
              J = J + JUMP
            END DO
!-----(end of loop across transforms)
            JA = JA + JSTEPX
            IF (JA<ISTART) JA = JA + NINC
          END DO
        END DO

!  finished if n2=2
!  ----------------
        IF (N2==2) GO TO 490

        KK = 2*LA

!  loop on nonzero k
!  -----------------
        DO K = INK, JSTEP - INK, INK
          CO1 = TRIGS(KK+1)
          SI1 = S*TRIGS(KK+2)

!  loop along transforms
!  ---------------------
          DO JJJ = K, (N-1)*INC, 2*JSTEP
            JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
            DO NU = 1, INQ
              JB = JA + JSTEPL
              IF (JB<ISTART) JB = JB + NINC
              J = 0

!  loop across transforms
!  ----------------------
              IF (KK==N2/2) THEN
!dir$ ivdep, shortloop
                DO L = 1, NVEX
                  T0 = SS*(A(JA+J)-A(JB+J))
                  A(JA+J) = A(JA+J) + A(JB+J)
                  A(JB+J) = SS*(B(JB+J)-B(JA+J))
                  B(JA+J) = B(JA+J) + B(JB+J)
                  B(JB+J) = T0
                  J = J + JUMP
                END DO

              ELSE

!dir$ ivdep, shortloop
                DO L = 1, NVEX
                  T0 = A(JA+J) - A(JB+J)
                  A(JA+J) = A(JA+J) + A(JB+J)
                  U0 = B(JA+J) - B(JB+J)
                  B(JA+J) = B(JA+J) + B(JB+J)
                  A(JB+J) = CO1*T0 - SI1*U0
                  B(JB+J) = SI1*T0 + CO1*U0
                  J = J + JUMP
                END DO

              END IF

!-----(end of loop across transforms)
              JA = JA + JSTEPX
              IF (JA<ISTART) JA = JA + NINC
            END DO
          END DO
!-----(end of loop along transforms)
          KK = KK + 2*LA
        END DO
!-----(end of loop on nonzero k)
!-----(end of radix-2 pass)

        LA = 2*LA
        GO TO 400

!  central radix-8 pass
!  --------------------
300     CONTINUE
        IF (M8==0) GO TO 400
        JSTEP = (N*INC)/(8*LA)
        JSTEPL = JSTEP - NINC
        MU = MOD(INQ,8)
        IF (ISIGN==-1) MU = 8 - MU
        C1 = 1.0_WP
        IF (MU==3 .OR. MU==7) C1 = -1.0_WP
        C2 = SQRT(0.5_WP)
        IF (MU==3 .OR. MU==5) C2 = -C2
        C3 = C1*C2

!  stage 1
!  -------
        DO K = 0, JSTEP - INK, INK
          DO JJJ = K, (N-1)*INC, 8*JSTEP
            JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
            DO NU = 1, INQ
              JB = JA + JSTEPL
              IF (JB<ISTART) JB = JB + NINC
              JC = JB + JSTEPL
              IF (JC<ISTART) JC = JC + NINC
              JD = JC + JSTEPL
              IF (JD<ISTART) JD = JD + NINC
              JE = JD + JSTEPL
              IF (JE<ISTART) JE = JE + NINC
              JF = JE + JSTEPL
              IF (JF<ISTART) JF = JF + NINC
              JG = JF + JSTEPL
              IF (JG<ISTART) JG = JG + NINC
              JH = JG + JSTEPL
              IF (JH<ISTART) JH = JH + NINC
              J = 0
!dir$ ivdep, shortloop
              DO L = 1, NVEX
                T0 = A(JA+J) - A(JE+J)
                A(JA+J) = A(JA+J) + A(JE+J)
                T1 = C1*(A(JC+J)-A(JG+J))
                A(JE+J) = A(JC+J) + A(JG+J)
                T2 = A(JB+J) - A(JF+J)
                A(JC+J) = A(JB+J) + A(JF+J)
                T3 = A(JD+J) - A(JH+J)
                A(JG+J) = A(JD+J) + A(JH+J)
                A(JB+J) = T0
                A(JF+J) = T1
                A(JD+J) = C2*(T2-T3)
                A(JH+J) = C3*(T2+T3)
                U0 = B(JA+J) - B(JE+J)
                B(JA+J) = B(JA+J) + B(JE+J)
                U1 = C1*(B(JC+J)-B(JG+J))
                B(JE+J) = B(JC+J) + B(JG+J)
                U2 = B(JB+J) - B(JF+J)
                B(JC+J) = B(JB+J) + B(JF+J)
                U3 = B(JD+J) - B(JH+J)
                B(JG+J) = B(JD+J) + B(JH+J)
                B(JB+J) = U0
                B(JF+J) = U1
                B(JD+J) = C2*(U2-U3)
                B(JH+J) = C3*(U2+U3)
                J = J + JUMP
              END DO
              JA = JA + JSTEPX
              IF (JA<ISTART) JA = JA + NINC
            END DO
          END DO
        END DO

!  stage 2
!  -------

!  k=0 (no twiddle factors)
!  ------------------------
        DO JJJ = 0, (N-1)*INC, 8*JSTEP
          JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
          DO NU = 1, INQ
            JB = JA + JSTEPL
            IF (JB<ISTART) JB = JB + NINC
            JC = JB + JSTEPL
            IF (JC<ISTART) JC = JC + NINC
            JD = JC + JSTEPL
            IF (JD<ISTART) JD = JD + NINC
            JE = JD + JSTEPL
            IF (JE<ISTART) JE = JE + NINC
            JF = JE + JSTEPL
            IF (JF<ISTART) JF = JF + NINC
            JG = JF + JSTEPL
            IF (JG<ISTART) JG = JG + NINC
            JH = JG + JSTEPL
            IF (JH<ISTART) JH = JH + NINC
            J = 0
!dir$ ivdep, shortloop
            DO L = 1, NVEX
              T0 = A(JA+J) + A(JE+J)
              T2 = A(JA+J) - A(JE+J)
              T1 = A(JC+J) + A(JG+J)
              T3 = C1*(A(JC+J)-A(JG+J))
              U0 = B(JA+J) + B(JE+J)
              U2 = B(JA+J) - B(JE+J)
              U1 = B(JC+J) + B(JG+J)
              U3 = C1*(B(JC+J)-B(JG+J))
              A(JA+J) = T0 + T1
              A(JE+J) = T0 - T1
              B(JA+J) = U0 + U1
              B(JE+J) = U0 - U1
              A(JC+J) = T2 - U3
              A(JG+J) = T2 + U3
              B(JC+J) = U2 + T3
              B(JG+J) = U2 - T3
              T0 = A(JB+J) + A(JD+J)
              T2 = A(JB+J) - A(JD+J)
              T1 = A(JF+J) - A(JH+J)
              T3 = A(JF+J) + A(JH+J)
              U0 = B(JB+J) + B(JD+J)
              U2 = B(JB+J) - B(JD+J)
              U1 = B(JF+J) - B(JH+J)
              U3 = B(JF+J) + B(JH+J)
              A(JB+J) = T0 - U3
              A(JH+J) = T0 + U3
              B(JB+J) = U0 + T3
              B(JH+J) = U0 - T3
              A(JD+J) = T2 + U1
              A(JF+J) = T2 - U1
              B(JD+J) = U2 - T1
              B(JF+J) = U2 + T1
              J = J + JUMP
            END DO
            JA = JA + JSTEPX
            IF (JA<ISTART) JA = JA + NINC
          END DO
        END DO

        IF (N2==8) GO TO 490

!  loop on nonzero k
!  -----------------
        KK = 2*LA

        DO K = INK, JSTEP - INK, INK

          CO1 = TRIGS(KK+1)
          SI1 = S*TRIGS(KK+2)
          CO2 = TRIGS(2*KK+1)
          SI2 = S*TRIGS(2*KK+2)
          CO3 = TRIGS(3*KK+1)
          SI3 = S*TRIGS(3*KK+2)
          CO4 = TRIGS(4*KK+1)
          SI4 = S*TRIGS(4*KK+2)
          CO5 = TRIGS(5*KK+1)
          SI5 = S*TRIGS(5*KK+2)
          CO6 = TRIGS(6*KK+1)
          SI6 = S*TRIGS(6*KK+2)
          CO7 = TRIGS(7*KK+1)
          SI7 = S*TRIGS(7*KK+2)

          DO JJJ = K, (N-1)*INC, 8*JSTEP
            JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
            DO NU = 1, INQ
              JB = JA + JSTEPL
              IF (JB<ISTART) JB = JB + NINC
              JC = JB + JSTEPL
              IF (JC<ISTART) JC = JC + NINC
              JD = JC + JSTEPL
              IF (JD<ISTART) JD = JD + NINC
              JE = JD + JSTEPL
              IF (JE<ISTART) JE = JE + NINC
              JF = JE + JSTEPL
              IF (JF<ISTART) JF = JF + NINC
              JG = JF + JSTEPL
              IF (JG<ISTART) JG = JG + NINC
              JH = JG + JSTEPL
              IF (JH<ISTART) JH = JH + NINC
              J = 0
!dir$ ivdep, shortloop
              DO L = 1, NVEX
                T0 = A(JA+J) + A(JE+J)
                T2 = A(JA+J) - A(JE+J)
                T1 = A(JC+J) + A(JG+J)
                T3 = C1*(A(JC+J)-A(JG+J))
                U0 = B(JA+J) + B(JE+J)
                U2 = B(JA+J) - B(JE+J)
                U1 = B(JC+J) + B(JG+J)
                U3 = C1*(B(JC+J)-B(JG+J))
                A(JA+J) = T0 + T1
                B(JA+J) = U0 + U1
                A(JE+J) = CO4*(T0-T1) - SI4*(U0-U1)
                B(JE+J) = SI4*(T0-T1) + CO4*(U0-U1)
                A(JC+J) = CO2*(T2-U3) - SI2*(U2+T3)
                B(JC+J) = SI2*(T2-U3) + CO2*(U2+T3)
                A(JG+J) = CO6*(T2+U3) - SI6*(U2-T3)
                B(JG+J) = SI6*(T2+U3) + CO6*(U2-T3)
                T0 = A(JB+J) + A(JD+J)
                T2 = A(JB+J) - A(JD+J)
                T1 = A(JF+J) - A(JH+J)
                T3 = A(JF+J) + A(JH+J)
                U0 = B(JB+J) + B(JD+J)
                U2 = B(JB+J) - B(JD+J)
                U1 = B(JF+J) - B(JH+J)
                U3 = B(JF+J) + B(JH+J)
                A(JB+J) = CO1*(T0-U3) - SI1*(U0+T3)
                B(JB+J) = SI1*(T0-U3) + CO1*(U0+T3)
                A(JH+J) = CO7*(T0+U3) - SI7*(U0-T3)
                B(JH+J) = SI7*(T0+U3) + CO7*(U0-T3)
                A(JD+J) = CO3*(T2+U1) - SI3*(U2-T1)
                B(JD+J) = SI3*(T2+U1) + CO3*(U2-T1)
                A(JF+J) = CO5*(T2-U1) - SI5*(U2+T1)
                B(JF+J) = SI5*(T2-U1) + CO5*(U2+T1)
                J = J + JUMP
              END DO
              JA = JA + JSTEPX
              IF (JA<ISTART) JA = JA + NINC
            END DO
          END DO
          KK = KK + 2*LA
        END DO

        LA = 8*LA

!  loop on type II radix-4 passes
!  ------------------------------
400     CONTINUE
        MU = MOD(INQ,4)
        IF (ISIGN==-1) MU = 4 - MU
        SS = 1.0_WP
        IF (MU==3) SS = -1.0_WP

        DO IPASS = MH + 1, M
          JSTEP = (N*INC)/(4*LA)
          JSTEPL = JSTEP - NINC
          LAINCL = LA*INK - NINC

!  k=0 loop (no twiddle factors)
!  -----------------------------
          DO LL = 0, (LA-1)*INK, 4*JSTEP

            DO JJJ = LL, (N-1)*INC, 4*LA*INK
              JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
              DO NU = 1, INQ
                JB = JA + JSTEPL
                IF (JB<ISTART) JB = JB + NINC
                JC = JB + JSTEPL
                IF (JC<ISTART) JC = JC + NINC
                JD = JC + JSTEPL
                IF (JD<ISTART) JD = JD + NINC
                JE = JA + LAINCL
                IF (JE<ISTART) JE = JE + NINC
                JF = JE + JSTEPL
                IF (JF<ISTART) JF = JF + NINC
                JG = JF + JSTEPL
                IF (JG<ISTART) JG = JG + NINC
                JH = JG + JSTEPL
                IF (JH<ISTART) JH = JH + NINC
                JI = JE + LAINCL
                IF (JI<ISTART) JI = JI + NINC
                JJ = JI + JSTEPL
                IF (JJ<ISTART) JJ = JJ + NINC
                JK = JJ + JSTEPL
                IF (JK<ISTART) JK = JK + NINC
                JL = JK + JSTEPL
                IF (JL<ISTART) JL = JL + NINC
                JM = JI + LAINCL
                IF (JM<ISTART) JM = JM + NINC
                JN = JM + JSTEPL
                IF (JN<ISTART) JN = JN + NINC
                JO = JN + JSTEPL
                IF (JO<ISTART) JO = JO + NINC
                JP = JO + JSTEPL
                IF (JP<ISTART) JP = JP + NINC
                J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
                DO L = 1, NVEX
                  T0 = A(JA+J) + A(JC+J)
                  T2 = A(JA+J) - A(JC+J)
                  T1 = A(JB+J) + A(JD+J)
                  T3 = SS*(A(JB+J)-A(JD+J))
                  A(JC+J) = A(JI+J)
                  U0 = B(JA+J) + B(JC+J)
                  U2 = B(JA+J) - B(JC+J)
                  U1 = B(JB+J) + B(JD+J)
                  U3 = SS*(B(JB+J)-B(JD+J))
                  A(JB+J) = A(JE+J)
                  A(JA+J) = T0 + T1
                  A(JI+J) = T0 - T1
                  B(JA+J) = U0 + U1
                  B(JC+J) = U0 - U1
                  B(JD+J) = B(JM+J)
                  A(JE+J) = T2 - U3
                  A(JD+J) = T2 + U3
                  B(JB+J) = U2 + T3
                  B(JM+J) = U2 - T3
!----------------------
                  T0 = A(JB+J) + A(JG+J)
                  T2 = A(JB+J) - A(JG+J)
                  T1 = A(JF+J) + A(JH+J)
                  T3 = SS*(A(JF+J)-A(JH+J))
                  A(JG+J) = A(JJ+J)
                  U0 = B(JE+J) + B(JG+J)
                  U2 = B(JE+J) - B(JG+J)
                  U1 = B(JF+J) + B(JH+J)
                  U3 = SS*(B(JF+J)-B(JH+J))
                  B(JE+J) = B(JB+J)
                  A(JB+J) = T0 + T1
                  A(JJ+J) = T0 - T1
                  B(JG+J) = B(JJ+J)
                  B(JB+J) = U0 + U1
                  B(JJ+J) = U0 - U1
                  A(JF+J) = T2 - U3
                  A(JH+J) = T2 + U3
                  B(JF+J) = U2 + T3
                  B(JH+J) = U2 - T3
!----------------------
                  T0 = A(JC+J) + A(JK+J)
                  T2 = A(JC+J) - A(JK+J)
                  T1 = A(JG+J) + A(JL+J)
                  T3 = SS*(A(JG+J)-A(JL+J))
                  U0 = B(JI+J) + B(JK+J)
                  U2 = B(JI+J) - B(JK+J)
                  A(JL+J) = A(JO+J)
                  U1 = B(JG+J) + B(JL+J)
                  U3 = SS*(B(JG+J)-B(JL+J))
                  B(JI+J) = B(JC+J)
                  A(JC+J) = T0 + T1
                  A(JK+J) = T0 - T1
                  B(JL+J) = B(JO+J)
                  B(JC+J) = U0 + U1
                  B(JK+J) = U0 - U1
                  A(JG+J) = T2 - U3
                  A(JO+J) = T2 + U3
                  B(JG+J) = U2 + T3
                  B(JO+J) = U2 - T3
!----------------------
                  T0 = A(JM+J) + A(JL+J)
                  T2 = A(JM+J) - A(JL+J)
                  T1 = A(JN+J) + A(JP+J)
                  T3 = SS*(A(JN+J)-A(JP+J))
                  A(JM+J) = A(JD+J)
                  U0 = B(JD+J) + B(JL+J)
                  U2 = B(JD+J) - B(JL+J)
                  U1 = B(JN+J) + B(JP+J)
                  U3 = SS*(B(JN+J)-B(JP+J))
                  A(JN+J) = A(JH+J)
                  A(JD+J) = T0 + T1
                  A(JL+J) = T0 - T1
                  B(JD+J) = U0 + U1
                  B(JL+J) = U0 - U1
                  B(JN+J) = B(JH+J)
                  A(JH+J) = T2 - U3
                  A(JP+J) = T2 + U3
                  B(JH+J) = U2 + T3
                  B(JP+J) = U2 - T3
                  J = J + JUMP
                END DO
!-----( end of loop across transforms )
                JA = JA + JSTEPX
                IF (JA<ISTART) JA = JA + NINC
              END DO
            END DO
          END DO
!-----( end of double loop for k=0 )

!  finished if last pass
!  ---------------------
          IF (IPASS==M) GO TO 490

          KK = 2*LA

!     loop on nonzero k
!     -----------------
          DO K = INK, JSTEP - INK, INK
            CO1 = TRIGS(KK+1)
            SI1 = S*TRIGS(KK+2)
            CO2 = TRIGS(2*KK+1)
            SI2 = S*TRIGS(2*KK+2)
            CO3 = TRIGS(3*KK+1)
            SI3 = S*TRIGS(3*KK+2)

!  double loop along first transform in block
!  ------------------------------------------
            DO LL = K, (LA-1)*INK, 4*JSTEP

              DO JJJ = LL, (N-1)*INC, 4*LA*INK
                JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
                DO NU = 1, INQ
                  JB = JA + JSTEPL
                  IF (JB<ISTART) JB = JB + NINC
                  JC = JB + JSTEPL
                  IF (JC<ISTART) JC = JC + NINC
                  JD = JC + JSTEPL
                  IF (JD<ISTART) JD = JD + NINC
                  JE = JA + LAINCL
                  IF (JE<ISTART) JE = JE + NINC
                  JF = JE + JSTEPL
                  IF (JF<ISTART) JF = JF + NINC
                  JG = JF + JSTEPL
                  IF (JG<ISTART) JG = JG + NINC
                  JH = JG + JSTEPL
                  IF (JH<ISTART) JH = JH + NINC
                  JI = JE + LAINCL
                  IF (JI<ISTART) JI = JI + NINC
                  JJ = JI + JSTEPL
                  IF (JJ<ISTART) JJ = JJ + NINC
                  JK = JJ + JSTEPL
                  IF (JK<ISTART) JK = JK + NINC
                  JL = JK + JSTEPL
                  IF (JL<ISTART) JL = JL + NINC
                  JM = JI + LAINCL
                  IF (JM<ISTART) JM = JM + NINC
                  JN = JM + JSTEPL
                  IF (JN<ISTART) JN = JN + NINC
                  JO = JN + JSTEPL
                  IF (JO<ISTART) JO = JO + NINC
                  JP = JO + JSTEPL
                  IF (JP<ISTART) JP = JP + NINC
                  J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
                  DO L = 1, NVEX
                    T0 = A(JA+J) + A(JC+J)
                    T2 = A(JA+J) - A(JC+J)
                    T1 = A(JB+J) + A(JD+J)
                    T3 = SS*(A(JB+J)-A(JD+J))
                    A(JC+J) = A(JI+J)
                    U0 = B(JA+J) + B(JC+J)
                    U2 = B(JA+J) - B(JC+J)
                    U1 = B(JB+J) + B(JD+J)
                    U3 = SS*(B(JB+J)-B(JD+J))
                    A(JB+J) = A(JE+J)
                    A(JA+J) = T0 + T1
                    B(JA+J) = U0 + U1
                    A(JE+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JB+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    B(JD+J) = B(JM+J)
                    A(JI+J) = CO2*(T0-T1) - SI2*(U0-U1)
                    B(JC+J) = SI2*(T0-T1) + CO2*(U0-U1)
                    A(JD+J) = CO3*(T2+U3) - SI3*(U2-T3)
                    B(JM+J) = SI3*(T2+U3) + CO3*(U2-T3)
!----------------------------------------
                    T0 = A(JB+J) + A(JG+J)
                    T2 = A(JB+J) - A(JG+J)
                    T1 = A(JF+J) + A(JH+J)
                    T3 = SS*(A(JF+J)-A(JH+J))
                    A(JG+J) = A(JJ+J)
                    U0 = B(JE+J) + B(JG+J)
                    U2 = B(JE+J) - B(JG+J)
                    U1 = B(JF+J) + B(JH+J)
                    U3 = SS*(B(JF+J)-B(JH+J))
                    B(JE+J) = B(JB+J)
                    A(JB+J) = T0 + T1
                    B(JB+J) = U0 + U1
                    B(JG+J) = B(JJ+J)
                    A(JF+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JF+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    A(JJ+J) = CO2*(T0-T1) - SI2*(U0-U1)
                    B(JJ+J) = SI2*(T0-T1) + CO2*(U0-U1)
                    A(JH+J) = CO3*(T2+U3) - SI3*(U2-T3)
                    B(JH+J) = SI3*(T2+U3) + CO3*(U2-T3)
!----------------------------------------
                    T0 = A(JC+J) + A(JK+J)
                    T2 = A(JC+J) - A(JK+J)
                    T1 = A(JG+J) + A(JL+J)
                    T3 = SS*(A(JG+J)-A(JL+J))
                    U0 = B(JI+J) + B(JK+J)
                    U2 = B(JI+J) - B(JK+J)
                    A(JL+J) = A(JO+J)
                    U1 = B(JG+J) + B(JL+J)
                    U3 = SS*(B(JG+J)-B(JL+J))
                    B(JI+J) = B(JC+J)
                    A(JC+J) = T0 + T1
                    B(JC+J) = U0 + U1
                    B(JL+J) = B(JO+J)
                    A(JG+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JG+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    A(JK+J) = CO2*(T0-T1) - SI2*(U0-U1)
                    B(JK+J) = SI2*(T0-T1) + CO2*(U0-U1)
                    A(JO+J) = CO3*(T2+U3) - SI3*(U2-T3)
                    B(JO+J) = SI3*(T2+U3) + CO3*(U2-T3)
!----------------------------------------
                    T0 = A(JM+J) + A(JL+J)
                    T2 = A(JM+J) - A(JL+J)
                    T1 = A(JN+J) + A(JP+J)
                    T3 = SS*(A(JN+J)-A(JP+J))
                    A(JM+J) = A(JD+J)
                    U0 = B(JD+J) + B(JL+J)
                    U2 = B(JD+J) - B(JL+J)
                    A(JN+J) = A(JH+J)
                    U1 = B(JN+J) + B(JP+J)
                    U3 = SS*(B(JN+J)-B(JP+J))
                    B(JN+J) = B(JH+J)
                    A(JD+J) = T0 + T1
                    B(JD+J) = U0 + U1
                    A(JH+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JH+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    A(JL+J) = CO2*(T0-T1) - SI2*(U0-U1)
                    B(JL+J) = SI2*(T0-T1) + CO2*(U0-U1)
                    A(JP+J) = CO3*(T2+U3) - SI3*(U2-T3)
                    B(JP+J) = SI3*(T2+U3) + CO3*(U2-T3)
                    J = J + JUMP
                  END DO
!-----(end of loop across transforms)
                  JA = JA + JSTEPX
                  IF (JA<ISTART) JA = JA + NINC
                END DO
              END DO
            END DO
!-----( end of double loop for this k )
            KK = KK + 2*LA
          END DO
!-----( end of loop over values of k )
          LA = 4*LA
        END DO
!-----( end of loop on type II radix-4 passes )
!-----( nvex transforms completed)
490     CONTINUE
        ISTART = ISTART + NVEX*JUMP
      END DO
!-----( end of loop on blocks of transforms )

      RETURN
    END SUBROUTINE GPFA2F
!     fortran version of *gpfa3* -
!     radix-3 section of self-sorting, in-place
!        generalized PFA

!-------------------------------------------------------------------

    SUBROUTINE GPFA3F(A,B,TRIGS,INC,JUMP,N,MM,LOT,ISIGN)
      USE DDPRECISION, ONLY : WP
      REAL (WP) :: C1, B, A, SI2, CO2, SI1, SIN60, CO1, U3, U1, U2, T3, T1, &
        T2, S, TRIGS
      DIMENSION A(*), B(*), TRIGS(*)
      DATA SIN60/0.866025403784437_WP/
      DATA LVR/64/

!     ***************************************************************
!     *                                                             *
!     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. *
!     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE *
!     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      *
!     *                                                             *
!     ***************************************************************

      N3 = 3**MM
      INQ = N/N3
      JSTEPX = (N3-N)*INC
      NINC = N*INC
      INK = INC*INQ
      MU = MOD(INQ,3)
      IF (ISIGN==-1) MU = 3 - MU
      M = MM
      MH = (M+1)/2
      S = REAL(ISIGN,KIND=WP)
      C1 = SIN60
      IF (MU==2) C1 = -C1

      NBLOX = 1 + (LOT-1)/LVR
      LEFT = LOT
      S = REAL(ISIGN,KIND=WP)
      ISTART = 1

!  loop on blocks of lvr transforms
!  --------------------------------
      DO NB = 1, NBLOX

        IF (LEFT<=LVR) THEN
          NVEX = LEFT
        ELSE IF (LEFT<(2*LVR)) THEN
          NVEX = LEFT/2
          NVEX = NVEX + MOD(NVEX,2)
        ELSE
          NVEX = LVR
        END IF
        LEFT = LEFT - NVEX

        LA = 1

!  loop on type I radix-3 passes
!  -----------------------------
        DO IPASS = 1, MH
          JSTEP = (N*INC)/(3*LA)
          JSTEPL = JSTEP - NINC

!  k = 0 loop (no twiddle factors)
!  -------------------------------
          DO JJJ = 0, (N-1)*INC, 3*JSTEP
            JA = ISTART + JJJ

!  "transverse" loop
!  -----------------
            DO NU = 1, INQ
              JB = JA + JSTEPL
              IF (JB<ISTART) JB = JB + NINC
              JC = JB + JSTEPL
              IF (JC<ISTART) JC = JC + NINC
              J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
              DO L = 1, NVEX
                T1 = A(JB+J) + A(JC+J)
                T2 = A(JA+J) - 0.5_WP*T1
                T3 = C1*(A(JB+J)-A(JC+J))
                U1 = B(JB+J) + B(JC+J)
                U2 = B(JA+J) - 0.5_WP*U1
                U3 = C1*(B(JB+J)-B(JC+J))
                A(JA+J) = A(JA+J) + T1
                B(JA+J) = B(JA+J) + U1
                A(JB+J) = T2 - U3
                B(JB+J) = U2 + T3
                A(JC+J) = T2 + U3
                B(JC+J) = U2 - T3
                J = J + JUMP
              END DO
              JA = JA + JSTEPX
              IF (JA<ISTART) JA = JA + NINC
            END DO
          END DO

!  finished if n3 = 3
!  ------------------
          IF (N3==3) GO TO 490
          KK = 2*LA

!  loop on nonzero k
!  -----------------
          DO K = INK, JSTEP - INK, INK
            CO1 = TRIGS(KK+1)
            SI1 = S*TRIGS(KK+2)
            CO2 = TRIGS(2*KK+1)
            SI2 = S*TRIGS(2*KK+2)

!  loop along transform
!  --------------------
            DO JJJ = K, (N-1)*INC, 3*JSTEP
              JA = ISTART + JJJ

!  "transverse" loop
!  -----------------
              DO NU = 1, INQ
                JB = JA + JSTEPL
                IF (JB<ISTART) JB = JB + NINC
                JC = JB + JSTEPL
                IF (JC<ISTART) JC = JC + NINC
                J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep,shortloop
                DO L = 1, NVEX
                  T1 = A(JB+J) + A(JC+J)
                  T2 = A(JA+J) - 0.5_WP*T1
                  T3 = C1*(A(JB+J)-A(JC+J))
                  U1 = B(JB+J) + B(JC+J)
                  U2 = B(JA+J) - 0.5_WP*U1
                  U3 = C1*(B(JB+J)-B(JC+J))
                  A(JA+J) = A(JA+J) + T1
                  B(JA+J) = B(JA+J) + U1
                  A(JB+J) = CO1*(T2-U3) - SI1*(U2+T3)
                  B(JB+J) = SI1*(T2-U3) + CO1*(U2+T3)
                  A(JC+J) = CO2*(T2+U3) - SI2*(U2-T3)
                  B(JC+J) = SI2*(T2+U3) + CO2*(U2-T3)
                  J = J + JUMP
                END DO
!-----( end of loop across transforms )
                JA = JA + JSTEPX
                IF (JA<ISTART) JA = JA + NINC
              END DO
            END DO
!-----( end of loop along transforms )
            KK = KK + 2*LA
          END DO
!-----( end of loop on nonzero k )
          LA = 3*LA
        END DO
!-----( end of loop on type I radix-3 passes)

!  loop on type II radix-3 passes
!  ------------------------------
400     CONTINUE

        DO IPASS = MH + 1, M
          JSTEP = (N*INC)/(3*LA)
          JSTEPL = JSTEP - NINC
          LAINCL = LA*INK - NINC

!  k=0 loop (no twiddle factors)
!  -----------------------------
          DO LL = 0, (LA-1)*INK, 3*JSTEP

            DO JJJ = LL, (N-1)*INC, 3*LA*INK
              JA = ISTART + JJJ

!  "transverse" loop
!  -----------------
              DO NU = 1, INQ
                JB = JA + JSTEPL
                IF (JB<ISTART) JB = JB + NINC
                JC = JB + JSTEPL
                IF (JC<ISTART) JC = JC + NINC
                JD = JA + LAINCL
                IF (JD<ISTART) JD = JD + NINC
                JE = JD + JSTEPL
                IF (JE<ISTART) JE = JE + NINC
                JF = JE + JSTEPL
                IF (JF<ISTART) JF = JF + NINC
                JG = JD + LAINCL
                IF (JG<ISTART) JG = JG + NINC
                JH = JG + JSTEPL
                IF (JH<ISTART) JH = JH + NINC
                JI = JH + JSTEPL
                IF (JI<ISTART) JI = JI + NINC
                J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
                DO L = 1, NVEX
                  T1 = A(JB+J) + A(JC+J)
                  T2 = A(JA+J) - 0.5_WP*T1
                  T3 = C1*(A(JB+J)-A(JC+J))
                  A(JB+J) = A(JD+J)
                  U1 = B(JB+J) + B(JC+J)
                  U2 = B(JA+J) - 0.5_WP*U1
                  U3 = C1*(B(JB+J)-B(JC+J))
                  B(JB+J) = B(JD+J)
                  A(JA+J) = A(JA+J) + T1
                  B(JA+J) = B(JA+J) + U1
                  A(JD+J) = T2 - U3
                  B(JD+J) = U2 + T3
                  A(JC+J) = T2 + U3
                  B(JC+J) = U2 - T3
!----------------------
                  T1 = A(JE+J) + A(JF+J)
                  T2 = A(JB+J) - 0.5_WP*T1
                  T3 = C1*(A(JE+J)-A(JF+J))
                  A(JF+J) = A(JH+J)
                  U1 = B(JE+J) + B(JF+J)
                  U2 = B(JB+J) - 0.5_WP*U1
                  U3 = C1*(B(JE+J)-B(JF+J))
                  B(JF+J) = B(JH+J)
                  A(JB+J) = A(JB+J) + T1
                  B(JB+J) = B(JB+J) + U1
                  A(JE+J) = T2 - U3
                  B(JE+J) = U2 + T3
                  A(JH+J) = T2 + U3
                  B(JH+J) = U2 - T3
!----------------------
                  T1 = A(JF+J) + A(JI+J)
                  T2 = A(JG+J) - 0.5_WP*T1
                  T3 = C1*(A(JF+J)-A(JI+J))
                  T1 = A(JG+J) + T1
                  A(JG+J) = A(JC+J)
                  U1 = B(JF+J) + B(JI+J)
                  U2 = B(JG+J) - 0.5_WP*U1
                  U3 = C1*(B(JF+J)-B(JI+J))
                  U1 = B(JG+J) + U1
                  B(JG+J) = B(JC+J)
                  A(JC+J) = T1
                  B(JC+J) = U1
                  A(JF+J) = T2 - U3
                  B(JF+J) = U2 + T3
                  A(JI+J) = T2 + U3
                  B(JI+J) = U2 - T3
                  J = J + JUMP
                END DO
!-----( end of loop across transforms )
                JA = JA + JSTEPX
                IF (JA<ISTART) JA = JA + NINC
              END DO
            END DO
          END DO
!-----( end of double loop for k=0 )

!  finished if last pass
!  ---------------------
          IF (IPASS==M) GO TO 490

          KK = 2*LA

!     loop on nonzero k
!     -----------------
          DO K = INK, JSTEP - INK, INK
            CO1 = TRIGS(KK+1)
            SI1 = S*TRIGS(KK+2)
            CO2 = TRIGS(2*KK+1)
            SI2 = S*TRIGS(2*KK+2)

!  double loop along first transform in block
!  ------------------------------------------
            DO LL = K, (LA-1)*INK, 3*JSTEP

              DO JJJ = LL, (N-1)*INC, 3*LA*INK
                JA = ISTART + JJJ

!  "transverse" loop
!  -----------------
                DO NU = 1, INQ
                  JB = JA + JSTEPL
                  IF (JB<ISTART) JB = JB + NINC
                  JC = JB + JSTEPL
                  IF (JC<ISTART) JC = JC + NINC
                  JD = JA + LAINCL
                  IF (JD<ISTART) JD = JD + NINC
                  JE = JD + JSTEPL
                  IF (JE<ISTART) JE = JE + NINC
                  JF = JE + JSTEPL
                  IF (JF<ISTART) JF = JF + NINC
                  JG = JD + LAINCL
                  IF (JG<ISTART) JG = JG + NINC
                  JH = JG + JSTEPL
                  IF (JH<ISTART) JH = JH + NINC
                  JI = JH + JSTEPL
                  IF (JI<ISTART) JI = JI + NINC
                  J = 0

!  loop across transforms
!  ----------------------
!dir$ ivdep, shortloop
                  DO L = 1, NVEX
                    T1 = A(JB+J) + A(JC+J)
                    T2 = A(JA+J) - 0.5_WP*T1
                    T3 = C1*(A(JB+J)-A(JC+J))
                    A(JB+J) = A(JD+J)
                    U1 = B(JB+J) + B(JC+J)
                    U2 = B(JA+J) - 0.5_WP*U1
                    U3 = C1*(B(JB+J)-B(JC+J))
                    B(JB+J) = B(JD+J)
                    A(JA+J) = A(JA+J) + T1
                    B(JA+J) = B(JA+J) + U1
                    A(JD+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JD+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    A(JC+J) = CO2*(T2+U3) - SI2*(U2-T3)
                    B(JC+J) = SI2*(T2+U3) + CO2*(U2-T3)
!----------------------
                    T1 = A(JE+J) + A(JF+J)
                    T2 = A(JB+J) - 0.5_WP*T1
                    T3 = C1*(A(JE+J)-A(JF+J))
                    A(JF+J) = A(JH+J)
                    U1 = B(JE+J) + B(JF+J)
                    U2 = B(JB+J) - 0.5_WP*U1
                    U3 = C1*(B(JE+J)-B(JF+J))
                    B(JF+J) = B(JH+J)
                    A(JB+J) = A(JB+J) + T1
                    B(JB+J) = B(JB+J) + U1
                    A(JE+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JE+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    A(JH+J) = CO2*(T2+U3) - SI2*(U2-T3)
                    B(JH+J) = SI2*(T2+U3) + CO2*(U2-T3)
!----------------------
                    T1 = A(JF+J) + A(JI+J)
                    T2 = A(JG+J) - 0.5_WP*T1
                    T3 = C1*(A(JF+J)-A(JI+J))
                    T1 = A(JG+J) + T1
                    A(JG+J) = A(JC+J)
                    U1 = B(JF+J) + B(JI+J)
                    U2 = B(JG+J) - 0.5_WP*U1
                    U3 = C1*(B(JF+J)-B(JI+J))
                    U1 = B(JG+J) + U1
                    B(JG+J) = B(JC+J)
                    A(JC+J) = T1
                    B(JC+J) = U1
                    A(JF+J) = CO1*(T2-U3) - SI1*(U2+T3)
                    B(JF+J) = SI1*(T2-U3) + CO1*(U2+T3)
                    A(JI+J) = CO2*(T2+U3) - SI2*(U2-T3)
                    B(JI+J) = SI2*(T2+U3) + CO2*(U2-T3)
                    J = J + JUMP
                  END DO
!-----(end of loop across transforms)
                  JA = JA + JSTEPX
                  IF (JA<ISTART) JA = JA + NINC
                END DO
              END DO
            END DO
!-----( end of double loop for this k )
            KK = KK + 2*LA
          END DO
!-----( end of loop over values of k )
          LA = 3*LA
        END DO
!-----( end of loop on type II radix-3 passes )
!-----( nvex transforms completed)
490     CONTINUE
        ISTART = ISTART + NVEX*JUMP
      END DO
!-----( end of loop on blocks of transforms )

      RETURN
    END SUBROUTINE GPFA3F
!     fortran version of *gpfa5* -
!     radix-5 section of self-sorting, in-place,
!        generalized pfa

!-------------------------------------------------------------------

    SUBROUTINE GPFA5F(A,B,TRIGS,INC,JUMP,N,MM,LOT,ISIGN)
      USE DDPRECISION, ONLY : WP
      REAL (WP) :: BX, AX, U11, U10, U9, U8, U7, U6, U5, U4, T11, T10, T9, T8, &
        T7, T6, T5, T4, QRT5, SIN72, SIN36, SI4, CO4, C3, C2, C1, SI3, CO3, &
        SI2, CO2, SI1, CO1, U3, U1, U2, T3, T1, T2, S, B, A, TRIGS
      DIMENSION A(*), B(*), TRIGS(*)
      DATA SIN36/0.587785252292473_WP/, SIN72/0.951056516295154_WP/, &
        QRT5/0.559016994374947_WP/
      DATA LVR/64/

!     ***************************************************************
!     *                                                             *
!     *  N.B. LVR = LENGTH OF VECTOR REGISTERS, SET TO 128 FOR C90. *
!     *  RESET TO 64 FOR OTHER CRAY MACHINES, OR TO ANY LARGE VALUE *
!     *  (GREATER THAN OR EQUAL TO LOT) FOR A SCALAR COMPUTER.      *
!     *                                                             *
!     ***************************************************************

      N5 = 5**MM
      INQ = N/N5
      JSTEPX = (N5-N)*INC
      NINC = N*INC
      INK = INC*INQ
      MU = MOD(INQ,5)
      IF (ISIGN==-1) MU = 5 - MU

      M = MM
      MH = (M+1)/2
      S = REAL(ISIGN,KIND=WP)
      C1 = QRT5
      C2 = SIN72
      C3 = SIN36
      IF (MU==2 .OR. MU==3) THEN
        C1 = -C1
        C2 = SIN36
        C3 = SIN72
      END IF
      IF (MU==3 .OR. MU==4) C2 = -C2
      IF (MU==2 .OR. MU==4) C3 = -C3

      NBLOX = 1 + (LOT-1)/LVR
      LEFT = LOT
      S = REAL(ISIGN,KIND=WP)
      ISTART = 1

!  loop on blocks of lvr transforms
!  --------------------------------
      DO NB = 1, NBLOX

        IF (LEFT<=LVR) THEN
          NVEX = LEFT
        ELSE IF (LEFT<(2*LVR)) THEN
          NVEX = LEFT/2
          NVEX = NVEX + MOD(NVEX,2)
        ELSE
          NVEX = LVR
        END IF
        LEFT = LEFT - NVEX

        LA = 1

!  loop on type I radix-5 passes
!  -----------------------------
        DO IPASS = 1, MH
          JSTEP = (N*INC)/(5*LA)
          JSTEPL = JSTEP - NINC
          KK = 0

!  loop on k
!  ---------
          DO K = 0, JSTEP - INK, INK

            IF (K>0) THEN
              CO1 = TRIGS(KK+1)
              SI1 = S*TRIGS(KK+2)
              CO2 = TRIGS(2*KK+1)
              SI2 = S*TRIGS(2*KK+2)
              CO3 = TRIGS(3*KK+1)
              SI3 = S*TRIGS(3*KK+2)
              CO4 = TRIGS(4*KK+1)
              SI4 = S*TRIGS(4*KK+2)
            END IF

!  loop along transform
!  --------------------
            DO JJJ = K, (N-1)*INC, 5*JSTEP
              JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
              DO NU = 1, INQ
                JB = JA + JSTEPL
                IF (JB<ISTART) JB = JB + NINC
                JC = JB + JSTEPL
                IF (JC<ISTART) JC = JC + NINC
                JD = JC + JSTEPL
                IF (JD<ISTART) JD = JD + NINC
                JE = JD + JSTEPL
                IF (JE<ISTART) JE = JE + NINC
                J = 0

!  loop across transforms
!  ----------------------
                IF (K==0) THEN

!dir$ ivdep, shortloop
                  DO L = 1, NVEX
                    T1 = A(JB+J) + A(JE+J)
                    T2 = A(JC+J) + A(JD+J)
                    T3 = A(JB+J) - A(JE+J)
                    T4 = A(JC+J) - A(JD+J)
                    T5 = T1 + T2
                    T6 = C1*(T1-T2)
                    T7 = A(JA+J) - 0.25_WP*T5
                    A(JA+J) = A(JA+J) + T5
                    T8 = T7 + T6
                    T9 = T7 - T6
                    T10 = C3*T3 - C2*T4
                    T11 = C2*T3 + C3*T4
                    U1 = B(JB+J) + B(JE+J)
                    U2 = B(JC+J) + B(JD+J)
                    U3 = B(JB+J) - B(JE+J)
                    U4 = B(JC+J) - B(JD+J)
                    U5 = U1 + U2
                    U6 = C1*(U1-U2)
                    U7 = B(JA+J) - 0.25_WP*U5
                    B(JA+J) = B(JA+J) + U5
                    U8 = U7 + U6
                    U9 = U7 - U6
                    U10 = C3*U3 - C2*U4
                    U11 = C2*U3 + C3*U4
                    A(JB+J) = T8 - U11
                    B(JB+J) = U8 + T11
                    A(JE+J) = T8 + U11
                    B(JE+J) = U8 - T11
                    A(JC+J) = T9 - U10
                    B(JC+J) = U9 + T10
                    A(JD+J) = T9 + U10
                    B(JD+J) = U9 - T10
                    J = J + JUMP
                  END DO

                ELSE

!dir$ ivdep,shortloop
                  DO L = 1, NVEX
                    T1 = A(JB+J) + A(JE+J)
                    T2 = A(JC+J) + A(JD+J)
                    T3 = A(JB+J) - A(JE+J)
                    T4 = A(JC+J) - A(JD+J)
                    T5 = T1 + T2
                    T6 = C1*(T1-T2)
                    T7 = A(JA+J) - 0.25_WP*T5
                    A(JA+J) = A(JA+J) + T5
                    T8 = T7 + T6
                    T9 = T7 - T6
                    T10 = C3*T3 - C2*T4
                    T11 = C2*T3 + C3*T4
                    U1 = B(JB+J) + B(JE+J)
                    U2 = B(JC+J) + B(JD+J)
                    U3 = B(JB+J) - B(JE+J)
                    U4 = B(JC+J) - B(JD+J)
                    U5 = U1 + U2
                    U6 = C1*(U1-U2)
                    U7 = B(JA+J) - 0.25_WP*U5
                    B(JA+J) = B(JA+J) + U5
                    U8 = U7 + U6
                    U9 = U7 - U6
                    U10 = C3*U3 - C2*U4
                    U11 = C2*U3 + C3*U4
                    A(JB+J) = CO1*(T8-U11) - SI1*(U8+T11)
                    B(JB+J) = SI1*(T8-U11) + CO1*(U8+T11)
                    A(JE+J) = CO4*(T8+U11) - SI4*(U8-T11)
                    B(JE+J) = SI4*(T8+U11) + CO4*(U8-T11)
                    A(JC+J) = CO2*(T9-U10) - SI2*(U9+T10)
                    B(JC+J) = SI2*(T9-U10) + CO2*(U9+T10)
                    A(JD+J) = CO3*(T9+U10) - SI3*(U9-T10)
                    B(JD+J) = SI3*(T9+U10) + CO3*(U9-T10)
                    J = J + JUMP
                  END DO

                END IF

!-----( end of loop across transforms )

                JA = JA + JSTEPX
                IF (JA<ISTART) JA = JA + NINC
              END DO
            END DO
!-----( end of loop along transforms )
            KK = KK + 2*LA
          END DO
!-----( end of loop on nonzero k )
          LA = 5*LA
        END DO
!-----( end of loop on type I radix-5 passes)

        IF (N==5) GO TO 490

!  loop on type II radix-5 passes
!  ------------------------------
400     CONTINUE

        DO IPASS = MH + 1, M
          JSTEP = (N*INC)/(5*LA)
          JSTEPL = JSTEP - NINC
          LAINCL = LA*INK - NINC
          KK = 0

!     loop on k
!     ---------
          DO K = 0, JSTEP - INK, INK

            IF (K>0) THEN
              CO1 = TRIGS(KK+1)
              SI1 = S*TRIGS(KK+2)
              CO2 = TRIGS(2*KK+1)
              SI2 = S*TRIGS(2*KK+2)
              CO3 = TRIGS(3*KK+1)
              SI3 = S*TRIGS(3*KK+2)
              CO4 = TRIGS(4*KK+1)
              SI4 = S*TRIGS(4*KK+2)
            END IF

!  double loop along first transform in block
!  ------------------------------------------
            DO LL = K, (LA-1)*INK, 5*JSTEP

              DO JJJ = LL, (N-1)*INC, 5*LA*INK
                JA = ISTART + JJJ

!     "transverse" loop
!     -----------------
                DO NU = 1, INQ
                  JB = JA + JSTEPL
                  IF (JB<ISTART) JB = JB + NINC
                  JC = JB + JSTEPL
                  IF (JC<ISTART) JC = JC + NINC
                  JD = JC + JSTEPL
                  IF (JD<ISTART) JD = JD + NINC
                  JE = JD + JSTEPL
                  IF (JE<ISTART) JE = JE + NINC
                  JF = JA + LAINCL
                  IF (JF<ISTART) JF = JF + NINC
                  JG = JF + JSTEPL
                  IF (JG<ISTART) JG = JG + NINC
                  JH = JG + JSTEPL
                  IF (JH<ISTART) JH = JH + NINC
                  JI = JH + JSTEPL
                  IF (JI<ISTART) JI = JI + NINC
                  JJ = JI + JSTEPL
                  IF (JJ<ISTART) JJ = JJ + NINC
                  JK = JF + LAINCL
                  IF (JK<ISTART) JK = JK + NINC
                  JL = JK + JSTEPL
                  IF (JL<ISTART) JL = JL + NINC
                  JM = JL + JSTEPL
                  IF (JM<ISTART) JM = JM + NINC
                  JN = JM + JSTEPL
                  IF (JN<ISTART) JN = JN + NINC
                  JO = JN + JSTEPL
                  IF (JO<ISTART) JO = JO + NINC
                  JP = JK + LAINCL
                  IF (JP<ISTART) JP = JP + NINC
                  JQ = JP + JSTEPL
                  IF (JQ<ISTART) JQ = JQ + NINC
                  JR = JQ + JSTEPL
                  IF (JR<ISTART) JR = JR + NINC
                  JS = JR + JSTEPL
                  IF (JS<ISTART) JS = JS + NINC
                  JT = JS + JSTEPL
                  IF (JT<ISTART) JT = JT + NINC
                  JU = JP + LAINCL
                  IF (JU<ISTART) JU = JU + NINC
                  JV = JU + JSTEPL
                  IF (JV<ISTART) JV = JV + NINC
                  JW = JV + JSTEPL
                  IF (JW<ISTART) JW = JW + NINC
                  JX = JW + JSTEPL
                  IF (JX<ISTART) JX = JX + NINC
                  JY = JX + JSTEPL
                  IF (JY<ISTART) JY = JY + NINC
                  J = 0

!  loop across transforms
!  ----------------------
                  IF (K==0) THEN

!dir$ ivdep, shortloop
                    DO L = 1, NVEX
                      T1 = A(JB+J) + A(JE+J)
                      T2 = A(JC+J) + A(JD+J)
                      T3 = A(JB+J) - A(JE+J)
                      T4 = A(JC+J) - A(JD+J)
                      A(JB+J) = A(JF+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JA+J) - 0.25_WP*T5
                      A(JA+J) = A(JA+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JC+J) = A(JK+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      U1 = B(JB+J) + B(JE+J)
                      U2 = B(JC+J) + B(JD+J)
                      U3 = B(JB+J) - B(JE+J)
                      U4 = B(JC+J) - B(JD+J)
                      B(JB+J) = B(JF+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JA+J) - 0.25_WP*U5
                      B(JA+J) = B(JA+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JC+J) = B(JK+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      A(JF+J) = T8 - U11
                      B(JF+J) = U8 + T11
                      A(JE+J) = T8 + U11
                      B(JE+J) = U8 - T11
                      A(JK+J) = T9 - U10
                      B(JK+J) = U9 + T10
                      A(JD+J) = T9 + U10
                      B(JD+J) = U9 - T10
!----------------------
                      T1 = A(JG+J) + A(JJ+J)
                      T2 = A(JH+J) + A(JI+J)
                      T3 = A(JG+J) - A(JJ+J)
                      T4 = A(JH+J) - A(JI+J)
                      A(JH+J) = A(JL+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JB+J) - 0.25_WP*T5
                      A(JB+J) = A(JB+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JI+J) = A(JQ+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      U1 = B(JG+J) + B(JJ+J)
                      U2 = B(JH+J) + B(JI+J)
                      U3 = B(JG+J) - B(JJ+J)
                      U4 = B(JH+J) - B(JI+J)
                      B(JH+J) = B(JL+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JB+J) - 0.25_WP*U5
                      B(JB+J) = B(JB+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JI+J) = B(JQ+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      A(JG+J) = T8 - U11
                      B(JG+J) = U8 + T11
                      A(JJ+J) = T8 + U11
                      B(JJ+J) = U8 - T11
                      A(JL+J) = T9 - U10
                      B(JL+J) = U9 + T10
                      A(JQ+J) = T9 + U10
                      B(JQ+J) = U9 - T10
!----------------------
                      T1 = A(JH+J) + A(JO+J)
                      T2 = A(JM+J) + A(JN+J)
                      T3 = A(JH+J) - A(JO+J)
                      T4 = A(JM+J) - A(JN+J)
                      A(JN+J) = A(JR+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JC+J) - 0.25_WP*T5
                      A(JC+J) = A(JC+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JO+J) = A(JW+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      U1 = B(JH+J) + B(JO+J)
                      U2 = B(JM+J) + B(JN+J)
                      U3 = B(JH+J) - B(JO+J)
                      U4 = B(JM+J) - B(JN+J)
                      B(JN+J) = B(JR+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JC+J) - 0.25_WP*U5
                      B(JC+J) = B(JC+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JO+J) = B(JW+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      A(JH+J) = T8 - U11
                      B(JH+J) = U8 + T11
                      A(JW+J) = T8 + U11
                      B(JW+J) = U8 - T11
                      A(JM+J) = T9 - U10
                      B(JM+J) = U9 + T10
                      A(JR+J) = T9 + U10
                      B(JR+J) = U9 - T10
!----------------------
                      T1 = A(JI+J) + A(JT+J)
                      T2 = A(JN+J) + A(JS+J)
                      T3 = A(JI+J) - A(JT+J)
                      T4 = A(JN+J) - A(JS+J)
                      A(JT+J) = A(JX+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JP+J) - 0.25_WP*T5
                      AX = A(JP+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JP+J) = A(JD+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      A(JD+J) = AX
                      U1 = B(JI+J) + B(JT+J)
                      U2 = B(JN+J) + B(JS+J)
                      U3 = B(JI+J) - B(JT+J)
                      U4 = B(JN+J) - B(JS+J)
                      B(JT+J) = B(JX+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JP+J) - 0.25_WP*U5
                      BX = B(JP+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JP+J) = B(JD+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      B(JD+J) = BX
                      A(JI+J) = T8 - U11
                      B(JI+J) = U8 + T11
                      A(JX+J) = T8 + U11
                      B(JX+J) = U8 - T11
                      A(JN+J) = T9 - U10
                      B(JN+J) = U9 + T10
                      A(JS+J) = T9 + U10
                      B(JS+J) = U9 - T10
!----------------------
                      T1 = A(JV+J) + A(JY+J)
                      T2 = A(JO+J) + A(JT+J)
                      T3 = A(JV+J) - A(JY+J)
                      T4 = A(JO+J) - A(JT+J)
                      A(JV+J) = A(JJ+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JU+J) - 0.25_WP*T5
                      AX = A(JU+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JU+J) = A(JE+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      A(JE+J) = AX
                      U1 = B(JV+J) + B(JY+J)
                      U2 = B(JO+J) + B(JT+J)
                      U3 = B(JV+J) - B(JY+J)
                      U4 = B(JO+J) - B(JT+J)
                      B(JV+J) = B(JJ+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JU+J) - 0.25_WP*U5
                      BX = B(JU+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JU+J) = B(JE+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      B(JE+J) = BX
                      A(JJ+J) = T8 - U11
                      B(JJ+J) = U8 + T11
                      A(JY+J) = T8 + U11
                      B(JY+J) = U8 - T11
                      A(JO+J) = T9 - U10
                      B(JO+J) = U9 + T10
                      A(JT+J) = T9 + U10
                      B(JT+J) = U9 - T10
                      J = J + JUMP
                    END DO

                  ELSE

!dir$ ivdep, shortloop
                    DO L = 1, NVEX
                      T1 = A(JB+J) + A(JE+J)
                      T2 = A(JC+J) + A(JD+J)
                      T3 = A(JB+J) - A(JE+J)
                      T4 = A(JC+J) - A(JD+J)
                      A(JB+J) = A(JF+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JA+J) - 0.25_WP*T5
                      A(JA+J) = A(JA+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JC+J) = A(JK+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      U1 = B(JB+J) + B(JE+J)
                      U2 = B(JC+J) + B(JD+J)
                      U3 = B(JB+J) - B(JE+J)
                      U4 = B(JC+J) - B(JD+J)
                      B(JB+J) = B(JF+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JA+J) - 0.25_WP*U5
                      B(JA+J) = B(JA+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JC+J) = B(JK+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      A(JF+J) = CO1*(T8-U11) - SI1*(U8+T11)
                      B(JF+J) = SI1*(T8-U11) + CO1*(U8+T11)
                      A(JE+J) = CO4*(T8+U11) - SI4*(U8-T11)
                      B(JE+J) = SI4*(T8+U11) + CO4*(U8-T11)
                      A(JK+J) = CO2*(T9-U10) - SI2*(U9+T10)
                      B(JK+J) = SI2*(T9-U10) + CO2*(U9+T10)
                      A(JD+J) = CO3*(T9+U10) - SI3*(U9-T10)
                      B(JD+J) = SI3*(T9+U10) + CO3*(U9-T10)
!----------------------
                      T1 = A(JG+J) + A(JJ+J)
                      T2 = A(JH+J) + A(JI+J)
                      T3 = A(JG+J) - A(JJ+J)
                      T4 = A(JH+J) - A(JI+J)
                      A(JH+J) = A(JL+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JB+J) - 0.25_WP*T5
                      A(JB+J) = A(JB+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JI+J) = A(JQ+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      U1 = B(JG+J) + B(JJ+J)
                      U2 = B(JH+J) + B(JI+J)
                      U3 = B(JG+J) - B(JJ+J)
                      U4 = B(JH+J) - B(JI+J)
                      B(JH+J) = B(JL+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JB+J) - 0.25_WP*U5
                      B(JB+J) = B(JB+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JI+J) = B(JQ+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      A(JG+J) = CO1*(T8-U11) - SI1*(U8+T11)
                      B(JG+J) = SI1*(T8-U11) + CO1*(U8+T11)
                      A(JJ+J) = CO4*(T8+U11) - SI4*(U8-T11)
                      B(JJ+J) = SI4*(T8+U11) + CO4*(U8-T11)
                      A(JL+J) = CO2*(T9-U10) - SI2*(U9+T10)
                      B(JL+J) = SI2*(T9-U10) + CO2*(U9+T10)
                      A(JQ+J) = CO3*(T9+U10) - SI3*(U9-T10)
                      B(JQ+J) = SI3*(T9+U10) + CO3*(U9-T10)
!----------------------
                      T1 = A(JH+J) + A(JO+J)
                      T2 = A(JM+J) + A(JN+J)
                      T3 = A(JH+J) - A(JO+J)
                      T4 = A(JM+J) - A(JN+J)
                      A(JN+J) = A(JR+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JC+J) - 0.25_WP*T5
                      A(JC+J) = A(JC+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JO+J) = A(JW+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      U1 = B(JH+J) + B(JO+J)
                      U2 = B(JM+J) + B(JN+J)
                      U3 = B(JH+J) - B(JO+J)
                      U4 = B(JM+J) - B(JN+J)
                      B(JN+J) = B(JR+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JC+J) - 0.25_WP*U5
                      B(JC+J) = B(JC+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JO+J) = B(JW+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      A(JH+J) = CO1*(T8-U11) - SI1*(U8+T11)
                      B(JH+J) = SI1*(T8-U11) + CO1*(U8+T11)
                      A(JW+J) = CO4*(T8+U11) - SI4*(U8-T11)
                      B(JW+J) = SI4*(T8+U11) + CO4*(U8-T11)
                      A(JM+J) = CO2*(T9-U10) - SI2*(U9+T10)
                      B(JM+J) = SI2*(T9-U10) + CO2*(U9+T10)
                      A(JR+J) = CO3*(T9+U10) - SI3*(U9-T10)
                      B(JR+J) = SI3*(T9+U10) + CO3*(U9-T10)
!----------------------
                      T1 = A(JI+J) + A(JT+J)
                      T2 = A(JN+J) + A(JS+J)
                      T3 = A(JI+J) - A(JT+J)
                      T4 = A(JN+J) - A(JS+J)
                      A(JT+J) = A(JX+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JP+J) - 0.25_WP*T5
                      AX = A(JP+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JP+J) = A(JD+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      A(JD+J) = AX
                      U1 = B(JI+J) + B(JT+J)
                      U2 = B(JN+J) + B(JS+J)
                      U3 = B(JI+J) - B(JT+J)
                      U4 = B(JN+J) - B(JS+J)
                      B(JT+J) = B(JX+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JP+J) - 0.25_WP*U5
                      BX = B(JP+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JP+J) = B(JD+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      B(JD+J) = BX
                      A(JI+J) = CO1*(T8-U11) - SI1*(U8+T11)
                      B(JI+J) = SI1*(T8-U11) + CO1*(U8+T11)
                      A(JX+J) = CO4*(T8+U11) - SI4*(U8-T11)
                      B(JX+J) = SI4*(T8+U11) + CO4*(U8-T11)
                      A(JN+J) = CO2*(T9-U10) - SI2*(U9+T10)
                      B(JN+J) = SI2*(T9-U10) + CO2*(U9+T10)
                      A(JS+J) = CO3*(T9+U10) - SI3*(U9-T10)
                      B(JS+J) = SI3*(T9+U10) + CO3*(U9-T10)
!----------------------
                      T1 = A(JV+J) + A(JY+J)
                      T2 = A(JO+J) + A(JT+J)
                      T3 = A(JV+J) - A(JY+J)
                      T4 = A(JO+J) - A(JT+J)
                      A(JV+J) = A(JJ+J)
                      T5 = T1 + T2
                      T6 = C1*(T1-T2)
                      T7 = A(JU+J) - 0.25_WP*T5
                      AX = A(JU+J) + T5
                      T8 = T7 + T6
                      T9 = T7 - T6
                      A(JU+J) = A(JE+J)
                      T10 = C3*T3 - C2*T4
                      T11 = C2*T3 + C3*T4
                      A(JE+J) = AX
                      U1 = B(JV+J) + B(JY+J)
                      U2 = B(JO+J) + B(JT+J)
                      U3 = B(JV+J) - B(JY+J)
                      U4 = B(JO+J) - B(JT+J)
                      B(JV+J) = B(JJ+J)
                      U5 = U1 + U2
                      U6 = C1*(U1-U2)
                      U7 = B(JU+J) - 0.25_WP*U5
                      BX = B(JU+J) + U5
                      U8 = U7 + U6
                      U9 = U7 - U6
                      B(JU+J) = B(JE+J)
                      U10 = C3*U3 - C2*U4
                      U11 = C2*U3 + C3*U4
                      B(JE+J) = BX
                      A(JJ+J) = CO1*(T8-U11) - SI1*(U8+T11)
                      B(JJ+J) = SI1*(T8-U11) + CO1*(U8+T11)
                      A(JY+J) = CO4*(T8+U11) - SI4*(U8-T11)
                      B(JY+J) = SI4*(T8+U11) + CO4*(U8-T11)
                      A(JO+J) = CO2*(T9-U10) - SI2*(U9+T10)
                      B(JO+J) = SI2*(T9-U10) + CO2*(U9+T10)
                      A(JT+J) = CO3*(T9+U10) - SI3*(U9-T10)
                      B(JT+J) = SI3*(T9+U10) + CO3*(U9-T10)
                      J = J + JUMP
                    END DO

                  END IF

!-----(end of loop across transforms)

                  JA = JA + JSTEPX
                  IF (JA<ISTART) JA = JA + NINC
                END DO
              END DO
            END DO
!-----( end of double loop for this k )
            KK = KK + 2*LA
          END DO
!-----( end of loop over values of k )
          LA = 5*LA
        END DO
!-----( end of loop on type II radix-5 passes )
!-----( nvex transforms completed)
490     CONTINUE
        ISTART = ISTART + NVEX*JUMP
      END DO
!-----( end of loop on blocks of transforms )

      RETURN
    END SUBROUTINE GPFA5F
