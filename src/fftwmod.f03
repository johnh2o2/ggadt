MODULE FFTW
    USE, INTRINSIC :: ISO_C_BINDING
       INCLUDE 'fftw3.f03'
    LOGICAL :: FIRST_TIME = .TRUE.
    TYPE(C_PTR) :: PLAN
    INTEGER ::  MODE= FFTW_ESTIMATE
    CHARACTER(len=100) :: PLAN_FILENAME
CONTAINS

FUNCTION FFT(F,X,Y)
	
    REAL, INTENT(IN) :: X(:), Y(:)
    COMPLEX(C_DOUBLE_COMPLEX), INTENT(INOUT) :: F(:,:)
    COMPLEX(C_DOUBLE_COMPLEX), DIMENSION(SIZE(X),SIZE(Y)) :: FFT
    INTEGER :: NX, NY, I, J
    INTEGER :: ERROR


    NX = SIZE(X)
    NY = SIZE(Y)
    IF (FIRST_TIME) THEN
        WRITE(0,*) "Finding best FFT algo..."
        write(PLAN_FILENAME,'(A,I0.4,A,I0.4,A,I0.3,A,A)') "plan_nx",NX,"_ny",NY,"_fftw_mode",MODE,".plan",CHAR(0)
        ERROR = fftw_import_wisdom_from_filename(PLAN_FILENAME)
        IF (ERROR == 0) THEN
            write (0,*) "No previous wisdom detected, searching for optimal fft algo."

            PLAN = FFTW_PLAN_DFT_2D(NY, NX, F ,FFT, FFTW_BACKWARD,MODE)
            ERROR = FFTW_EXPORT_WISDOM_TO_FILENAME(PLAN_FILENAME)
            IF (ERROR == 0) THEN
                write (0,*) "ERROR: Couldn't save plan to ",PLAN_FILENAME
            ELSE
                write (0,*) "Successfully saved plan to ",PLAN_FILENAME
            ENDIF
        ELSE
            write (0,*) "Found and loaded previous wisdom from '",PLAN_FILENAME,"'"
            PLAN = FFTW_PLAN_DFT_2D(NY, NX, F ,FFT, FFTW_BACKWARD,FFTW_WISDOM_ONLY)
        END IF
        WRITE(0,*) "[done]."
        FIRST_TIME = .FALSE.
    END IF
    !write (0,*) "About to do FFT"
    !PLAN = FFTW_PLAN_DFT_2D(NY, NX, F ,FFT, FFTW_BACKWARD,FFTW_PATIENT)
    CALL FFTW_EXECUTE_DFT(PLAN, F, FFT)
END FUNCTION FFT

END MODULE FFTW