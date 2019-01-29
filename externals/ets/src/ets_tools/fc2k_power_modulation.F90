! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine provides power array and time array  !
  !     for power modulation                              !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for ETS                     !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE FC2K_POWER_MODULATION(POWER_IN,  NLEVEL, TSTART, TEND, FREQUENCY, TAU, POWER_OUT, TIMES_OUT)                      

    USE ITM_CONSTANTS


    IMPLICIT NONE


! +++ i/o parameters:
    REAL (R8), INTENT (IN)     :: POWER_IN(1000)      
    REAL (R8), INTENT (OUT)    :: POWER_OUT(1000)      
    REAL (R8), INTENT (OUT)    :: TIMES_OUT(1000)      
    REAL (R8), INTENT (IN)     :: TSTART, TEND, FREQUENCY, TAU     
    REAL (R8)                  :: TSTEP     

    INTEGER                    :: I, K
    INTEGER                    :: NSTEP,  NLEVEL


!+++++++++++++++++++++++++++++++++++++++++

! +++ Number of steps:
    NSTEP               = INT((TEND-TSTART)*FREQUENCY)
    K                   = 0
    TSTEP               = (TEND-TSTART)/(NSTEP)

    TIMES_OUT           = 1.D10

    DO I = 1, MIN(500,NSTEP)
       K                = K+1

       TIMES_OUT(I*2-1) = TSTART + TSTEP*(I-1)
       TIMES_OUT(I*2)   = TSTART + TSTEP*I - MIN(TAU,TSTEP)*0.01_R8

       POWER_OUT(I*2-1) = POWER_IN(K)
       POWER_OUT(I*2)   = POWER_IN(K)

       IF (K.GE.NLEVEL) K = 0
    END DO


 10 RETURN


    END SUBROUTINE FC2K_POWER_MODULATION
