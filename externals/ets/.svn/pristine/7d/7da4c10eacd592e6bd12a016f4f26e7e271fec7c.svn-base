
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine tunes the mixing factors             !
  !     depending on the convergence                      !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for ETS                     !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE FC2K_CONTROL_AMIX(CONV,  CONV_ITER, TOLERANCE,  & !inputs
                                 AMIX,  AMIX_TR,   AMIX_SRC,   & !inputs/outputs
                                 CONTROL)                        !input

    USE ITM_CONSTANTS


    IMPLICIT NONE


! +++ i/o parameters:
    REAL (R8), INTENT (IN)     :: CONV      
    REAL (R8), INTENT (IN)     :: CONV_ITER      
    REAL (R8), INTENT (IN)     :: TOLERANCE      

    REAL (R8), INTENT (INOUT)  :: AMIX      
    REAL (R8), INTENT (INOUT)  :: AMIX_TR      
    REAL (R8), INTENT (INOUT)  :: AMIX_SRC   

    REAL (R8)                  :: P   

    INTEGER,   INTENT (IN)     :: CONTROL(3)    

!+++++++++++++++++++++++++++++++++++++++++
    P         = 2.0_R8
    IF (CONV.LT.TOLERANCE * 100._R8)                 &
    P         = 1.0_R8
    IF (CONV.LT.TOLERANCE * 10._R8)                  &
    P         = 0.5_R8


! +++ adjustment:
    IF (CONV.GT.CONV_ITER) THEN
       IF (CONTROL(1).GT.0)                          &
       AMIX      = AMIX     * (CONV_ITER / CONV) **P
       IF (CONTROL(2).GT.0)                          &
       AMIX_TR   = AMIX_TR  * (CONV_ITER / CONV) **P
       IF (CONTROL(3).GT.0)                          &
       AMIX_SRC  = AMIX_SRC * (CONV_ITER / CONV) **P
    END IF


    IF (CONV.LT.CONV_ITER) THEN
       IF (CONTROL(1).GT.0)                          &
       AMIX      = AMIX     * 1.05_R8 **P
       IF (CONTROL(2).GT.0)                          &
       AMIX_TR   = AMIX_TR  * 1.05_R8 **P 
       IF (CONTROL(3).GT.0)                          &
       AMIX_SRC  = AMIX_SRC * 1.05_R8 **P
    END IF


! +++ check that values do not exceed some limits:
    IF (CONTROL(1).GT.0)                             &
    AMIX         = MAX(TOLERANCE * 100._R8, AMIX)
    IF (CONTROL(2).GT.0)                             &
    AMIX_TR      = MAX(TOLERANCE * 100._R8, AMIX_TR)
    IF (CONTROL(3).GT.0)                             &
    AMIX_SRC     = MAX(TOLERANCE * 100._R8, AMIX_SRC)

    IF (CONTROL(2).GT.0.AND.CONTROL(1).GT.0)         &
    AMIX_TR      = MAX(AMIX**0.5,          AMIX_TR)
    IF (CONTROL(3).GT.0.AND.CONTROL(1).GT.0)         &
    AMIX_SRC     = MAX(AMIX**0.5,          AMIX_SRC)


    IF (CONTROL(1).GT.0)                             &
    AMIX         = MIN(1.0_R8, AMIX)
    IF (CONTROL(2).GT.0)                             &
    AMIX_TR      = MIN(1.0_R8, AMIX_TR)
    IF (CONTROL(3).GT.0)                             &
    AMIX_SRC     = MIN(1.0_R8, AMIX_SRC)



    RETURN


    END SUBROUTINE FC2K_CONTROL_AMIX 



