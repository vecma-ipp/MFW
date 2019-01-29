! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  !-------------------------------------------------------!
  !     This routine selects the power from the time      !
  !     sequence                                          !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     Denis.Kalupin@efda.org              !
  !                                                       !
  !     Comments:     created for ETS                     !
  !                                                       !
  !-------------------------------------------------------!

    SUBROUTINE FC2K_POWER_FROM_ARRAY(POWER_AR,  TIME_AR, TIME, POWER)                      

    USE ITM_CONSTANTS


    IMPLICIT NONE


! +++ i/o parameters:
    REAL (R8), INTENT (IN)     :: POWER_AR(1000)      
    REAL (R8), INTENT (IN)     :: TIME_AR(1000)     
    REAL (R8), INTENT (OUT)    :: POWER      
    REAL (R8), INTENT (IN)     :: TIME

    INTEGER                    :: I

!+++++++++++++++++++++++++++++++++++++++++

    DO I = 1, 999
       IF (TIME_AR(I).LE.TIME .AND. TIME.LE.TIME_AR(I+1)) THEN
          POWER = POWER_AR(I) + (POWER_AR(I+1)- POWER_AR(I))/(TIME_AR(I+1)-TIME_AR(I))*(TIME-TIME_AR(I))
          GOTO 10
       END IF

       IF (TIME_AR(I).GT.TIME_AR(I+1)) THEN
          WRITE (*,*)'NONMONOTHONIC TIME HAS BEEN SPECIFIED FOR POWER SEQUENCE'
          STOP
       END IF
    END DO


 10 RETURN


    END SUBROUTINE FC2K_POWER_FROM_ARRAY
