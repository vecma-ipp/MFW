!*DECK C3SB03
!*CALL PROCESS
SUBROUTINE SHAVE
  !        ################
  !
  !                                        AUTHORS:
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SB03  SHAVE AWAY ALL POLOIDAL FLUX SURFACES WHICH DO NOT SATISFY  *
  !         Q < QSHAVE. THE SURFACE Q=QSHAVE IS SAVED IN DISK FILE      *
  !         NRZPEL, AND AN BE RESUSED AS INPUT WITH NSURF=6 FOR A       *
  !         SUBSEQUENT RUN                                              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZQ
  INTEGER          ::     J
  REAL(RKIND)      ::     QSHAVE
  PARAMETER (QSHAVE=4.5_RKIND)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO J = NISO1EFF,1,-1
     ZQ = .5_RKIND * TMF(J) * CIDQ(J) / CPI
     IF (ZQ.LT.QSHAVE) GOTO 20
  END DO
  WRITE(6,*) ' Q0 =',ZQ,' > QSHAVE =',QSHAVE
  RETURN
  !
20 CONTINUE 
  WRITE(6,1000) 100._RKIND*SMISO(J)*SMISO(J),ZQ
  CALL SURFRZ(J,SIGPSI(1,J),TETPSI(1,J))
  RETURN
1000 FORMAT(' R-Z COORDINATES OF ',F5.1, &
       &       ' % FLUX SURFACE OUTPUT ON FILE NRZPEL',/, &
       &       ' SURFACE Q =',F7.2)
END SUBROUTINE SHAVE
