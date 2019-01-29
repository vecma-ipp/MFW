!*DECK C2SF03
!*CALL PROCESS
SUBROUTINE TSHIFT(PC,KN)
  !        ########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SF03 SHIFT T PROFILE. (SEE EQ. (33) IN PUBLICATION)               *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  INTEGER          ::     J2
  INTEGER          ::     J3
  REAL(RKIND)      ::     PC
  INTEGER          ::     KN
  INTEGER          ::     J1
  !
  IF (PC+TMF(KN)**2 .LE. 0._RKIND) THEN
     PRINT *,' WARNING: TSHIFT = ',PC,'  TMF(',KN,')= ',TMF(KN)
     PRINT *,' TSHIFT DIVIDED BY TWO => NOT CORRECT SCALING'
     PC = 0.5_RKIND * PC
  ENDIF
  DO J1=1,KN
     TMF(J1) = SQRT(ABS(TMF(J1)**2 + PC))
  END DO
  !
  T0 = SQRT(T0**2 + PC)
  !
  RETURN
END SUBROUTINE TSHIFT
