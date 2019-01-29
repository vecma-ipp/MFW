!*DECK C2ST01
!*CALL PROCESS
SUBROUTINE POLYNM(KN,PCOEF,PT,K)
  !        ################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2ST01  COMPUTE PLASMA DENSITY OR TEMPERATURE GIVEN AS A POLYNOMIAL*
  !          OF DEGREE NSOUR < 11 IN CARTESIAN COORDINATES              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     K, KN
  REAL(RKIND)      ::     PCOEF(*), PT(KN)
  !
  INTEGER          ::     J1, J2, J3, J4, J5, J6
  REAL(RKIND)      ::     ZDRODP(KN), ZP(KN), ZS(KN), ZSIG(KN), ZTET(KN)
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(PT,KN)
  CALL RMRAD(KN,SPSIM,RC0P,PANGLE,ZS,ZSIG,ZTET,1)
  !
  IF (K .EQ. 1) THEN
     CALL RESETR(ZP,KN,RC1P)
     !
     DO J2=1,NSOUR
        DO J1=1,KN
           PT(J1) = PT(J1) + PCOEF(J2) * ZP(J1)
           ZP(J1) = ZS(J1) * ZP(J1)
        END DO
     END DO
     !
     DO J3=1,KN
        IF (PT(J3) .LT. RC0P) PT(J3) = 0._RKIND
     END DO
     !
  ELSE
     CALL RESETR(ZP,KN,RC1P)
     DO J5=2,NSOUR
        DO J4=1,KN
           PT(J4) = PT(J4) + (J5 - 1) * PCOEF(J5) * ZP(J4)
           ZP(J4) = ZS(J4) * ZP(J4)
        END DO
     END DO
     !
     CALL DRHODP(KN,PANGLE,ZS,ZDRODP)
     !
     DO J6=1,KN
        PT(J6) = PT(J6) * ZDRODP(J6)
     END DO
  ENDIF
  !
  RETURN
END SUBROUTINE POLYNM
