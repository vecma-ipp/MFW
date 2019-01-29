!*DECK C2SY01
!*CALL PROCESS
SUBROUTINE BASIS1(KN,KPN,PS1,PS2,PT1,PT2,PS,PT,PF)
  !        ##################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SY01 EVALUATES THE 2-D CUBIC HERMITE BASIS FUNCTIONS AT           *
  !        (SIGMA,THETA) = (PS,PT)                                      *
  !                                                                     *
  ! THE 2-D BASIS FUNCTIONS A PRODUCTS OF 1-D FUNCTIONS DEFINED IN      *
  ! HERMITE.inc. IN CHEASE, THE BASIS FUNCTIONS ARE FUNCTIONS OF        *
  ! SIGMA AND THETA                                                     *
  !                                                                     *
  ! PS1 AND PS2 ARE THE CELL LIMITS IN THE SIGMA DIRECTION              *
  ! PT1 AND PT2 ARE THE CELL LIMITS IN THE THETA DIRECTION              *
  !                                                                     *
  ! KN IS THE NUMBER OF POINTS WHERE THE VALUES ARE COMPUTED            *
  !                                                                     *
  ! THE RESULT VECTOR IS CONTAINED BY PF                                *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PF
  REAL(RKIND)      ::     ZFN4T
  REAL(RKIND)      ::     ZFN3T
  REAL(RKIND)      ::     ZFN2T
  REAL(RKIND)      ::     PT2
  REAL(RKIND)      ::     PT1
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZFN1T
  REAL(RKIND)      ::     ZFN4S
  REAL(RKIND)      ::     ZFN3S
  REAL(RKIND)      ::     ZFN2S
  REAL(RKIND)      ::     PS2
  REAL(RKIND)      ::     PS1
  REAL(RKIND)      ::     PS
  REAL(RKIND)      ::     ZFN1S
  INTEGER          ::     J1
  INTEGER          ::     KPN
  INTEGER          ::     KN
  DIMENSION &
       &   PS(KN),    PS1(KN),    PS2(KN),    PT(KN), &
       &   PT1(KN),   PT2(KN),    PF(KPN,16)
  !
  INCLUDE 'HERMIT.inc'
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO J1=1,KN
     !     
     ZFN1S = FN1(PS(J1),PS1(J1),PS2(J1))
     ZFN2S = FN2(PS(J1),PS1(J1),PS2(J1))
     ZFN3S = FN3(PS(J1),PS1(J1),PS2(J1))
     ZFN4S = FN4(PS(J1),PS1(J1),PS2(J1))
     !     
     ZFN1T = FN1(PT(J1),PT1(J1),PT2(J1))
     ZFN2T = FN2(PT(J1),PT1(J1),PT2(J1))
     ZFN3T = FN3(PT(J1),PT1(J1),PT2(J1))
     ZFN4T = FN4(PT(J1),PT1(J1),PT2(J1))

     PF(J1, 1) = ZFN1S * ZFN1T
     PF(J1, 2) = ZFN2S * ZFN1T
     PF(J1, 3) = ZFN1S * ZFN2T
     PF(J1, 4) = ZFN2S * ZFN2T
     !
     PF(J1, 5) = ZFN3S * ZFN1T
     PF(J1, 6) = ZFN4S * ZFN1T
     PF(J1, 7) = ZFN3S * ZFN2T
     PF(J1, 8) = ZFN4S * ZFN2T
     !
     PF(J1, 9) = ZFN1S * ZFN3T
     PF(J1,10) = ZFN2S * ZFN3T
     PF(J1,11) = ZFN1S * ZFN4T
     PF(J1,12) = ZFN2S * ZFN4T
     !
     PF(J1,13) = ZFN3S * ZFN3T
     PF(J1,14) = ZFN4S * ZFN3T
     PF(J1,15) = ZFN3S * ZFN4T
     PF(J1,16) = ZFN4S * ZFN4T
     !
  END DO
  !
  RETURN
END SUBROUTINE BASIS1
