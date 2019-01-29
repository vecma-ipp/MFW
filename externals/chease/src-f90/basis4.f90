!*DECK C2SY04
!*CALL PROCESS
SUBROUTINE BASIS4(KN,KPN,PS1,PS2,PT1,PT2,PS,PT,PDFDS)
  !        #####################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SY04 IS SIMILAR TO C2SY01. IT COMPUTES FIRST DERIVATIVES OF 2-D   *
  !        CUBIC HERMITE BASIS FUNCTIONS IN THE SIGMA DIRECTION         *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PDFDS
  REAL(RKIND)      ::     ZDFN4S
  REAL(RKIND)      ::     ZDFN3S
  REAL(RKIND)      ::     ZDFN2S
  REAL(RKIND)      ::     PS2
  REAL(RKIND)      ::     PS1
  REAL(RKIND)      ::     PS
  REAL(RKIND)      ::     ZDFN1S
  REAL(RKIND)      ::     ZFN4T
  REAL(RKIND)      ::     ZFN3T
  REAL(RKIND)      ::     ZFN2T
  REAL(RKIND)      ::     PT2
  REAL(RKIND)      ::     PT1
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZFN1T
  INTEGER          ::     KN
  INTEGER          ::     J1
  INTEGER          ::     KPN
  DIMENSION &
       &   PS(KPN),    PS1(KPN),    PS2(KPN),       PT(KPN), &
       &   PT1(KPN),   PT2(KPN),    PDFDS(KPN,16)
  !
  INCLUDE 'HERMIT.inc'
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO J1=1,KN
     !     
     ZFN1T = FN1(PT(J1),PT1(J1),PT2(J1))
     ZFN2T = FN2(PT(J1),PT1(J1),PT2(J1))
     ZFN3T = FN3(PT(J1),PT1(J1),PT2(J1))
     ZFN4T = FN4(PT(J1),PT1(J1),PT2(J1))
     !
     ZDFN1S = DFN1(PS(J1),PS1(J1),PS2(J1))
     ZDFN2S = DFN2(PS(J1),PS1(J1),PS2(J1))
     ZDFN3S = DFN3(PS(J1),PS1(J1),PS2(J1))
     ZDFN4S = DFN4(PS(J1),PS1(J1),PS2(J1))
     !         
     PDFDS(J1, 1) = ZDFN1S * ZFN1T
     PDFDS(J1, 2) = ZDFN2S * ZFN1T
     PDFDS(J1, 3) = ZDFN1S * ZFN2T
     PDFDS(J1, 4) = ZDFN2S * ZFN2T
     !
     PDFDS(J1, 5) = ZDFN3S * ZFN1T
     PDFDS(J1, 6) = ZDFN4S * ZFN1T
     PDFDS(J1, 7) = ZDFN3S * ZFN2T
     PDFDS(J1, 8) = ZDFN4S * ZFN2T
     !
     PDFDS(J1, 9) = ZDFN1S * ZFN3T
     PDFDS(J1,10) = ZDFN2S * ZFN3T
     PDFDS(J1,11) = ZDFN1S * ZFN4T
     PDFDS(J1,12) = ZDFN2S * ZFN4T
     !
     PDFDS(J1,13) = ZDFN3S * ZFN3T
     PDFDS(J1,14) = ZDFN4S * ZFN3T
     PDFDS(J1,15) = ZDFN3S * ZFN4T
     PDFDS(J1,16) = ZDFN4S * ZFN4T
     !
  END DO
  !
  RETURN
END SUBROUTINE BASIS4
