!*DECK C2SY03
!*CALL PROCESS
SUBROUTINE BASIS3(KN,KPN,PS1,PS2,PT1,PT2,PS,PT,PDFDS,PDFDT, &
     &                     PDFDST,PD2FS2,PD2FT2)
  !        ###########################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SY03 IS SIMILAR TO C2SY01. IT COMPUTES FIRST AND SECOND           *
  !        DERIVATIVES OF 2-D CUBIC HERMITE BASIS FUNCTIONS             *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PD2FT2
  REAL(RKIND)      ::     PD2FS2
  REAL(RKIND)      ::     PDFDST
  REAL(RKIND)      ::     PDFDT
  REAL(RKIND)      ::     PDFDS
  REAL(RKIND)      ::     ZD2FN4T
  REAL(RKIND)      ::     ZD2FN3T
  REAL(RKIND)      ::     ZD2FN2T
  REAL(RKIND)      ::     ZD2FN1T
  REAL(RKIND)      ::     ZD2FN4S
  REAL(RKIND)      ::     ZD2FN3S
  REAL(RKIND)      ::     ZD2FN2S
  REAL(RKIND)      ::     ZD2FN1S
  REAL(RKIND)      ::     ZDFN4T
  REAL(RKIND)      ::     ZDFN3T
  REAL(RKIND)      ::     ZDFN2T
  REAL(RKIND)      ::     ZDFN1T
  REAL(RKIND)      ::     ZDFN4S
  REAL(RKIND)      ::     ZDFN3S
  REAL(RKIND)      ::     ZDFN2S
  REAL(RKIND)      ::     ZDFN1S
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
  INTEGER          ::     KN
  INTEGER          ::     J1
  INTEGER          ::     KPN
  DIMENSION &
       &   PS(KPN),        PS1(KPN),       PS2(KPN),       PT(KPN), &
       &   PT1(KPN),       PT2(KPN),       PDFDS(KPN,16),  PDFDT(KPN,16), &
       &   PDFDST(KPN,16), PD2FS2(KPN,16), PD2FT2(KPN,16)
  !
  INCLUDE 'HERMIT.inc'
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO J1=1,KN
     ZFN1S = FN1(PS(J1),PS1(J1),PS2(J1))
     ZFN2S = FN2(PS(J1),PS1(J1),PS2(J1))
     ZFN3S = FN3(PS(J1),PS1(J1),PS2(J1))
     ZFN4S = FN4(PS(J1),PS1(J1),PS2(J1))
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
     ZDFN1T = DFN1(PT(J1),PT1(J1),PT2(J1))
     ZDFN2T = DFN2(PT(J1),PT1(J1),PT2(J1))
     ZDFN3T = DFN3(PT(J1),PT1(J1),PT2(J1))
     ZDFN4T = DFN4(PT(J1),PT1(J1),PT2(J1))
     !
     ZD2FN1S = D2FN1(PS(J1),PS1(J1),PS2(J1))
     ZD2FN2S = D2FN2(PS(J1),PS1(J1),PS2(J1))
     ZD2FN3S = D2FN3(PS(J1),PS1(J1),PS2(J1))
     ZD2FN4S = D2FN4(PS(J1),PS1(J1),PS2(J1))
     !     
     ZD2FN1T = D2FN1(PT(J1),PT1(J1),PT2(J1))
     ZD2FN2T = D2FN2(PT(J1),PT1(J1),PT2(J1))
     ZD2FN3T = D2FN3(PT(J1),PT1(J1),PT2(J1))
     ZD2FN4T = D2FN4(PT(J1),PT1(J1),PT2(J1))
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
     PDFDT(J1, 1) = ZFN1S * ZDFN1T
     PDFDT(J1, 2) = ZFN2S * ZDFN1T
     PDFDT(J1, 3) = ZFN1S * ZDFN2T
     PDFDT(J1, 4) = ZFN2S * ZDFN2T
     !
     PDFDT(J1, 5) = ZFN3S * ZDFN1T
     PDFDT(J1, 6) = ZFN4S * ZDFN1T
     PDFDT(J1, 7) = ZFN3S * ZDFN2T
     PDFDT(J1, 8) = ZFN4S * ZDFN2T
     !
     PDFDT(J1, 9) = ZFN1S * ZDFN3T
     PDFDT(J1,10) = ZFN2S * ZDFN3T
     PDFDT(J1,11) = ZFN1S * ZDFN4T
     PDFDT(J1,12) = ZFN2S * ZDFN4T
     !
     PDFDT(J1,13) = ZFN3S * ZDFN3T
     PDFDT(J1,14) = ZFN4S * ZDFN3T
     PDFDT(J1,15) = ZFN3S * ZDFN4T
     PDFDT(J1,16) = ZFN4S * ZDFN4T
     !         
     PDFDST(J1, 1) = ZDFN1S * ZDFN1T
     PDFDST(J1, 2) = ZDFN2S * ZDFN1T
     PDFDST(J1, 3) = ZDFN1S * ZDFN2T
     PDFDST(J1, 4) = ZDFN2S * ZDFN2T
     !
     PDFDST(J1, 5) = ZDFN3S * ZDFN1T
     PDFDST(J1, 6) = ZDFN4S * ZDFN1T
     PDFDST(J1, 7) = ZDFN3S * ZDFN2T
     PDFDST(J1, 8) = ZDFN4S * ZDFN2T
     !
     PDFDST(J1, 9) = ZDFN1S * ZDFN3T
     PDFDST(J1,10) = ZDFN2S * ZDFN3T
     PDFDST(J1,11) = ZDFN1S * ZDFN4T
     PDFDST(J1,12) = ZDFN2S * ZDFN4T
     !
     PDFDST(J1,13) = ZDFN3S * ZDFN3T
     PDFDST(J1,14) = ZDFN4S * ZDFN3T
     PDFDST(J1,15) = ZDFN3S * ZDFN4T
     PDFDST(J1,16) = ZDFN4S * ZDFN4T
     !
     PD2FS2(J1, 1) = ZD2FN1S * ZFN1T
     PD2FS2(J1, 2) = ZD2FN2S * ZFN1T
     PD2FS2(J1, 3) = ZD2FN1S * ZFN2T
     PD2FS2(J1, 4) = ZD2FN2S * ZFN2T
     !
     PD2FS2(J1, 5) = ZD2FN3S * ZFN1T
     PD2FS2(J1, 6) = ZD2FN4S * ZFN1T
     PD2FS2(J1, 7) = ZD2FN3S * ZFN2T
     PD2FS2(J1, 8) = ZD2FN4S * ZFN2T
     !
     PD2FS2(J1, 9) = ZD2FN1S * ZFN3T
     PD2FS2(J1,10) = ZD2FN2S * ZFN3T
     PD2FS2(J1,11) = ZD2FN1S * ZFN4T
     PD2FS2(J1,12) = ZD2FN2S * ZFN4T
     !
     PD2FS2(J1,13) = ZD2FN3S * ZFN3T
     PD2FS2(J1,14) = ZD2FN4S * ZFN3T
     PD2FS2(J1,15) = ZD2FN3S * ZFN4T
     PD2FS2(J1,16) = ZD2FN4S * ZFN4T
     !         
     PD2FT2(J1, 1) = ZFN1S * ZD2FN1T
     PD2FT2(J1, 2) = ZFN2S * ZD2FN1T
     PD2FT2(J1, 3) = ZFN1S * ZD2FN2T
     PD2FT2(J1, 4) = ZFN2S * ZD2FN2T
     !
     PD2FT2(J1, 5) = ZFN3S * ZD2FN1T
     PD2FT2(J1, 6) = ZFN4S * ZD2FN1T
     PD2FT2(J1, 7) = ZFN3S * ZD2FN2T
     PD2FT2(J1, 8) = ZFN4S * ZD2FN2T
     !
     PD2FT2(J1, 9) = ZFN1S * ZD2FN3T
     PD2FT2(J1,10) = ZFN2S * ZD2FN3T
     PD2FT2(J1,11) = ZFN1S * ZD2FN4T
     PD2FT2(J1,12) = ZFN2S * ZD2FN4T
     !
     PD2FT2(J1,13) = ZFN3S * ZD2FN3T
     PD2FT2(J1,14) = ZFN4S * ZD2FN3T
     PD2FT2(J1,15) = ZFN3S * ZD2FN4T
     PD2FT2(J1,16) = ZFN4S * ZD2FN4T
     !
     !
  END DO
  !
  RETURN
END SUBROUTINE BASIS3
