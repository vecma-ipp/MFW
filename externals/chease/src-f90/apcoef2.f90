!*DECK C2SP06
!*CALL PROCESS
SUBROUTINE APCOEF2(KN,PX,PAP,PT)
  !        ################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP06  EVALUATE P' IF GIVEN AS POLYNOMIALS IN SEVERAL SECTIONS    *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZS6
  REAL(RKIND)      ::     ZS5
  REAL(RKIND)      ::     ZS4
  REAL(RKIND)      ::     ZS3
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZSG6
  REAL(RKIND)      ::     ZSG5
  REAL(RKIND)      ::     ZSG4
  REAL(RKIND)      ::     ZSG3
  REAL(RKIND)      ::     ZSG2
  REAL(RKIND)      ::     PX
  REAL(RKIND)      ::     ZSG1
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZF2
  REAL(RKIND)      ::     ZF1
  REAL(RKIND)      ::     ZF0
  REAL(RKIND)      ::     ZE1
  REAL(RKIND)      ::     ZE0
  REAL(RKIND)      ::     ZD3
  REAL(RKIND)      ::     ZD2
  REAL(RKIND)      ::     ZD1
  REAL(RKIND)      ::     ZD0
  REAL(RKIND)      ::     ZC2
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZC0
  REAL(RKIND)      ::     ZB3
  REAL(RKIND)      ::     ZB2
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZB0
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZA0
  REAL(RKIND)      ::     PAP
  INTEGER          ::     KN
  DIMENSION &
       &   PAP(*),  PT(KN),   PX(KN)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL COPYAPP(PAP,ZA0,ZA1,ZB0,ZB1,ZB2,ZB3,ZC0,ZC1,ZC2, &
       &                    ZD0,ZD1,ZD2,ZD3,ZE0,ZE1,ZF0,ZF1,ZF2)
  !
  DO J1=1,KN
     !
     zs1 = 0._rkind
     zs2 = 0._rkind
     zs3 = 0._rkind
     zs4 = 0._rkind
     zs5 = 0._rkind
     zs6 = 0._rkind
     !
     if (px(j1).lt.pap(1))                      zs1=1._rkind
     if (px(j1).ge.pap(1).and.px(j1).lt.pap(2)) zs2=1._rkind
     if (px(j1).ge.pap(2).and.px(j1).lt.pap(3)) zs3=1._rkind
     if (px(j1).ge.pap(3).and.px(j1).lt.pap(4)) zs4=1._rkind
     if (px(j1).ge.pap(4).and.px(j1).lt.pap(5)) zs5=1._rkind
     if (px(j1).ge.pap(5))                      zs6=1._rkind
     !
     PT(J1) = PT(J1) - ZS1 * (ZA0 + ZA1 * PX(J1)) - &
          &                     ZS2 * (ZB0 + PX(J1) * (ZB1 + PX(J1) * &
          &                                     (ZB2 + PX(J1) * ZB3))) - &
          &                     ZS3 * (ZC0 + PX(J1) * (ZC1 + PX(J1) * ZC2)) - &
          &                     ZS4 * (ZD0 + PX(J1) * (ZD1 + PX(J1) * &
          &                                     (ZD2 + PX(J1) * ZD3))) - &
          &                     ZS5 * (ZE0 + ZE1 * PX(J1)) - &
          &                     ZS6 * (ZF0 + PX(J1) * (ZF1 + PX(J1) * ZF2))
     !
  END DO
  !
  RETURN
END SUBROUTINE APCOEF2
