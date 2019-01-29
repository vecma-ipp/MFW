!*DECK C2SP04
!*CALL PROCESS
SUBROUTINE APCOEF(KN,PX,PAP,PT)
  !        ###############################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP04  EVALUATE P' IF GIVEN AS POLYNOMIALS IN SEVERAL SECTIONS    *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZS3
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZSG3
  REAL(RKIND)      ::     ZSG2
  REAL(RKIND)      ::     PX
  REAL(RKIND)      ::     ZSG1
  INTEGER          ::     J1
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
  CALL COPYAP(PAP,ZA0,ZA1,ZB0,ZB1,ZB2,ZB3,ZC0,ZC1,ZC2)
  !
  DO J1=1,KN
     !
     zs1 = 0._rkind
     zs2 = 0._rkind
     zs3 = 0._rkind
     !
     if (px(j1).lt.pap(1))                      zs1=1._rkind
     if (px(j1).ge.pap(1).and.px(j1).le.pap(2)) zs2=1._rkind
     if (px(j1).gt.pap(2))                      zs3=1._rkind
     !
     PT(J1) = PT(J1) - ZS1 * (ZA0 + ZA1 * PX(J1)) - &
          &                     ZS2 * (ZB0 + PX(J1) * (ZB1 + PX(J1) * &
          &                                     (ZB2 + PX(J1) * ZB3))) - &
          &                     ZS3 * (ZC0 + PX(J1) * (ZC1 + PX(J1) * ZC2))
     !
  END DO
  !
  RETURN
END SUBROUTINE APCOEF
