!*DECK C2SI02
!*CALL PROCESS
SUBROUTINE ATCOEF(KN,PX,PAT,PT,K)
  !        #################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SI02  EVALUATE TT', I* OR I_PARALLEL IF GIVEN AS POLYNOMIALS IN  *
  !         SEVERALS SECTIONS                                           *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     ZEXP
  INTEGER          ::     J2
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZS3
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     Z3
  REAL(RKIND)      ::     Z2
  REAL(RKIND)      ::     PX
  REAL(RKIND)      ::     Z1
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZC0
  REAL(RKIND)      ::     ZB3
  REAL(RKIND)      ::     ZB2
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZB0
  REAL(RKIND)      ::     ZA2
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZA0
  REAL(RKIND)      ::     PAT
  INTEGER          ::     K
  INTEGER          ::     KN
  DIMENSION &
       &   PAT(*),   PT(KN),   PX(KN)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (K .EQ. 1) THEN
     !
     CALL COPYAT(PAT,ZA0,ZA1,ZA2,ZB0,ZB1,ZB2,ZB3,ZC0,ZC1)
     !
     DO J1=1,KN
        !
        zs1 = 0._rkind
        zs2 = 0._rkind
        zs3 = 0._rkind
        !
        if (px(j1).lt.pat(1))                      zs1=1._rkind
        if (px(j1).ge.pat(1).and.px(j1).le.pat(2)) zs2=1._rkind
        if (px(j1).gt.pat(2))                      zs3=1._rkind
        !
        PT(J1) = PT(J1) + ZS1 * (ZA0 + PX(J1) * (ZA1 + PX(J1) * ZA2)) + &
             &                     ZS2 * (ZB0 + PX(J1) * (ZB1 + PX(J1) * &
             &                                     (ZB2 + PX(J1) * ZB3))) + &
             &                     ZS3 * (ZC0 + ZC1 * PX(J1))
        !
     END DO
     !
     !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
     !
  ELSE IF (K .EQ. 2) THEN
     !
     DO J2=1,KN
        !
        ZEXP = ((PX(J2) - PAT(1)) / PAT(2))**2
        !
        IF (ZEXP .LT. 100._RKIND) THEN
           !            
           PT(J2) = PT(J2) + PAT(3) * EXP(-ZEXP)
           !
        ENDIF
        !
     END DO
     !
  ENDIF
  !
  RETURN
END SUBROUTINE ATCOEF
