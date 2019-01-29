!*DECK C2SP02
!*CALL PROCESS
SUBROUTINE BSFUNC(KN,PP,PT)
  !        ###########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP02  FRACTION OF BOOTSTRAP CURRENT                              *
  !          NBSFUN = 1  -----> POLYNOMIAL                              *
  !          NBSFUN = 2  -----> POLYNOMIAL IN 3 SECTIONS                *
  !          NBSFUN = 3  -----> PRINCETON DEFINITION                    *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZPT2
  REAL(RKIND)      ::     ZPT1
  INTEGER          ::     J5
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J3
  REAL(RKIND)      ::     PP
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J1
  INTEGER          ::     J2
  REAL(RKIND)      ::     PT
  INTEGER          ::     KN
  DIMENSION &
       &   PP(KN),   PT(KN),    ZS(KN)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(PT,KN)
  !
  IF (NBSFUN .EQ. 1) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! BSFUNC(PSI) DEFINED AS POLYNOMIAL IN PSI/PSIMIN OF DEGREE NSOUR     *
     !                                                                     *
     !**********************************************************************
     !
     CALL RESETR(PT,KN,AFBS(NSOUR))
     !
     DO J2=NSOUR-1,1,-1
        !
        DO J1=1, KN
           !
           ZS1    = PP(J1) / SPSIM
           PT(J1) = PT(J1) * ZS1 + AFBS(J2)
           !
        END DO
     END DO
     !
  ELSE IF (NBSFUN .EQ. 2) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  NPP =1 OR 2                                                        *
     !                                                                     *
     !   BSFUNC IS GIVEN AS A SUM OF NPP PROFILES                          *
     !                                                                     *
     !  FOR S=0     TO S=AFBS(1)  BSFUNC(PSI)=-(ZA0+ZA1*S)                 *
     !  FOR S=AFBS(1) TO S=AFBS(2)  BSFUNC(PSI)=-(ZB0+ZB1*S+ZB2*S**2+      *
     !                                           ZB3*S**3)                 *
     !  FOR S=AFBS(2) TO S=1      BSFUNC(PSI)=-(ZC0+ZC1*S+ZC2*S**2)        *
     !                                                                     *
     !         FOR FIRST PROFILE  S = S1 = (1-PSI/PSIM)                    *
     !             SECOND PROFILE S = S2 = SQRT(S1)                        *
     !                                                                     *
     !**********************************************************************
     !
     !**********************************************************************
     !                                                                     *
     !  FIRST PROFILE                                                      *
     !                                                                     *
     !**********************************************************************
     !
     DO J3=1,KN
        !
        ZS(J3) = 1._RKIND - PP(J3) / SPSIM
        !
        IF (ZS(J3) .LT. 0._RKIND) ZS(J3) = 0._RKIND
        !
     END DO
     !
     CALL APCOEF(KN,ZS,AFBS,PT)
     !
     IF (NPP .EQ. 1) RETURN
     !
     !**********************************************************************
     !                                                                     *
     !  SECOND PROFILE                                                     *
     !                                                                     *
     !**********************************************************************
     !
     DO J4=1,KN
        !
        ZS1 = 1._RKIND - PP(J4) / SPSIM
        !
        IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
        !
        ZS(J4) = SQRT(ZS1)
        !
     END DO
     !
     CALL APCOEF(KN,ZS,AFBS2,PT)
     !
  ELSE IF (NBSFUN .EQ. 3) THEN
     !
     !**********************************************************************
     !                                                                     *
     !                                                                     *
     !   BSFUNC(PSI) = AFBS(1) * (1 - S**AFBS(3))**AFBS(2)                 *
     !                                                                     *
     !  WHERE  S = 1 - PSI / SPSIM                                         *
     !                                                                     *
     !**********************************************************************
     !
     DO J5=1,KN
        !
        ZS1 = 1._RKIND - PP(J5) / SPSIM
        !
        IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
        !
        zpt1 = AFBS(1) * (1 - ZS1**AFBS(3))**AFBS(2)
        zpt2 = afbs(4) * zs1**afbs(5)
        if (ap(6).gt.0._RKIND) zpt2 = zpt2 * (1._RKIND-exp((zs1-1._RKIND)/afbs(6)))
        pt(j5) = zpt1 + zpt2
        !
     END DO
     !
  ENDIF
  !
  RETURN
END SUBROUTINE BSFUNC
