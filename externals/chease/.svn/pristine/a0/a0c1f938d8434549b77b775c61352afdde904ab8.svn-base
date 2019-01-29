!*DECK C2SP03
!*CALL PROCESS
SUBROUTINE PPSPLN(KN,PP,KPP,PS,RPP,D2RPP,PT,PTP,KOPT)
  !        #####################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP03  CUBIC SPLINE INTEPOLATION OF EXPERIMENTAL P-PRIME PROFILE  *
  !                                                                     *
  !     INTERPOLATES ANY GIVEN FUNCTION RPP(I) ON PS(I), I=1,KPP+1      *
  !     WITH D2RPP COMPUTED WITH SUBROUTINE SPLINE BEFORE.              *
  !     IT COMPUTES THE INTERPOLATED FUNCTION PT(I) ON PP(I),I=1,KN     *
  !                                                                     *
  !     WARNING: PP PROPORTIONAL TO PSI AND PS TO S                     *
  !                                                                     *
  !     KOPT = 0: COMPUTE ONLY PT (PTP NOT USED)                        *
  !     KOPT = 1: COMPUTE ALSO PTP, WITH PTP = D(PT) / DPSI             *
  !     KOPT = 2: COMPUTE ALSO PTP, BUT  PTP = D(PT) / DS               *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE prec_const
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PTP
  INTEGER          ::     KOPT
  REAL(RKIND)      ::     D2RPP
  REAL(RKIND)      ::     RPP
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  INTEGER          ::     J2
  REAL(RKIND)      ::     PS
  INTEGER          ::     I1
  REAL(RKIND)      ::     PP
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     JG
  INTEGER          ::     JS
  INTEGER          ::     IC
  INTEGER          ::     KN
  INTEGER          ::     KPP
  DIMENSION &
       &   I1(KN),  IC(KN), &
       &   D2RPP(KPP+1),   PP(KN),   PS(KPP+1), &
       &   RPP(KPP+1),     PT(KN),   PTP(KN)
  !
  !
  !   BRACKET OUT [PS(I); PS(I+1)] INTERVAL SUCH THAT
  !   PSIISO(I) <= PP(J) <= PSIISO(I+1), J=1,...,KN
  ! 
  CALL RESETI(IC,KN,1)
  DO JS = 1,KPP+1
     DO JG=1,KN
        IF (IC(JG).EQ.1) THEN
           ZS1 = 1._RKIND - PP(JG) / SPSIM
           IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
           I1(JG) = JS-1
           IF (SQRT(ZS1).LE.PS(JS)) IC(JG) = 0
        ENDIF
     ENDDO
  ENDDO
  !
  !**********************************************************************
  !                                                                     *
  !  COMPUTE P-PRIME                                                    *
  !                                                                     *
  !**********************************************************************
  !
  DO J2=1,KN
     !
     IF (I1(J2) .LT. 1)   I1(J2) = 1
     IF (I1(J2) .GT. KPP) I1(J2) = KPP
     !
     ZS1 = 1._RKIND - PP(J2) / SPSIM
     !
     IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
     !
     ZS1 = SQRT(ZS1)
     !
     ZH = PS(I1(J2)+1) - PS(I1(J2))
     ZA = (PS(I1(J2)+1) - ZS1) / ZH
     ZB = (ZS1 - PS(I1(J2))) / ZH
     ZC = (ZA + 1) * (ZA - 1) * ZH * (PS(I1(J2)+1) - ZS1) / 6._RKIND
     ZD = (ZB + 1) * (ZB - 1) * ZH * (ZS1 - PS(I1(J2))) / 6._RKIND
     ! 
     PT(J2) = ZA*RPP(I1(J2))   + ZB*RPP(I1(J2)+1) + &
          &            ZC*D2RPP(I1(J2)) + ZD*D2RPP(I1(J2)+1)
     !
     IF (KOPT .GE. 1) THEN
        PTP(J2) = (RPP(I1(J2)+1) - RPP(I1(J2))) / ZH - &
             &       ((3._RKIND*ZA*ZA-1._RKIND)*D2RPP(I1(J2))-(3._RKIND*ZB*ZB-1._RKIND)*D2RPP(I1(J2)+1)) &
             &       *ZH/6._RKIND
        IF (KOPT.EQ.1 .AND. ZS1.NE.0._RKIND) &
             &       PTP(J2) = - PTP(J2) / (2._RKIND*ZS1*SPSIM)
        IF (KOPT.EQ.1 .AND. ZS1.EQ.0._RKIND) PTP(J2) = 0.0_RKIND
     ENDIF
     !
  END DO
  !         
  RETURN
END SUBROUTINE PPSPLN
