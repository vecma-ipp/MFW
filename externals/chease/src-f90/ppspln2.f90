SUBROUTINE PPSPLN2(KN,PP,KPP,PS,RPP,D2RPP,PT,PTP,D2PT)
  !
  ! A. Bottino Fri Mar 28 20:11:42 CET 2003, MODIFIED (KPP+1 IS END INDEX AS PPSPLN, THUS MODIFY OUTELIT) BY O. SAUTER
  ! IN
  ! PS(1:KPP+1) : INPUT MESH
  ! RPP: FUNCTION ON PS MESH TO BE INTERPOLATED
  ! D2RPP: D2(RPP)/DPS^2, SECOND DERIVATIVE CALCULATED WITH SPLINE ROUTINE
  ! PP(1:KN): OUTPUT MESH ON WHICH FUNCTION AND DERIVATIVES ARE CALCULATED
  !
  ! OUTOUT :: 
  ! PT function on grid PP
  ! PTP first derivative on grid PP
  ! D2PT second derivative on grid PP
  !
  ! NO ASSUMPTION ON X MESH, THUS PP AND PS SHOULD BE SAME TYPE OF MESH
  ! EITHER BOTH ON S OR BOTH ON PSI, ETC
  !
  USE prec_const
  IMPLICIT NONE
  !
  INTEGER, INTENT(IN) :: KN
  INTEGER, INTENT(IN) :: KPP
  REAL(RKIND),DIMENSION(1:KN), INTENT(IN) :: PP
  REAL(RKIND),DIMENSION(1:KPP+1), INTENT(IN) :: PS
  REAL(RKIND),DIMENSION(1:KPP+1), INTENT(IN) :: RPP
  REAL(RKIND), DIMENSION(1:KPP+1), INTENT(IN) :: D2RPP
  REAL(RKIND), DIMENSION(1:KN), INTENT(OUT) :: PT 
  REAL(RKIND), DIMENSION(1:KN), INTENT(OUT) :: PTP 
  REAL(RKIND), DIMENSION(1:KN), INTENT(INOUT) :: D2PT
  !
  REAL(RKIND) ::     ZD    
  REAL(RKIND) ::     ZC    
  REAL(RKIND) ::     ZB    
  REAL(RKIND) ::     ZA    
  REAL(RKIND) ::     ZH    
  REAL(RKIND) ::     ZS1    
  INTEGER     ::     JG    
  INTEGER     ::     JS    
  INTEGER     ::     J2    
  INTEGER, DIMENSION(1:KN) :: I1
  INTEGER, DIMENSION(1:KN) :: IC    
  ! 
  CALL RESETI(IC,KN,1)
  DO JS = 1,KPP+1
     DO JG=1,KN
        IF (IC(JG).EQ.1) THEN
           ZS1 = PP(JG)
           I1(JG) = JS-1
           IF (ZS1.LE.PS(JS)) IC(JG) = 0
        ENDIF
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  !  COMPUTE P-PRIME                                                    *
  !                                                                     *
  !**********************************************************************
  !
  DO J2=1,KN
     IF (I1(J2) .LT. 1)   I1(J2) = 1
     IF (I1(J2) .GT. KPP) I1(J2) = KPP
     ! 
     ZS1 = PP(J2)
     !
     ZH = PS(I1(J2)+1) - PS(I1(J2))
     ZA = (PS(I1(J2)+1) - ZS1) / ZH
     ZB = (ZS1 - PS(I1(J2))) / ZH
     ZC = (ZA + 1) * (ZA - 1) * ZH * (PS(I1(J2)+1) - ZS1) / 6._RKIND
     ZD = (ZB + 1) * (ZB - 1) * ZH * (ZS1 - PS(I1(J2))) / 6._RKIND
     PT(J2) = ZA*RPP(I1(J2))   + ZB*RPP(I1(J2)+1) + &
          &            ZC*D2RPP(I1(J2)) + ZD*D2RPP(I1(J2)+1)
     PTP(J2) = (RPP(I1(J2)+1) - RPP(I1(J2))) / ZH - &
          &       ((3._RKIND*ZA*ZA-1._RKIND)*D2RPP(I1(J2))-(3._RKIND*ZB*ZB-1._RKIND)*D2RPP(I1(J2)+1)) &
          &       *ZH/6._RKIND
     D2PT(J2) =  ZA*D2RPP(I1(J2)) + ZB*D2RPP(I1(J2)+1)
  END DO
  !        
  RETURN
END SUBROUTINE PPSPLN2
