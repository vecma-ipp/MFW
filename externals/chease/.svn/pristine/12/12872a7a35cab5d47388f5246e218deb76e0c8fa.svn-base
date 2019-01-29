!
SUBROUTINE CURENT(KN,PPSI,PR,PJIPHI)
  !        ####################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SI02 COMPUTE CURRENT DENSITY PROFILE FOR PPSI(I), I=1,KN          *
  !        (SEE EQ. (3), EQ. (10) AND EQ. (11) OF PUBLICATION)          *
  !
  ! assumes ppsi on CSIPR
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     KN
  REAL(RKIND)      ::     PPSI(KN), PR(KN), PJIPHI(KN)
  !
  INTEGER          ::     I1(KN), IC(KN)
  INTEGER          ::     J1, J3, J301, JG, JS
  REAL(RKIND)      ::     ZCID0(KN), ZCID2(KN), ZFUNC(KN), ZPPRIM(KN), ZTMF(KN)
  REAL(RKIND)      ::     ZA, ZB, ZC, ZD, ZH, ZS, ZX
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (NSURF .EQ. 1) GOTO 300
  !
  IF (NPROFZ .EQ. 0) THEN
    CALL PPRIME(KN,PPSI,ZPPRIM)
    CALL PRFUNC(KN,PPSI,ZFUNC)
  ENDIF
  ! 
  IF (NSTTP .EQ. 1 .AND. NPROFZ .EQ. 0) THEN
    DO J1=1,KN
      PJIPHI(J1) = - PR(J1) * ZPPRIM(J1) - ZFUNC(J1) / PR(J1)
    END DO
    !
  ELSE IF (NSTTP .NE. 1 .OR. NPROFZ .EQ. 1) THEN
    !   BRACKET OUT [PS(I); PS(I+1)] INTERVAL SUCH THAT
    !   PSIISO(I) <= PPSI(J) <= PSIISO(I+1), J=1,...,KN
    !   WHEN I* OR <J . B> IS SPECIFIED
    CALL RESETI(IC,KN,1)
    DO JS = 1,NISO
      DO JG=1,KN
        IF (IC(JG).EQ.1) THEN
          ZS = 1._RKIND - PPSI(JG) / SPSIM
          IF (ZS .LT. 0._RKIND) ZS = 0._RKIND
          I1(JG) = JS-1
          IF (SQRT(ZS).LE.CSIPR(JS)) IC(JG) = 0
        ENDIF
      ENDDO
    ENDDO
    !
    DO J3=1,KN
      IF (I1(J3) .LT. 1)      I1(J3) = 1
      IF (I1(J3) .GT. NISO-1) I1(J3) = NISO - 1
      !
      ZS = 1._RKIND - PPSI(J3) / SPSIM
      IF (ZS .LT. 0._RKIND) ZS = 0._RKIND
      ZS = SQRT(ZS)
      !
      ZH = CSIPR(I1(J3)+1) - CSIPR(I1(J3))
      ZA = (CSIPR(I1(J3)+1) - ZS) / ZH
      ZB = (ZS - CSIPR(I1(J3))) / ZH
      ZC = (ZA + 1) * (ZA - 1) * ZH * (CSIPR(I1(J3)+1) - ZS) / 6._RKIND
      ZD = (ZB + 1) * (ZB - 1) * ZH * (ZS - CSIPR(I1(J3))) / 6._RKIND
      !
      IF (NPROFZ .EQ. 1) THEN
        !
        ZPPRIM(J3) = ZA * CPPR(I1(J3))   + ZB * CPPR(I1(J3)+1) + &
             &                      ZC * D2CPPR(I1(J3)) + ZD * D2CPPR(I1(J3)+1)
        ZFUNC(J3)  = ZA * TTP(I1(J3))    + ZB * TTP(I1(J3)+1) + &
             &                      ZC * D2TTP(I1(J3))  + ZD * D2TTP(I1(J3)+1)
        PJIPHI(J3) = - PR(J3) * ZPPRIM(J3) - ZFUNC(J3) / PR(J3)
        !
      ELSE IF (NPROFZ .EQ. 0) THEN
        ! 
        ZCID0(J3) = ZA * CID0(I1(J3))   + ZB * CID0(I1(J3)+1) + &
             &                     ZC * D2CID0(I1(J3)) + ZD * D2CID0(I1(J3)+1)
        ZCID2(J3) = ZA * CID2(I1(J3))   + ZB * CID2(I1(J3)+1) + &
             &                     ZC * D2CID2(I1(J3)) + ZD * D2CID2(I1(J3)+1)
        !
        IF (NSTTP .EQ. 2) THEN
          PJIPHI(J3) = ZFUNC(J3) * ZCID0(J3) / PR(J3) + &
               &                       ZPPRIM(J3) * (ZCID2(J3) / PR(J3) - PR(J3))
          !
        ELSE IF (NSTTP .EQ. 3) THEN
          ZTMF(J3)  = ZA*TMF(I1(J3))   + ZB*TMF(I1(J3)+1) + &
               &                          ZC*D2TMF(I1(J3)) + ZD*D2TMF(I1(J3)+1)
          ZX           = 1._RKIND + ZCID2(J3) / ZTMF(J3)**2
          PJIPHI(J3) = ZFUNC(J3)/(ZX*PR(J3))+ZPPRIM(J3) * &
               &                           (ZCID0(J3)/(ZX*PR(J3))-PR(J3))
        ELSE IF (NSTTP .EQ. 4) THEN
          ZTMF(J3)  = ZA*TMF(I1(J3))   + ZB*TMF(I1(J3)+1) + &
               &                          ZC*D2TMF(I1(J3)) + ZD*D2TMF(I1(J3)+1)
          ZX           = 1._RKIND + ZCID2(J3) / ZTMF(J3)**2
          PJIPHI(J3) = ZFUNC(J3)/(ZX*PR(J3))*ZCID0(J3)/ZTMF(J3)+ZPPRIM(J3) * &
               &                           (ZCID0(J3)/(ZX*PR(J3))-PR(J3))
        ENDIF
      ENDIF
    END DO
  ENDIF
  !
  RETURN
  !
300 CONTINUE 
  !**********************************************************************
  !                                                                     *
  !  PROFILES FOR SOLOVEV CASE                                          *
  !                                                                     *
  !**********************************************************************
  DO J301=1,KN
    PJIPHI(J301) = - PR(J301) * CPP
  END DO
  !
  RETURN
END SUBROUTINE CURENT
