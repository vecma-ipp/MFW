!*DECK C2SU02
!*CALL PROCESS
SUBROUTINE CUBRT(KN,PSIAXE,PSIBND,PISO,PTET,PSIG)
  !        #################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SU02  COMPUTE INTERSECTIONS OF CONSTANT FLUX SURFACE WITH        *
  !          THETA=PTET(I) LINE.                                        *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZTEST
  REAL(RKIND)      ::     ZFMID
  INTEGER          ::     I9
  INTEGER          ::     J9
  REAL(RKIND)      ::     ZPSI
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZPCEL
  INTEGER          ::     J6
  INTEGER          ::     IS0
  INTEGER          ::     JS
  INTEGER          ::     ISSUM
  INTEGER          ::     ITEST
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J4
  INTEGER          ::     J10
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  INTEGER          ::     IT0
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     ID
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     PTET
  REAL(RKIND)      ::     PSIG
  INTEGER          ::     IC
  REAL(RKIND)      ::     PSIBND
  REAL(RKIND)      ::     ZFMAX
  REAL(RKIND)      ::     PISO
  REAL(RKIND)      ::     PSIAXE
  REAL(RKIND)      ::     ZFMIN
  REAL(RKIND)      ::     ZSIGMX
  REAL(RKIND)      ::     ZSIGMN
  INTEGER          ::     I1
  INTEGER          ::     J1
  INTEGER          ::     KP
  INTEGER          ::     KTET
  INTEGER          ::     LOOPMAX
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     KN
  DIMENSION &
       &   IS0(2*NTP1*(NPMGS+1)*NPPSCUB),  IT0(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ID(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   PSIG(KN),                     PTET(KN), &
       &   ZBND(2*NTP1*(NPMGS+1)*NPPSCUB), IC(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ZF(2*NTP1*(NPMGS+1)*NPPSCUB,16),ZFMIN(2*NTP1*(NPMGS+1)*NPPSCUB) &
       &  ,ZFMAX(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ZPCEL(2*NTP1*(NPMGS+1)*NPPSCUB,16), &
       &   ZPSI(2*NTP1*(NPMGS+1)*NPPSCUB), PISO(NPISOEFF), &
       &   ZSIGMN(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ZSIGMX(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ZS1(2*NTP1*(NPMGS+1)*NPPSCUB),  ZS2(2*NTP1*(NPMGS+1)*NPPSCUB), &
       &   ZT1(2*NTP1*(NPMGS+1)*NPPSCUB),  ZT2(2*NTP1*(NPMGS+1)*NPPSCUB)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ZEPS = RC1M13
  LOOPMAX = 100
  KTET = NT1 * (NMGAUS+1)
  KP = (KN - 1) / KTET + 1
  !
  DO J1=1,KN
     I1 = (J1 - 1) / KTET + 1
     ZSIGMN(J1) = 0._RKIND
     ZSIGMX(J1) = 1._RKIND
     ZFMIN(J1)  = PSIAXE - PISO(I1)
     ZFMAX(J1)  = PSIBND - PISO(I1)
     IC(J1)     = 1
     PSIG(J1)   = .5_RKIND
     !
     IF (ZFMIN(J1) .EQ. 0._RKIND) THEN
        ZSIGMX(J1) = 0._RKIND
        PSIG(J1)   = 0._RKIND
        ZFMAX(J1)  = ZFMIN(J1)
     ELSE IF (ZFMAX(J1) .EQ. 0._RKIND) THEN
        ZSIGMN(J1) = 1._RKIND
        PSIG(J1)   = 1._RKIND
        ZFMIN(J1)  = ZFMAX(J1)
     ENDIF
  END DO
  !     
  CALL BOUND(KN,PTET,ZBND)
  !
  CALL RESETI(ID,KN,1)
  DO JT = 1,NT1
     DO JG=1,KN
        IF (ID(JG).NE.0) THEN
           IT0(JG) = JT-1
           IF (PTET(JG).LE.CT(JT)) ID(JG)  = 0
        ENDIF
     END DO
  END DO
  !
  DO JG=1,KN
     IF (IT0(JG).LT.1) IT0(JG) = 1
     ZT1(JG) = CT(IT0(JG))
     ZT2(JG) = CT(IT0(JG)+1)
  END DO
  !     
  DO J10=1,LOOPMAX
     !     
     IF (J10.NE.1) THEN
        CALL RESETI(ID,KN,1)
        DO J4=1,KN
           IF (ZSIGMN(J4).GE.ZS1(J4).AND.ZSIGMX(J4).LE.ZS2(J4)) &
                &              ID(J4) = 0
        END DO
        ITEST = ISSUM(KN,ID,1)
        IF (ITEST .EQ. 0) GOTO 7
     ENDIF

     CALL RESETI(ID,KN,1)
     DO JS = 1,NS1
        DO JG=1,KN
           IF (ID(JG).NE.0) THEN
              IS0(JG) = JS-1
              IF (PSIG(JG).LE.CSIG(JS)) ID(JG)  = 0
           ENDIF
        END DO
     END DO
     !
     DO J6=1,KN
        IF (IS0(J6) .LT. 1)  IS0(J6) = 1
        !
        ZS1(J6) = CSIG(IS0(J6))
        ZS2(J6) = CSIG(IS0(J6)+1)
     END DO
     !
     CALL PSICEL(IS0,IT0,KN,2*NTP1*(NPMGS+1)*NPPSCUB,ZPCEL,CPSICL)
     !
7    CONTINUE 
     !
     CALL BASIS1(KN,2*NTP1*(NPMGS+1)*NPPSCUB,ZS1,ZS2,ZT1,ZT2, &
          &               PSIG,PTET,ZF)
     !
     DO J8=1,KN
        ZPSI(J8) = ZPCEL(J8, 1) * ZF(J8, 1) + &
             &                ZPCEL(J8, 2) * ZF(J8, 2) + &
             &                ZPCEL(J8, 3) * ZF(J8, 3) + &
             &                ZPCEL(J8, 4) * ZF(J8, 4) + &
             &                ZPCEL(J8, 5) * ZF(J8, 5) + &
             &                ZPCEL(J8, 6) * ZF(J8, 6) + &
             &                ZPCEL(J8, 7) * ZF(J8, 7) + &
             &                ZPCEL(J8, 8) * ZF(J8, 8) + &
             &                ZPCEL(J8, 9) * ZF(J8, 9) + &
             &                ZPCEL(J8,10) * ZF(J8,10) + &
             &                ZPCEL(J8,11) * ZF(J8,11) + &
             &                ZPCEL(J8,12) * ZF(J8,12) + &
             &                ZPCEL(J8,13) * ZF(J8,13) + &
             &                ZPCEL(J8,14) * ZF(J8,14) + &
             &                ZPCEL(J8,15) * ZF(J8,15) + &
             &                ZPCEL(J8,16) * ZF(J8,16)
     END DO
     !
     DO J9=1,KN
        IF (IC(J9) .NE. 0) THEN
           !
           IF (ZFMIN(J9) .NE. 0._RKIND) THEN
              I9 = (J9 - 1) / KTET + 1
              ZFMID = ZPSI(J9) - PISO(I9)
           ELSE
              ZFMID = 0._RKIND
           ENDIF
           !
           ZTEST = .5_RKIND * ABS(ZSIGMX(J9) - ZSIGMN(J9))
           !
           IF (ZTEST .LE. ZEPS) THEN
              IC(J9)  = 0
              IF (PSIG(J9).GT.1._RKIND) PSIG(J9) = 1._RKIND
           ELSE IF (ZTEST .GT. ZEPS) THEN
              IF (ZFMIN(J9) * ZFMID .LE. 0._RKIND) THEN
                 ZSIGMX(J9) = PSIG(J9)
                 ZFMAX(J9)  = ZFMID
              ELSE
                 ZSIGMN(J9) = PSIG(J9)
                 ZFMIN(J9)  = ZFMID
              ENDIF
              PSIG(J9) = .5_RKIND * (ZSIGMX(J9) + ZSIGMN(J9))
           ENDIF
           !
        ENDIF
     END DO
     !
     ITEST = ISSUM(KN,IC,1)
     !
     IF (ITEST .EQ. 0) GOTO 11
  END DO
  PRINT*,'CUBRT NOT CONVERGED'
  STOP
11 CONTINUE 
  !
  RETURN
END SUBROUTINE CUBRT
