!*DECK C2SM04
!*CALL PROCESS
SUBROUTINE ERDATA(KP)
  !        #####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM04 COMPUTE EQ'S FOR ERATO (SEE [1], APPENDIX C2, TABLE 2 AND    *
  !        SECTION 5.4.1 OF PUBLICATION)
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZZ2
  REAL(RKIND)      ::     ZR2
  REAL(RKIND)      ::     ZRHO2
  REAL(RKIND)      ::     ZZ1
  REAL(RKIND)      ::     ZR1
  REAL(RKIND)      ::     ZRHO1
  INTEGER          ::     J7
  INTEGER          ::     J6
  INTEGER          ::     JU
  REAL(RKIND)      ::     ZITJ0
  REAL(RKIND)      ::     ZCURV
  REAL(RKIND)      ::     ZTMF2
  REAL(RKIND)      ::     Z4
  REAL(RKIND)      ::     Z3
  REAL(RKIND)      ::     ZDGDPC
  REAL(RKIND)      ::     ZDGDCP
  REAL(RKIND)      ::     ZDZDPC
  REAL(RKIND)      ::     ZDRDPC
  REAL(RKIND)      ::     ZDZDCP
  REAL(RKIND)      ::     ZDRDCP
  REAL(RKIND)      ::     ZDZDPN
  REAL(RKIND)      ::     ZDRDPN
  REAL(RKIND)      ::     ZDGDPN
  REAL(RKIND)      ::     ZDFDZ
  REAL(RKIND)      ::     ZDFDR
  REAL(RKIND)      ::     ZDFDT
  REAL(RKIND)      ::     ZDFDS
  REAL(RKIND)      ::     ZDZ2DT
  REAL(RKIND)      ::     ZDZ2DS
  REAL(RKIND)      ::     Z2
  REAL(RKIND)      ::     ZDZ1DT
  REAL(RKIND)      ::     ZDZ1DS
  REAL(RKIND)      ::     Z1
  REAL(RKIND)      ::     ZDPDZ
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZDTDZ
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDTDR
  REAL(RKIND)      ::     ZDSDR
  REAL(RKIND)      ::     ZJAC
  REAL(RKIND)      ::     ZJPHI
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  REAL(RKIND)      ::     ZGRADP
  REAL(RKIND)      ::     ZFP
  REAL(RKIND)      ::     ZD2PT2
  REAL(RKIND)      ::     ZD2PS2
  REAL(RKIND)      ::     ZD2PST
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  REAL(RKIND)      ::     ZD2RST
  REAL(RKIND)      ::     ZDRSDT
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZD2BT2
  REAL(RKIND)      ::     ZD2BS2
  REAL(RKIND)      ::     ZDBDST
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZS
  REAL(RKIND)      ::     ZT
  INTEGER          ::     J4
  INTEGER          ::     IS0
  INTEGER          ::     JS
  INTEGER          ::     IT0
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZBND
  INTEGER          ::     KP, KP1
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZEPS
  DIMENSION &
       &   IS0(NPCHI),       IT0(NPCHI),       IC(NPCHI), &
       &   ZBND(NPCHI,5),    ZCURV(NPCHI1),    ZDBDS(NPCHI,16), &
       &   ZDBDT(NPCHI,16),  ZDBDST(NPCHI,16), ZD2BS2(NPCHI,16), &
       &   ZD2BT2(NPCHI,16), ZPCEL(NPCHI,16),  ZS(NPCHI), &
       &   ZS1(NPCHI),       ZS2(NPCHI),       ZTETA(NPCHI,5), &
       &   ZT(NPCHI),        ZT1(NPCHI),       ZT2(NPCHI)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ZEPS  = 1.E-3_RKIND
  ! now computes on effectively surfaces (2:NISO1EFF1) so save result on KP+1 so that can insert on-axis values later (same as for eqchease_out in surface)
  KP1 = KP + 1
  !
  DO J1=1,NCHI
     !
     ZTETA(J1,1) = TETCHI(J1,KP)
     ZTETA(J1,2) = TETCHI(J1,KP) - 2._RKIND * ZEPS
     ZTETA(J1,3) = TETCHI(J1,KP) -      ZEPS
     ZTETA(J1,4) = TETCHI(J1,KP) +      ZEPS
     ZTETA(J1,5) = TETCHI(J1,KP) + 2._RKIND * ZEPS
     !
  END DO
  !
  CALL BOUND(NCHI,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(NCHI,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(NCHI,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(NCHI,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(NCHI,ZTETA(1,5),ZBND(1,5))
  !
  CALL RESETI(IC,NCHI,1)
  DO JT = 1,NT1
     DO JG=1,NCHI
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (TETCHI(JG,KP).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC,NCHI,1)
  DO JS = 1,NS1
     DO JG=1,NCHI
        IF (IC(JG).EQ.1) THEN
           IS0(JG) = JS-1
           IF (SIGCHI(JG,KP).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,NCHI
     !
     ZT(J4) = TETCHI(J4,KP)
     ZS(J4) = SIGCHI(J4,KP)
     !
     IF (IS0(J4) .GT. NS) IS0(J4) = NS
     IF (IS0(J4) .LT. 1)  IS0(J4) = 1
     IF (IT0(J4) .GT. NT) IT0(J4) = NT
     IF (IT0(J4) .LT. 1)  IT0(J4) = 1
     !
     ZS1(J4) = CSIG(IS0(J4))
     ZS2(J4) = CSIG(IS0(J4)+1)
     ZT1(J4) = CT(IT0(J4))
     ZT2(J4) = CT(IT0(J4)+1)
     !
  END DO
  !
  CALL PSICEL(IS0,IT0,NCHI,NPCHI,ZPCEL,CPSICL)
  CALL BASIS3(NCHI,NPCHI,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT, &
       &               ZDBDST,ZD2BS2,ZD2BT2)
  !
  DO J5=1,NCHI
     !
     ZDRSDT = (ZBND(J5,2) + 8*(ZBND(J5,4) - ZBND(J5,3)) - &
          &             ZBND(J5,5)) / (12._RKIND * ZEPS)
     ZD2RST = (- ZBND(J5,2) + 16._RKIND * ZBND(J5,3) - &
          &             30._RKIND * ZBND(J5,1) + 16._RKIND * ZBND(J5,4) - &
          &             ZBND(J5,5)) / (12._RKIND * ZEPS**2)
     !
     ZDPDS = ZDBDS(J5, 1) * ZPCEL(J5, 1) + &
          &           ZDBDS(J5, 2) * ZPCEL(J5, 2) + &
          &           ZDBDS(J5, 3) * ZPCEL(J5, 3) + &
          &           ZDBDS(J5, 4) * ZPCEL(J5, 4) + &
          &           ZDBDS(J5, 5) * ZPCEL(J5, 5) + &
          &           ZDBDS(J5, 6) * ZPCEL(J5, 6) + &
          &           ZDBDS(J5, 7) * ZPCEL(J5, 7) + &
          &           ZDBDS(J5, 8) * ZPCEL(J5, 8) + &
          &           ZDBDS(J5, 9) * ZPCEL(J5, 9) + &
          &           ZDBDS(J5,10) * ZPCEL(J5,10) + &
          &           ZDBDS(J5,11) * ZPCEL(J5,11) + &
          &           ZDBDS(J5,12) * ZPCEL(J5,12) + &
          &           ZDBDS(J5,13) * ZPCEL(J5,13) + &
          &           ZDBDS(J5,14) * ZPCEL(J5,14) + &
          &           ZDBDS(J5,15) * ZPCEL(J5,15) + &
          &           ZDBDS(J5,16) * ZPCEL(J5,16)
     !
     ZDPDT = ZDBDT(J5, 1) * ZPCEL(J5, 1) + &
          &           ZDBDT(J5, 2) * ZPCEL(J5, 2) + &
          &           ZDBDT(J5, 3) * ZPCEL(J5, 3) + &
          &           ZDBDT(J5, 4) * ZPCEL(J5, 4) + &
          &           ZDBDT(J5, 5) * ZPCEL(J5, 5) + &
          &           ZDBDT(J5, 6) * ZPCEL(J5, 6) + &
          &           ZDBDT(J5, 7) * ZPCEL(J5, 7) + &
          &           ZDBDT(J5, 8) * ZPCEL(J5, 8) + &
          &           ZDBDT(J5, 9) * ZPCEL(J5, 9) + &
          &           ZDBDT(J5,10) * ZPCEL(J5,10) + &
          &           ZDBDT(J5,11) * ZPCEL(J5,11) + &
          &           ZDBDT(J5,12) * ZPCEL(J5,12) + &
          &           ZDBDT(J5,13) * ZPCEL(J5,13) + &
          &           ZDBDT(J5,14) * ZPCEL(J5,14) + &
          &           ZDBDT(J5,15) * ZPCEL(J5,15) + &
          &           ZDBDT(J5,16) * ZPCEL(J5,16)
     !
     ZD2PST = ZDBDST(J5, 1) * ZPCEL(J5, 1) + &
          &            ZDBDST(J5, 2) * ZPCEL(J5, 2) + &
          &            ZDBDST(J5, 3) * ZPCEL(J5, 3) + &
          &            ZDBDST(J5, 4) * ZPCEL(J5, 4) + &
          &            ZDBDST(J5, 5) * ZPCEL(J5, 5) + &
          &            ZDBDST(J5, 6) * ZPCEL(J5, 6) + &
          &            ZDBDST(J5, 7) * ZPCEL(J5, 7) + &
          &            ZDBDST(J5, 8) * ZPCEL(J5, 8) + &
          &            ZDBDST(J5, 9) * ZPCEL(J5, 9) + &
          &            ZDBDST(J5,10) * ZPCEL(J5,10) + &
          &            ZDBDST(J5,11) * ZPCEL(J5,11) + &
          &            ZDBDST(J5,12) * ZPCEL(J5,12) + &
          &            ZDBDST(J5,13) * ZPCEL(J5,13) + &
          &            ZDBDST(J5,14) * ZPCEL(J5,14) + &
          &            ZDBDST(J5,15) * ZPCEL(J5,15) + &
          &            ZDBDST(J5,16) * ZPCEL(J5,16)
     !
     ZD2PS2 = ZD2BS2(J5, 1) * ZPCEL(J5, 1) + &
          &            ZD2BS2(J5, 2) * ZPCEL(J5, 2) + &
          &            ZD2BS2(J5, 3) * ZPCEL(J5, 3) + &
          &            ZD2BS2(J5, 4) * ZPCEL(J5, 4) + &
          &            ZD2BS2(J5, 5) * ZPCEL(J5, 5) + &
          &            ZD2BS2(J5, 6) * ZPCEL(J5, 6) + &
          &            ZD2BS2(J5, 7) * ZPCEL(J5, 7) + &
          &            ZD2BS2(J5, 8) * ZPCEL(J5, 8) + &
          &            ZD2BS2(J5, 9) * ZPCEL(J5, 9) + &
          &            ZD2BS2(J5,10) * ZPCEL(J5,10) + &
          &            ZD2BS2(J5,11) * ZPCEL(J5,11) + &
          &            ZD2BS2(J5,12) * ZPCEL(J5,12) + &
          &            ZD2BS2(J5,13) * ZPCEL(J5,13) + &
          &            ZD2BS2(J5,14) * ZPCEL(J5,14) + &
          &            ZD2BS2(J5,15) * ZPCEL(J5,15) + &
          &            ZD2BS2(J5,16) * ZPCEL(J5,16)
     !
     ZD2PT2 = ZD2BT2(J5, 1) * ZPCEL(J5, 1) + &
          &            ZD2BT2(J5, 2) * ZPCEL(J5, 2) + &
          &            ZD2BT2(J5, 3) * ZPCEL(J5, 3) + &
          &            ZD2BT2(J5, 4) * ZPCEL(J5, 4) + &
          &            ZD2BT2(J5, 5) * ZPCEL(J5, 5) + &
          &            ZD2BT2(J5, 6) * ZPCEL(J5, 6) + &
          &            ZD2BT2(J5, 7) * ZPCEL(J5, 7) + &
          &            ZD2BT2(J5, 8) * ZPCEL(J5, 8) + &
          &            ZD2BT2(J5, 9) * ZPCEL(J5, 9) + &
          &            ZD2BT2(J5,10) * ZPCEL(J5,10) + &
          &            ZD2BT2(J5,11) * ZPCEL(J5,11) + &
          &            ZD2BT2(J5,12) * ZPCEL(J5,12) + &
          &            ZD2BT2(J5,13) * ZPCEL(J5,13) + &
          &            ZD2BT2(J5,14) * ZPCEL(J5,14) + &
          &            ZD2BT2(J5,15) * ZPCEL(J5,15) + &
          &            ZD2BT2(J5,16) * ZPCEL(J5,16)
     !
     ZFP    = (ZDPDS**2 + (ZDPDT / SIGCHI(J5,KP) - ZDPDS * ZDRSDT / &
          &             ZBND(J5,1))**2) / ZBND(J5,1)**2
     ZGRADP = SQRT(ZFP)
     !
     ZCOST = COS(ZTETA(J5,1))
     ZSINT = SIN(ZTETA(J5,1))
     !
     ZRHO   = SIGCHI(J5,KP) * ZBND(J5,1)
     ZR     = ZRHO * ZCOST + R0
     ZZ     = ZRHO * ZSINT + RZ0
     ZJPHI  = - ZR * CPPR(KP) - TTP(KP) / ZR
     ZJAC   = CP(KP) * ZR**NER * ZGRADP**NEGP
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBND(J5,1) * ZCOST) / ZBND(J5,1)**2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBND(J5,1) * ZSINT - ZDRSDT * ZCOST) / ZBND(J5,1)**2
     ZDTDZ = ZCOST / ZRHO
     !
     ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     Z1     = ZDPDS
     ZDZ1DS = ZD2PS2
     ZDZ1DT = ZD2PST
     Z2     = ZDPDT / SIGCHI(J5,KP) - ZDRSDT * ZDPDS / ZBND(J5,1)
     ZDZ2DS = (ZD2PST - ZDPDT / SIGCHI(J5,KP)) / SIGCHI(J5,KP) - &
          &             ZDRSDT * ZD2PS2 / ZBND(J5,1)
     ZDZ2DT = - ZD2PST*ZDRSDT/ZBND(J5,1) + ZD2PT2/SIGCHI(J5,KP) + &
          &              ZDPDS * (ZDRSDT**2-ZD2RST*ZBND(J5,1))/ZBND(J5,1)**2
     !
     ZDFDS = 2 * (Z1 * ZDZ1DS + Z2 * ZDZ2DS) / ZBND(J5,1)**2
     ZDFDT = - 2 * ZFP * ZDRSDT / ZBND(J5,1) + &
          &           2 * (Z1 * ZDZ1DT + Z2 * ZDZ2DT) / ZBND(J5,1)**2
     !
     ZDFDR = ZDFDS * ZDSDR + ZDFDT * ZDTDR
     ZDFDZ = ZDFDS * ZDSDZ + ZDFDT * ZDTDZ
     !
     ZDGDPN = (ZDFDR * ZDPDR + ZDFDZ * ZDPDZ) / ZFP
     ZDRDPN = ZDPDR / ZFP
     ZDZDPN = ZDPDZ / ZFP
     !
     ZDRDCP = - ZJAC * ZDPDZ / ZR
     ZDZDCP =   ZJAC * ZDPDR / ZR
     ZDRDPC = ZDRDPN - .5_RKIND*EQ13(J5,KP1) * ZDRDCP / (CPSRF * SMISO(KP))
     ZDZDPC = ZDZDPN - .5_RKIND*EQ13(J5,KP1) * ZDZDCP / (CPSRF * SMISO(KP))
     ZDGDCP = ZJAC * (ZDPDR * ZDFDZ - ZDPDZ * ZDFDR) / ZR
     ZDGDPC = ZDGDPN - .5_RKIND*EQ13(J5,KP1) * ZDGDCP / (CPSRF * SMISO(KP))
     !
     Z3 = (2-NER)*ZDRDPC/ZR - .5_RKIND*NEGP*ZDGDPC/ZFP - CPDP(KP)/CP(KP)
     Z4 = (2-NER)*ZDRDCP/ZR - .5_RKIND*NEGP*ZDGDCP/ZFP
     !
     IF (NDEQ .LT. 25) GO TO 999
     !
     EQ( 1,J5,KP1) = SMISOP1(KP1)
     EQ( 2,J5,KP1) = CHI(J5)
     !
     IF (KP .LT. NPSI1-1) THEN
       !
       EQ( 3,J5,KP1) = SMISOP1(KP1+1)
       EQ( 5,J5,KP1) = 0.5 * (SMISOP1(KP1) + SMISOP1(KP1+1))
       !
     ELSEIF (KP .EQ. NPSI1-1) THEN
       EQ( 3,J5,KP1) = 0._RKIND
       EQ( 5,J5,KP1) = SMISOP1(KP1)
     ENDIF
     !
     EQ( 4,J5,KP1) = CHI(J5+1)
     EQ( 6,J5,KP1) = CHIM(J5)
     !
     !        EQ( 7,J5,KP1) CONTAINS MASS DENSITY
     !
     EQ( 7,J5,KP1) = 1
     EQ( 8,J5,KP1) = GAMMA * CPR(KP) / (Q0 * CPSRF)
     EQ( 9,J5,KP1) = TMF(KP)
     !
     !        EQ(10,J5,KP1) CONTAINS HELICAL PARAMETER
     !
     EQ(10,J5,KP1) = 0
     EQ(11,J5,KP1) = QPSI(KP) / Q0
     EQ(12,J5,KP1) = PSIISO(KP) * ZR**2 / (Q0 * ZFP)
     !
     !        EQ13(J5,KP1)  IT IS COMPUTED BY SUBROUTINE CHIPSI.
     !                     IT CONTAINS NEW BETACHI ON (CHI-MID,S-MID) NODES
     !
     EQ(13,J5,KP1) = EQ13(J5,KP1)
     EQ(14,J5,KP1) = ZR**2
     EQ(15,J5,KP1) = 4 * CPSRF * SMISO(KP) * ZDRDPC / ZR
     EQ(16,J5,KP1) = 2 * ZDRDCP / ZR
     EQ(17,J5,KP1) = 2 * SMISO(KP) * CPSRF * (Z3 - ZR * ZJPHI / ZFP)
     EQ(18,J5,KP1) = 2 * PSIISO(KP) * (ZJPHI * ZJPHI / ZFP - &
          &                  .5_RKIND * ZJPHI * ZDGDPN / (ZR * ZFP) - &
          &                  CPPR(KP) * ZDRDPN / ZR) / Q0
     EQ(19,J5,KP1) = ZR**2 / ZJAC
     EQ(20,J5,KP1) = Z4
     EQ(21,J5,KP1) = 2 * SMISO(KP) * CPSRF * CDQ(KP) * EQ24(J5,KP1) + &
          &                  QPSI(KP) * EQ22(J5,KP1) - &
          &                  TMF(KP) * ZJAC * EQ13(J5,KP1) / ZR**2
     !
     !        EQ22(J5,KP1)  IS COMPUTED BY SUBROUTINE CHIPSI
     !                     IT CONTAINS OLD BETACHI ON (CHI-MID,S-MID) NODES
     !
     EQ(22,J5,KP1) = EQ22(J5,KP1)
     EQ(23,J5,KP1) = ZDGDCP
     !
     !        EQ24(J5,KP1)  IS COMPUTED BY SUBROUTINE CHIPSI
     !                     IT CONTAINS OLD CHI ON (CHI-MID,S-MID) NODES
     !
     EQ(24,J5,KP1) = EQ24(J5,KP1)
     CALL ACOPY(NCHI-1,EQ(24,2,KP1),NDEQ,EQ(25,1,KP1),NDEQ)
     EQ(25,NCHI,KP1) = 2._RKIND * CPI
     !
     !  QUANTITIES FOR LION ONLY
     !
     IF (NIDEAL .EQ. 2) THEN
        !
        EQ(26,J5,KP1) = TTP(KP) / TMF(KP)
        EQ(27,J5,KP1) = ZJPHI
        EQ(28,J5,KP1) = .5_RKIND * ZDGDPN / ZFP
        EQ(29,J5,KP1) = Z3
        !
     ENDIF
     !
999  CONTINUE 
     !
     ZTMF2      = TMF(KP)**2
     ZCURV(J5)  = ZFP * (ZR * ZJPHI / (ZFP + ZTMF2) + &
          &                ZDRDPN / ZR + .5_RKIND*ZDGDPN / (ZFP + ZTMF2))
     !%OS         ZCURV(J5)  = ZGRADP * (-ZR * ZJPHI / (ZFP + ZTMF2) -
     !%OS     +                ZDRDPN / ZR + .5*ZDGDPN / (ZFP + ZTMF2))
     !
     CR(J5,KP1)   = ZR
     CZ(J5,KP1)   = ZZ
     !
     IF (NIDEAL .NE. 0) THEN
        !
        CNR1(J5,KP1) = ZDPDR
        CNZ1(J5,KP1) = ZDPDZ
        !
     ELSE
        !
        CNR1(J5,KP1) = 2 * SMISO(KP) * CPSRF * ZDRDPC
        CNZ1(J5,KP1) = 2 * SMISO(KP) * CPSRF * ZDZDPC
        CNR2(J5,KP1) = ZDRDCP
        CNZ2(J5,KP1) = ZDZDCP
        !
     ENDIF
     !
     RSHEAR(J5,KP) = (TTP(KP) / ZR**2 + ZTMF2 * (ZJPHI - &
          &                    ZDGDPN / ZR) / (ZFP * ZR)) / TMF(KP)
     !
  END DO
  !
  ZCURV(NCHI1) = ZCURV(1)
  !
  !**********************************************************************
  !                                                                     *
  !  CORRECT EQ(21,J,KP1) WHICH SHOULD BE THE INTEGRAL OF D(JT/R**2)/DPSI*
  !                                                                     *
  !     IF NCHI IS EVEN                                                 *
  !     - THE INTEGRAL IS FROM 0 TO CHI IF (K .LE. NCHI/2+1) AND FROM 0 *
  !       TO -CHI IF (K .GT. NCHI/2+1)                                  *
  !                                                                     *
  !     IF NCHI IS ODD                                                  *
  !     - THE INTEGRAL IS FROM 0 TO CHI IF (K .LE. (NCHI+1)/2+1) AND    *
  !       FROM 0 TO -CHI IF (K .GT. (NCHI+1)/2+1)                       *
  !                                                                     *
  !**********************************************************************
  !
  IF (NDEQ .GE. 25) THEN
     !
     ZITJ0 = 4._RKIND * CPI * SMISO(KP) * CPSRF * CDQ(KP)
     !
     JU = NCHI / 2 + 2
     !
     IF (MOD(NCHI,2) .EQ. 1) JU = (NCHI + 1) / 2 + 1
     !
     DO J6=JU,NCHI
        !
        EQ(21,J6,KP1) = EQ(21,J6,KP1) - ZITJ0
        !
     END DO
     !
  ENDIF
  !
  !-----------------------------------------------------------------------
  !     COMPUTE (R,Z) OF ZERO CURVATURE LINE
  !
  DO  J7=1,NCHI
     !
     IF (NCURV+1 .GT. 4*2*NPPSI1) GO TO 7
     !
     IF (ZCURV(J7) * ZCURV(J7+1) .LE. 0._RKIND) THEN
        !
        NCURV = NCURV + 1
        !
        ZRHO1 = SIGCHI(J7,KP) * ZBND(J7,1)
        ZR1   = ZRHO1 * COS(ZTETA(J7,1)) + R0
        ZZ1   = ZRHO1 * SIN(ZTETA(J7,1)) + RZ0
        !
        IF (J7 .NE. NCHI) THEN
           !
           ZRHO2 = SIGCHI(J7+1,KP) * ZBND(J7+1,1)
           ZR2   = ZRHO2 * COS(ZTETA(J7+1,1)) + R0
           ZZ2   = ZRHO2 * SIN(ZTETA(J7+1,1)) + RZ0
           !
        ELSE
           !
           ZRHO2 = SIGCHI(1,KP) * ZBND(1,1)
           ZR2   = ZRHO2 * COS(ZTETA(1,1)) + R0
           ZZ2   = ZRHO2 * SIN(ZTETA(1,1)) + RZ0
           !
        ENDIF
        !
        RRCURV(NCURV) = (ZCURV(J7) * ZR2 - ZCURV(J7+1) * ZR1) / &
             &                      (ZCURV(J7) - ZCURV(J7+1))
        RZCURV(NCURV) = (ZCURV(J7) * ZZ2 - ZCURV(J7+1) * ZZ1) / &
             &                      (ZCURV(J7) - ZCURV(J7+1))
        !
     ENDIF
     !
  END DO
7 CONTINUE 
  !
  RETURN
END SUBROUTINE ERDATA
