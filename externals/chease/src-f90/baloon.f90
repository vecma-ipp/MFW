!*DECK C2SM11
!*CALL PROCESS
SUBROUTINE BALOON(KP1,KP2,PSM)
  !        ################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM11 EVALUATE BALLOONING STABILITY AND LOCAL INTERCHANGE CRITERIA *
  !        REDUCED NUMBER OF PSI SURFACES => KP2-KP1+1.LE.NPPSBAL       *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J15
  INTEGER          ::     J12PSI
  INTEGER          ::     J12
  INTEGER          ::     J13
  INTEGER          ::     J11
  REAL(RKIND)      ::     ZX22
  REAL(RKIND)      ::     ZX12
  REAL(RKIND)      ::     ZX11
  REAL(RKIND)      ::     ZZP
  REAL(RKIND)      ::     ZZA
  REAL(RKIND)      ::     ZDY
  REAL(RKIND)      ::     ZG
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZCOEF
  REAL(RKIND)      ::     ZGBAR
  INTEGER          ::     JCHI
  INTEGER          ::     JTURN
  INTEGER          ::     JTOT
  REAL(RKIND)      ::     ZNU0
  INTEGER          ::     J9PSI
  INTEGER          ::     J9
  INTEGER          ::     J10
  INTEGER          ::     JC
  INTEGER          ::     J14
  INTEGER          ::     I
  INTEGER          ::     J
  REAL(RKIND)      ::     ZDPBC
  REAL(RKIND)      ::     ZDPBP
  REAL(RKIND)      ::     ZDRDPC
  REAL(RKIND)      ::     PSM
  REAL(RKIND)      ::     ZDGDPC
  REAL(RKIND)      ::     ZDGDCP
  REAL(RKIND)      ::     ZDRDCP
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
  REAL(RKIND)      ::     ZR2
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
  INTEGER          ::     J7
  REAL(RKIND)      ::     ZTMF2
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
  INTEGER          ::     J6
  INTEGER          ::     IS0
  INTEGER          ::     JS
  INTEGER          ::     IT0
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J3
  INTEGER          ::     J8PSI
  INTEGER          ::     J8
  INTEGER          ::     ISMIN
  REAL(RKIND)      ::     ZDCHI0
  INTEGER          ::     J1
  INTEGER          ::     J2
  INTEGER          ::     ICHIMX
  REAL(RKIND)      ::     ZDCHI
  INTEGER          ::     ICHI0
  INTEGER          ::     KP1
  INTEGER          ::     IPSIBAL
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     IM1
  INTEGER          ::     IMAX
  INTEGER          ::     KP2
  DIMENSION &
       &   ICHI0(NPBLC0),      IS0(NPCHI),         IT0(NPCHI), &
       &   IC(NPCHI),          PSM(KP2), &
       &   ZBND(NPCHI,5),      ZDBDS(NPCHI,16),    ZDBDT(NPCHI,16), &
       &   ZDBDST(NPCHI,16),   ZDCHI0(NPCHI),      ZDPBC(NPPSBAL,NPCHI), &
       &   ZDPBP(NPPSBAL,NPCHI),ZD2BS2(NPCHI,16),  ZD2BT2(NPCHI,16), &
       &   ZFP(NPPSBAL,NPCHI), ZJAC(NPPSBAL,NPCHI),ZPCEL(NPCHI,16), &
       &   ZR2(NPPSBAL,NPCHI), ZS(NPCHI),          ZS1(NPCHI), &
       &   ZS2(NPCHI),         ZTETA(NPCHI,5),     ZT(NPCHI), &
       &   ZT1(NPCHI),         ZT2(NPCHI)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IMAX  = 2 * NTURN * NCHI + 1
  IM1   = IMAX + 1
  ZEPS  = 1.E-3_RKIND
  IPSIBAL = KP2 - KP1 + 1
  IF (IPSIBAL .GT. NPPSBAL) THEN
    IF (NVERBOSE .GE. 0) write(0,*) ' ERROR IN BALOON, KP2-KP1+1= ',IPSIBAL, &
      &       ' > NPPSBAL= ',NPPSBAL
    STOP 'BALOON'
  ENDIF
  !
  IF (NBLC0 .EQ. 1) THEN
     !
     ICHI0(1) = 1
     !
  ELSE IF (NBLC0 .GT. 1) THEN
     !
     IF (NSYM .EQ. 0) THEN
        !
        ZDCHI  = CHIM(NCHI) / (NBLC0 - 1._RKIND)
        ICHIMX = NCHI
        !
     ELSE IF (NSYM .EQ. 1) THEN
        !
        ZDCHI  = CHIM(NCHI/2+1) / (NBLC0 - 1._RKIND)
        ICHIMX = NCHI / 2 + 1
        !
     ENDIF
     !
     DO J2=1,NBLC0
        !
        DO J1=1,ICHIMX
           !
           ZDCHI0(J1) = ABS((J2-1)*ZDCHI-CHIM(J1))
           !
        END DO
        !
        ICHI0(J2) = ISMIN(ICHIMX,ZDCHI0,1)
        !
     END DO
     !
  ENDIF
  !
  DO J8=1,IPSIBAL
     !
     J8PSI = J8 + KP1 -1
     DO J3=1,NCHI
        !
        ZTETA(J3,1) = TETCHI(J3,J8PSI)
        ZTETA(J3,2) = TETCHI(J3,J8PSI) - 2._RKIND * ZEPS
        ZTETA(J3,3) = TETCHI(J3,J8PSI) -      ZEPS
        ZTETA(J3,4) = TETCHI(J3,J8PSI) +      ZEPS
        ZTETA(J3,5) = TETCHI(J3,J8PSI) + 2._RKIND * ZEPS
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
              IF (TETCHI(JG,J8PSI).LE.CT(JT)) IC(JG)  = 0
           ENDIF
        END DO
     END DO
     CALL RESETI(IC,NCHI,1)
     DO JS = 1,NS1
        DO JG=1,NCHI
           IF (IC(JG).EQ.1) THEN
              IS0(JG) = JS-1
              IF (SIGCHI(JG,J8PSI).LE.CSIG(JS)) IC(JG)  = 0
           ENDIF
        END DO
     END DO
     !
     DO J6=1,NCHI
        !
        ZT(J6) = TETCHI(J6,J8PSI)
        ZS(J6) = SIGCHI(J6,J8PSI)
        !
        IF (IS0(J6) .GT. NS) IS0(J6) = NS
        IF (IS0(J6) .LT. 1)  IS0(J6) = 1
        IF (IT0(J6) .GT. NT) IT0(J6) = NT
        IF (IT0(J6) .LT. 1)  IT0(J6) = 1
        !
        ZS1(J6) = CSIG(IS0(J6))
        ZS2(J6) = CSIG(IS0(J6)+1)
        ZT1(J6) = CT(IT0(J6))
        ZT2(J6) = CT(IT0(J6)+1)
        !
     END DO
     !
     CALL PSICEL(IS0,IT0,NCHI,NPCHI,ZPCEL,CPSICL)
     CALL BASIS3(NCHI,NPCHI,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT, &
          &               ZDBDST,ZD2BS2,ZD2BT2)
     !
     ZTMF2  = TMF(J8PSI)**2
     !
     DO J7=1,NCHI
        !
        ZDRSDT = (ZBND(J7,2) + 8*(ZBND(J7,4) - ZBND(J7,3)) - &
             &            ZBND(J7,5)) / (12._RKIND * ZEPS)
        ZD2RST = (- ZBND(J7,2) + 16._RKIND * ZBND(J7,3) - &
             &             30._RKIND * ZBND(J7,1) + 16._RKIND * ZBND(J7,4) - &
             &             ZBND(J7,5)) / (12._RKIND * ZEPS**2)
        !
        ZDPDS = ZDBDS(J7, 1) * ZPCEL(J7, 1) + &
             &           ZDBDS(J7, 2) * ZPCEL(J7, 2) + &
             &           ZDBDS(J7, 3) * ZPCEL(J7, 3) + &
             &           ZDBDS(J7, 4) * ZPCEL(J7, 4) + &
             &           ZDBDS(J7, 5) * ZPCEL(J7, 5) + &
             &           ZDBDS(J7, 6) * ZPCEL(J7, 6) + &
             &           ZDBDS(J7, 7) * ZPCEL(J7, 7) + &
             &           ZDBDS(J7, 8) * ZPCEL(J7, 8) + &
             &           ZDBDS(J7, 9) * ZPCEL(J7, 9) + &
             &           ZDBDS(J7,10) * ZPCEL(J7,10) + &
             &           ZDBDS(J7,11) * ZPCEL(J7,11) + &
             &           ZDBDS(J7,12) * ZPCEL(J7,12) + &
             &           ZDBDS(J7,13) * ZPCEL(J7,13) + &
             &           ZDBDS(J7,14) * ZPCEL(J7,14) + &
             &           ZDBDS(J7,15) * ZPCEL(J7,15) + &
             &           ZDBDS(J7,16) * ZPCEL(J7,16)
        !
        ZDPDT = ZDBDT(J7, 1) * ZPCEL(J7, 1) + &
             &           ZDBDT(J7, 2) * ZPCEL(J7, 2) + &
             &           ZDBDT(J7, 3) * ZPCEL(J7, 3) + &
             &           ZDBDT(J7, 4) * ZPCEL(J7, 4) + &
             &           ZDBDT(J7, 5) * ZPCEL(J7, 5) + &
             &           ZDBDT(J7, 6) * ZPCEL(J7, 6) + &
             &           ZDBDT(J7, 7) * ZPCEL(J7, 7) + &
             &           ZDBDT(J7, 8) * ZPCEL(J7, 8) + &
             &           ZDBDT(J7, 9) * ZPCEL(J7, 9) + &
             &           ZDBDT(J7,10) * ZPCEL(J7,10) + &
             &           ZDBDT(J7,11) * ZPCEL(J7,11) + &
             &           ZDBDT(J7,12) * ZPCEL(J7,12) + &
             &           ZDBDT(J7,13) * ZPCEL(J7,13) + &
             &           ZDBDT(J7,14) * ZPCEL(J7,14) + &
             &           ZDBDT(J7,15) * ZPCEL(J7,15) + &
             &           ZDBDT(J7,16) * ZPCEL(J7,16)
        !
        ZD2PST = ZDBDST(J7, 1) * ZPCEL(J7, 1) + &
             &            ZDBDST(J7, 2) * ZPCEL(J7, 2) + &
             &            ZDBDST(J7, 3) * ZPCEL(J7, 3) + &
             &            ZDBDST(J7, 4) * ZPCEL(J7, 4) + &
             &            ZDBDST(J7, 5) * ZPCEL(J7, 5) + &
             &            ZDBDST(J7, 6) * ZPCEL(J7, 6) + &
             &            ZDBDST(J7, 7) * ZPCEL(J7, 7) + &
             &            ZDBDST(J7, 8) * ZPCEL(J7, 8) + &
             &            ZDBDST(J7, 9) * ZPCEL(J7, 9) + &
             &            ZDBDST(J7,10) * ZPCEL(J7,10) + &
             &            ZDBDST(J7,11) * ZPCEL(J7,11) + &
             &            ZDBDST(J7,12) * ZPCEL(J7,12) + &
             &            ZDBDST(J7,13) * ZPCEL(J7,13) + &
             &            ZDBDST(J7,14) * ZPCEL(J7,14) + &
             &            ZDBDST(J7,15) * ZPCEL(J7,15) + &
             &            ZDBDST(J7,16) * ZPCEL(J7,16)
        !
        ZD2PS2 = ZD2BS2(J7, 1) * ZPCEL(J7, 1) + &
             &            ZD2BS2(J7, 2) * ZPCEL(J7, 2) + &
             &            ZD2BS2(J7, 3) * ZPCEL(J7, 3) + &
             &            ZD2BS2(J7, 4) * ZPCEL(J7, 4) + &
             &            ZD2BS2(J7, 5) * ZPCEL(J7, 5) + &
             &            ZD2BS2(J7, 6) * ZPCEL(J7, 6) + &
             &            ZD2BS2(J7, 7) * ZPCEL(J7, 7) + &
             &            ZD2BS2(J7, 8) * ZPCEL(J7, 8) + &
             &            ZD2BS2(J7, 9) * ZPCEL(J7, 9) + &
             &            ZD2BS2(J7,10) * ZPCEL(J7,10) + &
             &            ZD2BS2(J7,11) * ZPCEL(J7,11) + &
             &            ZD2BS2(J7,12) * ZPCEL(J7,12) + &
             &            ZD2BS2(J7,13) * ZPCEL(J7,13) + &
             &            ZD2BS2(J7,14) * ZPCEL(J7,14) + &
             &            ZD2BS2(J7,15) * ZPCEL(J7,15) + &
             &            ZD2BS2(J7,16) * ZPCEL(J7,16)
        !
        ZD2PT2 = ZD2BT2(J7, 1) * ZPCEL(J7, 1) + &
             &            ZD2BT2(J7, 2) * ZPCEL(J7, 2) + &
             &            ZD2BT2(J7, 3) * ZPCEL(J7, 3) + &
             &            ZD2BT2(J7, 4) * ZPCEL(J7, 4) + &
             &            ZD2BT2(J7, 5) * ZPCEL(J7, 5) + &
             &            ZD2BT2(J7, 6) * ZPCEL(J7, 6) + &
             &            ZD2BT2(J7, 7) * ZPCEL(J7, 7) + &
             &            ZD2BT2(J7, 8) * ZPCEL(J7, 8) + &
             &            ZD2BT2(J7, 9) * ZPCEL(J7, 9) + &
             &            ZD2BT2(J7,10) * ZPCEL(J7,10) + &
             &            ZD2BT2(J7,11) * ZPCEL(J7,11) + &
             &            ZD2BT2(J7,12) * ZPCEL(J7,12) + &
             &            ZD2BT2(J7,13) * ZPCEL(J7,13) + &
             &            ZD2BT2(J7,14) * ZPCEL(J7,14) + &
             &            ZD2BT2(J7,15) * ZPCEL(J7,15) + &
             &            ZD2BT2(J7,16) * ZPCEL(J7,16)
        !
        ZFP(J8,J7) = (ZDPDS**2 + (ZDPDT / SIGCHI(J7,J8PSI) - &
             &                 ZDPDS * ZDRSDT / ZBND(J7,1))**2) / &
             &                ZBND(J7,1)**2
        ZGRADP = SQRT(ZFP(J8,J7))
        !
        ZCOST = COS(ZTETA(J7,1))
        ZSINT = SIN(ZTETA(J7,1))
        !
        ZRHO        = SIGCHI(J7,J8PSI) * ZBND(J7,1)
        ZR          = ZRHO * ZCOST + R0
        ZR2(J8,J7)  = ZR**2
        ZJAC(J8,J7) = CP(J8PSI) * ZR**NER * ZGRADP**NEGP
        !
        ZDSDR = (ZDRSDT * ZSINT + ZBND(J7,1) * ZCOST) / ZBND(J7,1)**2
        ZDTDR = - ZSINT / ZRHO
        ZDSDZ = (ZBND(J7,1) * ZSINT - ZDRSDT * ZCOST) / ZBND(J7,1)**2
        ZDTDZ = ZCOST / ZRHO
        !
        ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
        ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
        !
        Z1     = ZDPDS
        ZDZ1DS = ZD2PS2
        ZDZ1DT = ZD2PST
        Z2     = ZDPDT / SIGCHI(J7,J8PSI) - ZDRSDT * ZDPDS / ZBND(J7,1)
        ZDZ2DS = (ZD2PST-ZDPDT / SIGCHI(J7,J8PSI)) / SIGCHI(J7,J8PSI) - &
             &             ZDRSDT * ZD2PS2 / ZBND(J7,1)
        ZDZ2DT = - ZD2PST*ZDRSDT/ZBND(J7,1) + ZD2PT2/SIGCHI(J7,J8PSI) + &
             &            ZDPDS * (ZDRSDT**2-ZD2RST*ZBND(J7,1)) / ZBND(J7,1)**2
        !
        ZDFDS = 2._RKIND * (Z1 * ZDZ1DS + Z2 * ZDZ2DS) / ZBND(J7,1)**2
        ZDFDT = - 2._RKIND * ZFP(J8,J7) * ZDRSDT / ZBND(J7,1) + &
             &           2._RKIND * (Z1 * ZDZ1DT + Z2 * ZDZ2DT) / ZBND(J7,1)**2
        !
        ZDFDR = ZDFDS * ZDSDR + ZDFDT * ZDTDR
        ZDFDZ = ZDFDS * ZDSDZ + ZDFDT * ZDTDZ
        !
        ZDGDPN = (ZDFDR * ZDPDR + ZDFDZ * ZDPDZ) / ZFP(J8,J7)
        ZDRDPN = ZDPDR / ZFP(J8,J7)
        !
        ZDRDCP = - ZJAC(J8,J7) * ZDPDZ / ZR
        ZDGDCP = ZJAC(J8,J7) * (ZDPDR * ZDFDZ - ZDPDZ * ZDFDR) / ZR
        ZDGDPC = ZDGDPN - .5_RKIND*ZDGDCP *EQ13(J7,J8PSI)/(PSM(J8PSI)*CPSRF)
        ZDRDPC = ZDRDPN - .5_RKIND*ZDRDCP *EQ13(J7,J8PSI)/(PSM(J8PSI)*CPSRF)
        !
        ZDPBP(J8,J7) = CPPR(J8PSI) +(TTP(J8PSI)+.5_RKIND*ZDGDPN)/ZR2(J8,J7) - &
             &                  (ZTMF2+ZFP(J8,J7)) * ZDRDPN / (ZR * ZR2(J8,J7))
        ZDPBC(J8,J7) = .5_RKIND * ZDGDCP / ZR2(J8,J7) - (ZTMF2+ZFP(J8,J7)) * &
             &                  ZDRDCP / (ZR * ZR2(J8,J7))
        !
     END DO
  END DO
  !
  CALL RESETI(NCBAL(KP1),IPSIBAL,0)
  DO J=1,NBLC0
     DO I=KP1,KP2
        NCBLNS(I,J) = 0
     ENDDO
  ENDDO
  !
  DO J14=1,NBLC0
     !
     CALL VZERO(ABAL,2*NPPSBAL*IM1)
     !
     JC = ICHI0(J14)
     !
     DO J10=1,IMAX
        !
        DO J9=1,IPSIBAL
           !
           J9PSI = J9 + KP1 - 1
           ZNU0 = ZJAC(J9,JC) * TMF(J9PSI) / ZR2(J9,JC)
           !
           JTOT  = J10 + JC - 2
           JTURN = JTOT / NCHI
           JCHI  = JTOT - JTURN * NCHI + 1
           !
           ! EQ. (18) IN PUBLICATION
           !
           ZGBAR = .5_RKIND * (ZNU0 * EQ13(JC,J9PSI) + QPSI(J9PSI) * &
                &          (EQ22(JCHI,J9PSI)-EQ22(JC,J9PSI)))/(PSM(J9PSI)*CPSRF)+ &
                &           CDQ(J9PSI) * (EQ24(JCHI,J9PSI)-EQ24(JC,J9PSI) + &
                &           CPI*(2*(JTURN - NTURN)))
           ZCOEF = ZR2(J9,JCHI) / (TMF(J9PSI)**2 + ZFP(J9,JCHI))
           !
           !  COEFFICIENT OF D(XSI) / D(CHI) (C_1, EQ. (19) IN PUBLICATION)
           !
           ZF = (1._RKIND + ZCOEF * (ZGBAR * ZFP(J9,JCHI))**2) / &
                &        (ZJAC(J9,JCHI)**2 * ZFP(J9,JCHI))
           !
           !  COEFFICIENT OF XSI (C_2, EQ. (17) IN PUBLICATION)
           !
           ZG = - 2._RKIND * CPPR(J9PSI) * ZCOEF * (ZDPBP(J9,JCHI) - ZCOEF * &
                &          TMF(J9PSI) * ZGBAR * ZDPBC(J9,JCHI) / ZJAC(J9,JCHI))
           !
           !  COMPUTATTION OF ABAL
           !
           ZDY = CHI(JCHI+1) - CHI(JCHI)
           !
           ZZA  = ZJAC(J9,JCHI) * ZF
           ZZP  = ZJAC(J9,JCHI) * ZG
           ZX11 = .25_RKIND * ZZP + ZZA / (ZDY * ZDY)
           ZX12 = .25_RKIND * ZZP - ZZA / (ZDY * ZDY)
           ZX22 = .25_RKIND * ZZP + ZZA / (ZDY * ZDY)
           !
           ABAL(J9,1,J10)   = ABAL(J9,1,J10)   + ZX11 * ZDY
           ABAL(J9,2,J10)   = ABAL(J9,2,J10)   + ZX12 * ZDY
           ABAL(J9,1,J10+1) = ABAL(J9,1,J10+1) + ZX22 * ZDY
           !
        END DO
     END DO
     !
     ! BOUNDARY CONDITIONS
     !
     DO J11=1,IPSIBAL
        !
        ABAL(J11,1,1)    = 1._RKIND
        ABAL(J11,2,1)    = 0._RKIND
        ABAL(J11,2,IMAX) = 0._RKIND
        ABAL(J11,1,IM1)  = 1._RKIND
        ABAL(J11,2,IM1)  = 0._RKIND
        !
     END DO
     !
     ! FIND THE NUMBER OF NEGATIVE EIGENVALUES
     !
     CALL NTRIDG(ABAL,NPPSBAL,1,IPSIBAL,IM1)
     !
     DO J13=1,IM1
        !
        DO J12=1,IPSIBAL
           !
           IF (ABAL(J12,1,J13) .LT. 0._RKIND) THEN
              !
              J12PSI = J12 + KP1 - 1
              NCBAL(J12PSI)      = NCBAL(J12PSI) + 1
              NCBLNS(J12PSI,J14) = NCBLNS(J12PSI,J14) + 1
              !
           ENDIF
           !
        END DO
     END DO
  END DO
  !
  DO J15=1,NBLC0
     !
     CHI0(J15) = CHIM(ICHI0(J15))
     !
  END DO
  !
  RETURN
END SUBROUTINE BALOON
