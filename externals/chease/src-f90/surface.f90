!*DECK C2SM02
!*CALL PROCESS
SUBROUTINE SURFACE(K,PSIGMA,PTETA,PGWGT,PS)
  !        ################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM02 EVALUATE FLUX SURFACE INTEGRALS ALONG CONSTANT POLOIDAL FLUX *
  !        SURFACES                                                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  USE sigmaneomod
  USE interpos_module
  IMPLICIT NONE
  !
!!$         PARAMETER (NPGMAP=NPMGS*NTP1)
  !
  INTEGER ::              K
  REAL(RKIND)      ::     PGWGT(*), PSIGMA(*), PTETA(*)
  REAL(RKIND)      ::     PS
  !
  REAL(RKIND)      ::     ZFTKM1
  REAL(RKIND)      ::     ZALFKM1
  REAL(RKIND)      ::     ZL32KM1
  REAL(RKIND)      ::     ZL31KM1
  REAL(RKIND)      ::     ZL34KM1
  REAL(RKIND)      ::     ZNEKM1
  REAL(RKIND)      ::     ZRPEKM1
  REAL(RKIND)      ::     ZEFKM1
  REAL(RKIND)      ::     ZTIPKM1
  REAL(RKIND)      ::     ZTIKM1
  REAL(RKIND)      ::     ZTEPKM1
  REAL(RKIND)      ::     ZTEKM1
  REAL(RKIND)      ::     ZL32_0
  REAL(RKIND)      ::     ZL31_0
  REAL(RKIND)      ::     ZSIG_0
  REAL(RKIND)      ::     ZSIG
  REAL(RKIND)      ::     ZFTEF3
  REAL(RKIND)      ::     ZFTEF2
  REAL(RKIND)      ::     ZFTEF
  REAL(RKIND)      ::     ZRTET0
  REAL(RKIND)      ::     ZOTAUE
  REAL(RKIND)      ::     ZVTE
  REAL(RKIND)      ::     ZLNLAM
  REAL(RKIND)      ::     ZPMKSA
  REAL(RKIND)      ::     ZMU0
  REAL(RKIND)      ::     ZERGEV
  REAL(RKIND)      ::     ZCHARGE
  REAL(RKIND)      ::     ZMASSE
  REAL(RKIND)      ::     ZRPEOP
  REAL(RKIND)      ::     ZTEMPIP
  REAL(RKIND)      ::     ZTEMPI
  REAL(RKIND)      ::     ZDENSEP
  REAL(RKIND)      ::     ZDENSE
  REAL(RKIND)      ::     ZTEMPEP
  REAL(RKIND)      ::     ZTEMPE
  REAL(RKIND)      ::     ZFT2
  REAL(RKIND)      ::     ZFT
  REAL(RKIND)      ::     ZALFA_0
  REAL(RKIND)      ::     ZALFA
  REAL(RKIND)      ::     ZA2E
  REAL(RKIND)      ::     ZA2I
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZL32
  REAL(RKIND)      ::     ZL31
  REAL(RKIND)      ::     ZL34
  REAL(RKIND)      ::     ZDX
  REAL(RKIND)      ::     ZX
  REAL(RKIND)      ::     ZJ0
  REAL(RKIND)      ::     ZRZION
  REAL(RKIND)      ::     ZETAEI
  REAL(RKIND)      ::     ZVPAR
  REAL(RKIND)      ::     ZIVPAR
  REAL(RKIND)      ::     ZB2MIN
  REAL(RKIND)      ::     ZB2MAX
  REAL(RKIND)      ::     ZTB2MIN
  REAL(RKIND)      ::     ZRMID
  REAL(RKIND)      ::     ZX2
  REAL(RKIND)      ::     ZX1
  REAL(RKIND)      ::     ZROFZMN
  REAL(RKIND)      ::     ZZMIN
  REAL(RKIND)      ::     ZROFZMX
  REAL(RKIND)      ::     ZZMAX
  REAL(RKIND)      ::     ZRMIN
  REAL(RKIND)      ::     ZTMIN
  REAL(RKIND)      ::     ZRMAX
  REAL(RKIND)      ::     ZTRMAX
  REAL(RKIND)      ::     ZTMAX
  REAL(RKIND)      ::     ZB2SHF
  REAL(RKIND)      ::     ZGPISO
  REAL(RKIND)      ::     ZZISO
  REAL(RKIND)      ::     ZRISO
  REAL(RKIND)      ::     ZCP1
  REAL(RKIND)      ::     ZIDQ
  REAL(RKIND)      ::     ZIJ7(10)  ! should be .ge. to 2nd dim of RJ7s in g_2.f90
  REAL(RKIND)      ::     ZIJ6
  REAL(RKIND)      ::     ZIJ5P
  REAL(RKIND)      ::     ZIJ5
  REAL(RKIND)      ::     ZIJ4
  REAL(RKIND)      ::     ZIJ3
  REAL(RKIND)      ::     ZIJ2
  REAL(RKIND)      ::     ZIJ1
  REAL(RKIND)      ::     ZDBCHIO
  REAL(RKIND)      ::     ZDBCHIN
  REAL(RKIND)      ::     ZBINT2
  REAL(RKIND)      ::     ZBINT1
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
  REAL(RKIND)      ::     ZINTLE1
  REAL(RKIND)      ::     ZINTLE
  REAL(RKIND)      ::     ZINTIR
  REAL(RKIND)      ::     ZINTIE
  REAL(RKIND)      ::     ZINTV
  REAL(RKIND)      ::     ZINTB
  REAL(RKIND)      ::     ZINTB2
  REAL(RKIND)      ::     ZINTBI2
  REAL(RKIND)      ::     ZINTR2
  REAL(RKIND)      ::     ZINTP2
  REAL(RKIND)      ::     ZINTP
  REAL(RKIND)      ::     ZIDA
  REAL(RKIND)      ::     ZIPR2
  REAL(RKIND)      ::     ZIPR1
  REAL(RKIND)      ::     ZDCHIO
  REAL(RKIND)      ::     ZDCHIN
  REAL(RKIND)      ::     ZINT2
  REAL(RKIND)      ::     ZINT1
  REAL(RKIND)      ::     ZJAC2
  REAL(RKIND)      ::     ZJAC1
  REAL(RKIND)      ::     ZR2
  REAL(RKIND)      ::     ZB2
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
  REAL(RKIND)      ::     ZD2BT2
  REAL(RKIND)      ::     ZD2BS2
  REAL(RKIND)      ::     ZDBDST
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZTETA
  REAL(RKIND)      ::     ZDPSIS
  REAL(RKIND)      ::     ZCDQ
  REAL(RKIND)      ::     ZC2
  REAL(RKIND)      ::     ZRIPR2
  REAL(RKIND)      ::     ZRIPR1
  REAL(RKIND)      ::     ZCPRK
  REAL(RKIND)      ::     ZEPS08
  REAL(RKIND)      ::     ZEPS
  REAL(RKIND)      ::     ABSQRT
  REAL(RKIND)      ::     ZNI, ZSIGNEO, ZSIGSPTZ, ZDUMMY
  REAL(RKIND)      ::     ZDAH, ZDAHPR1, ZDAHPR2, ZDTHSHA, ZDFM
  REAL(RKIND)      ::     ZAHPR1, ZAHPR2, ZTHSHA, ZTHSHA2, ZSHA, ZFM1, ZFM2, ZFM3, ZFM4
  REAL(RKIND)      ::     ZD2BCN, TMP1, TMP2, TMP3
  REAL(RKIND)      ::     ZBETCHI, ZTET, ZTET2
  INTEGER          ::     IZMIN, IZMAX, ISMIN, IRMIN, ISMAX, IRMAX
  INTEGER          ::     I7, IATRMAX, IC, IGMAX, IS0, IT0, KGAUS, NPGMAP
  INTEGER          ::     J, J1, J5, J6, J7, J8, J9, J10, JFM
  INTEGER          ::     JG, JS, JT, JTEFF, JTJUMP, JT2LAST
  INTEGER          ::     JTET0, JTETRMX, JB2MIN

  DIMENSION &
       &   IS0(npmgs*ntp1),       IT0(npmgs*ntp1),       IC(npmgs*ntp1), &
       &   ZBND(npmgs*ntp1,5),    ZB2(npmgs*ntp1),   ZDBDS(npmgs*ntp1,16), &
       &   ZDBDT(npmgs*ntp1,16),  ZDBDST(npmgs*ntp1,16), ZDCHIN(npmgs*ntp1), &
       &   ZDCHIO(npmgs*ntp1),    ZDBCHIN(npmgs*ntp1),   ZDBCHIO(npmgs*ntp1), &
       &   ZD2BS2(npmgs*ntp1,16), ZD2BT2(npmgs*ntp1,16), ZPCEL(npmgs*ntp1,16), &
       &   ZS1(npmgs*ntp1),       ZS2(npmgs*ntp1), &
       &   ZTETA(npmgs*ntp1,5),   ZT1(npmgs*ntp1), &
       &   ZT2(0:npmgs*ntp1+1),   ZIVPAR(NPISOEFF),      ZB2SHF(0:npmgs*ntp1+1), &
       &   ZDAH(npmgs*ntp1),      ZDAHPR1(npmgs*ntp1),   ZDAHPR2(npmgs*ntp1), &
       &   ZDTHSHA(npmgs*ntp1),   ZSHA(npmgs*ntp1),      ZDFM(npmgs*ntp1), &
       &   ZAHPR1(ntp2),          ZAHPR2(ntp2),          ZTHSHA(ntp2),    ZTHSHA2(3*ntp2),&
       &   tmp1(NTP2),            tmp2(npmgs*ntp1),      tmp3(npmgs*ntp1),  &
       &   ZD2BCN(NTP2),          ZBETCHI(npmgs*ntp1),   ZTET(NTP2),          ZTET2(3*NTP2)
 
  !
  !     ARRAYS FOR MIN, MAX SEARCH (ADD PERIODIC POINT)
  DIMENSION &
       &     ZRISO(0:npmgs*ntp1+1), ZZISO(0:npmgs*ntp1+1), ZGPISO(0:npmgs*ntp1+1)
  !
  !.......................................................................
  SAVE ZTEKM1, ZTEPKM1, ZTIKM1, ZTIPKM1, ZEFKM1, ZRPEKM1, ZNEKM1 &
       &     ,ZL31KM1, ZL32KM1, ZL34KM1, ZALFKM1, ZFTKM1
  !.......................................................................
  !
  REAL(rkind) :: xx
  ABSQRT(XX) = SQRT(ABS(XX))
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  NPGMAP=NPMGS*NTP1
  !
  ZEPS = 1.E-3_RKIND
  ZEPS08 = 1.E-08_RKIND
  ZCPRK = MAX(ZEPS08,CPR(K))
  !
  ZRIPR1    = 0._RKIND
  ZRIPR2    = 0._RKIND
  RARE(K)   = 0._RKIND
  RIP(K)    = 0._RKIND
  RIP2(K)   = 0._RKIND
  RIB(K)    = 0._RKIND
  RIB2(K)   = 0._RKIND
  RIBI2(K)  = 0._RKIND
  RIR2(K)   = 0._RKIND
  RIVOL(K)  = 0._RKIND
  RIIE(K)   = 0._RKIND
  RIIR(K)   = 0._RKIND
  RLENG(K)  = 0._RKIND
  RLENG1(K) = 0._RKIND
  eqchease_out(1)%profiles_1d%surface(K+1) = 0._RKIND
  ZC2       = 0._RKIND
  RJ1(K)    = 0._RKIND
  RJ2(K)    = 0._RKIND
  RJ3(K)    = 0._RKIND
  RJ4(K)    = 0._RKIND
  RJ5(K)    = 0._RKIND
  RJ5P(K)   = 0._RKIND
  RJ6(K)    = 0._RKIND
  RJ7s(K,:)  = 0._RKIND
  ZCDQ      = 0._RKIND
  !ab
  !ab      abs(dpsi/ds)
  !ab
  ZDPSIS = 2._RKIND*PS*CPSRF
  !ab
  !
  IGMAX = NMGAUS * NT1
  !C
  CALL RESETI(IC,IGMAX,1)
  DO JT = 1,NT1
     DO JG=1,IGMAX
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (PTETA(JG).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC,IGMAX,1)
  DO JS = 1,NS1
     DO JG=1,IGMAX
        IF (IC(JG).EQ.1) THEN
           IS0(JG) = JS-1
           IF (PSIGMA(JG).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J5=1,IGMAX
     !
     IF (IS0(J5).LT.  1) IS0(J5) = 1
     IF (IS0(J5).GE.NS1) IS0(J5) = NS
     IF (IT0(J5).LT.  1) IT0(J5) = 1
     IF (IT0(J5).GE.NT1) IT0(J5) = NT
     !
     ZTETA(J5,1) = PTETA(J5)
     ZTETA(J5,2) = PTETA(J5) - 2._RKIND * ZEPS
     ZTETA(J5,3) = PTETA(J5) -      ZEPS
     ZTETA(J5,4) = PTETA(J5) +      ZEPS
     ZTETA(J5,5) = PTETA(J5) + 2._RKIND * ZEPS
     !
     ZS1(J5) = CSIG(IS0(J5))
     ZS2(J5) = CSIG(IS0(J5)+1)
     ZT1(J5) = CT(IT0(J5))
     ZT2(J5) = CT(IT0(J5)+1)
  END DO
  !
  CALL BOUND(IGMAX,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(IGMAX,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(IGMAX,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(IGMAX,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(IGMAX,ZTETA(1,5),ZBND(1,5))
  !
  CALL PSICEL(IS0,IT0,IGMAX,NPGMAP,ZPCEL,CPSICL)
  CALL BASIS3(IGMAX,NPGMAP,ZS1,ZS2,ZT1,ZT2(1),PSIGMA,PTETA, &
       &               ZDBDS,ZDBDT,ZDBDST,ZD2BS2,ZD2BT2)
  !
  DO J6=1,IGMAX
     !
     ZDRSDT = (ZBND(J6,2) + 8*(ZBND(J6,4) - ZBND(J6,3)) - &
          &             ZBND(J6,5)) / (12._RKIND * ZEPS)
     ZD2RST = (- ZBND(J6,2) + 16._RKIND * ZBND(J6,3) - &
          &             30._RKIND * ZBND(J6,1) + 16._RKIND * ZBND(J6,4) - &
          &             ZBND(J6,5)) / (12._RKIND * ZEPS**2)
     !
     ZDPDS = ZDBDS(J6, 1) * ZPCEL(J6, 1) + &
          &           ZDBDS(J6, 2) * ZPCEL(J6, 2) + &
          &           ZDBDS(J6, 3) * ZPCEL(J6, 3) + &
          &           ZDBDS(J6, 4) * ZPCEL(J6, 4) + &
          &           ZDBDS(J6, 5) * ZPCEL(J6, 5) + &
          &           ZDBDS(J6, 6) * ZPCEL(J6, 6) + &
          &           ZDBDS(J6, 7) * ZPCEL(J6, 7) + &
          &           ZDBDS(J6, 8) * ZPCEL(J6, 8) + &
          &           ZDBDS(J6, 9) * ZPCEL(J6, 9) + &
          &           ZDBDS(J6,10) * ZPCEL(J6,10) + &
          &           ZDBDS(J6,11) * ZPCEL(J6,11) + &
          &           ZDBDS(J6,12) * ZPCEL(J6,12) + &
          &           ZDBDS(J6,13) * ZPCEL(J6,13) + &
          &           ZDBDS(J6,14) * ZPCEL(J6,14) + &
          &           ZDBDS(J6,15) * ZPCEL(J6,15) + &
          &           ZDBDS(J6,16) * ZPCEL(J6,16)
     !
     ZDPDT = ZDBDT(J6, 1) * ZPCEL(J6, 1) + &
          &           ZDBDT(J6, 2) * ZPCEL(J6, 2) + &
          &           ZDBDT(J6, 3) * ZPCEL(J6, 3) + &
          &           ZDBDT(J6, 4) * ZPCEL(J6, 4) + &
          &           ZDBDT(J6, 5) * ZPCEL(J6, 5) + &
          &           ZDBDT(J6, 6) * ZPCEL(J6, 6) + &
          &           ZDBDT(J6, 7) * ZPCEL(J6, 7) + &
          &           ZDBDT(J6, 8) * ZPCEL(J6, 8) + &
          &           ZDBDT(J6, 9) * ZPCEL(J6, 9) + &
          &           ZDBDT(J6,10) * ZPCEL(J6,10) + &
          &           ZDBDT(J6,11) * ZPCEL(J6,11) + &
          &           ZDBDT(J6,12) * ZPCEL(J6,12) + &
          &           ZDBDT(J6,13) * ZPCEL(J6,13) + &
          &           ZDBDT(J6,14) * ZPCEL(J6,14) + &
          &           ZDBDT(J6,15) * ZPCEL(J6,15) + &
          &           ZDBDT(J6,16) * ZPCEL(J6,16)
     !
     ZD2PST = ZDBDST(J6, 1) * ZPCEL(J6, 1) + &
          &            ZDBDST(J6, 2) * ZPCEL(J6, 2) + &
          &            ZDBDST(J6, 3) * ZPCEL(J6, 3) + &
          &            ZDBDST(J6, 4) * ZPCEL(J6, 4) + &
          &            ZDBDST(J6, 5) * ZPCEL(J6, 5) + &
          &            ZDBDST(J6, 6) * ZPCEL(J6, 6) + &
          &            ZDBDST(J6, 7) * ZPCEL(J6, 7) + &
          &            ZDBDST(J6, 8) * ZPCEL(J6, 8) + &
          &            ZDBDST(J6, 9) * ZPCEL(J6, 9) + &
          &            ZDBDST(J6,10) * ZPCEL(J6,10) + &
          &            ZDBDST(J6,11) * ZPCEL(J6,11) + &
          &            ZDBDST(J6,12) * ZPCEL(J6,12) + &
          &            ZDBDST(J6,13) * ZPCEL(J6,13) + &
          &            ZDBDST(J6,14) * ZPCEL(J6,14) + &
          &            ZDBDST(J6,15) * ZPCEL(J6,15) + &
          &            ZDBDST(J6,16) * ZPCEL(J6,16)
     !
     ZD2PS2 = ZD2BS2(J6, 1) * ZPCEL(J6, 1) + &
          &            ZD2BS2(J6, 2) * ZPCEL(J6, 2) + &
          &            ZD2BS2(J6, 3) * ZPCEL(J6, 3) + &
          &            ZD2BS2(J6, 4) * ZPCEL(J6, 4) + &
          &            ZD2BS2(J6, 5) * ZPCEL(J6, 5) + &
          &            ZD2BS2(J6, 6) * ZPCEL(J6, 6) + &
          &            ZD2BS2(J6, 7) * ZPCEL(J6, 7) + &
          &            ZD2BS2(J6, 8) * ZPCEL(J6, 8) + &
          &            ZD2BS2(J6, 9) * ZPCEL(J6, 9) + &
          &            ZD2BS2(J6,10) * ZPCEL(J6,10) + &
          &            ZD2BS2(J6,11) * ZPCEL(J6,11) + &
          &            ZD2BS2(J6,12) * ZPCEL(J6,12) + &
          &            ZD2BS2(J6,13) * ZPCEL(J6,13) + &
          &            ZD2BS2(J6,14) * ZPCEL(J6,14) + &
          &            ZD2BS2(J6,15) * ZPCEL(J6,15) + &
          &            ZD2BS2(J6,16) * ZPCEL(J6,16)
     !
     ZD2PT2 = ZD2BT2(J6, 1) * ZPCEL(J6, 1) + &
          &            ZD2BT2(J6, 2) * ZPCEL(J6, 2) + &
          &            ZD2BT2(J6, 3) * ZPCEL(J6, 3) + &
          &            ZD2BT2(J6, 4) * ZPCEL(J6, 4) + &
          &            ZD2BT2(J6, 5) * ZPCEL(J6, 5) + &
          &            ZD2BT2(J6, 6) * ZPCEL(J6, 6) + &
          &            ZD2BT2(J6, 7) * ZPCEL(J6, 7) + &
          &            ZD2BT2(J6, 8) * ZPCEL(J6, 8) + &
          &            ZD2BT2(J6, 9) * ZPCEL(J6, 9) + &
          &            ZD2BT2(J6,10) * ZPCEL(J6,10) + &
          &            ZD2BT2(J6,11) * ZPCEL(J6,11) + &
          &            ZD2BT2(J6,12) * ZPCEL(J6,12) + &
          &            ZD2BT2(J6,13) * ZPCEL(J6,13) + &
          &            ZD2BT2(J6,14) * ZPCEL(J6,14) + &
          &            ZD2BT2(J6,15) * ZPCEL(J6,15) + &
          &            ZD2BT2(J6,16) * ZPCEL(J6,16)
     !
     ZFP    = (ZDPDS**2 + (ZDPDT / PSIGMA(J6) - ZDPDS * ZDRSDT / &
          &            ZBND(J6,1))**2) / ZBND(J6,1)**2
     ZGRADP = SQRT(ZFP)
     !
     ZCOST  = COS(PTETA(J6))
     ZSINT  = SIN(PTETA(J6))
     !
     ZRHO    = PSIGMA(J6) * ZBND(J6,1)
     ZR      = ZRHO * ZCOST + R0
     ZZ      = ZRHO * ZSINT + RZ0
     ZJPHI   = - ZR * CPPR(K) - TTP(K) / ZR
     ZB2(J6) = (TMF(K)**2 + ZFP) / ZR**2
     ZR2     = ZR**2
     !
     ZJAC1 = ZR**(NER-1) * ZGRADP**NEGP * ZDPDS
     ZJAC2 = ZR * ZDPDS
     !
     ZINT1 = ZRHO * ZBND(J6,1) / ZJAC1
     ZINT2 = ZRHO * ZBND(J6,1) / ZJAC2
     !
     ZDCHIN(J6) = ZINT1 * PGWGT(J6)
     ZDCHIO(J6) = ZINT2 * PGWGT(J6)
     !
     ZIPR1 = ZJPHI * ZRHO * ZBND(J6,1) * ZR / ZJAC2
     ZIPR2 = ZRHO * ZBND(J6,1) * ZR / ZJAC2
     !
     ZRIPR1 = ZRIPR1 + ZIPR1 * PGWGT(J6)
     ZRIPR2 = ZRIPR2 + ZIPR2 * PGWGT(J6)
     !
     ZIDA    = ZRHO * ZBND(J6,1) * ZR     / ZJAC2
     ZINTP   = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZCPRK
     ZINTP2  = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZCPRK**2
     ZINTB   = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * sqrt(ZB2(J6))
     ZINTB2  = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZB2(J6)
     ZINTBI2 = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 / ZB2(J6)
     ZINTR2  = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZR2 
     ZINTV   = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2
     ZINTIE  = ZRHO * ZBND(J6,1) * ZR     / ZJAC2 * ZJPHI
     ZINTIR  = ZRHO * ZBND(J6,1) * ZFP    / ZJAC2
     ZINTLE  = ZRHO * ZBND(J6,1) * ZR     / ZJAC2 * ZGRADP
     ZINTLE1 = ZRHO * ZBND(J6,1)          / ZJAC2 * ZGRADP
     !
     RARE(K)   = RARE(K)   + ZIDA    * PGWGT(J6)
     RIP(K)    = RIP(K)    + ZINTP   * PGWGT(J6)
     RIP2(K)   = RIP2(K)   + ZINTP2  * PGWGT(J6)
     RIB(K)    = RIB(K)    + ZINTB   * PGWGT(J6)
     RIB2(K)   = RIB2(K)   + ZINTB2  * PGWGT(J6)
     RIBI2(K)  = RIBI2(K)  + ZINTBI2 * PGWGT(J6)
     RIR2(K)   = RIR2(K)   + ZINTR2  * PGWGT(J6)
     RIVOL(K)  = RIVOL(K)  + ZINTV   * PGWGT(J6)
     RIIE(K)   = RIIE(K)   + ZINTIE  * PGWGT(J6)
     RIIR(K)   = RIIR(K)   + ZINTIR  * PGWGT(J6)
     RLENG(K)  = RLENG(K)  + ZINTLE  * PGWGT(J6)
     RLENG1(K) = RLENG1(K) + ZINTLE1 * PGWGT(J6)
     ! toroidal flux surface envelop: dlp 2pi R
     eqchease_out(1)%profiles_1d%surface(K+1) = eqchease_out(1)%profiles_1d%surface(K+1) + ZINTLE * ZR * PGWGT(J6)
     ZC2       = ZC2       + ZINT2   * PGWGT(J6)
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBND(J6,1) * ZCOST) / ZBND(J6,1)**2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBND(J6,1) * ZSINT - ZDRSDT * ZCOST) / ZBND(J6,1)**2
     ZDTDZ = ZCOST / ZRHO
     !
     ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     Z1     = ZDPDS
     ZDZ1DS = ZD2PS2
     ZDZ1DT = ZD2PST
     Z2     = ZDPDT / PSIGMA(J6) - ZDRSDT * ZDPDS / ZBND(J6,1)
     ZDZ2DS = (ZD2PST - ZDPDT / PSIGMA(J6))  / PSIGMA(J6) - &
          &            ZDRSDT * ZD2PS2 / ZBND(J6,1)
     ZDZ2DT = - ZD2PST * ZDRSDT / ZBND(J6,1) + ZD2PT2 / PSIGMA(J6) + &
          &            ZDPDS * (ZDRSDT**2-ZD2RST*ZBND(J6,1)) / ZBND(J6,1)**2
     !
     ZDFDS = 2 * (Z1 * ZDZ1DS + Z2 * ZDZ2DS) / ZBND(J6,1)**2
     ZDFDT = - 2 * ZFP * ZDRSDT / ZBND(J6,1) + &
          &           2 * (Z1 * ZDZ1DT + Z2 * ZDZ2DT) / ZBND(J6,1)**2
     !
     ZDFDR = ZDFDS * ZDSDR + ZDFDT * ZDTDR
     ZDFDZ = ZDFDS * ZDSDZ + ZDFDT * ZDTDZ
     !
     ZDGDPN = (ZDFDR * ZDPDR + ZDFDZ * ZDPDZ) / ZFP
     ZDRDPN = ZDPDR / ZFP
     !
     ZBINT1 = ZRHO * ZBND(J6,1) * ((ZR * ZJPHI - &
          &            (1._RKIND + .5_RKIND * NEGP) * ZDGDPN) / ZFP - &
          &            (NER - 2) * ZDRDPN / ZR) / ZJAC1
     ZBINT2 = ZRHO * ZBND(J6,1) * (ZR * ZJPHI - ZDGDPN) / &
          &            (ZFP * ZJAC2)
     !
     ZDBCHIN(J6) = ZBINT1 * PGWGT(J6)
     ZDBCHIO(J6) = ZBINT2 * PGWGT(J6)
     ZDFM(J6) = ZINTV * PGWGT(J6)
     !
     !     COMPUTE INTEGRALS FOR LOCAL INTECHANGE FORMULAS (EQ.(22) IN
     !     CHEASE PAPER)
     !
     ZIJ1  = ZRHO * ZBND(J6,1)          / ZJAC2 / ZFP
     ZIJ2  = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 / ZFP
     ZIJ3  = ZRHO * ZBND(J6,1) * ZR2**2 / ZJAC2 / ZFP
     ZIJ4  = ZRHO * ZBND(J6,1)          / ZJAC2
     ZIJ5  = ZRHO * ZBND(J6,1) * ZR2    / ZJAC2
     ZIJ6  = ZRHO * ZBND(J6,1)          / ZJAC2 * ZFP    ! for <|grad(psi)|**2/R**2>
     ZIJ7(1)=ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZFP    ! for <|grad(psi)|**2>
     ZIJ7(2)=ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZGRADP ! for <|grad(psi)|>
     ZIJ7(3)=ZRHO * ZBND(J6,1) * ZR     / ZJAC2          ! for <1/R>
     ZIJ7(4)=ZRHO * ZBND(J6,1) * ZR2*ZR / ZJAC2          ! for <R>
     ZIJ7(5)=ZRHO * ZBND(J6,1) * ZR2*ZR2/ ZJAC2          ! for <R**2>
     ZIJ7(6)=ZRHO * ZBND(J6,1) * ZR2    / ZJAC2 * ZFP/ZB2(J6) ! for <|grad(psi)|**2/B**2>
     ZIJ7(7)=ZRHO * ZBND(J6,1) * ZR2*ZR / ZJAC2 / ZGRADP ! for <R/|grad(psi)|>=<1/Bp>
     ZIJ7(8)=ZRHO * ZBND(J6,1) * ZR2*ZR / ZJAC2 * ZGRADP / ZB2(J6) ! for <R |grad(psi)| / B**2>
     ZIJ7(9)=ZRHO * ZBND(J6,1) * ZR     / ZJAC2 * ZGRADP ! for <|grad(psi)| / R>
     ! j.B=j// B = -G dp/dpsi - dG/dpsi B^2 / mu0
     ZIJ7(10)=ZRHO* ZBND(J6,1) * ZR2 * ZB2(J6)/ZFP  / ZJAC2 ! for int(B^2/R^2/Bp^2 dlp/Bp) for Connor PoP 1998, 5, 2687, Eq. (1) last term
     ZIJ5P = (ZBND(J6,1) / ZDPDS)**2 * &
          &           (2._RKIND * ZR - R0 - PSIGMA(J6) * ZR * ZD2PS2 / ZDPDS)
     ZIDQ  = (ZBND(J6,1) / ZDPDS)**2 / ZR * &
          &           (R0 / ZR - PSIGMA(J6) * ZD2PS2 / ZDPDS)
     !
     RJ1(K)  = RJ1(K)  + ZIJ1  * PGWGT(J6)
     RJ2(K)  = RJ2(K)  + ZIJ2  * PGWGT(J6)
     RJ3(K)  = RJ3(K)  + ZIJ3  * PGWGT(J6)
     RJ4(K)  = RJ4(K)  + ZIJ4  * PGWGT(J6)
     RJ5(K)  = RJ5(K)  + ZIJ5  * PGWGT(J6)
     RJ5P(K) = RJ5P(K) + ZIJ5P * PGWGT(J6)
     RJ6(K)  = RJ6(K)  + ZIJ6  * PGWGT(J6)
     RJ7s(K,1)  = RJ7s(K,1)  + ZIJ7(1)  * PGWGT(J6)
     RJ7s(K,2)  = RJ7s(K,2)  + ZIJ7(2)  * PGWGT(J6)
     RJ7s(K,3)  = RJ7s(K,3)  + ZIJ7(3)  * PGWGT(J6)
     RJ7s(K,4)  = RJ7s(K,4)  + ZIJ7(4)  * PGWGT(J6)
     RJ7s(K,5)  = RJ7s(K,5)  + ZIJ7(5)  * PGWGT(J6)
     RJ7s(K,6)  = RJ7s(K,6)  + ZIJ7(6)  * PGWGT(J6)
     RJ7s(K,7)  = RJ7s(K,7)  + ZIJ7(7)  * PGWGT(J6)
     RJ7s(K,8)  = RJ7s(K,8)  + ZIJ7(8)  * PGWGT(J6)
     RJ7s(K,9)  = RJ7s(K,9)  + ZIJ7(9)  * PGWGT(J6)
     RJ7s(K,10) = RJ7s(K,10) + ZIJ7(10) * PGWGT(J6)
     ZCDQ    = ZCDQ    + ZIDQ  * PGWGT(J6)
     !
     RRISO(J6,K)  = ZR
     RZISO(J6,K)  = ZZ
     GPISO(J6,K)  = ZGRADP
     RHOISO(J6,K) = ZRHO
     BNDISO(J6,K) = ZBND(J6,1)
     DPTISO(J6,K) = ZDPDT
     DPSISO(J6,K) = ZDPDS
     DGNISO(J6,K) = ZDGDPN
     DGRISO(J6,K) = ZDFDR
     DGZISO(J6,K) = ZDFDZ
     DRNISO(J6,K) = ZDRDPN
     DPRISO(J6,K) = ZDPDR
     DPZISO(J6,K) = ZDPDZ
     ! %YC
     ! Elements to compute AH and AHPR for the transformation to field aligned 
     ! Hamada coordinates (in hamada.f90)
     ! AH = int_0^chi {1/R^2 J dchi}/C(psi) and 
     ! AHPR = int_0^chi {-2/R^3 dR/dpsi J dchi}/C(psi) with C(psi)=CP the constant in ZJAC
     ! Note that AHPR=dAH/dpsi if NER=0 and NEGP=0
     ZDAH(J6) = ZIJ4 * PGWGT(J6)
     ZDAHPR1(J6) = ZIJ4 * ZDPDR / ZR / ZFP * PGWGT(J6)
     ZDAHPR2(J6) = ZR**NER * ZGRADP**NEGP * &
          &            (PSIGMA(J6) * ZDRSDT * ZDPDS * ZCOST &
          &          - ZRHO * ZDPDS * ZSINT &
          &          - ZBND(J6,1) * ZDPDT * ZCOST) / ZR**3 / ZDPDS * PGWGT(J6)
     ! And element to compute Theta_Shaing (big theta in Shaing papers)
     ZDTHSHA(J6) = ZINTB * PGWGT(J6)
     ! %YC
     !
  END DO
  !
  eqchease_out(1)%profiles_1d%surface(K+1) = eqchease_out(1)%profiles_1d%surface(K+1) * TWOPI
  !
  BCHIN(1,K) = 0._RKIND
  BCHIO(1,K) = 0._RKIND
  CHIN(1,K)  = 0._RKIND
  CHIO(1,K)  = 0._RKIND
  AH(1,K)    = 0._RKIND
  ZTHSHA(1)  = 0._RKIND
  !
  DO J7=1,NT1
     !
     I7 = (J7 - 1) * NMGAUS
     !
     BCHIN(J7+1,K) = BCHIN(J7,K)
     BCHIO(J7+1,K) = BCHIO(J7,K)
     CHIN(J7+1,K)  = CHIN(J7,K)
     CHIO(J7+1,K)  = CHIO(J7,K)
     AH(J7+1,K)    = AH(J7,K)
     ZTHSHA(J7+1)  = ZTHSHA(J7)
     !
     DO KGAUS=1,NMGAUS
        BCHIN(J7+1,K) = BCHIN(J7+1,K) + ZDBCHIN(I7+KGAUS)
        BCHIO(J7+1,K) = BCHIO(J7+1,K) + ZDBCHIO(I7+KGAUS)
        CHIN(J7+1,K)  = CHIN(J7+1,K) + ZDCHIN(I7+KGAUS)
        CHIO(J7+1,K)  = CHIO(J7+1,K) + ZDCHIO(I7+KGAUS)
        AH(J7+1,K)    = AH(J7+1,K) + ZDAH(I7+KGAUS)
        ZTHSHA(J7+1)  = ZTHSHA(J7+1) + ZDTHSHA(I7+KGAUS)
     END DO
     !
  END DO
  !
  ZCP1    = CHIO(NT2,K) / TWOPI
  QPSI(K) = ZCP1 * TMF(K)
  CDQ(K)  = TMF(K) * BCHIO(NT2,K) / TWOPI + ZCP1 * TTP(K) / TMF(K)
  CP(K)   = CHIN(NT2,K) / TWOPI
  CPDP(K) = BCHIN(NT2,K) / TWOPI
  !
  CALL DSCAL(NT2,RC1P/ZCP1,CHIO(1,K),1)
  CALL DSCAL(NT2,RC1P/ZCP1,BCHIO(1,K),1)
  CALL DSCAL(NT2,RC1P/CP(K),CHIN(1,K),1)
  CALL DSCAL(NT2,RC1P/CP(K),BCHIN(1,K),1)
  CALL DSCAL(NT2,RC1P/CP(K),AH(1,K),1)
  CALL DSCAL(NT2,TWOPI/RIB(K),ZTHSHA(1),1)
  !
  CALL DAXPY(NT2,TTP(K)/(TMF(K)*TMF(K))-CDQ(K)/QPSI(K), &
       &              CHIO(1,K),1,BCHIO(1,K),1)
  CALL DAXPY(NT2,-CPDP(K)/CP(K),CHIN(1,K),1, &
       &              BCHIN(1,K),1)
  !
  CALL DSCAL(NT2,ZDPSIS,BCHIO(1,K),1)
  CALL DSCAL(NT2,ZDPSIS,BCHIN(1,K),1)
  !
  RIPR(K)   = ZRIPR1 / ZRIPR2
  ! jp of Eq. 2.14, I-parallel
  RJDOTB(K) = - CPPR(K) * RIVOL(K) / ZC2 - TTP(K) * &
       &                 (1._RKIND + RIIR(K) / (TMF(K)**2 * ZC2))
  RB2AV(K)  = RIB2(K) / RIVOL(K)
  ! <j.B> of (44)
  RJPAR(K)  = - (TMF(K) * CPPR(K) + TTP(K) * RB2AV(K) / TMF(K))
  !
  !  NORMALIZE INTEGRALS FOR LOCAL INTERCHANGE STABILITY CRITERIA
  !
  RJ1(K)  = RJ1(K) / TWOPI
  RJ2(K)  = RJ2(K) / TWOPI
  RJ3(K)  = RJ3(K) / TWOPI
  RJ4(K)  = RJ4(K) / TWOPI
  RJ5(K)  = RJ5(K) / TWOPI
  RJ5P(K) = RJ5P(K)/ TWOPI
  RJ6(K)  = RJ6(K) / TWOPI
  RJ7s(K,:)  = RJ7s(K,:) / TWOPI
  !
  ZCDQ = ZCDQ / TWOPI
  CDQ(K) = TMF(K) * ZCDQ + TTP(K) * RJ4(K) / TMF(K)
  !
  ! %YC
  ! Add another loop to compute AHPR (BCHIN required in the integral)
  ! 1) interpolate BCHIN on PTETA -> ZBETCHI (for Gauss integration)
  !    also interpolate ZTHSHA to compute the coeff. for neo. viscosity
  CALL DCOPY(NT2,TETMAP(1,K),1,ZTET,1)
  DO J1=2,NT2
     IF (ZTET(J1) .LT. ZTET(J1-1)) THEN
        ZTET(J1) = ZTET(J1) + TWOPI * (1._RKIND + &
             &                   INT(ABS(ZTET(J1) - ZTET(J1-1)) / TWOPI))
     ENDIF
  ENDDO
  !
  ! change from spline to interpos tested in ../WK/TESTCASES/NIDEAL10/matlab/debug_fort42.m
  ! keep write commands commented at this stage
  !OS CALL SPLINE(NT2,ZTET,BCHIN(1,K),ZD2BCN,tmp1)
  !OS CALL PPSPLN2(IGMAX,PTETA,NT2-1,ZTET, &
  !OS &          BCHIN(1,K),ZD2BCN,ZBETCHI,tmp2,tmp3)
  !
  !OS write(42,*) k,nt2, niso1eff, igmax
  !OS write(42,*) (ZTET(j1),j1=1,nt2)
  !OS write(42,*) (bchin(j1,k),j1=1,nt2)
  !OS write(42,*) (pteta(j1),j1=1,igmax)
  !OS write(42,*) (Zbetchi(j1),j1=1,igmax)
  ZTET2(1:NT1)=ZTET(1:NT1)
  ! periodic spline only with non-zero tension
  CALL INTERPOS(ZTET2(1:NT1),BCHIN(1:NT1,K),NT1,nout=igmax,xout=PTETA(1:igmax),tension=-3._rkind,yout=ZBETCHI(1:igmax), &
       & nbc=-1,ybc=twopi)
  !OS write(42,*) (Zbetchi(j1),j1=1,igmax)
  !
  !OS CALL SPLINE(NT2,ZTET,ZTHSHA(1),ZD2BCN,tmp1)
  !OS CALL PPSPLN2(IGMAX,PTETA,NT2-1,ZTET, &
  !OS &          ZTHSHA(1),ZD2BCN,ZSHA,tmp2,tmp3)
  !OS write(42,*) (ZTHSHA(j1),j1=1,nt2)
  !OS write(42,*) (ZSHA(j1),j1=1,igmax)
  ! for ZTHSHA, it is not periodic since it increases by 2*pi every 2*pi.
  ! One should add a curve with -2pi and one with +2pi to have correct values at edges
  ! Take into account the fact that the point at nt2 is a repeat of point at (1)+2pi
  ZTET2=(/ ZTET2(1:NT1)-TWOPI, ZTET2(1:NT1), ZTET(1:NT1)+TWOPI /)
  ZTHSHA2=(/ ZTHSHA(1:NT1)-TWOPI, ZTHSHA(1:NT1), ZTHSHA(1:NT1)+TWOPI /)
  CALL INTERPOS(ZTET2,ZTHSHA2,3*NT1,nout=igmax,xout=PTETA(1:igmax),tension=-0.001_rkind,yout=ZSHA(1:igmax))
  !OS write(42,*) (ZTET2(j1),j1=1,3*nt1)
  !OS write(42,*) (ZTHSHA2(j1),j1=1,3*nt1)
  !OS write(42,*) (ZSHA(j1),j1=1,igmax)
  !OS write(42,*) (PGWGT(j1),j1=1,igmax) ! to check fm6
  !OS write(42,*) (ZB2(j1),j1=1,igmax) ! to check fm6
  !OS write(42,*) (ZDFM(j1),j1=1,igmax) ! to check fm6
  !OS write(42,*) RIB(K), RIB2(K)
  !
  ! 2) calculate AHPR 
  ! AHPRCOR is used for a correction due to non zero ZBETCHI(THETA=0), applied in chipsimetrics.f90
  AHPR(1,K)=0._RKIND
  AHPRCOR(1,K)=0._RKIND
  ZAHPR1(1)=0._RKIND
  ZAHPR2(1)=0._RKIND
!
  DO J7=1,NT1
     I7 = (J7 - 1) * NMGAUS
     ZAHPR1(J7+1) = ZAHPR1(J7)
     ZAHPR2(J7+1) = ZAHPR2(J7)
     AHPRCOR(J7+1,K) = AHPRCOR(J7,K)
     !
     DO KGAUS=1,NMGAUS
        ZAHPR1(J7+1) = ZAHPR1(J7+1) + ZDAHPR1(I7+KGAUS)
        ZAHPR2(J7+1) = ZAHPR2(J7+1) + ZBETCHI(I7+KGAUS)/ZDPSIS*ZDAHPR2(I7+KGAUS)
        AHPRCOR(J7+1,K) = AHPRCOR(J7+1,K) + ZDAHPR2(I7+KGAUS)
     END DO
     !
     AHPR(J7+1,K)= -2*ZAHPR1(J7+1)/CP(K) + 2*ZAHPR2(J7+1)
     !
  END DO
  !
  DO JFM=1,6
    ZFM1=0._RKIND
    ZFM2=0._RKIND
    ZFM3=0._RKIND
    ZFM4=0._RKIND
    DO J6=1,IGMAX
      ZFM1 = ZFM1 + real(JFM,RKIND)*cos(real(JFM,RKIND)*ZSHA(J6))*sqrt(ZB2(J6))*log(sqrt(ZB2(J6)))*ZDFM(J6)
      ZFM2 = ZFM2 + real(JFM,RKIND)*cos(real(JFM,RKIND)*ZSHA(J6))*ZB2(J6)*ZDFM(J6)
      ZFM3 = ZFM3 + real(JFM,RKIND)*sin(real(JFM,RKIND)*ZSHA(J6))*sqrt(ZB2(J6))*log(sqrt(ZB2(J6)))*ZDFM(J6)
      ZFM4 = ZFM4 + real(JFM,RKIND)*sin(real(JFM,RKIND)*ZSHA(J6))*ZB2(J6)*ZDFM(J6)
    tmp1(1)= tmp1(1) + cos(ZSHA(J6))*PGWGT(J6)
    tmp2(1)= tmp2(1) + sin(ZSHA(J6))*PGWGT(J6)
    END DO
    FM(JFM,K)= 8._RKIND*CPI**2*(ZFM1*ZFM2+ZFM3*ZFM4)/RIB(K)**3/RIB2(K)
  END DO

!!$    ZFM1=0._RKIND
!!$    ZFM2=0._RKIND
!!$    ZFM3=0._RKIND
!!$    ZFM4=0._RKIND
!!$    tmp1(1) = 0._RKIND
!!$    tmp2(1) = 0._RKIND
!!$    DO J6=1,IGMAX
!!$      ZFM1 = ZFM1 + sqrt(ZB2(J6))*log(sqrt(ZB2(J6)))*ZDFM(J6)
!!$      ZFM2 = ZFM2 + ZB2(J6)*ZDFM(J6)
!!$      ZFM3 = ZFM3 + sqrt(ZB2(J6))*log(sqrt(ZB2(J6)))*ZDFM(J6)
!!$      ZFM4 = ZFM4 + ZB2(J6)*ZDFM(J6)
!!$      tmp1(1)= tmp1(1) + cos(ZSHA(J6))*PGWGT(J6)
!!$      tmp2(1)= tmp2(1) + sin(ZSHA(J6))*PGWGT(J6)
!!$    END DO
!!$    FM(1,K)= ZFM1
!!$    FM(2,K)= ZFM2
!!$    FM(3,K)= ZFM3
!!$    FM(4,K)= ZFM4
!!$    FM(5,K)= tmp1(1)
!!$    FM(6,K)= tmp2(2)

  ! %YC end
  !
  !     FOR FITS OF MIN AND MAX ALONG PTETA, ADD AN EXTRA POINT
  !     BEFORE AND AFTER THE JUMP AND SHIFT PTETA TO ZT2 SUCH
  !     THAT ZT2(1) JUST AFTER JUMP => ADD ZT2(0) AND ZT2(IGMAX+1)
  !
  !     FIND JTJUMP
  !
  DO JT=1,IGMAX-1
     IF (PTETA(JT+1) .LE. PTETA(JT)) JTJUMP = JT+1
  ENDDO
  !     COPY ARRAYS FOR INTERPOLATION WITH JTJUMP SHIFT
  !
  DO JT=JTJUMP,IGMAX
     JTEFF = JT-JTJUMP+1
     ZT2(JTEFF)    = PTETA(JT)
     ZRISO(JTEFF)  = RRISO(JT,K)
     ZZISO(JTEFF)  = RZISO(JT,K)
     ZGPISO(JTEFF) = GPISO(JT,K)
     ZB2SHF(JTEFF) = ZB2(JT)
  END DO
  JT2LAST = IGMAX-JTJUMP+1
  DO JT=1,JTJUMP-1
     ZT2(JT2LAST+JT)    = PTETA(JT)
     ZRISO(JT2LAST+JT)  = RRISO(JT,K)
     ZZISO(JT2LAST+JT)  = RZISO(JT,K)
     ZGPISO(JT2LAST+JT) = GPISO(JT,K)
     ZB2SHF(JT2LAST+JT) = ZB2(JT)
  END DO
  ZT2(0) = ZT2(IGMAX) - TWOPI
  ZT2(IGMAX+1) = ZT2(1) + TWOPI
  ZRISO(0) = ZRISO(IGMAX)
  ZRISO(IGMAX+1) = ZRISO(1)
  ZZISO(0) = ZZISO(IGMAX)
  ZZISO(IGMAX+1) = ZZISO(1)
  ZB2SHF(0) = ZB2SHF(IGMAX)
  ZB2SHF(IGMAX+1) = ZB2SHF(1)
  ZGPISO(0) = ZGPISO(IGMAX)
  ZGPISO(IGMAX+1) = ZGPISO(1)
  !
  !%OS
  DO J=0,IGMAX
     IF (ZT2(J+1) .LE. ZT2(J)) THEN
        PRINT *,' ERROR IN ZT2 IN SURFACE:'
        PRINT *,' ZT2(J=',J,')= ',ZT2(J),'  >  ','ZT2(J+1)= ' &
             &         ,ZT2(J+1)
        PRINT *,' IGMAX= ',IGMAX,' K,PS= ',K,PS
        STOP 'ZT2'
     ENDIF
  ENDDO
  !%OS
  IRMAX = ISMAX(IGMAX,ZRISO(1),1)
  IRMIN = ISMIN(IGMAX,ZRISO(1),1)
  IZMAX = ISMAX(IGMAX,ZZISO(1),1)
  IZMIN = ISMIN(IGMAX,ZZISO(1),1)
  !
  ZTMAX = - .5_RKIND * FB1(ZRISO(IRMAX-1),ZRISO(IRMAX), &
       &                      ZRISO(IRMAX+1),ZT2(IRMAX-1), &
       &                      ZT2(IRMAX),ZT2(IRMAX+1)) / &
       &                  FB2(ZRISO(IRMAX-1),ZRISO(IRMAX), &
       &                      ZRISO(IRMAX+1),ZT2(IRMAX-1), &
       &                      ZT2(IRMAX),ZT2(IRMAX+1))
  !
  ZTRMAX = ZTMAX
  JTETRMX = IRMAX-1
  ZRMAX = FQQQ0(ZRISO(IRMAX-1),ZRISO(IRMAX), &
       &                 ZRISO(IRMAX+1),ZT2(IRMAX-1), &
       &                 ZT2(IRMAX),ZT2(IRMAX+1),ZTMAX)
  !
  ZTMIN = - .5_RKIND * FB1(ZRISO(IRMIN-1),ZRISO(IRMIN), &
       &                      ZRISO(IRMIN+1),ZT2(IRMIN-1), &
       &                      ZT2(IRMIN),ZT2(IRMIN+1)) / &
       &                  FB2(ZRISO(IRMIN-1),ZRISO(IRMIN), &
       &                      ZRISO(IRMIN+1),ZT2(IRMIN-1), &
       &                      ZT2(IRMIN),ZT2(IRMIN+1))
  !
  ZRMIN = FQQQ0(ZRISO(IRMIN-1),ZRISO(IRMIN), &
       &                 ZRISO(IRMIN+1),ZT2(IRMIN-1), &
       &                 ZT2(IRMIN),ZT2(IRMIN+1),ZTMIN)
  !
  ZTMAX = - .5_RKIND * FB1(ZZISO(IZMAX-1),ZZISO(IZMAX), &
       &                      ZZISO(IZMAX+1),ZT2(IZMAX-1), &
       &                      ZT2(IZMAX),ZT2(IZMAX+1)) / &
       &                  FB2(ZZISO(IZMAX-1),ZZISO(IZMAX), &
       &                      ZZISO(IZMAX+1),ZT2(IZMAX-1), &
       &                      ZT2(IZMAX),ZT2(IZMAX+1))
  !
  ZZMAX = FQQQ0(ZZISO(IZMAX-1),ZZISO(IZMAX), &
       &                 ZZISO(IZMAX+1),ZT2(IZMAX-1), &
       &                 ZT2(IZMAX),ZT2(IZMAX+1),ZTMAX)
  !
  ZROFZMX = FQQQ0(ZRISO(IZMAX-1),ZRISO(IZMAX), &
       &                 ZRISO(IZMAX+1),ZT2(IZMAX-1), &
       &                 ZT2(IZMAX),ZT2(IZMAX+1),ZTMAX)
  !
  ZTMIN = - .5_RKIND * FB1(ZZISO(IZMIN-1),ZZISO(IZMIN), &
       &                      ZZISO(IZMIN+1),ZT2(IZMIN-1), &
       &                      ZT2(IZMIN),ZT2(IZMIN+1)) / &
       &                  FB2(ZZISO(IZMIN-1),ZZISO(IZMIN), &
       &                      ZZISO(IZMIN+1),ZT2(IZMIN-1), &
       &                      ZT2(IZMIN),ZT2(IZMIN+1))
  !
  ZZMIN = FQQQ0(ZZISO(IZMIN-1),ZZISO(IZMIN), &
       &                 ZZISO(IZMIN+1),ZT2(IZMIN-1), &
       &                 ZT2(IZMIN),ZT2(IZMIN+1),ZTMIN)
  !
  ZROFZMN = FQQQ0(ZRISO(IZMIN-1),ZRISO(IZMIN), &
       &                 ZRISO(IZMIN+1),ZT2(IZMIN-1), &
       &                 ZT2(IZMIN),ZT2(IZMIN+1),ZTMIN)
  !
  ZX1 = ZZMAX - ZZMIN
  ZX2 = ZRMAX - ZRMIN
  !
  RELL(K)   = (ZX1 - ZX2) / (ZX1 + ZX2)
  eqchease_out(1)%profiles_1d%elongation(K+1) = ZX1 / ZX2
  !
  eqchease_out(1)%profiles_1d%r_inboard(K+1) = ZRMIN
  eqchease_out(1)%profiles_1d%r_outboard(K+1) = ZRMAX
  ZRMID = 0.5_RKIND * (ZRMAX + ZRMIN)
  eqchease_out_add_1d(K+1,iiamin) = 0.5_RKIND * ZX2
  eqchease_out_add_1d(K+1,iirgeo) = ZRMID
  ARATIO(K+1) = ZRMID / eqchease_out_add_1d(K+1,iiamin)
  eqchease_out(1)%profiles_1d%tria_lower(K+1) = (ZRMID-ZROFZMN) / eqchease_out_add_1d(K+1,iiamin)
  eqchease_out(1)%profiles_1d%tria_upper(K+1) = (ZRMID-ZROFZMX) / eqchease_out_add_1d(K+1,iiamin)
  !
  IRMAX = ISMAX(IGMAX,ZB2SHF(1),1)
  IRMIN = ISMIN(IGMAX,ZB2SHF(1),1)
  !
  ZTMAX = - .5_RKIND * FB1(ZB2SHF(IRMAX-1),ZB2SHF(IRMAX), &
       &                      ZB2SHF(IRMAX+1),ZT2(IRMAX-1), &
       &                      ZT2(IRMAX),ZT2(IRMAX+1)) / &
       &                  FB2(ZB2SHF(IRMAX-1),ZB2SHF(IRMAX), &
       &                      ZB2SHF(IRMAX+1),ZT2(IRMAX-1), &
       &                      ZT2(IRMAX),ZT2(IRMAX+1))
  !
  ZB2MAX = FQQQ0(ZB2SHF(IRMAX-1),ZB2SHF(IRMAX), &
       &                     ZB2SHF(IRMAX+1),ZT2(IRMAX-1), &
       &                     ZT2(IRMAX),ZT2(IRMAX+1),ZTMAX)
  !
  JB2MIN = IRMIN-1
  ZTB2MIN = - .5_RKIND * FB1(ZB2SHF(IRMIN-1),ZB2SHF(IRMIN), &
       &                      ZB2SHF(IRMIN+1),ZT2(IRMIN-1), &
       &                      ZT2(IRMIN),ZT2(IRMIN+1)) / &
       &                  FB2(ZB2SHF(IRMIN-1),ZB2SHF(IRMIN), &
       &                      ZB2SHF(IRMIN+1),ZT2(IRMIN-1), &
       &                      ZT2(IRMIN),ZT2(IRMIN+1))
  ZB2MIN = FQQQ0(ZB2SHF(IRMIN-1),ZB2SHF(IRMIN), &
       &                     ZB2SHF(IRMIN+1),ZT2(IRMIN-1), &
       &                     ZT2(IRMIN),ZT2(IRMIN+1),ZTB2MIN)
  eqchease_out_add_1d(K+1,iiBmin) = SQRT(ZB2MIN)
  eqchease_out_add_1d(K+1,iiBmax) = SQRT(ZB2MAX)
  eqchease_out(1)%profiles_1d%b_min(K+1) = SQRT(ZB2MIN)
  eqchease_out(1)%profiles_1d%b_max(K+1) = SQRT(ZB2MAX)
  !
  !        FOR MIDPOINT INTEGRATION IN Y = LAMBDA * BMAX OF
  !        FCIRC = FRACTION OF CIRCULATING PARTICLES IN HIRSHMAN,
  !        PHYS.FLUIDS 31 (10), 1988, P3150, THE CSIPR(J),J=1,NISO-1
  !        MESH IS USED FOR Y. BMAX IS ASSUMED TO BE ON THE INSIDE
  !        OF THE PLASMA.
  !
  DO J9=1,NISO-1
     !
     ZIVPAR(J9) = 0._RKIND
     !
     DO J8=1,IGMAX
        !
        ZVPAR  = ZRISO(J8) * PSIGMA(J8) * ZBND(J8,1)**2 * &
             &            ABSQRT(1._RKIND - CSIPR(J9) * SQRT(ZB2(J8) / ZB2MAX))/ &
             &            DPSISO(J8,K)
        !
        ZIVPAR(J9) = ZIVPAR(J9) + ZVPAR * PGWGT(J8)
        !
     END DO
     !
     ZIVPAR(J9) = ZIVPAR(J9) / RIVOL(K)
     !
  END DO
  !
  ! INTEGRATE FCIRC USING CONSTANT STEPS
  !
  RFCIRC(K) = 0._RKIND
  !
  DO J10=1,NISO-1
     !
     RFCIRC(K) = RFCIRC(K) + 0.75_RKIND * RB2AV(K) * CSIPR(J10) * &
          &               (CSIPRI(J10+1) - CSIPRI(J10)) / &
          &               (ZB2MAX * ZIVPAR(J10))
     !
  END DO
  !
  ZFT  = 1._RKIND - RFCIRC(K)
  ZFT2 = 1.46_RKIND / SQRT(ASPCT)
  !  FIX-UP TO GET RID OF INACCURATE BEHAVIOUR NEAR THE ORIGIN
  IF (ZFT .LE. 0.0_RKIND) ZFT = ZFT2
  !
  !  BOOTSTRAP CURRENT DENSITY GIVEN FROM O.SAUTER (INCLUDING COLLISIONALITY)
  !  RJBSOS(K). RZION IS THE ION CHARGE, RPOPE=P/P_E, T_I/T_E=CST
  !  ZFT THE FRACTION FTRAPPED, NEED ALSO T_E PROFILE => USE ETAEI OR AT4
  !  IF ETAEI>=0: T_E FROM ETAEI AND ASSUME N_E[1E19]=T_E[KEV]
  !  IF ETAEI<0 : T_E GIVEN AS POLYNOMIALS OF S**2 WITH AT4 IN EV
  !
  !     RZION < 0 => RZION IS A PROFILE GIVEN WITH AT2 ARRAY
  !     RPOPE < 0 => PE/P  IS A PROFILE GIVEN WITH AT3 ARRAY
  !
  !     PE/P = RPEOP OR FROM AT3 POLYNOMIAL IF RPEOP.LT.0.
  !
  !     OR PROFILES GIVEN FROM EXPEQ FILE, IN THIS CASE, AT3 MIGHT GIVE NE
  !
  CALL BSTNZPRO(ZTEMPE,ZTEMPEP,ZDENSE,ZDENSEP,ZRZION,ZTEMPI, &
       &     ZTEMPIP,ZRPEOP,PS,1,ZCPRK,CPR(1),CPPR(K),1)
  !
  !%OS     Take ni such as pi=ni*Ti and pi=p-pe=(1-Rpe)*p, but if p_chease and ne*Te from experiment don't match => too large ni
  !%OS         zni=(1._RKIND-ZRPEOP) * CPR(K) * B0EXP**2 / (4.E-07_RKIND * CPI) / ZTEMPI / 1.602E-19_RKIND
  !%OS         zni=max(zdense,zni)
  !        ni=(p-pe)/Ti, but from input pe and pe/p instead of p from equilibrium as could not match:
  !        ni=(1-Rpe)/Rpe *pe/Ti with pe=ne*Te and Rpe as inputs
  !         zni=(1._RKIND-ZRPEOP)/ZRPEOP * ZDENSE * ZTEMPE / ZTEMPI
  ! assumes z_imp=6
  !
  zni=(6._RKIND-ZRZION)/(6._RKIND-1._RKIND) * ZDENSE
  !
  CALL SIGMANEO(zsigneo,zsigsptz,RNUSTAR(K),ZFT,ZDENSE,ZTEMPE,ZRZION, &
       & QPSI(K),eqchease_out_add_1d(K+1,iirgeo)*R0EXP,1._RKIND/ARATIO(K+1))
  eqchease_out_add_1d(K+1,IITE) = ZTEMPE
  eqchease_out_add_1d(K+1,IIDTEDPSI) = ZTEMPEP
  eqchease_out_add_1d(K+1,IINE) = ZDENSE
  eqchease_out_add_1d(K+1,IIDNEDPSI) = ZDENSEP
  eqchease_out_add_1d(K+1,IITI) = ZTEMPI
  eqchease_out_add_1d(K+1,IIDTIDPSI) = ZTEMPIP
  eqchease_out_add_1d(K+1,IINI) = zni
  eqchease_out_add_1d(K+1,IIDNIDPSI) = (6._RKIND-ZRZION)/(6._RKIND-1._RKIND) * ZDENSEP
  eqchease_out_add_1d(K+1,IIZEFF) = ZRZION
  eqchease_out_add_1d(K+1,IINUESTAR) = RNUSTAR(K)
  eqchease_out_add_1d(K+1,iisigneo) = zsigneo
  !
  ! Within chease uses nuestar with Zeff=1 for backward compatibility of diagnostic
  RNUSTAR(K) = RNUSTAR(K) / ZRZION
  !
  ! Computes profiles and derivatives with respect to psi. Save them in EQCHEASE_OUT_ADD_1D array for later use
  !
  CALL BSCOEFF(ZFT,QPSI(K),eqchease_out_add_1d(K+1,iirgeo)*R0EXP,1._RKIND/ARATIO(K+1),ZTEMPE,ZDENSE,ZTEMPI, &
       & zni,ZRZION,1._RKIND,ZL31_0,ZL32_0,ZALFA_0,ZL31,ZL32,ZL34,ZALFA)
  !
  ZA2E = 0._RKIND
  IF (ZTEMPE .NE. 0._RKIND) ZA2E = ZRPEOP * ZCPRK * ZTEMPEP / ZTEMPE
  ZA2I = 0._RKIND
  IF (ZTEMPI .NE. 0._RKIND) ZA2I = ZRPEOP * ZCPRK * ZTEMPIP / ZTEMPI
  IF (ZTEMPEP .EQ. 0.0_RKIND) ZTEMPEP = 1.0E-13_RKIND
  IF (ZTEMPIP .EQ. 0.0_RKIND) ZTEMPIP = 1.0E-13_RKIND
  ZA1 = CPPR(K)
  !     For RJBSOS(K,4), use the standard definition. However one mixes p' from chease inputs and ne', Te', Ti' from
  !     experimental data. If ne*Te+ni*Ti exper. is not near p_chease, it can give strange profiles, as some cancellation do not appear.
  !     Thus develop p'=ne' Te + ne Te' + ni' Ti + ni Ti', and assume ni'/ni = ne'/ne and take ni*Ti=p-pe:
  !     => for (K,1-3) use developed formula as well as for nue*=0 case
  !
  ! All quantities are already in CHEASE units except [pe]_chease=e ne Te * mu0/B0^2
  RJBSOS(K,1) = - TMF(K) * ZDENSE*ZTEMPE/ZRPEOP*1.602E-19_RKIND*4.E-07_RKIND*CPI/B0EXP**2 * &
       (ZL31_0*ZDENSEP/ZDENSE + ZRPEOP*(ZL31_0+ZL32_0)*ZTEMPEP/ZTEMPE + &
       (1._RKIND-ZRPEOP)*(1._RKIND+ZALFA_0)*ZL31_0*ZTEMPIP/ZTEMPI)
  RJBSOS(K,2) = - TMF(K) * ZDENSE*ZTEMPE/ZRPEOP*1.602E-19_RKIND*4.E-07_RKIND*CPI/B0EXP**2 * &
       (ZL31*ZDENSEP/ZDENSE + ZRPEOP*(ZL31+ZL32)*ZTEMPEP/ZTEMPE + &
       (1._RKIND-ZRPEOP)*(ZL31+ZL34*ZALFA)*ZTEMPIP/ZTEMPI)
  !
  eqchease_out_add_1d(K+1,iijbsBav) = RJBSOS(K,2)
  !
  RJBSOS(K,3) = - TMF(K) * CPR(K) * &
       (ZL31*ZDENSEP/ZDENSE + ZRPEOP*(ZL31+ZL32)*ZTEMPEP/ZTEMPE + &
       (1._RKIND-ZRPEOP)*(ZL31+ZL34*ZALFA)*ZTEMPIP/ZTEMPI)
  RJBSOS(K,4) = - TMF(K)* &
       &     (ZL31  * ZA1+ZL34*ZALFA*(1._RKIND/ZRPEOP-1._RKIND)*ZA2I + ZL32 * ZA2E)
  !
  !     R(TET=ZTB2MIN), B_P(TET=ZTB2MIN), NUESTAR
  !
  ZRTET0 = ZRMAX
  IATRMAX = 1
  IF (IATRMAX .EQ. 1) THEN
     !     COMPUTE BPOL AT R=RMAX
     JTET0 = JTETRMX
     RBPOL0(K) = FQQQ0(ZGPISO(JTET0),ZGPISO(JTET0+1), &
          &       ZGPISO(JTET0+2),ZT2(JTET0),ZT2(JTET0+1),ZT2(JTET0+2), &
          &       ZTRMAX) / ZRTET0
  ELSE
     !     COMPUTE BPOL AT B2MIN LOCATION (DIFFERS SLIGTHLY)
     JTET0 = JB2MIN
     RBPOL0(K) = FQQQ0(ZGPISO(JTET0),ZGPISO(JTET0+1), &
          &       ZGPISO(JTET0+2),ZT2(JTET0),ZT2(JTET0+1),ZT2(JTET0+2), &
          &       ZTB2MIN) / ZRTET0
  ENDIF
  !
  IF ((NDIAGOP .GE. 0) .AND. (NVERBOSE.GE.2)) THEN
     IF (K.EQ.1) WRITE(6,'("  K  PSI/PSIMAX  PE[CHEASE] ", &
          &        "  NE[E19]     TE[KEV]     L31    L32 ", &
          &        "  alfa       q         EPS*B/BP     ZEFF   ", &
          &        "  PEPRIM[CHEA]   TI[KEV] DTE/DPSI[~KEV]", &
          &        " DTI/DPSI[~KEV] Dne/DPSI[~1e19]  ptot*T  ni  sigma_neo  eta_neo/eta_Sptzr")')
!!$     &        " DTI/DPSI[~KEV] Dne/DPSI[~1e19]  ptot*T  ni")')
     WRITE(*,'(I3,1P19E12.4)') K,PS**2,ZRPEOP*ZCPRK, &
          &        ZDENSE*1.E-19_RKIND,ZTEMPE*1.E-03_RKIND,ZL31, &
          &        ZL32,ZALFA,QPSI(K), &
          &        SQRT(ZB2MIN)/RBPOL0(K)/ARATIO(K+1),ZRZION,CPPR(K)*ZRPEOP, &
          &        ZTEMPI*1E-3_RKIND,ZTEMPEP*1E-3_RKIND,ZTEMPIP*1E-3_RKIND,ZDENSEP*1E-19_RKIND &
          &        ,TMF(K) * ZDENSE*ZTEMPE/ZRPEOP*1.602E-19_RKIND*4.E-07_RKIND*CPI/B0EXP**2, &
          &        zni*1.E-19_RKIND &
          &        ,eqchease_out_add_1d(K+1,iisigneo),zsigsptz/eqchease_out_add_1d(K+1,iisigneo)
  ENDIF
  !
  !.......................................................................
  !
  !   ADD DIAGNOSTICS
  !
  if (NDIAGOP .GE. 0) then
    CALL SURFADD(K,ZRMIN,ZRMAX,ZZMIN,ZZMAX,ZB2MIN,ZROFZMN,ZROFZMX &
         &       ,ZDENSE,ZTEMPE,ZTEMPI,ZDENSEP,ZTEMPEP,ZTEMPIP,ZRZION,ZRPEOP &
         &       ,ZL31,ZL32,ZALFA,ZFT,PS,K,1)
    CALL SURFADD(K,ZRMIN,ZRMAX,ZZMIN,ZZMAX,ZB2MIN,ZROFZMN,ZROFZMX &
         &       ,ZDENSE,ZTEMPE,ZTEMPI,ZDENSEP,ZTEMPEP,ZTEMPIP,ZRZION,ZRPEOP &
         &       ,ZL31,ZL32,ZALFA,ZFT,PS,K,2)
    CALL SURFADD(K,ZRMIN,ZRMAX,ZZMIN,ZZMAX,ZB2MIN,ZROFZMN,ZROFZMX &
         &       ,ZDENSE,ZTEMPE,ZTEMPI,ZDENSEP,ZTEMPEP,ZTEMPIP,ZRZION,ZRPEOP &
         &       ,ZL31,ZL32,ZALFA,ZFT,PS,K,3)
  end if
  !
  !.......................................................................
  !
  !     STOP IF P IS NEGATIF SOMEWHERE
  !
  IF (ZCPRK .LT. 0) THEN
     WRITE(6,'(//,58("*"),/,5X, &
          &       "ERROR: THE PRESSURE PROFILE IS NEGATIVE => STOP",/,10X, &
          &       "CPR(K=",I4,")= ",1PE13.4,/,58("*"))') K,ZCPRK
     STOP 'SURFACE'
  ENDIF
  !
  RETURN
END SUBROUTINE SURFACE
