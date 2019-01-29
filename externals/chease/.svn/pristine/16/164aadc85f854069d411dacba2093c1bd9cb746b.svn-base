!*DECK C2SM20
!*CALL PROCESS
SUBROUTINE OUTXT(KP,PS,KCASE)
  !        #######################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM20 EVALUATE EQ'S FOR XTOR                                       *
  !                                                                     *
  !**********************************************************************
  USE globals
  IMPLICIT NONE
  !
  !         INCLUDE 'DECLAR.inc'
  !         INCLUDE 'COMDIM.inc'
  !         INCLUDE 'COMBLA.inc'
  !         INCLUDE 'COMCON.inc'
  !         INCLUDE 'COMERA.inc'
  !         INCLUDE 'COMESH.inc'
  !         INCLUDE 'COMIOD.inc'
  !         INCLUDE 'COMMAP.inc'
  !         INCLUDE 'COMNUM.inc'
  !         INCLUDE 'COMPHY.inc'
  !         INCLUDE 'COMPLO.inc'
  !         INCLUDE 'COMSOL.inc'
  !         INCLUDE 'COMSUR.inc'
  !
  INTEGER :: KP
  INTEGER :: KP1
  INTEGER :: KCASE
  REAL(RKIND) :: PS
  INTEGER,     DIMENSION(:),   ALLOCATABLE :: IC, IS0, IT0
  REAL(RKIND), DIMENSION(:),   ALLOCATABLE :: ZJAC, ZGSS, ZGST, ZGTT, ZGPP, ZR, ZS, ZS1, ZS2, ZT, ZT1, ZT2, ZZ
  REAL(RKIND), DIMENSION(:,:), ALLOCATABLE :: ZBND, ZDBDS, ZDBDT, ZPCEL, ZTETA
  !
  INTEGER :: J1, JT, JG, JS, J4, J5, L, NTT
  REAL(RKIND) :: ZEPS, ZDPSIS, ZDRSDT, ZDPDS, ZDPDT, ZCOST, ZSINT, &
       & ZRHO, ZBND2, ZDZ, ZDR, ZDSUM, ZDTTDR, ZDTTDZ, ZDSDR, ZDTDR, &
       & ZDSDZ, ZDTDZ, ZDPDR, ZDPDZ
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ZEPS   = 1.E-3_RKIND
  ZDPSIS = 2._RKIND * PS * CPSRF
  kp1=kp+1
  !
  IF (KCASE==1) THEN
     NTT=NTNOVA
  ELSE IF (KCASE==2) THEN
     NTT=MDT*NTNOVA
  ELSE
     PRINT*, 'OPTION KCASE=',KCASE,' NOT AVAILABLE'
     STOP
  ENDIF
  !
  ALLOCATE(IC(NTT),IS0(NTT),IT0(NTT),ZBND(NTT,5),ZDBDS(NTT,16),ZDBDT(NTT,16),ZJAC(NTT), &
       &   ZGSS(NTT),ZGST(NTT),ZGTT(NTT),ZGPP(NTT),ZPCEL(NTT,16),ZR(NTT),ZS(NTT),ZS1(NTT), &
       &   ZS2(NTT),ZTETA(NTT,5),ZT(NTT),ZT1(NTT),ZT2(NTT),ZZ(NTT))
  !
  DO J1=1,NTT
     ZTETA(J1,1) = TETPEN(J1)
     ZTETA(J1,2) = TETPEN(J1) - 2._RKIND * ZEPS
     ZTETA(J1,3) = TETPEN(J1) -      ZEPS
     ZTETA(J1,4) = TETPEN(J1) +      ZEPS
     ZTETA(J1,5) = TETPEN(J1) + 2._RKIND * ZEPS
  END DO
  !
  CALL BOUND(NTT,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(NTT,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(NTT,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(NTT,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(NTT,ZTETA(1,5),ZBND(1,5))
  !
  CALL RESETI(IC,NTT,1)
  DO JT = 1,NT1
     DO JG=1,NTT
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (TETPEN(JG).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC,NTT,1)
  DO JS = 1,NS1
     DO JG=1,NTT
        IF (IC(JG).EQ.1) THEN
           IS0(JG) = JS-1
           IF (SIGPEN(JG).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,NTT
     IF (IS0(J4) .GT. NS) IS0(J4) = NS
     IF (IS0(J4) .LT. 1)  IS0(J4) = 1
     IF (IT0(J4) .GT. NT) IT0(J4) = NT
     IF (IT0(J4) .LT. 1)  IT0(J4) = 1
     !
     ZT(J4)  = TETPEN(J4)
     ZS(J4)  = SIGPEN(J4)
     ZS1(J4) = CSIG(IS0(J4))
     ZS2(J4) = CSIG(IS0(J4)+1)
     ZT1(J4) = CT(IT0(J4))
     ZT2(J4) = CT(IT0(J4)+1)
  END DO
  !
  CALL PSICEL(IS0,IT0,NTT,NTT,ZPCEL,CPSICL)
  CALL BASIS2(NTT,NTT,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT)
  !
  DO J5=1,NTT
     !
     ZDRSDT = (ZBND(J5,2) + 8._RKIND*(ZBND(J5,4) - ZBND(J5,3)) - &
          & ZBND(J5,5)) / (12._RKIND * ZEPS)
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
     ZCOST  = COS(ZTETA(J5,1))
     ZSINT  = SIN(ZTETA(J5,1))
     !
     ZRHO   = SIGPEN(J5) * ZBND(J5,1)
     ZR(J5) = ZRHO * ZCOST + R0
     ZZ(J5) = ZRHO * ZSINT + RZ0
     ZBND2  = ZBND(J5,1)**2
     !
     !  COMPUTE FIRST DERIVATIVES OF THETA-TILD WITH
     !  RESPECT TO R AND Z
     !
     ZDZ    = ZZ(J5) - RZMAG
     ZDR    = ZR(J5) - RMAG
     ZDSUM  = ZDR**2 + ZDZ**2
     !
     ZDTTDR  = - ZDZ / ZDSUM
     ZDTTDZ  =   ZDR / ZDSUM
     !
     !  COMPUTE ALL OTHER DERIVATIVES OF GEOMETRIC QUANTITIES
     !  RELATED TO THE EQUILIBRIUM MESH AND INVOLVED IN THE
     !  COMPUTATION OF THE FIRST DERIVATIVES OF PSI
     !  WITH RESPECT TO R AND Z.
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBND(J5,1) * ZCOST) / ZBND2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBND(J5,1) * ZSINT - ZDRSDT * ZCOST) / ZBND2
     ZDTDZ = ZCOST / ZRHO
     !
     ZDPDR  = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ  = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     ZJAC(J5)  = (ZDPDR * ZDTTDZ - ZDPDZ * ZDTTDR) / (ZR(J5)*ZDPSIS)
     ZGSS(J5) = (ZDPDR**2 + ZDPDZ**2) / ZDPSIS**2
     ZGTT(J5) = ZDTTDR**2 + ZDTTDZ**2
     ZGPP(J5) = 1._RKIND/ ZR(J5)**2
     ZGST(J5) = (ZDPDR * ZDTTDR + ZDPDZ * ZDTTDZ) / ZDPSIS

  END DO
  !
  WRITE(NXTOR) PS,PSIISO(KP),CPR(KP),TMF(KP)
  WRITE(NXTOR) TTP(KP),CPPR(KP),ZDPSIS
  !   suppressed RJBSH, RJBSR should suppress in xtor as well and then removed dummy triple below
  WRITE(NXTOR) RJBSOS(KP,1),RJBSOS(KP,2),RJBSOS(KP,3),RJBSOS(KP,4)
  WRITE(NXTOR) (ZR(L),L=1,NTT)
  WRITE(NXTOR) (ZZ(L),L=1,NTT)
  WRITE(NXTOR) (ZJAC(L),L=1,NTT)
  WRITE(NXTOR) (ZGSS(L),L=1,NTT)
  WRITE(NXTOR) (ZGTT(L),L=1,NTT)
  WRITE(NXTOR) (ZGPP(L),L=1,NTT)
  WRITE(NXTOR) (ZGST(L),L=1,NTT)
  !
  DEALLOCATE(IC,IS0,IT0,ZBND,ZDBDS,ZDBDT,ZJAC, &
       &   ZGSS,ZGST,ZGTT,ZGPP,ZPCEL,ZR,ZS,ZS1, &
       &   ZS2,ZTETA,ZT,ZT1,ZT2,ZZ)
  !
  RETURN
END SUBROUTINE OUTXT
