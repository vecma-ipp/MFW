!*DECK C2SM21
!*CALL PROCESS
SUBROUTINE OUTPEN(KPSI,KSPEN)
  !        #############################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM21 EVALUATE EQ'S FOR PENN (SEE EQ. (39) OF PUBLICATION)         *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
!!$         PARAMETER (NPPOINT = NPCHI * (NPMGS + 1))
  !
  !
  INTEGER       	::  	KPSI 	! <outpen.f90>
  INTEGER       	::  	L 	! <outpen.f90>
  INTEGER       	::  	KSPEN 	! <outpen.f90>
  REAL(RKIND)   	::  	ZSURF 	! <outpen.f90>
  INTEGER       	::  	J6 	! <outpen.f90>
  INTEGER       	::  	IMGAUS1 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2PZ2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2PR2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2PRZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDPDZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDPDR 	! <outpen.f90>
  REAL(RKIND)   	::  	Z7 	! <outpen.f90>
  REAL(RKIND)   	::  	Z6 	! <outpen.f90>
  REAL(RKIND)   	::  	Z5 	! <outpen.f90>
  REAL(RKIND)   	::  	Z4 	! <outpen.f90>
  REAL(RKIND)   	::  	Z3 	! <outpen.f90>
  REAL(RKIND)   	::  	Z2 	! <outpen.f90>
  REAL(RKIND)   	::  	Z1 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDTDZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDSDZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDTDR 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDSDR 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2TTZ2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2TTR2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2TTRZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDTTDZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDTTDR 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDSUM2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDSUM 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDR2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDR 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDZ2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZFB 	! <outpen.f90>
  REAL(RKIND)   	::  	ZBND3 	! <outpen.f90>
  REAL(RKIND)   	::  	ZBND2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZZ 	! <outpen.f90>
  REAL(RKIND)   	::  	ZR 	! <outpen.f90>
  REAL(RKIND)   	::  	ZRHO2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZRHO 	! <outpen.f90>
  REAL(RKIND)   	::  	ZSIN2T 	! <outpen.f90>
  REAL(RKIND)   	::  	ZCOS2T 	! <outpen.f90>
  REAL(RKIND)   	::  	ZSINT 	! <outpen.f90>
  REAL(RKIND)   	::  	ZCOST 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2PT2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2PS2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2PST 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDPDT 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDPDS 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2RST 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDRSDT 	! <outpen.f90>
  INTEGER       	::  	J5 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2BT2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZD2BS2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDBDST 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDBDT 	! <outpen.f90>
  REAL(RKIND)   	::  	ZDBDS 	! <outpen.f90>
  REAL(RKIND)   	::  	ZPCEL 	! <outpen.f90>
  REAL(RKIND)   	::  	ZT2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZT1 	! <outpen.f90>
  REAL(RKIND)   	::  	ZS2 	! <outpen.f90>
  REAL(RKIND)   	::  	ZS1 	! <outpen.f90>
  REAL(RKIND)   	::  	ZS 	! <outpen.f90>
  REAL(RKIND)   	::  	ZT 	! <outpen.f90>
  INTEGER       	::  	J4 	! <outpen.f90>
  INTEGER       	::  	IS0 	! <outpen.f90>
  INTEGER       	::  	JS 	! <outpen.f90>
  INTEGER       	::  	IT0 	! <outpen.f90>
  INTEGER       	::  	JG 	! <outpen.f90>
  INTEGER       	::  	JT 	! <outpen.f90>
  INTEGER       	::  	IC 	! <outpen.f90>
  REAL(RKIND)   	::  	ZBND 	! <outpen.f90>
  REAL(RKIND)   	::  	ZTETA 	! <outpen.f90>
  INTEGER       	::  	J1 	! <outpen.f90>
  INTEGER       	::  	NPOINT 	! <outpen.f90>
  REAL(RKIND)   	::  	ZEPS 	! <outpen.f90>
  INTEGER       	::  	NPPOINT 	! <outpen.f90>
  DIMENSION &
       &   IC(npchi*(npmgs+1)),        IS0(npchi*(npmgs+1)),&
       &   IT0(npchi*(npmgs+1)), &
       &   ZBND(npchi*(npmgs+1),5),    ZDBDS(npchi*(npmgs+1),16), &
       &   ZDBDT(npchi*(npmgs+1),16),  ZDBDST(npchi*(npmgs+1),16), &
       &   ZDPDR(npchi*(npmgs+1)),     ZDPDZ(npchi*(npmgs+1)), &
       &   ZD2PRZ(npchi*(npmgs+1)),    ZD2PR2(npchi*(npmgs+1)), &
       &   ZD2PZ2(npchi*(npmgs+1)),    ZDTTDR(npchi*(npmgs+1)), &
       &   ZDTTDZ(npchi*(npmgs+1)),    ZD2TTRZ(npchi*(npmgs+1)), &
       &   ZD2TTR2(npchi*(npmgs+1)),   ZD2TTZ2(npchi*(npmgs+1)), &
       &   ZD2BS2(npchi*(npmgs+1),16), ZD2BT2(npchi*(npmgs+1),16), &
       &   ZPCEL(npchi*(npmgs+1),16),  ZR(npchi*(npmgs+1)), &
       &   ZS(npchi*(npmgs+1)),        ZS1(npchi*(npmgs+1)), &
       &   ZS2(npchi*(npmgs+1)),       ZTETA(npchi*(npmgs+1),5), &
       &   ZT(npchi*(npmgs+1)),        ZT1(npchi*(npmgs+1)), &
       &   ZT2(npchi*(npmgs+1)),       ZZ(npchi*(npmgs+1)), &
       &   ZSURF(npchi*(npmgs+1))
  !
  NPPOINT = NPCHI * (NPMGS + 1)    
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ZEPS   = 1.E-3_RKIND
  NPOINT = NCHI * (NMGAUS + 1)
  !
  DO J1=1,NPOINT
     ZTETA(J1,1) = TETPEN(J1)
     ZTETA(J1,2) = TETPEN(J1) - 2._RKIND * ZEPS
     ZTETA(J1,3) = TETPEN(J1) -      ZEPS
     ZTETA(J1,4) = TETPEN(J1) +      ZEPS
     ZTETA(J1,5) = TETPEN(J1) + 2._RKIND * ZEPS
  END DO
  !
  CALL BOUND(NPOINT,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(NPOINT,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(NPOINT,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(NPOINT,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(NPOINT,ZTETA(1,5),ZBND(1,5))
  !
  CALL RESETI(IC,NPOINT,1)
  DO JT = 1,NT1
     DO JG=1,NPOINT
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (TETPEN(JG).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC,NPOINT,1)
  DO JS = 1,NS1
     DO JG=1,NPOINT
        IF (IC(JG).EQ.1) THEN
           IS0(JG) = JS-1
           IF (SIGPEN(JG).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,NPOINT
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
  CALL PSICEL(IS0,IT0,NPOINT,NPPOINT,ZPCEL,CPSICL)
  CALL BASIS3(NPOINT,NPPOINT,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT, &
       &               ZDBDST,ZD2BS2,ZD2BT2)
  !
  DO J5=1,NPOINT
     !
     ZDRSDT = (ZBND(J5,2) + 8._RKIND*(ZBND(J5,4) - ZBND(J5,3)) - &
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
     ZCOST  = COS(ZTETA(J5,1))
     ZSINT  = SIN(ZTETA(J5,1))
     ZCOS2T = (ZCOST + ZSINT) * (ZCOST - ZSINT)
     ZSIN2T = 2._RKIND * ZSINT * ZCOST
     !
     ZRHO   = SIGPEN(J5) * ZBND(J5,1)
     ZRHO2  = ZRHO**2
     ZR(J5) = ZRHO * ZCOST + R0
     ZZ(J5) = ZRHO * ZSINT + RZ0
     ZBND2  = ZBND(J5,1)**2
     ZBND3  = ZBND2 * ZBND(J5,1)
     ZFB    = ZBND2 + 2._RKIND * ZDRSDT**2 - ZBND(J5,1) * ZD2RST
     !
     !  COMPUTE FIRST AND SECOND DERIVATIVES OF THETA-TILD WITH
     !  RESPECT TO R AND Z
     !
     ZDZ    = ZZ(J5) - RZMAG
     ZDZ2   = ZDZ**2
     ZDR    = ZR(J5) - RMAG
     ZDR2   = ZDR**2
     ZDSUM  = ZDR2 + ZDZ2
     ZDSUM2 = ZDSUM**2
     !
     ZDTTDR(J5)  = - ZDZ / ZDSUM
     ZDTTDZ(J5)  =   ZDR / ZDSUM
     ZD2TTRZ(J5) =   (ZDZ2 - ZDR2) / ZDSUM2
     ZD2TTR2(J5) =   2._RKIND * ZDR * ZDZ / ZDSUM2
     ZD2TTZ2(J5) = - 2._RKIND * ZDR * ZDZ / ZDSUM2
     !
     !  COMPUTE ALL OTHER DERIVATIVES OF GEOMETRIC QUANTITIES
     !  RELATED TO THE EQUILIBRIUM MESH AND INVOLVED IN THE
     !  COMPUTATION OF ALL FIRST AND SECOND DERIVATIVES OF PSI
     !  WITH RESPECT TO R AND Z.
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBND(J5,1) * ZCOST) / ZBND2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBND(J5,1) * ZSINT - ZDRSDT * ZCOST) / ZBND2
     ZDTDZ = ZCOST / ZRHO
     !
     Z1 = (ZBND(J5,1) * ZCOS2T + ZDRSDT * ZSIN2T) / (ZRHO * ZBND2)
     Z2 = - ZCOS2T / ZRHO2
     Z3 =   ZSIN2T / ZRHO2
     Z4 = - ZSIN2T / ZRHO2
     Z5 = - ZSINT * ZCOST * ZFB / (ZRHO * ZBND3)
     Z6 =   ZSINT**2 * ZFB / (ZRHO * ZBND3)
     Z7 =   ZCOST**2 * ZFB / (ZRHO * ZBND3)
     !
     ZDPDR(J5)  = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ(J5)  = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     ZD2PRZ(J5) = ZD2PS2 * ZDSDR * ZDSDZ + ZD2PT2 * ZDTDR * ZDTDZ + &
          &                ZD2PST * Z1 + ZDPDT * Z2 + ZDPDS * Z5
     ZD2PR2(J5) = ZD2PS2 * ZDSDR**2 + ZD2PT2 * ZDTDR**2 + &
          &                2._RKIND * ZD2PST * ZDSDR * ZDTDR + ZDPDT * Z3 + &
          &                ZDPDS * Z6
     ZD2PZ2(J5) = ZD2PS2 * ZDSDZ**2 + ZD2PT2 * ZDTDZ**2 + &
          &                2._RKIND * ZD2PST * ZDSDZ * ZDTDZ + ZDPDT * Z4 + &
          &                ZDPDS * Z7
     !
  END DO
  !
  IMGAUS1 = NMGAUS + 1
  !
  DO J6=0,NMGAUS
     !
     ZSURF(1) = KSPEN
     ZSURF(2) = J6
     !
     WRITE(NPENN) (ZSURF(L),L=1,NCHI)
     WRITE(NPENN) (ZR(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZZ(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZDTTDR(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZDTTDZ(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZD2TTRZ(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZD2TTR2(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZD2TTZ2(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZDPDR(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZDPDZ(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZD2PRZ(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZD2PR2(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     WRITE(NPENN) (ZD2PZ2(((L-1)*IMGAUS1)+J6+1),L=1,NCHI)
     !
  END DO
  !
  RETURN
END SUBROUTINE OUTPEN
