!*DECK C2SM05
!*CALL PROCESS
SUBROUTINE CINT(K,PSIGMA,PTETA,PGWGT)
  !        #####################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM05 EVALUATE FLUX SURFACE INTEGRALS REQUIRED FOR THE DEFINITION  *
  !        OF I* AND I_PARALLEL (SEE EQ. (9) IN PUBLICATION)            *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
!!$         PARAMETER (NPGMAP = NPMGS * NTP1)
  REAL(RKIND)      ::     ZINT3
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDRSDT
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZBND1
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J7
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     PGWGT
  REAL(RKIND)      ::     ZINT2
  REAL(RKIND)      ::     ZINT1
  REAL(RKIND)      ::     ZINT0
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZCOST
  REAL(RKIND)      ::     ZDPDS
  INTEGER          ::     J6
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J5
  REAL(RKIND)      ::     PSIGMA
  INTEGER          ::     IS0
  INTEGER          ::     JS
  INTEGER          ::     IT0
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     PTETA
  INTEGER          ::     IGMAP
  INTEGER          ::     K
  INTEGER          ::     NPGMAP
  REAL(RKIND)      ::     ZEPS
  PARAMETER (ZEPS = 1.E-3_RKIND)
  !
!!$         DIMENSION &
!!$     &   IS0(NPGMAP),        IT0(NPGMAP),       IC(NPGMAP), &
!!$     &   PSIGMA(*),          PTETA(*),          PGWGT(*), &
!!$     &   ZBND(NPGMAP),       ZBND1(NPGMAP,4),   ZDBDS(NPGMAP,16), &
!!$     &   ZDBDT(NPGMAP,16),   ZPCEL(NPGMAP,16), &
!!$     &   ZS1(NPGMAP),        ZS2(NPGMAP), &
!!$     &   ZTETA(NPGMAP,4),    ZT1(NPGMAP),       ZT2(NPGMAP)
  !
  DIMENSION &
       &   IS0(npmgs*ntp1),IT0(npmgs*ntp1),IC(npmgs*ntp1), &
       &   PSIGMA(*),PTETA(*),PGWGT(*), &
       &   ZBND(npmgs*ntp1),ZBND1(npmgs*ntp1,4),ZDBDS(npmgs*ntp1,16), &
       &   ZDBDT(npmgs*ntp1,16),ZPCEL(npmgs*ntp1,16), &
       &   ZS1(npmgs*ntp1), ZS2(npmgs*ntp1), &
       &   ZTETA(npmgs*ntp1,4),ZT1(npmgs*ntp1),ZT2(npmgs*ntp1)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  NPGMAP = NPMGS * NTP1
  !
  CID0(K) = 0._RKIND
  CIDR(K) = 0._RKIND
  CIDQ(K) = 0._RKIND
  CID2(K) = 0._RKIND
  !
  IGMAP = NMGAUS * NT1
  !
  CALL BOUND(NMGAUS*NT1,PTETA,ZBND)
  !
  CALL RESETI(IC,IGMAP,1)
  DO JT = 1,NT1
     DO JG=1,IGMAP
        IF (IC(JG).NE.0) THEN
           IT0(JG) = JT-1
           IF (PTETA(JG).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     END DO
  END DO
  CALL RESETI(IC,IGMAP,1)
  DO JS = 1,NS1
     DO JG=1,IGMAP
        IF (IC(JG).NE.0) THEN
           IS0(JG) = JS-1
           IF (PSIGMA(JG).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     END DO
  END DO
  !     
  DO J5=1,IGMAP
     !
     IF (IS0(J5).LT.  1) IS0(J5) = 1
     IF (IS0(J5).GE.NS1) IS0(J5) = NS
     IF (IT0(J5).LT.  1) IT0(J5) = 1
     IF (IT0(J5).GE.NT1) IT0(J5) = NT
     !
     ZS1(J5) = CSIG(IS0(J5))
     ZS2(J5) = CSIG(IS0(J5)+1)
     ZT1(J5) = CT(IT0(J5))
     ZT2(J5) = CT(IT0(J5)+1)
  END DO
  !
  CALL PSICEL(IS0,IT0,IGMAP,NPGMAP,ZPCEL,CPSICL)
  !
  IF (NSTTP .LE. 2) THEN
     !
     CALL BASIS4(IGMAP,NPGMAP,ZS1,ZS2,ZT1,ZT2,PSIGMA,PTETA,ZDBDS)
     !
     DO J6=1,IGMAP
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
        ZCOST = COS(PTETA(J6))
        ZRHO  = PSIGMA(J6) * ZBND(J6)
        ZR    = ZRHO * ZCOST + R0
        ZINT0 = ZRHO * ZBND(J6) / ZDPDS
        ZINT1 = ZR * ZRHO * ZBND(J6) / ZDPDS
        ZINT2 = ZRHO * ZBND(J6) / (ZR * ZDPDS)
        !
        CID0(K) = CID0(K) + ZINT0 * PGWGT(J6)
        CIDR(K) = CIDR(K) + ZINT1 * PGWGT(J6)
        CIDQ(K) = CIDQ(K) + ZINT2 * PGWGT(J6)
        !
     END DO
     !
  ELSE IF (NSTTP .GE. 3) THEN
     !
     CALL BASIS2(IGMAP,NPGMAP,ZS1,ZS2,ZT1,ZT2,PSIGMA,PTETA, &
          &               ZDBDS,ZDBDT)
     !
     DO J7=1,IGMAP
        !
        ZTETA(J7,1) = PTETA(J7) - 2._RKIND * ZEPS
        ZTETA(J7,2) = PTETA(J7) -      ZEPS
        ZTETA(J7,3) = PTETA(J7) +      ZEPS
        ZTETA(J7,4) = PTETA(J7) + 2._RKIND * ZEPS
        !
     END DO
     !
     CALL BOUND(IGMAP,ZTETA(1,1),ZBND1(1,1))
     CALL BOUND(IGMAP,ZTETA(1,2),ZBND1(1,2))
     CALL BOUND(IGMAP,ZTETA(1,3),ZBND1(1,3))
     CALL BOUND(IGMAP,ZTETA(1,4),ZBND1(1,4))
     !
     DO J8=1,IGMAP
        !
        ZDRSDT = (ZBND1(J8,1) + 8*(ZBND1(J8,3) - ZBND1(J8,2)) - &
             &             ZBND1(J8,4)) / (12._RKIND * ZEPS)
        !
        ZDPDS = ZDBDS(J8, 1) * ZPCEL(J8, 1) + &
             &           ZDBDS(J8, 2) * ZPCEL(J8, 2) + &
             &           ZDBDS(J8, 3) * ZPCEL(J8, 3) + &
             &           ZDBDS(J8, 4) * ZPCEL(J8, 4) + &
             &           ZDBDS(J8, 5) * ZPCEL(J8, 5) + &
             &           ZDBDS(J8, 6) * ZPCEL(J8, 6) + &
             &           ZDBDS(J8, 7) * ZPCEL(J8, 7) + &
             &           ZDBDS(J8, 8) * ZPCEL(J8, 8) + &
             &           ZDBDS(J8, 9) * ZPCEL(J8, 9) + &
             &           ZDBDS(J8,10) * ZPCEL(J8,10) + &
             &           ZDBDS(J8,11) * ZPCEL(J8,11) + &
             &           ZDBDS(J8,12) * ZPCEL(J8,12) + &
             &           ZDBDS(J8,13) * ZPCEL(J8,13) + &
             &           ZDBDS(J8,14) * ZPCEL(J8,14) + &
             &           ZDBDS(J8,15) * ZPCEL(J8,15) + &
             &           ZDBDS(J8,16) * ZPCEL(J8,16)
        !
        ZDPDT = ZDBDT(J8, 1) * ZPCEL(J8, 1) + &
             &           ZDBDT(J8, 2) * ZPCEL(J8, 2) + &
             &           ZDBDT(J8, 3) * ZPCEL(J8, 3) + &
             &           ZDBDT(J8, 4) * ZPCEL(J8, 4) + &
             &           ZDBDT(J8, 5) * ZPCEL(J8, 5) + &
             &           ZDBDT(J8, 6) * ZPCEL(J8, 6) + &
             &           ZDBDT(J8, 7) * ZPCEL(J8, 7) + &
             &           ZDBDT(J8, 8) * ZPCEL(J8, 8) + &
             &           ZDBDT(J8, 9) * ZPCEL(J8, 9) + &
             &           ZDBDT(J8,10) * ZPCEL(J8,10) + &
             &           ZDBDT(J8,11) * ZPCEL(J8,11) + &
             &           ZDBDT(J8,12) * ZPCEL(J8,12) + &
             &           ZDBDT(J8,13) * ZPCEL(J8,13) + &
             &           ZDBDT(J8,14) * ZPCEL(J8,14) + &
             &           ZDBDT(J8,15) * ZPCEL(J8,15) + &
             &           ZDBDT(J8,16) * ZPCEL(J8,16)
        !
        ZCOST = COS(PTETA(J8))
        ZRHO  = PSIGMA(J8) * ZBND(J8)
        ZR    = ZRHO * ZCOST + R0
        ZINT1 = ZR * ZRHO * ZBND(J8) / ZDPDS
        ZINT2 = ZRHO * ZBND(J8) / (ZR * ZDPDS)
        ZINT3 = (PSIGMA(J8) * ZDPDS + (ZDPDT / ZDPDS - PSIGMA(J8) * &
             &            ZDRSDT / ZBND(J8)) * (ZDPDT / PSIGMA(J8) - &
             &            ZDPDS * ZDRSDT / ZBND(J8))) / ZR
        !
        CIDR(K) = CIDR(K) + ZINT1 * PGWGT(J8)
        CIDQ(K) = CIDQ(K) + ZINT2 * PGWGT(J8)
        CID2(K) = CID2(K) + ZINT3 * PGWGT(J8)
        !
     END DO
     !
  ENDIF
  !
  IF (NSTTP .LE. 2) THEN
     !
     CID0(K) = CID0(K) / CIDQ(K)
     CID2(K) = CIDR(K) / CIDQ(K)
     !
  ELSE IF (NSTTP .GE. 3) THEN
     !
     CID0(K) = CIDR(K) / CIDQ(K)
     CID2(K) = CID2(K) / CIDQ(K)
     !
  ENDIF
  !
  RETURN
END SUBROUTINE CINT
