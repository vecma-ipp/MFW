!*DECK C2SX01
!*CALL PROCESS
SUBROUTINE BOUND(KN,PT,PR)
  !        ##########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !     INTRODUCE JITERMX MAX. NUMBER OF NEWTON ITERATIONS TO PREVENT
  !     BEING TRAPPED IN INFINITE LOOPS. APLET 5/1/96
  !
  !**********************************************************************
  !                                                                     *
  !  C2SX01  DEFINE PLASMA SURFACE                                      *
  !             NSURF = 1   SOLOVEV PLASMA SURFACE                      *
  !                     2   INTOR-LIKE SURFACE                          *
  !                     3   TCV-LIKE SURFACE                            *
  !                     4   XPOINT SURFACE                              *
  !                     5   HOCTUPOLE SURFACE                           *
  !                     6   EXPERIMENTAL POINTS                         *
  !                     7   SINE AND COSINE DECOMPOSITION               *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
!!$         PARAMETER (NPKNMAX = 12*NPT+2*NPPSCUB*(NPMGS+1)*NTP1)
!!$!
!!$         DIMENSION &
!!$     &   PT(KN),                 PR(KN), &
!!$     &   IC(NPKNMAX),            ZF(NPKNMAX), &
!!$     &   ZPT(NPKNMAX),           ZPT0(NPKNMAX), &
!!$     &   ZCOSPT(NPKNMAX),        ZSINPT(NPKNMAX), &
!!$     &   ZRHOJ(NPKNMAX),         ZRHOJP(NPKNMAX)
!!$!
  !----------------------------------------------------------------------
  REAL(RKIND)   	::  	ZBK 	! <bound.f90>
  REAL(RKIND)   	::  	ZAK 	! <bound.f90>
  REAL(RKIND)   	::  	ZK 	! <bound.f90>
  REAL(RKIND)   	::  	ZFJ 	! <bound.f90>
  INTEGER       	::  	K 	! <bound.f90>
  REAL(RKIND)   	::  	ZD 	! <bound.f90>
  REAL(RKIND)   	::  	ZC 	! <bound.f90>
  REAL(RKIND)   	::  	ZB 	! <bound.f90>
  REAL(RKIND)   	::  	ZA 	! <bound.f90>
  REAL(RKIND)   	::  	ZH 	! <bound.f90>
  INTEGER       	::  	JJ 	! <bound.f90>
  REAL(RKIND)   	::  	ZZZ 	! <bound.f90>
  REAL(RKIND)   	::  	ZRO 	! <bound.f90>
  INTEGER       	::  	J324 	! <bound.f90>
  INTEGER       	::  	J322 	! <bound.f90>
  INTEGER       	::  	J321 	! <bound.f90>
  REAL(RKIND)   	::  	Z1 	! <bound.f90>
  INTEGER       	::  	J312 	! <bound.f90>
  INTEGER       	::  	J311 	! <bound.f90>
  REAL(RKIND)   	::  	ZARG 	! <bound.f90>
  REAL(RKIND)   	::  	ZZ 	! <bound.f90>
  REAL(RKIND)   	::  	ZR 	! <bound.f90>
  INTEGER       	::  	ISSUM 	! <bound.f90>
  INTEGER       	::  	ITEST 	! <bound.f90>
  REAL(RKIND)   	::  	ZEP 	! <bound.f90>
  REAL(RKIND)   	::  	ZFP 	! <bound.f90>
  INTEGER       	::  	JITER 	! <bound.f90>
  INTEGER       	::  	J 	! <bound.f90>
  INTEGER       	::  	JITERMX 	! <bound.f90>
  REAL(RKIND)   	::  	Z50REAL 	! <bound.f90>
  REAL(RKIND)   	::  	ZEPS 	! <bound.f90>
  REAL(RKIND)   	::  	ZPTETP 	! <bound.f90>
  REAL(RKIND)   	::  	RHOPFP 	! <bound.f90>
  REAL(RKIND)   	::  	RPTETP 	! <bound.f90>
  REAL(RKIND)   	::  	ZPTET 	! <bound.f90>
  REAL(RKIND)   	::  	RHOPF 	! <bound.f90>
  REAL(RKIND)   	::  	RPTET 	! <bound.f90>
  REAL(RKIND)   	::  	ZFIZP 	! <bound.f90>
  REAL(RKIND)   	::  	ZFIRP 	! <bound.f90>
  REAL(RKIND)   	::  	ZFIZ 	! <bound.f90>
  REAL(RKIND)   	::  	ZFIR 	! <bound.f90>
  REAL(RKIND)   	::  	ZDFDT 	! <bound.f90>
  REAL(RKIND)   	::  	ZFT 	! <bound.f90>
  REAL(RKIND)   	::  	ZRHOP 	! <bound.f90>
  REAL(RKIND)   	::  	ZRHO 	! <bound.f90>
  REAL(RKIND)   	::  	ZEX 	! <bound.f90>
  REAL(RKIND)   	::  	RZVRP 	! <bound.f90>
  REAL(RKIND)   	::  	RZVR 	! <bound.f90>
  REAL(RKIND)   	::  	RRVRP 	! <bound.f90>
  REAL(RKIND)   	::  	RRVR 	! <bound.f90>
  REAL(RKIND)   	::  	TRIP 	! <bound.f90>
  REAL(RKIND)   	::  	BEAN 	! <bound.f90>
  REAL(RKIND)   	::  	DS 	! <bound.f90>
  REAL(RKIND)   	::  	ZTPT 	! <bound.f90>
  REAL(RKIND)   	::  	S 	! <bound.f90>
  REAL(RKIND)   	::  	DT 	! <bound.f90>
  INTEGER       	::  	INTPI2 	! <bound.f90>
  REAL(RKIND)   	::  	T 	! <bound.f90>
  REAL(RKIND)   	::  	Z2 	! <bound.f90>
  REAL(RKIND)   	::  	ZDFV 	! <bound.f90>
  REAL(RKIND)   	::  	ZEXA 	! <bound.f90>
  REAL(RKIND)   	::  	ZEXV 	! <bound.f90>
  REAL(RKIND)   	::  	ZFV 	! <bound.f90>
  INTEGER       	::  	INTS 	! <bound.f90>
  REAL(RKIND)   	::  	ZFI2P 	! <bound.f90>
  REAL(RKIND)   	::  	ZFI2 	! <bound.f90>
  REAL(RKIND)   	::  	ZCOS2 	! <bound.f90>
  REAL(RKIND)   	::  	ZFI1P 	! <bound.f90>
  REAL(RKIND)   	::  	ZSIN2 	! <bound.f90>
  REAL(RKIND)   	::  	ZFI1 	! <bound.f90>
  REAL(RKIND)   	::  	ZF2P 	! <bound.f90>
  REAL(RKIND)   	::  	ZF2 	! <bound.f90>
  REAL(RKIND)   	::  	ZSIN 	! <bound.f90>
  REAL(RKIND)   	::  	ZF1P 	! <bound.f90>
  REAL(RKIND)   	::  	ZCOS 	! <bound.f90>
  REAL(RKIND)   	::  	ZF1 	! <bound.f90>
  INTEGER       	::  	KN 	! <bound.f90>
  REAL(rkind) Z,COSTF,SINTF
  !
  REAL(RKIND) :: PT(KN),  PR(KN)
  INTEGER :: NPKNMAX
  INTEGER, DIMENSION(:), ALLOCATABLE :: IC
  REAL(RKIND), DIMENSION(:), ALLOCATABLE :: ZF, ZPT,ZPT0,ZCOSPT,&
       & ZSINPT,ZRHOJ,ZRHOJP
  !----------------------------------------------------------------------
  !
  !**********************************************************************
  !                                                                     *
  ! 0.1. FUNCTIONS FOR SOLOVEV SURFACE                                  *
  !                                                                     *
  !**********************************************************************
  !
  ZF1(Z)  = BPS(2) * SQRT(1 + 2 * BPS(4) * ZCOS) - BPS(1)
  ZF1P(Z) = - BPS(2) * BPS(4) * ZSIN / &
       &             SQRT(1 + 2 * BPS(4) * ZCOS)
  ZF2(Z)  = BPS(5) * BPS(4) * BPS(2) * ZSIN / &
       &             SQRT(1 + 2 * BPS(4) * ZCOS)
  ZF2P(Z) = BPS(5) * BPS(4) * BPS(2) * (ZCOS + BPS(4) * &
       &             ZSIN**2 / (1 + 2 * BPS(4) * ZCOS)) / &
       &             SQRT(1 + 2 * BPS(4) * ZCOS)
  !
  !**********************************************************************
  !                                                                     *
  ! 0.2. FUNCTIONS FOR INTOR-LIKE SURFACE                               *
  !                                                                     *
  !**********************************************************************
  !
  ZFI1 (Z) = BPS(3) + BPS(6) * (1._RKIND + BPS(8) * ZCOS) * &
       &              COS(Z + BPS(7) * ZSIN - BPS(9) * ZSIN2)
  ZFI1P(Z) = - BPS(6) * (COS(Z+BPS(7)*ZSIN-BPS(9)*ZSIN2) * &
       &                BPS(8) * ZSIN + (1._RKIND + BPS(7) * ZCOS - &
       &                2._RKIND* BPS(9)*ZCOS2) * SIN(Z + BPS(7) * ZSIN - &
       &                BPS(9) * ZSIN2) * (1._RKIND + BPS(8) * ZCOS))
  ZFI2 (Z) = BPS(5) * BPS(6) * SIN(Z + BPS(10) * ZSIN2)
  ZFI2P(Z) = BPS(5) * BPS(6) * COS(Z + BPS(10) * ZSIN2) * &
       &              (1._RKIND + 2._RKIND * BPS(10) * ZCOS2)
  !
  !**********************************************************************
  !                                                                     *
  ! 0.3. FUNCTIONS FOR TCV-LIKE SURFACE                                 *
  !                                                                     *
  !**********************************************************************
  !
  INTS(Z) = CPI * INT(2._RKIND * Z / CPI) 
  ZFV(Z)  = ZEXV + (1._RKIND - ZEXV) * ZEXA
  ZDFV(Z) = BPS(10) * (1._RKIND - ZEXV) * ZEXA * &
       &             SIGN(Z2,ZCOS * ZSIN) / ZSIN**2
  !
  T(Z)     = INTPI2 + (Z - INTPI2) * ZFV(Z)
  DT(Z)    = (Z - INTPI2) * ZDFV(Z) +  ZFV(Z)
  !
  S(Z)     = COS(ZTPT + BPS(7) * SIN(ZTPT))
  DS(Z)    = DT(Z) * (1._RKIND + BPS(7) * COS(ZTPT)) * &
       &              SIN(ZTPT + BPS(7) * SIN(ZTPT))
  BEAN(Z)  = 1._RKIND + BPS(8) * ZCOS
  TRIP(Z)  = 1._RKIND + BPS(11) * COS(2.5_RKIND * CPI * ZSIN)
  !
  RRVR(Z)  = BPS(6) * BEAN(Z) * TRIP(Z) * S(Z) + BPS(3)
  !
  RRVRP(Z) = - BPS(6) * ((BPS(8) * ZSIN * TRIP(Z) + &
       &                           2.5_RKIND * CPI * BPS(11) * ZCOS * BEAN(Z) * &
       &                           SIN(2.5_RKIND * CPI * ZSIN)) * S(Z) + &
       &                          BEAN(Z) * TRIP(Z) * DS(Z))
  !
  RZVR(Z)  = BPS(5) * BPS(6) * ZSIN
  RZVRP(Z) = BPS(5) * BPS(6) * ZCOS
  !
  !**********************************************************************
  !                                                                     *
  ! 0.4. FUNCTIONS FOR X-POINT SURFACE                                  *
  !                                                                     *
  !**********************************************************************
  !
  ZEX(Z)   = EXP( BPS(7) * LOG(SIN(.5_RKIND * (Z - BPS(9)))**2 + &
       &                                BPS(8)))
  ZRHO(Z)  = BPS(6) * (1._RKIND + BPS(10) * BPS(11) / &
       &                                 (ZEX(Z) + BPS(11)))
  ZRHOP(Z) = .5_RKIND * BPS(6) * BPS(7) * BPS(10) * BPS(11) * &
       &              ZEX(Z) * SIN(Z - BPS(9)) / &
       &              ((SIN(.5_RKIND * (Z - BPS(9)))**2 + BPS(8)) * &
       &              (ZEX(Z) + BPS(11))**2)
  ZFT(Z)   = (ZRHO(Z) * SIN(Z) * BPS(5) - BPS(12)) * ZCOS - &
       &              (ZRHO(Z) * COS(Z + BPS(13) * SIN(Z)) * &
       &              (1._RKIND + BPS(14) * COS(Z)) + BPS(3)) * ZSIN
  ZDFDT(Z) = ZRHOP(Z) * SIN(Z) * BPS(5) * ZCOS + &
       &              ZRHO(Z)  * COS(Z) * BPS(5) * ZCOS - &
       &              ZRHOP(Z) * COS(Z + BPS(13) * SIN(Z)) * &
       &              (1._RKIND + BPS(14) * COS(Z)) * ZSIN + ZRHO(Z) * &
       &              (SIN(Z + BPS(13) * SIN(Z)) * &
       &               (1._RKIND + BPS(13) * COS(Z)) * &
       &               (1._RKIND + BPS(14) * COS(Z)) + &
       &               COS(Z + BPS(13) * SIN(Z)) * &
       &               BPS(14) * SIN(Z)) * ZSIN
  !
  !**********************************************************************
  !                                                                     *
  ! 0.5. FUNCTIONS FOR OCTUPOLE TYPE  SURFACE                           *
  !                                                                     *
  !**********************************************************************
  !
  ZFIR(Z)= BPS(3) + BPS(6) * ZCOS &
       &            /(1._RKIND-BPS(5)*(COS(4._RKIND*(Z+BPS(8)))-1._RKIND))**BPS(7)

  ZFIZ(Z)= BPS(6)*ZSIN &
       &            /(1._RKIND-BPS(5)*(COS(4._RKIND*(Z+BPS(8)))-1._RKIND))**BPS(7)

  ZFIRP(Z)=-ZFIZ(Z) &
       &           -4._RKIND*BPS(5)*BPS(6)*SIN(4._RKIND*(Z+BPS(8)))*ZCOS*BPS(7) &
       &            /(1._RKIND-BPS(5)*(COS(4._RKIND*(Z+BPS(8)))-1._RKIND))**(BPS(7)+1._RKIND)

  ZFIZP(Z)=(ZFIR(Z)-BPS(3)) &
       &              -4._RKIND*BPS(5)*BPS(6)*SIN(4._RKIND*(Z+BPS(8)))*ZSIN*BPS(7) &
       &            /(1._RKIND-BPS(5)*(COS(4._RKIND*(Z+BPS(8)))-1._RKIND))**(BPS(7)+1._RKIND)

  !
  !**********************************************************************
  !                                                                     *
  ! 0.7. FUNCTIONS FOR FOURRIER TRANSFORM                               *
  !                                                                     *
  !**********************************************************************
  RPTET(COSTF) = BPS(3) + RHOPF*COSTF
  ZPTET(SINTF) = RHOPF*SINTF + BPS(6) - BPS(12)
  RPTETP(COSTF,SINTF) = RHOPFP*COSTF - RHOPF*SINTF
  ZPTETP(COSTF,SINTF) = RHOPFP*SINTF + RHOPF*COSTF
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  NPKNMAX = 12*NPT+2*NPPSCUB*(NPMGS+1)*NTP1
  ALLOCATE(IC(NPKNMAX), ZF(NPKNMAX), ZPT(NPKNMAX), ZPT0(NPKNMAX),&
       & ZCOSPT(NPKNMAX), ZSINPT(NPKNMAX), &
       & ZRHOJ(NPKNMAX), ZRHOJP(NPKNMAX) )
  !----------------------------------------------------------------------
  !
  ZEPS = RC1M13
  Z50REAL = 50._RKIND
  !
  ! MAX NUMBER OF NEWTON ITERATIONS
  !
  JITERMX = 20
  !
  IF (KN .GT. NPKNMAX) THEN
     !
     PRINT*,'DIMENSION OF LOCAL ARRAYS IS TO SMALL: KN= ',KN
     STOP
     !
  ENDIF
  !
  GO TO (100,200,300,400,500,600,700) NSURF
  !
100 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 1. SOLOVEV PLASMA SURFACE                                           *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 1.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !
  !
  DO J = 1,KN
     !
     ZPT(J) = PT(J)
     IC(J)  = 1
     ZSIN   = SIN(ZPT(J))
     ZCOS   = COS(ZPT(J))
     ZF(J)  = SIN(PT(J)) * ZF1(ZPT(J)) - COS(PT(J)) * ZF2(ZPT(J))
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 1.2. NEWTON'S RULE                                                  *
  !                                                                     *
  !**********************************************************************
  !
  DO JITER=1,JITERMX
     !
     DO J = 1,KN
        ZSIN = SIN(ZPT(J))
        ZCOS = COS(ZPT(J))
        ZFP  = SIN(PT(J))*ZF1P(ZPT(J))-COS(PT(J))*ZF2P(ZPT(J))
        ZEP  = ZEPS-ABS(ZF(J))
        !
        IF (ZEP .LE. 0._RKIND) ZPT(J) = ZPT(J) - ZF(J) / ZFP
        !
        ZSIN  = SIN(ZPT(J))
        ZCOS  = COS(ZPT(J))
        ZF(J) = SIN(PT(J))*ZF1(ZPT(J))-COS(PT(J))*ZF2(ZPT(J))
        !
        IF (ZEP .GT. 0._RKIND) IC(J) = 0
        !
     END DO
     !
     ITEST = ISSUM(KN,IC,1)
     !
     IF (ITEST .LE. 0) GO TO 122
     !
  END DO
  !
  IF (NVERBOSE .GE. 0) WRITE(0,9001)
  !
122 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 1.3. DEFINE R(ZPT) AND Z(ZPT)                                       *
  !                                                                     *
  !**********************************************************************
  !     DEFINE R(ZPT) AND Z(ZPT)
  !
  DO J = 1,KN
     ZSIN = SIN(ZPT(J))
     ZCOS = COS(ZPT(J))
     ZR   = ZF1(ZPT(J))
     ZZ   = ZF2(ZPT(J))
     !
     PR(J) = SQRT(ZR**2 + ZZ**2)
  END DO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
200 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 2. INTOR-LIKE PLASMA SURFACE                                        *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 2.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !
  DO J = 1,KN
     ZPT(J) = PT(J)
     IC(J) = 1
     ZSIN  = SIN(ZPT(J))
     ZCOS  = COS(ZPT(J))
     ZSIN2 = SIN(2._RKIND*ZPT(J))
     ZF(J) = SIN(PT(J))*ZFI1(ZPT(J))-COS(PT(J))*ZFI2(ZPT(J))
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 2.2. NEWTON'S RULE                                                  *
  !                                                                     *
  !**********************************************************************
  !
  DO JITER=1,JITERMX
     !
     DO J = 1,KN
        ZSIN  = SIN(ZPT(J))
        ZCOS  = COS(ZPT(J))
        ZSIN2 = SIN(2._RKIND*ZPT(J))
        ZCOS2 = COS(2._RKIND*ZPT(J))
        ZFP   = SIN(PT(J))*ZFI1P(ZPT(J))-COS(PT(J))*ZFI2P(ZPT(J))
        ZEP   = ZEPS-ABS(ZF(J))
        !
        IF (ZEP .LE. 0._RKIND) ZPT(J) = ZPT(J) - ZF(J) / ZFP
        !
        ZSIN  = SIN(ZPT(J))
        ZCOS  = COS(ZPT(J))
        ZSIN2 = SIN(2._RKIND*ZPT(J))
        ZF(J) = SIN(PT(J))*ZFI1(ZPT(J))-COS(PT(J))*ZFI2(ZPT(J))
        !
        IF (ZEP .GT. 0._RKIND) IC(J) = 0
        !
     END DO
     !
     ITEST = ISSUM(KN,IC,1)
     !
     IF (ITEST.le.0) GO TO 222
     !
  END DO
  IF (NVERBOSE .GE. 0) WRITE(0,9001)
  !
222 CONTINUE 
  !
  DO J = 1,KN
     ZSIN  = SIN(ZPT(J))
     ZCOS  = COS(ZPT(J))
     ZSIN2 = SIN(2._RKIND*ZPT(J))
     ZR    = ZFI1(ZPT(J))
     ZZ    = ZFI2(ZPT(J))
     !
     PR(J) = SQRT(ZR*ZR+ZZ*ZZ)
  END DO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
300 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 3. TCV-LIKE PLASMA SURFACE                                          *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 3.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !
  ZARG = BPS(9) * BPS(10)
  !
  IF (ZARG .LE. Z50REAL) THEN
     !
     ZEXV = EXP(- ZARG)
     !
  ELSE
     !
     ZEXV = 0._RKIND
     !
  ENDIF
  !
  DO J311=1,KN
     !
     IF (PT(J311) .GE. 0._RKIND) THEN
        !
        ZPT(J311) = PT(J311) - 2._RKIND*CPI * INT(.5_RKIND*PT(J311)/CPI)
        !
     ELSE
        !
        ZPT(J311) = PT(J311) + 2._RKIND*CPI * (1._RKIND - INT(.5_RKIND*PT(J311)/CPI))
        !
     ENDIF
     !
  END DO
  !
  DO J312=1,KN
     !
     IF (ZPT(J312) .GT. CPI) ZPT(J312) = 2._RKIND * CPI - ZPT(J312)
     !
     ZPT0(J312) = ZPT(J312)
     IC(J312)   = 1
     ZCOS       = COS(ZPT(J312))
     ZSIN       = SIN(ZPT(J312))
     INTPI2     = INTS(ZPT(J312))
     !
     IF (ZSIN .NE. 0._RKIND) THEN
        !
        Z1       = MIN(Z50REAL,BPS(10) * ABS(ZCOS / ZSIN))
        ZEXA     = EXP(- Z1)
        ZTPT     = T(ZPT(J312))
        ZF(J312) = ZSIN * RRVR(ZPT(J312)) - &
             &                 ZCOS * RZVR(ZPT(J312))
        !
     ELSE
        !
        ZF(J312) = 0._RKIND
        !
     ENDIF
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3.2. NEWTON'S RULE                                                  *
  !                                                                     *
  !**********************************************************************
  !
  DO JITER=1,JITERMX
     !
     DO J321=1,KN
        !
        ZCOS   = COS(ZPT(J321))
        ZSIN   = SIN(ZPT(J321))
        INTPI2 = INTS(ZPT(J321))
        !
        IF (ZSIN .NE. 0._RKIND) THEN
           !
           !
           Z1   = MIN(Z50REAL,BPS(10) * ABS(ZCOS / ZSIN))
           Z2   = 1._RKIND
           ZEXA = EXP(- Z1)
           ZTPT = T(ZPT(J321))
           ZFP  = SIN(ZPT0(J321)) * RRVRP(ZPT(J321)) - &
                &             COS(ZPT0(J321)) * RZVRP(ZPT(J321))
           ZEP  = ZEPS - ABS(ZF(J321))
           !
        ELSE
           !
           !           DUMMY VALUE FOR ZFP TO PERMIT VECTORZATION            
           !
           ZFP  = 1._RKIND
           ZEP  = ZEPS
           !
        ENDIF
        !
        IF (ZEP .LE. 0._RKIND) ZPT(J321) = ZPT(J321) - ZF(J321) / ZFP
        IF (ZEP .GT. 0._RKIND) IC(J321)  = 0
        !
     END DO
     !
     DO J322=1,KN
        !
        IF (ZPT(J322) .GT. CPI) ZPT(J322) = 2._RKIND * CPI - ZPT(J322)
        !
        ZCOS   = COS(ZPT(J322))
        ZSIN   = SIN(ZPT(J322))
        INTPI2 = INTS(ZPT(J322))
        !
        IF (ZSIN .NE. 0._RKIND) THEN
           !
           Z1       = MIN(Z50REAL,BPS(10) * ABS(ZCOS / ZSIN))
           ZEXA     = EXP(- Z1)
           ZTPT     = T(ZPT(J322))
           ZF(J322) = SIN(ZPT0(J322)) * RRVR(ZPT(J322)) - &
                &                 COS(ZPT0(J322)) * RZVR(ZPT(J322))
           !
        ELSE
           !
           ZF(J322) = 0._RKIND
           !
        ENDIF
        !
     END DO
     !
     ITEST = ISSUM(KN,IC,1)
     !
     IF (ITEST.le.0) GO TO 323
     !
  END DO
  !
  IF (NVERBOSE .GE. 0) WRITE(0,9001)
  !
323 CONTINUE 
  !
  DO J324=1,KN
     !
     ZSIN   = SIN(ZPT(J324))
     ZCOS   = COS(ZPT(J324))
     INTPI2 = INTS(ZPT(J324))
     !
     IF (ZSIN .NE. 0._RKIND) THEN
        !
        Z1   = MIN(Z50REAL,BPS(10) * ABS(ZCOS / ZSIN))
        ZEXA = EXP(- Z1)
        ZTPT = T(ZPT(J324))
        ZR   = RRVR(ZPT(J324))
        ZZ   = RZVR(ZPT(J324))
        !
     ELSE
        !
        ZR = BPS(6) * BEAN(ZPT(J324)) * TRIP(ZPT(J324)) * ZCOS + &
             &           BPS(3)
        ZZ = 0._RKIND
        !
     ENDIF
     !
     PR(J324) = SQRT(ZR**2 + ZZ**2)
     !
  END DO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
400 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 4. X-POINT SURFACE                                                  *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 4.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !
  DO J = 1,KN
     ZPT(J) = PT(J)
     ZSIN  = SIN(PT(J))
     ZCOS  = COS(PT(J))
     ZF(J) = ZFT(PT(J))
     IC(J) = 1
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 4.2. NEWTON'S RULE                                                  *
  !                                                                     *
  !**********************************************************************
  !
  DO JITER=1,JITERMX
     !
     DO J = 1,KN
        ZSIN = SIN(PT(J))
        ZCOS = COS(PT(J))
        ZEP  = ZEPS-ABS(ZF(J))
        !
        IF (ZEP .LE. 0._RKIND) ZPT(J) = ZPT(J) - ZF(J) / ZDFDT(ZPT(J))
        !
        ZF(J) = ZFT(ZPT(J))
        !
        IF (ZEP .GT. 0._RKIND) IC(J) = 0
        !
     END DO
     !
     ITEST = ISSUM(KN,IC,1)
     !
     IF (ITEST.le.0) GO TO 422
     !
  END DO
  !
  IF (NVERBOSE .GE. 0) WRITE(0,9001)
  !
422 CONTINUE 
  !
  DO J = 1,KN
     ZRO  = ZRHO(ZPT(J))
     ZZZ  = ZPT(J)
     ZR   = BPS(3)+ZRO*COS(ZZZ+BPS(13)*SIN(ZZZ)) &
          &         *(1.0_RKIND+BPS(14)*COS(ZZZ))
     ZZ = ZRO*SIN(ZPT(J))*BPS(5)-BPS(12)
     PR(J) = SQRT(ZR**2+ZZ**2)
  END DO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
500 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 5. OCTUPOLE TYPE PLASMA SURFACE                                     *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 5.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !
  DO J = 1,KN
     ZPT(J) = PT(J)
     IC(J) = 1
     ZSIN  = SIN(ZPT(J))
     ZCOS  = COS(ZPT(J))
     ZF(J) = SIN(PT(J))*ZFIR(ZPT(J))-COS(PT(J))*ZFIZ(ZPT(J))
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 5.2. NEWTON'S RULE                                                  *
  !                                                                     *
  !**********************************************************************
  !
  DO JITER=1,JITERMX
     !
     DO J = 1,KN
        ZSIN = SIN(ZPT(J))
        ZCOS = COS(ZPT(J))
        ZFP  = SIN(PT(J))*ZFIRP(ZPT(J))-COS(PT(J))*ZFIZP(ZPT(J))
        ZEP  = ZEPS-ABS(ZF(J))
        !
        IF (ZEP .LE. 0._RKIND) ZPT(J) = ZPT(J) - ZF(J) / ZFP
        !
        ZSIN  = SIN(ZPT(J))
        ZCOS  = COS(ZPT(J))
        ZF(J) = SIN(PT(J))*ZFIR(ZPT(J))-COS(PT(J))*ZFIZ(ZPT(J))
        !
        IF (ZEP .GT. 0._RKIND) IC(J) = 0
        !
     END DO
     !
     ITEST = ISSUM(KN,IC,1)
     !
     IF (ITEST.le.0) GO TO 522
     !
  END DO
  !
  IF (NVERBOSE .GE. 0) WRITE(0,9001)
  !
522 CONTINUE 
  !
  DO J = 1,KN
     ZSIN = SIN(ZPT(J))
     ZCOS = COS(ZPT(J))
     ZR   = ZFIR(ZPT(J))
     ZZ   = ZFIZ(ZPT(J))
     !
     PR(J) = SQRT(ZR*ZR+ZZ*ZZ)
  END DO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
600 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 6. PLASMA SURFACE GIVEN BY ARRAY OF R AND Z NODES                   *
  !                                                                     *
  !**********************************************************************
  !
  DO J=1,KN
     ZPT(J) = PT(J)
     IF (ZPT(J) .LT. TETBPS(1))    ZPT(J) = ZPT(J) + 2._RKIND * CPI
     IF (ZPT(J) .GT. TETBPS(NBPS)) ZPT(J) = ZPT(J) - 2._RKIND * CPI
  END DO
  !
  DO J=1,KN
     DO JJ=1,NBPS-1
        IF ((ZPT(J)-TETBPS(JJ)) * (ZPT(J)-TETBPS(JJ+1)) .LE. 0._RKIND) &
             &      IC(J) = JJ
     END DO
  END DO
  !
  DO J=1,KN
     ZH = TETBPS(IC(J)+1) - TETBPS(IC(J))
     ZA = (TETBPS(IC(J)+1) - ZPT(J)) / ZH
     ZB = (ZPT(J) - TETBPS(IC(J))) / ZH
     ZC = (ZA+1)*(ZA-1)*ZH * (TETBPS(IC(J)+1) - ZPT(J)) / 6._RKIND
     ZD = (ZB+1)*(ZB-1)*ZH * (ZPT(J) - TETBPS(IC(J))) / 6._RKIND
     !
     ZR = ZA * RRBPS(IC(J))  + ZB * RRBPS(IC(J)+1) + &
          &           ZC * D2RBPS(IC(J)) + ZD * D2RBPS(IC(J)+1) - BPS(1)
     ZZ = ZA * RZBPS(IC(J))  + ZB * RZBPS(IC(J)+1) + &
          &           ZC * D2ZBPS(IC(J)) + ZD * D2ZBPS(IC(J)+1) - BPS(12)
     !
     PR(J) = SQRT(ZR**2+ZZ**2)
  END DO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
700 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! 7. SURFACE CALCULATED BY REAL FOURRIER COEFFICIENTS                 *
  !                                                                     *
  !**********************************************************************
  !                                                                     *
  ! 7.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !        
  DO J = 1,KN
     ZPT(J) = PT(J)
     IC(J) = 1
     ZCOSPT(J) = COS(PT(J))
     ZSINPT(J) = SIN(PT(J))
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 7.2. NEWTON'S RULE                                                  *
  !      TWO WAYS OF COMPUTING TO SAVE CPU ON VECTORIZED MACHINE        *
  !      SCALAR MACHINE SHOULD PROBABLY USE THE 1ST OPTION (K INNER LOOP)
  !                                                                     *
  !**********************************************************************
  !
  DO JITER=1,JITERMX
     !
     IF (KN .LE. NFOURPB) THEN
        !
        !     K MOST INNER LOOP
        !
        DO J = 1,KN
           !
           IF (IC(J) .NE. 0) THEN
              !
              RHOPF  = ALZERO
              RHOPFP = 0._RKIND
              DO K=1,NFOURPB
                 RHOPF = RHOPF + BPSCOS(K)*COS(K*ZPT(J)) &
                      &             + BPSSIN(K)*SIN(K*ZPT(J))
                 RHOPFP = RHOPFP - K*BPSCOS(K)*SIN(K*ZPT(J)) &
                      &             + K*BPSSIN(K)*COS(K*ZPT(J))
              ENDDO
              !
              ZSIN = SIN(ZPT(J))
              ZCOS = COS(ZPT(J))
              ZRHOJ(J) = RHOPF
              !
              ZFP = ZSINPT(J)*RPTETP(ZCOS,ZSIN) &
                   &           - ZCOSPT(J)*ZPTETP(ZCOS,ZSIN)
              ZFJ = ZSINPT(J)*RPTET(ZCOS)-ZCOSPT(J)*ZPTET(ZSIN)
              ZEP = ZEPS-ABS(ZFJ)
              !
              IF (ZEP .LE. 0._RKIND) THEN
                 ZPT(J) = ZPT(J) - ZFJ / ZFP
              ELSE
                 IC(J) = 0
              ENDIF
           ENDIF
           !
        END DO
        !
     ELSE
        !
        !     J MOST INNER LOOP
        !
        DO J=1,KN
           ZRHOJ (J) = ALZERO
           ZRHOJP(J) = 0.0_RKIND
        ENDDO
        !
        DO K=1,NFOURPB
           ZK = K
           ZAK = BPSCOS(K)
           ZBK = BPSSIN(K)
           !
           DO J=1,KN
              ZRHOJ (J) = ZRHOJ (J) + ZAK*COS(ZK*ZPT(J)) &
                   &             + ZBK*SIN(ZK*ZPT(J))
              ZRHOJP(J) = ZRHOJP(J) - ZK*ZAK*SIN(ZK*ZPT(J)) &
                   &             + ZK*ZBK*COS(ZK*ZPT(J))
           ENDDO
           !
        END DO
        !
        DO J=1,KN
           !
           RHOPF  = ZRHOJ (J)
           RHOPFP = ZRHOJP(J)
           ZSIN = SIN(ZPT(J))
           ZCOS = COS(ZPT(J))
           ZFP = ZSINPT(J)*RPTETP(ZCOS,ZSIN) &
                &           - ZCOSPT(J)*ZPTETP(ZCOS,ZSIN)
           ZFJ = ZSINPT(J)*RPTET(ZCOS)-ZCOSPT(J)*ZPTET(ZSIN)
           ZEP = ZEPS - ABS(ZFJ)
           !
           IF (ZEP .LE. 0._RKIND) THEN
              ZPT(J) = ZPT(J) - ZFJ / ZFP
           ELSE
              IC(J) = 0
           ENDIF
           !
        ENDDO
        !
     ENDIF
     !
     ITEST = ISSUM(KN,IC,1)
     !        
     IF (ITEST.LE.0) GO TO 723
     !
  END DO
  !
  IF (NVERBOSE .GE. 0) WRITE(0,9001)
  !
723 CONTINUE 
  !
  DO J=1,KN
     RHOPF = ZRHOJ(J)
     ZR = RPTET(COS(ZPT(J)))
     ZZ = ZPTET(SIN(ZPT(J)))
     PR(J) = SQRT(ZR*ZR+ZZ*ZZ)
  ENDDO
  !
  DEALLOCATE(IC, ZF, ZPT, ZPT0, ZCOSPT, ZSINPT, ZRHOJ, ZRHOJP )
  RETURN
  !
  !-----------------------------------------------------------------------
  !
9001 FORMAT(/,'WARNING: MAX # OF NEWTON ITERATIONS REACHED IN' &
       &     ,' BOUND (INCREASE ZEPS OR JITERMX)')
END SUBROUTINE BOUND
