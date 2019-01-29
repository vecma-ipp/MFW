!
SUBROUTINE DRHODP(KN,PT,PRHO,PDRODP)
  !        ####################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2ST02  EVALUATE D(RHO)/D(PSI) FOR A GIVEN SET OF PSI VALUES,      *
  !          WHERE RHO IS A NORMALIZED RADIUS ALONG A LINE STARTING     *
  !          AT THE MAGNETIC AXIS AND WITH AN ANGLE OF PT WITH          *
  !          RESPECT TO THE Z=0 PLANE                                   *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PDRODP
  REAL(RKIND)      ::     ZDPDZ
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZDTDZ
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDTDR
  REAL(RKIND)      ::     ZDSDR
  REAL(RKIND)      ::     ZCOS
  REAL(RKIND)      ::     ZSIN
  REAL(RKIND)      ::     ZDRSDT
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZDFDT
  REAL(RKIND)      ::     ZDFDS
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     IT0
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     IS0
  REAL(RKIND)      ::     ZSIGMA
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZBND2
  REAL(RKIND)      ::     ZBND1
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZTETA2
  REAL(RKIND)      ::     ZTETA1
  REAL(RKIND)      ::     ZTETA
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     PRHO
  REAL(RKIND)      ::     ZR
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZTEPS
  REAL(RKIND)      ::     ZBNDT0
  REAL(RKIND)      ::     PT
  INTEGER          ::     KN
  DIMENSION &
       &   IS0(KN),       IT0(KN), &
       &   PRHO(KN),      PDRODP(KN), &
       &   ZBND(KN), &
       &   ZBND1(KN),     ZBND2(KN), &
       &   ZDFDS(KN,16),  ZDFDT(KN,16), &
       &   ZPCEL(KN,16),  ZRHO(KN), &
       &   ZS1(KN),       ZS2(KN), &
       &   ZTETA(KN),     ZTETA1(KN), &
       &   ZTETA2(KN),    ZT1(KN), &
       &   ZT2(KN) ,      ZSIGMA(KN)    
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  BPS( 1) = RMAG
  BPS(12) = RZMAG
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  CALL BOUND(1,PT,ZBNDT0)
  !
  BPS( 1) = R0
  BPS(12) = RZ0
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  ZTEPS = 1.E-4_RKIND
  !
  DO J1=1,KN
     !
     ZR         = PRHO(J1) * ZBNDT0 * COS(PT) + RMAG
     ZZ         = PRHO(J1) * ZBNDT0 * SIN(PT) + RZMAG
     ZRHO(J1)   = SQRT((ZR - R0)**2 + (ZZ - RZ0)**2)
     ZTETA(J1)  = ATAN2(ZZ - RZ0,ZR - R0)
     !
     IF (ZTETA(J1) .LT. CT(1)) ZTETA(J1) = ZTETA(J1) + 2._RKIND * CPI
     !
     ZTETA1(J1) = ZTETA(J1) - ZTEPS
     ZTETA2(J1) = ZTETA(J1) + ZTEPS
     !
  END DO
  !
  CALL BOUND(KN,ZTETA,ZBND)
  CALL BOUND(KN,ZTETA1,ZBND1)
  CALL BOUND(KN,ZTETA2,ZBND2)
  !
  DO J2=1,KN
     !
     ZSIGMA(J2) = ZRHO(J2) / ZBND(J2)
     !
     IS0(J2) = ISRCHFGE(NS1,CSIG,1,ZSIGMA(J2)) - 1
     !
     IF (IS0(J2) .GT. NS) THEN
        !
        IS0(J2) = NS
        !
     ELSE IF (IS0(J2) .LT. 1) THEN
        !
        IS0(J2) = 1
        !
     ENDIF
     !
     IT0(J2) = ISRCHFGE(NT1,CT,1,ZTETA(J2)) - 1
     !
     IF (IT0(J2) .GT. NT) THEN
        !
        IT0(J2) = NT
        !
     ELSE IF (IT0(J2) .LT. 1) THEN
        !
        IT0(J2) = 1
        !
     ENDIF
     !
     ZS1(J2) = CSIG(IS0(J2))
     ZS2(J2) = CSIG(IS0(J2)+1)
     ZT1(J2) = CT(IT0(J2))
     ZT2(J2) = CT(IT0(J2)+1)
     !
  END DO
  !
  CALL BASIS2(KN,KN,ZS1,ZS2,ZT1,ZT2,ZSIGMA,ZTETA,ZDFDS, &
       &               ZDFDT)
  CALL PSICEL(IS0,IT0,KN,KN,ZPCEL,CPSICL)
  !
  DO J3=1,KN
     !
     ZDPDS = ZPCEL(J3, 1) * ZDFDS(J3, 1) + &
          &           ZPCEL(J3, 2) * ZDFDS(J3, 2) + &
          &           ZPCEL(J3, 3) * ZDFDS(J3, 3) + &
          &           ZPCEL(J3, 4) * ZDFDS(J3, 4) + &
          &           ZPCEL(J3, 5) * ZDFDS(J3, 5) + &
          &           ZPCEL(J3, 6) * ZDFDS(J3, 6) + &
          &           ZPCEL(J3, 7) * ZDFDS(J3, 7) + &
          &           ZPCEL(J3, 8) * ZDFDS(J3, 8) + &
          &           ZPCEL(J3, 9) * ZDFDS(J3, 9) + &
          &           ZPCEL(J3,10) * ZDFDS(J3,10) + &
          &           ZPCEL(J3,11) * ZDFDS(J3,11) + &
          &           ZPCEL(J3,12) * ZDFDS(J3,12) + &
          &           ZPCEL(J3,13) * ZDFDS(J3,13) + &
          &           ZPCEL(J3,14) * ZDFDS(J3,14) + &
          &           ZPCEL(J3,15) * ZDFDS(J3,15) + &
          &           ZPCEL(J3,16) * ZDFDS(J3,16) 
     ! 
     ZDPDT =  ZPCEL(J3, 1) * ZDFDT(J3, 1) + &
          &           ZPCEL(J3, 2) * ZDFDT(J3, 2) + &
          &           ZPCEL(J3, 3) * ZDFDT(J3, 3) + &
          &           ZPCEL(J3, 4) * ZDFDT(J3, 4) + &
          &           ZPCEL(J3, 5) * ZDFDT(J3, 5) + &
          &           ZPCEL(J3, 6) * ZDFDT(J3, 6) + &
          &           ZPCEL(J3, 7) * ZDFDT(J3, 7) + &
          &           ZPCEL(J3, 8) * ZDFDT(J3, 8) + &
          &           ZPCEL(J3, 9) * ZDFDT(J3, 9) + &
          &           ZPCEL(J3,10) * ZDFDT(J3,10) + &
          &           ZPCEL(J3,11) * ZDFDT(J3,11) + &
          &           ZPCEL(J3,12) * ZDFDT(J3,12) + &
          &           ZPCEL(J3,13) * ZDFDT(J3,13) + &
          &           ZPCEL(J3,14) * ZDFDT(J3,14) + &
          &           ZPCEL(J3,15) * ZDFDT(J3,15) + &
          &           ZPCEL(J3,16) * ZDFDT(J3,16) 
     !
     ZDRSDT = .5_RKIND * (ZBND2(J3) - ZBND1(J3)) / ZTEPS
     ZSIN   = SIN(ZTETA(J3))
     ZCOS   = COS(ZTETA(J3))
     !
     ZDSDR = (ZDRSDT * ZSIN + ZBND(J3) * ZCOS) / ZBND(J3)**2
     ZDTDR = - ZSIN / ZRHO(J3) 
     ZDSDZ = (ZBND(J3) * ZSIN - ZDRSDT * ZCOS) / ZBND(J3)**2
     ZDTDZ = ZCOS / ZRHO(J3)  
     !
     ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     PDRODP(J3) = 1._RKIND / (ZBNDT0 * &
          &                 (COS(PT) * ZDPDR + SIN(PT) * ZDPDZ))
     !
  END DO
  !
  RETURN
END SUBROUTINE DRHODP
