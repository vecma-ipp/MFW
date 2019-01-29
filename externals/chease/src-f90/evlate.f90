!*DECK C2SC02
!*CALL PROCESS 
SUBROUTINE EVLATE(KCASE,PR,PZ,PDPDR,PDPDZ,PSI)
  !        ##############################################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SC02 EVALUATE PSI, D(PSI)/D(R) AND D(PSI)/D(Z) AT (R,Z)           *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PDPDZ
  REAL(RKIND)      ::     PDPDR
  REAL(RKIND)      ::     ZDTDZ
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDTDR
  REAL(RKIND)      ::     ZDSDR
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  REAL(RKIND)      ::     ZDRSDT
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  REAL(RKIND)      ::     ZDFDT
  REAL(RKIND)      ::     ZDFDS
  REAL(RKIND)      ::     DDOT
  REAL(RKIND)      ::     PSI
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZCPSI
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     IT
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     IS
  REAL(RKIND)      ::     ZSIG
  REAL(RKIND)      ::     ZBND
  INTEGER          ::     KCASE
  REAL(RKIND)      ::     ZTET
  REAL(RKIND)      ::     PZ
  REAL(RKIND)      ::     PR
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZDR
  REAL(RKIND)      ::     ZEPS
  DIMENSION &
       &      ZBND(5),    ZCPSI(1,16),     ZDFDS(1,16),    ZDFDT(1,16), &
       &      ZF(1,16),   ZTET(5)
  !
  ZEPS = 1.E-3_RKIND
  !
  ZDR=MAX(ABS(PR-R0),1.E-14_RKIND)
  !
  ZRHO    = SQRT(ZDR**2 + (PZ - RZ0)**2)
  !
  IF (ABS(PR-R0).LE.1.E-14_RKIND) THEN
  !
     ZTET(1) = 0.5_RKIND*CPI*SIGN(1._RKIND,PZ-RZ0)
  !
  ELSE
  !
     ZTET(1) = ATAN2(PZ - RZ0,PR - R0)
  !
  ENDIF
  !
  IF (ZTET(1) .LT. CT(1)) ZTET(1) = ZTET(1) + 2._RKIND * CPI
  !
  IF (KCASE .EQ. 1) THEN
     !
     CALL BOUND(1,ZTET,ZBND)
     !
  ELSE IF (KCASE .EQ. 2) THEN
     !
     ZTET(2) = ZTET(1) - 2._RKIND * ZEPS
     ZTET(3) = ZTET(1) -      ZEPS
     ZTET(4) = ZTET(1) +      ZEPS
     ZTET(5) = ZTET(1) + 2._RKIND * ZEPS
     !
     CALL BOUND(5,ZTET,ZBND)
     !
  ENDIF
  !
  ZSIG = ZRHO / ZBND(1)
  !
  IS = ISRCHFGE(NS1,CSIG,1,ZSIG)  - 1
  IT = ISRCHFGE(NT1,CT,1,ZTET(1)) - 1
  !
  IF (IS .LT. 1)  IS = 1
  IF (IS .GT. NS) IS = NS
  IF (IT .LT. 1)  IT = 1
  IF (IT .GT. NT) IT = NT
  !
  ZS1 = CSIG(IS)
  ZS2 = CSIG(IS+1)
  ZT1 = CT(IT)
  ZT2 = CT(IT+1)
  !
  CALL PSICEL(IS,IT,1,1,ZCPSI,CPSICL)
  !
  IF (KCASE .EQ. 1) THEN
     !
     CALL BASIS1(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG,ZTET(1),ZF)
     !
     PSI = DDOT(16,ZF,1,ZCPSI,1)
     !
  ELSE IF (KCASE .EQ. 2) THEN
     !
     ! EVALUATE FIRST DERIVATIVES WITH RESPECT TO SIGMA AND THETA
     !
     CALL BASIS2(1,1,ZS1,ZS2,ZT1,ZT2,ZSIG,ZTET(1),ZDFDS,ZDFDT)
     !
     ZDPDS = DDOT(16,ZDFDS,1,ZCPSI,1)
     ZDPDT = DDOT(16,ZDFDT,1,ZCPSI,1)
     !
     ZDRSDT = (ZBND(2) + 8*(ZBND(4) - ZBND(3)) - ZBND(5)) / &
          &               (12._RKIND * ZEPS)
     !
     ZCOST = COS(ZTET(1))
     ZSINT = SIN(ZTET(1))
     !
     ZDSDR = (ZDRSDT * ZSINT + ZBND(1) * ZCOST) / ZBND(1)**2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBND(1) * ZSINT - ZDRSDT * ZCOST) / ZBND(1)**2
     ZDTDZ = ZCOST / ZRHO
     !
     PDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     PDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
  ENDIF
  !
  RETURN
END SUBROUTINE EVLATE
