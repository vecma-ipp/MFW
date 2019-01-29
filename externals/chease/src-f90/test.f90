!*DECK C2SG01
!*CALL PROCESS
SUBROUTINE TEST
  !        ###############
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SG01 FOR SOLOVEV EQUILIBRIUM (NSURF = 1), COMPUTE NUMERICAL AND   *
  !        ANALYTICAL VALUES OF PSI AT (CSIG(JS),THETA(JT) NODES,       *
  !        AS WELL AS ABSOLUTE DIFFERENCES AND RESIDU                   *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     SSUM
  REAL(RKIND)      ::     ZNDST
  REAL(RKIND)      ::     ZNDT
  REAL(RKIND)      ::     ZNDS
  REAL(RKIND)      ::     ZND
  REAL(RKIND)      ::     ZD2Z
  REAL(RKIND)      ::     ZD2R
  REAL(RKIND)      ::     ZDZDT
  REAL(RKIND)      ::     ZDRDT
  REAL(RKIND)      ::     ZDZDS
  REAL(RKIND)      ::     ZDRDS
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  INTEGER          ::     I
  INTEGER          ::     J1
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZNDST1
  REAL(RKIND)      ::     ZNDT1
  REAL(RKIND)      ::     ZNDS1
  REAL(RKIND)      ::     ZND1
  DIMENSION &
       &   ZND(NPT),  ZNDS(NPT),  ZNDST(NPT),  ZNDT(NPT)
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  INCLUDE 'SOLOV.inc'
  !
  !**********************************************************************
  !                                                                     *
  ! 1. TEST IF NSURF = 1                                                *
  !                                                                     *
  !**********************************************************************
  !
  IF (NSURF .NE. 1) RETURN
  !
  !**********************************************************************
  !                                                                     *
  ! 2.1. INITIALIZATION                                                 *
  !                                                                     *
  !**********************************************************************
  !
  ZND1   = 0._RKIND
  ZNDS1  = 0._RKIND
  ZNDT1  = 0._RKIND
  ZNDST1 = 0._RKIND
  !
  !**********************************************************************
  !                                                                     *
  ! 2.2. SCAN OVER ALL INTERVALS                                        *
  !                                                                     *
  !**********************************************************************
  !
  DO J2=1,NS1
     !
     DO J1=1,NT
        !
        I = (J2 - 1) * NT + J1
        !
        ZR = CSIG(J2) * RHOS(J1) * COS(CT(J1)) + R0
        ZZ = CSIG(J2) * RHOS(J1) * SIN(CT(J1)) + RZ0
        !
        ZDRDS = RHOS(J1) * COS(CT(J1))
        ZDZDS = RHOS(J1) * SIN(CT(J1))
        ZDRDT = CSIG(J2) * (DRSDT(J1) * COS(CT(J1)) - &
             &                       RHOS(J1) * SIN(CT(J1)))
        ZDZDT = CSIG(J2) * (DRSDT(J1) * SIN(CT(J1)) + &
             &                       RHOS(J1) * COS(CT(J1)))
        ZD2R  = DRSDT(J1) * COS(CT(J1)) - RHOS(J1) * SIN(CT(J1))
        ZD2Z  = DRSDT(J1) * SIN(CT(J1)) + RHOS(J1) * COS(CT(J1))
        !
        CPSI1T(I) = SPS(ZR,ZZ)
        DPDSTH(I) = DSPSDR(ZR,ZZ) * ZDRDS + DSPSDZ(ZR,ZZ) * ZDZDS
        DPDTTH(I) = DSPSDR(ZR,ZZ) * ZDRDT + DSPSDZ(ZR,ZZ) * ZDZDT
        D2PSTT(I) = D2SPSR(ZR,ZZ) * ZDRDT * ZDRDS + &
             &               D2SPSZ(ZR,ZZ) * ZDZDT * ZDZDS + &
             &               DSPSRZ(ZR,ZZ) * (ZDRDT * ZDZDS + &
             &                                ZDRDS * ZDZDT) + &
             &               DSPSDR(ZR,ZZ) * ZD2R + &
             &               DSPSDZ(ZR,ZZ) * ZD2Z
        DPDSNU(I) = CPSICL(4*(I-1)+2)
        DPDTNU(I) = CPSICL(4*(I-1)+3)
        D2PSTN(I) = CPSICL(4*(I-1)+4)
        !
        DIFFP(I)  = CPSI1T(I) - CPSICL(4*(I-1)+1)
        DIFFDS(I) = DPDSTH(I) - DPDSNU(I)
        DIFFDT(I) = DPDTTH(I) - DPDTNU(I)
        DIFFST(I) = D2PSTT(I) - D2PSTN(I)
        !
        ZND(J1)   = DIFFP(I)**2
        ZNDS(J1)  = DIFFDS(I)**2
        ZNDT(J1)  = DIFFDT(I)**2
        ZNDST(J1) = DIFFST(I)**2
        !
     END DO
     !
     ZND1   = ZND1   + SSUM(NT,ZND,1)
     ZNDS1  = ZNDS1  + SSUM(NT,ZNDS,1)
     ZNDT1  = ZNDT1  + SSUM(NT,ZNDT,1)
     ZNDST1 = ZNDST1 + SSUM(NT,ZNDST,1)

  END DO
  !
  RESPSI = SQRT(ZND1)   / NSTMAX
  RESDPS = SQRT(ZNDS1)  / NSTMAX
  RESDPT = SQRT(ZNDT1)  / NSTMAX
  RESDST = SQRT(ZNDST1) / NSTMAX
  !
  RETURN
END SUBROUTINE TEST
