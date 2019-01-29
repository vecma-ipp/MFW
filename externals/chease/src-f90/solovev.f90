!*DECK C2SG02
!*CALL PROCESS
SUBROUTINE SOLOVEV
  !        ##################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SG02 COMPUTE ANALYTIC PROJECTION ON BICUBIC HERMITE ELEMENTS OF   *
  !        SOLOVEV EQUILIBRIUM SOLUTION                                 *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  INCLUDE 'SOLOV.inc'
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
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
        IF (J2 .NE. NS1) THEN
           !
           CPSICL(4*(I-1)+1) = SPS(ZR,ZZ) - SPSI0
           CPSICL(4*(I-1)+3) = DSPSDR(ZR,ZZ) * ZDRDT + &
                &                          DSPSDZ(ZR,ZZ) * ZDZDT
           !
        ELSE
           !
           CPSICL(4*(I-1)+1) = 0._RKIND
           CPSICL(4*(I-1)+3) = 0._RKIND
           !
        ENDIF
        !
        CPSICL(4*(I-1)+2) = DSPSDR(ZR,ZZ) * ZDRDS + &
             &                       DSPSDZ(ZR,ZZ) * ZDZDS
        CPSICL(4*(I-1)+4) = D2SPSR(ZR,ZZ) * ZDRDT * ZDRDS + &
             &                       D2SPSZ(ZR,ZZ) * ZDZDT * ZDZDS + &
             &                       DSPSRZ(ZR,ZZ) * (ZDRDT * ZDZDS + &
             &                                        ZDRDS * ZDZDT) + &
             &                       DSPSDR(ZR,ZZ) * ZD2R + &
             &                       DSPSDZ(ZR,ZZ) * ZD2Z
        !
     END DO
  END DO
  !
  SPSIM = - SPSI0
  RMAG  = RC
  RZMAG = 0._RKIND
  !
  CALL OUTPUT(4)
  !
  RETURN
END SUBROUTINE SOLOVEV
