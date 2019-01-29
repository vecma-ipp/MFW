SUBROUTINE PREMAP(K)
  !        ####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! LEAD SET-UP OF STABILITY S-MESH, TRACING OF CONSTANT FLUX    *
  !        SURFACES AND EVALUATION OF PROFILES                          *
  !
  ! USE GENERIC MESH NAMES FOR ALL CASES TO FACILITATE MAPPIN:
  !
  ! PSIISO: (PSIEDGE-PSIMIN)*(1-S**2) AS BEFORE, PSI MESH FOR MAX IN CENTER AND 0 AT EDGE
  ! SMISO: S ON PSIISO MESH, CALLED SMISO SINCE IS CSM FOR STANDARD CASE
  ! SMISOP1: [0., SMISO], SMISO MESH WITH EXTRA 0. VALUE TO HAVE AXIS (FOR EQCHEASE_OUT ETC )
  ! NISO1EFF: LENGTH(PSIISO): NPSI1, 2*NPSI, ETC DEPENDING ON NIDEAL AND PREMAP CALL
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     K
  !
  REAL(RKIND)      ::     ZPAR(NPISOEFF), ZS(NPISOEFF)
  INTEGER          ::     IC(NPISOEFF), IS0(NPISOEFF)
  INTEGER          ::     I1, I2, ISIPR, JS, JG
  INTEGER          ::     J1, J2, J3, J4, J5, J6, J7
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(CID0,NPISOEFF)
  CALL VZERO(CIDQ,NPISOEFF)
  CALL VZERO(CIDR,NPISOEFF)
  CALL VZERO(CID2,NPISOEFF)
  CALL VZERO(SIGPSI,NPMGS*NTP1*NPISOEFF)
  CALL VZERO(TETPSI,NPMGS*NTP1*NPISOEFF)
  CALL VZERO(WGTPSI,NPMGS*NTP1*NPISOEFF)
  !
  IF (K .EQ. 1) THEN
    !
    NISO1EFF = NISO
    SMISO(1:NISO1EFF)  = CSIPR(1:NISO1EFF)
    PSIISO(1:NISO1EFF) = SPSIM * (1._RKIND - SMISO(1:NISO1EFF)**2)
    ! this case should not be called for mappin but since has already rho=0 do not add it
    NISO1EFF1 = NISO1EFF
    SMISOP1(1:NISO1EFF) = SMISO(1:NISO1EFF)
    !
  ELSE IF (K .EQ. 2) THEN
    !
    NISO1EFF = NPPR+1
    SMISO(1:NISO1EFF)  = PCSM(1:NISO1EFF)
    PSIISO(1:NISO1EFF) = SPSIM * (1._RKIND - SMISO(1:NISO1EFF)**2)
    NISO1EFF1 = NISO1EFF + 1
    SMISOP1(1:NISO1EFF+1) = (/ 0._RKIND, SMISO(1:NISO1EFF) /)
    !
  ELSE IF (K .EQ. 3) THEN
    !
    NISO1EFF = NPSI
    SMISO(1:NISO1EFF)  = CS(2:NISO1EFF+1)
    PSIISO(1:NISO1EFF) = SPSIM * (1._RKIND - PSISCL * SMISO(1:NISO1EFF)**2)
    NISO1EFF1 = NISO1EFF + 1
    SMISOP1(1:NISO1EFF+1) = (/ 0._RKIND, SMISO(1:NISO1EFF) /)
    !
  ELSE IF (K .EQ. 4) THEN
    !
    NISO1EFF = 2 * NPSI
    SMISO(1:NISO1EFF-1:2) = CSM(1:NPSI)
    SMISO(2:NISO1EFF:2)   = CS(2:NPSI1)
    PSIISO(1:NISO1EFF) = SPSIM * (1._RKIND - PSISCL * SMISO(1:NISO1EFF)**2)
    NISO1EFF1 = NISO1EFF + 1
    SMISOP1(1:NISO1EFF1) = (/ 0._RKIND, SMISO(1:NISO1EFF) /)
    !
  ELSE IF (K .EQ. 5) THEN
    !
    NISO1EFF = NPSI * (NMGAUS + 2)
    I1 = 1
    I2 = 1
    !
    DO J5=1,NISO1EFF
      IF (MOD(J5,NMGAUS+2) .EQ. 3) THEN
        SMISO(J5) = CSM(I1)
        I1        = I1 + 1
      ELSE
        SMISO(J5) = CSPEN(I2)
        I2        = I2 + 1
      ENDIF
    END DO
    PSIISO(1:NISO1EFF) = SPSIM * (1._RKIND - PSISCL * SMISO(1:NISO1EFF)**2)
    NISO1EFF1 = NISO1EFF + 1
    SMISOP1(1:NISO1EFF+1) = (/ 0._RKIND, SMISO(1:NISO1EFF) /)
    !
  ELSE IF (K .EQ. 6) THEN
    ! EQUIDISTANT WITH DX=1/(NPSI), FIRST POINT AT ZS=1/(NPSI), LAST AT ZS=1
    NISO1EFF = NPSI
    DO J6=1,NPSI
      SMISO(J6)=REAL(J6,RKIND)/REAL(NPSI,RKIND)
    END DO
    PSIISO(1:NISO1EFF) = SPSIM * (1._RKIND - PSISCL * SMISO(1:NISO1EFF)**2)
    NISO1EFF1 = NISO1EFF + 1
    SMISOP1(1:NISO1EFF+1) = (/ 0._RKIND, SMISO(1:NISO1EFF) /)
    !
  ENDIF
  !
  IF (((NSTTP.EQ.3) .OR. (NSTTP.EQ.4)) .AND. K .GT. 2) THEN
    !
    CALL RESETI(IC,NISO1EFF,1)
    DO JS = 1,NISO
      DO JG=1,NISO1EFF
        IF (IC(JG).EQ.1) THEN
          IS0(JG) = JS-1
          IF (SMISO(JG).LE.CSIPR(JS)) IC(JG)  = 0
        ENDIF
      ENDDO
    ENDDO
    !
    DO J7=1,NISO1EFF
      !
      ISIPR = IS0(J7)
      !
      IF (ISIPR .GE. NISO - 2) ISIPR = NISO - 2
      IF (ISIPR .LE. 2)        ISIPR = 2
      !
      TMF(J7) = FCCCC0(TMFO(ISIPR-1),TMFO(ISIPR), &
           &           TMFO(ISIPR+1),TMFO(ISIPR+2), &
           &           CSIPR(ISIPR-1),CSIPR(ISIPR), &
           &           CSIPR(ISIPR+1),CSIPR(ISIPR+2), &
           &           SMISO(J7))
      !
    END DO
    !
  ENDIF
  !
  CALL RMRAD(NISO1EFF,SPSIM,RC0P,RC0P,ZPAR,SIGMAP,TETMAP,NTP2)
  CALL PROFILE(NISO1EFF)
  !
  RETURN
END SUBROUTINE PREMAP
