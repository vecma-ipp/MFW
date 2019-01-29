!
SUBROUTINE PROFILE(KN)
  !        ######################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM10 TRACE CONSTANT FLUX SURFACES AND EVALUATE PROFILES           *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  ! 
  INTEGER          ::     KN
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !     ZPSITEST IN BALLIT IS A GUESS FOR PSIISO TO CHECK IF
  !     PSIISO(1).LT.CPSICL(1). WRITE THESE VALUES SO THAT ONE CAN COMPARE
  !     ON THE OUTPUT WITH ZPSITEST WRITTEN BY BALLIT.
  !
  REAL(RKIND)      ::     ZWORK(KN+1)
  REAL(RKIND)      ::     ZCID0, ZCID2
  INTEGER          ::     IP, J1, J2
  ! FUNCTION
  INTEGER          ::     ISRCHFGE
  !
  IF (NVERBOSE .GE. 2) WRITE(6,'(/,"IN PROFILE: PSIISO(1)= ",1PE15.8,"  CPSICL(1)= ", &
    &     E15.8)') PSIISO(1), CPSICL(1)
  !
  IP = ISRCHFGE(KN,PSIISO,1,CPSICL(1))
  !
  IF (IP.LT.1)  IP = 1
  IF (IP.GT.KN) IP = KN
  !
  !     IF IP.NE.1 THEN SIGPSI,.. NOT DEFINED IN ISOFIND WHICH MAY CAUSE
  !     A PROBLEM IN SURFACE FOR EXAMPLE
  !
  IF (IP .NE. 1) THEN
    IF (NVERBOSE .GE. 2) WRITE(6,'(//,"********************  WARNING ***************", &
      &       /,5X,"IN PROFILE: IP=",I4,/,5X, &
      &       "IP SHOULD BE 1, MAY BE R0 TOO FAR FROM RMAG,", &
      &       " DO AN EXTRA ITERATION",/,5x,"USE FILE NOUT WITH NOPT=-2", &
      &       /,"********************  WARNING ***************"//)') IP
  ENDIF
  !
  CALL ISOFIND(IP,KN,SIGPSI,TETPSI,WGTPSI,SPSIM,RC0P)
  !
  DO J1=IP,KN
    !
    CALL CINT(J1,SIGPSI(1,J1),TETPSI(1,J1),WGTPSI(1,J1))
    !
  END DO
  !
  IF (IP .GT. 1) THEN
    if (IP+3 .GT. KN) stop 'profile'
    !
    IF (NSTTP .LE. 2) THEN
      !
      ZCID0 = RMAG
      ZCID2 = RMAG**2
      !
    ELSE IF ((NSTTP.EQ.3) .OR. (NSTTP.EQ.4)) THEN
      !
      ZCID0 = RMAG**2
      ZCID2 = 0._RKIND
      !
    ENDIF
    !
    DO J2=1,IP-1
      !
      CID0(J2) = FCCCC0(ZCID0,CID0(IP),CID0(IP+1),CID0(IP+2), &
        &                        SPSIM,PSIISO(IP),PSIISO(IP+1), &
        &                        PSIISO(IP+2),PSIISO(J2))
      CIDR(J2) = FCCCC0(CIDR(IP),CIDR(IP+1),CIDR(IP+2),CIDR(IP+3), &
        &                        PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2), &
        &                        PSIISO(IP+3),PSIISO(J2))
      CIDQ(J2) = FCCCC0(CIDQ(IP),CIDQ(IP+1),CIDQ(IP+2),CIDQ(IP+3), &
        &                        PSIISO(IP),PSIISO(IP+1),PSIISO(IP+2), &
        &                        PSIISO(IP+3),PSIISO(J2))
      CID2(J2) = FCCCC0(ZCID2,CID2(IP),CID2(IP+1),CID2(IP+2), &
        &                        SPSIM,PSIISO(IP),PSIISO(IP+1), &
        &                        PSIISO(IP+2),PSIISO(J2))
      !
    END DO
    !
  ENDIF
  !
  !        COMPUTE RHO_TOR MESH IF NEEDED TO GET PROFILES
  !
  IF (NRHOMESH .GT. 0) THEN
    IF (KN .NE. NISO) THEN
      IF (NVERBOSE .GE. 1) PRINT *,'KN= ',KN,' .NE. NISO= ',NISO, &
           & ' => CIDRTOR NOT RECALCULATED SINCE TMF NOT YET DEFINED ON FULL MESH'
      IF (NVERBOSE .GE. 1) PRINT *,' TMF(KN)= ',TMF(1:KN)
      !        CASE OF MAPPING ON WHATEVER PSIISO SURFACES AND KN CAN BE WHATEVER. BETTER TO KEEP LAST
      !        VALUES OF CIDRTOR WITH LAST TMF DURING ITERATIONS. MIGHT BE THAT ONE SHOULD RECALCULATED 
      !        ONCE EQUILIBRIUM HAS CONVERGED.
      !        NOTE, ONE COULD CALCULATE HERE TMF ON PRESENT PSIISO MESH...
      !              
    ELSE
      IF (NVERBOSE .GE. 1) PRINT *,'KN= ',KN,' , NISO= ',NISO,' CIDRTOR recalculated'
      IF (ABS(TMF(KN)) .LE. 1.E-08_RKIND) THEN
        IF (NVERBOSE .GE. 1) print *,' profile special case, KN= ',KN,'  NISO= ',NISO,' TMF(KN)= ',TMF(KN)
      endif
      IF (ABS(TMF(KN)) .GT. 1.E-08_RKIND) THEN
        !        COMPUTE RHOTOR ON CSIPRI MESH (EQUIVALENT TO CS MESH, SINCE CIDQ ON CSM MESH)
        CIDRTOR(1)=0._RKIND
        DO J2=2,KN
          CIDRTOR(J2) = CIDRTOR(J2-1) + CIDQ(J2-1)*TMF(J2-1)*SPSIM*(CSIPRI(J2-1)**2-CSIPRI(J2)**2)
        END DO
        CIDRTOR(1:KN) = sqrt(CIDRTOR(1:KN)/CIDRTOR(KN))
      ELSE
        ! ASSUME TMF=1
        IF (NVERBOSE .GE. 1) PRINT *,' ASSUME T=1 TO COMPUTE CIDRTOR=RHOTOR ON CSIPRI MESH'
        CIDRTOR(1)=0._RKIND
        DO J2=2,KN
          CIDRTOR(J2) = CIDRTOR(J2-1) + CIDQ(J2-1)*SPSIM*(CSIPRI(J2-1)**2-CSIPRI(J2)**2)
        END DO
        CIDRTOR(1:KN) = sqrt(CIDRTOR(1:KN)/CIDRTOR(KN))
      ENDIF
      CALL SPLINE(KN,CSIPRI,CIDRTOR,D2CIDRTOR,ZWORK)
    ENDIF
    IF (KN .GE. NISO+5) THEN
      !        TMF NOT YET DEFINED (ON NEW MESH) COMPUTE RHOTOR ON CSIPRI MESH ASSUME TMF=1
      !        THIS CASE WAS WHILE DEVELOPING, NOT SURE RELEVANT ACTION TO DO, BETTER KEEP LAST VALUE CALCULATED
      !        IF THIS CASE STOPS HERE, CONTACT O. SAUTER FOR FURTHER DEVELOPMENT 
      IF (NVERBOSE .GE. 0) print *,' profile special case, KN= ',KN,'  NISO= ',NISO
      stop 'CONTACT O. SAUTER'
      CIDRTOR(1)=0._RKIND
      DO J2=2,KN
        CIDRTOR(J2) = CIDRTOR(J2-1) + CIDQ(J2-1)*SPSIM*(CSIPRI(J2-1)**2-CSIPRI(J2)**2)
      END DO
      CIDRTOR(1:KN) = sqrt(CIDRTOR(1:KN)/CIDRTOR(KN))
      CALL SPLINE(KN,CSIPRI,CIDRTOR,D2CIDRTOR,ZWORK)
    ENDIF
  ENDIF
  !
  !        COMPUTE PROFILES
  !
  CALL ISOFUN(KN)
  !
  RETURN
END SUBROUTINE PROFILE
