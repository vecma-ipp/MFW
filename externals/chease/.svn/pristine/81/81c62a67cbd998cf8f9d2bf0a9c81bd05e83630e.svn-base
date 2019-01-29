!*DECK C3SB08
!*CALL PROCESS
SUBROUTINE BSTNZPRO(PTE,PTEP,PNE,PNEP,PZEFF,PTI,PTIP,PPEOP,PPCS &
     &  ,KN,PPRESS,PPRESS0,PPPRESS,KOPT)
  !        ##############################################################
  !                                        AUTHORS:
  !                                        O. SAUTER,  CRPP-EPFL
  !                                        H. LUTJENS, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SB08 COMPUTE T_E, N_E, ZEFF, AND T_I FOR BOOSTRAP CALCULATION     *
  !        ON PPCS(1,..,KN) FROM EXPERIMENTAL VALUES                    *
  !        KOPT = 0: COMPUTE ONLY FUNCTIONS                             *
  !        KOPT = 1: COMPUTE ALSO PTEP AND PTIP (D/DPSI)                *
  !        KOPT = 2: COMPUTE ALSO PTEP AND PTIP, BUT  PTP = D(PT) / DS  *
  !                                                                     *
  !        ASSUME PPCS SAME MESH AS FOR CPR(K). IF KN=1, K=KCPRVAL      *
  !                                                                     *
  !        IPRO GIVES IN OUTPUT WHICH PROFILE HAVE BEEN COMPUTED, IN    *
  !        FORMAT AS NBSEXPQ                                            *
  !
  !     PPRESS = LOCAL PRESSURE
  !     PPPRESS = LOCAL DP/DPSI
  !     PPRESS0 = pressure on axis
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PPPRESS
  REAL(RKIND)      ::     ZTEMPE
  REAL(RKIND)      ::     ZTEMPE0
  REAL(RKIND)      ::     PPRESS0
  REAL(RKIND)      ::     ZPMKSA0
  REAL(RKIND)      ::     PPEOP
  REAL(RKIND)      ::     ZPMKSA
  REAL(RKIND)      ::     PPRESS
  REAL(RKIND)      ::     ZPPRESSK
  INTEGER          ::     K
  REAL(RKIND)      ::     ZMU0
  REAL(RKIND)      ::     PTIP
  REAL(RKIND)      ::     PTI
  REAL(RKIND)      ::     ZZZ
  REAL(RKIND)      ::     PZEFF
  REAL(RKIND)      ::     PNEP
  REAL(RKIND)      ::     PNE
  REAL(RKIND)      ::     ZPSI1
  INTEGER          ::     KOPT
  REAL(RKIND)      ::     PTEP
  REAL(RKIND)      ::     PTE
  REAL(RKIND)      ::     PPCS
  REAL(RKIND)      ::     ZPSII
  INTEGER          ::     I
  REAL(RKIND)      ::     ZETAEI
  INTEGER          ::     ITI
  INTEGER          ::     IZEFF
  INTEGER          ::     INE
  INTEGER          ::     ITE
  REAL(RKIND)      ::     ZEPS08
  INTEGER          ::     KN
  DIMENSION PTE(KN), PTEP(KN), PNE(KN), PNEP(KN), PZEFF(KN), &
       &     PTI(KN), PTIP(KN), PPCS(KN), ZPSII(KN), PPRESS(KN), &
       &     PPEOP(KN), PPPRESS(KN)
  CHARACTER*4 TEXT
  !-----------------------------------------------------------------------
  ZEPS08 = 1.E-08_RKIND
  ITE = 0
  INE = 0
  IZEFF = 0
  ITI = 0
  IF (NBSEXPQ .GT. 0) THEN
     WRITE(TEXT,'(I4.4)') NBSEXPQ
     READ(TEXT(1:1),'(I1)') ITE
     READ(TEXT(2:2),'(I1)') INE
     READ(TEXT(3:3),'(I1)') IZEFF
     READ(TEXT(4:4),'(I1)') ITI
  ENDIF
  !
  !     1. PROFILES VIA EXPEQ OR AT2 (TE) OR AT4 (ZEFF)
  !
  !     1.1 ELECTRON TEMPERATURE
  !
  ZETAEI = ABS(ETAEI)
  IF (ITE .EQ. 1) THEN
     DO I=1,KN
        ZPSII(I) = (1._RKIND-PPCS(I)**2) * SPSIM
     END DO
     CALL PPSPLN(KN,ZPSII,NTNZPT,FCSMTNZ,BSTEMPE,BSD2TE,PTE,PTEP, &
          &       KOPT)
  ELSE IF (ETAEI .LT. 0._RKIND) THEN
     DO I=1,KN
        ZPSI1 = (1._RKIND-PPCS(I)**2) * SPSIM
        CALL POLYFUN(NSOUR,AT4,1,ZPSI1,PTE(I),1,1)
        IF (KOPT .EQ. 1)CALL POLYFUN(NSOUR,AT4,1,ZPSI1,PTEP(I),2,1)
     END DO
     ITE = 1
  ENDIF
  !
  !     1.2 ELECTRON DENSITY
  !
  IF (INE .EQ. 1) THEN
     DO I=1,KN
        ZPSII(I) = (1._RKIND-PPCS(I)**2) * SPSIM
     END DO
     CALL PPSPLN(KN,ZPSII,NTNZPT,FCSMTNZ,BSDENSE,BSD2NE,PNE,PNEP, &
          &       KOPT)
  ENDIF
  !
  !     1.3 ZEFF
  !
  IF (IZEFF .EQ. 1) THEN
     DO I=1,KN
        ZPSII(I) = (1._RKIND-PPCS(I)**2) * SPSIM
     END DO
     CALL PPSPLN(KN,ZPSII,NTNZPT,FCSMTNZ,BSZEFF,BSD2ZEF,PZEFF,ZZZ &
          &       ,0)
  ELSE
     DO I=1,KN
        ZPSI1 = (1._RKIND-PPCS(I)**2) * SPSIM
        PZEFF(I) = RZION
        IF(RZION.LT.0._RKIND)CALL POLYFUN(NSOUR,AT2,1,ZPSI1,PZEFF(I),1,1)
     END DO
  ENDIF
  IZEFF = 1
  !
  !     1.4 ION TEMPERATURE
  !
  IF (ITI .EQ. 1) THEN
     DO I=1,KN
        ZPSII(I) = (1._RKIND-PPCS(I)**2) * SPSIM
     END DO
     CALL PPSPLN(KN,ZPSII,NTNZPT,FCSMTNZ,BSTEMPI,BSD2TI,PTI,PTIP, &
          &       KOPT)
  ENDIF
  !
  !     2. DETERMINE PROFILES NOT GIVEN VIA EXPEQ OR AT2, AT4
  !     ASSUMES THAT IF NE IS GIVEN IN EXPEQ, SO IS TE
  !
  ZMU0 = 4.E-07_RKIND * CPI
  !
  DO K=1,KN
     !
     ZPPRESSK = MAX(ZEPS08,PPRESS(K))
     ZPMKSA = ZPPRESSK *B0EXP**2 / ZMU0
     !
     !     2.1 DETERMINE PE/P
     !
     !     DIRECTLY FROM NAMELIST VARIABLES IF RPEOP > 0
     IF (RPEOP .GE. 0._RKIND) THEN
        PPEOP(K) = RPEOP
     ELSE
        IF (INE .EQ. 0) THEN
           CALL POLYFUN(NSOUR,AT3,1,(1._RKIND-PPCS(K)**2)*SPSIM,PPEOP(K),1,1)
        ELSE
           !     VIA N_E FROM EXPEQ => NEED RPEOP<0 IN NAMELIST
           PPEOP(K) = 1.0_RKIND
           IF (ZPMKSA .NE. 0._RKIND) PPEOP(K)=PNE(K)*PTE(K)*1.602E-19_RKIND/ZPMKSA
           IF (PPEOP(K).GT.1._RKIND) THEN
             IF (NVERBOSE .GE. 0) write(0,*) ' WARNING in bstnzpro, PPEOP(',K,') = ',PPEOP(K),' > 1.; SET TO 0.99'
             PPEOP(K) = 0.99_RKIND
           ENDIF
         ENDIF
     ENDIF
     !
     !     2.2 ELECTRON TEMPERATURE
     !
     IF (ITE .EQ. 0) THEN
        ZPMKSA0 = PPRESS0 *B0EXP**2 / ZMU0
        !     TE IN EV
        ZTEMPE0 = SQRT(PPEOP(K)*ABS(ZPMKSA0)/1.E+19_RKIND/1.602E-16_RKIND) &
             &         * 1000._RKIND
        IF ((RPEOP .GE. 0._RKIND) .AND. AT3(1) .LE. 0._RKIND) THEN
          ! TE0_keV/NE0_19 = abs(AT3(1))
          ! write(*,*) 'Assumes Te0[keV] / ne0 [1e19] = abs(AT3(1)) = ',abs(AT3(1))
          ZTEMPE0 =  sqrt(abs(AT3(1)) * PPEOP(K)*ABS(ZPMKSA0) / 1.602_RKIND * 1000._RKIND)
        END IF
        ZTEMPE = ZEPS08
        IF (ZPMKSA0 .NE. 0._RKIND) &
             &         ZTEMPE = ZTEMPE0 * ABS(ZPMKSA/ZPMKSA0)**(ZETAEI/(1._RKIND+ZETAEI))
        IF (ZTEMPE.LE.1.E-12_RKIND .AND. K.NE.1 .AND. ZPMKSA0.NE.0._RKIND) THEN
           ZPMKSA = ZPPRESSK *B0EXP**2 / ZMU0
           ZTEMPE = ZTEMPE0 * ABS(ZPMKSA/ZPMKSA0)**(ZETAEI/(1._RKIND+ZETAEI))
        ENDIF
        PTE(K) = ZTEMPE
        IF (KOPT .GE. 1) PTEP(K) = ZTEMPE * ZETAEI / (1 + ZETAEI) &
             &         * PPPRESS(K) / ZPPRESSK
     ENDIF
     !
     !     2.3 ION TEMPERATURE
     !
     IF (ITI .EQ. 0) THEN
        PTI(K) = PTE(K)
        IF (KOPT .GE. 1) PTIP(K) = PTEP(K)
     ENDIF
     !
     !     2.4 ELECTRON DENSITY, USING PE/P
     !
     IF (INE .EQ. 0) THEN
        PNE(K) = 1.0_RKIND
        IF (PTE(K) .NE. 0._RKIND) &
             &         PNE(K) = PPEOP(K) / PTE(K) / 1.602E-19_RKIND * ZPMKSA
        IF (KOPT .GE. 1) THEN
          PNEP(K) = PTEP(K) * PNE(K) / PTE(K) / ZETAEI
          IF (ABS(PNEP(K)) .LE. 1.E-13_RKIND) PNEP(K) = 1.E-13_RKIND * SIGN(1._RKIND,PNEP(K))
        END IF
     ENDIF
     !
     !     set to nonzero values
     !
     IF (PTE(K) .EQ. 0._RKIND) PTE(K) = ZEPS08
     IF (PTI(K) .EQ. 0._RKIND) PTI(K) = ZEPS08
     IF (PNE(K) .EQ. 0._RKIND) PNE(K) = ZEPS08
  END DO
  !
  RETURN
END SUBROUTINE BSTNZPRO
