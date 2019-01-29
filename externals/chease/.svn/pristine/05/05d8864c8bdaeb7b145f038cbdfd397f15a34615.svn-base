!*DECK C3SA02
!*CALL PROCESS
SUBROUTINE PRIQQU
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SA02  PRINT EQUILIBRIUM QUANTITIES AT Q-VALUES DEFINED BY QPLACS  *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZQR
  INTEGER          ::     IR
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZSTEP
  REAL(RKIND)      ::     ZRBAL
  REAL(RKIND)      ::     ZSBAL
  REAL(RKIND)      ::     ZRRINT
  REAL(RKIND)      ::     ZSRINT
  REAL(RKIND)      ::     ZRMERC
  REAL(RKIND)      ::     ZSMERC
  INTEGER          ::     J3
  INTEGER          ::     IBAL
  INTEGER          ::     IRINT
  INTEGER          ::     IMERC
  REAL(RKIND)      ::     ZDEDR
  REAL(RKIND)      ::     ZELL
  REAL(RKIND)      ::     ZHGG
  REAL(RKIND)      ::     ZRINT
  REAL(RKIND)      ::     ZMERC
  REAL(RKIND)      ::     ZSY
  REAL(RKIND)      ::     ZDI
  REAL(RKIND)      ::     ZVOL
  REAL(RKIND)      ::     ZSHR
  REAL(RKIND)      ::     ZQS
  REAL(RKIND)      ::     ZDPRIM
  REAL(RKIND)      ::     ZLI
  REAL(RKIND)      ::     ZBETQ
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     IS
  INTEGER          ::     J1
  DIMENSION &
       &     ZBETQ(11),   ZDEDR(11),   ZDI(11),     ZDPRIM(11),  ZELL(11), &
       &     ZHGG(11),    ZLI(11),     ZMERC(11),   ZQR(11), &
       &     ZQS(11),     ZRINT(11),   ZRBAL(11),   ZRMERC(11), &
       &     ZRRINT(11),  ZS(11),      ZSHR(11),    ZVOL(11), &
       &     ZSBAL(11),   ZSMERC(11),  ZSRINT(11),  ZSY(11)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (NMESHA .NE. 2) GOTO 2
  !
  WRITE(6,901)
  !
  DO J1=1,NPOIDA
     !
     IS  = ISRCHFGE(NISO1EFF1,SMISOP1,1,APLACE(J1))
     !
     IF (IS .LT. 1)    IS = 1
     IF (IS .GT. NISO1EFF) IS = NISO1EFF
     !
     ZBETQ(J1) = BETAB(IS) + (BETAB(IS+1) - BETAB(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZLI(J1)   = eqchease_out(1)%profiles_1d%li(IS) &
          & + (eqchease_out(1)%profiles_1d%li(IS+1) - eqchease_out(1)%profiles_1d%li(IS)) &
          & * (APLACE(J1) - SMISO(IS)) / (SMISO(IS+1) - SMISO(IS))
     ZDPRIM(J1)= DPRIME(IS) + (DPRIME(IS+1) - DPRIME(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZQS(J1)   = eqchease_out(1)%profiles_1d%q(IS) + (eqchease_out(1)%profiles_1d%q(IS+1) - eqchease_out(1)%profiles_1d%q(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZSHR(J1)  = CDRQ(IS) + (CDRQ(IS+1) - CDRQ(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZVOL(J1)  = eqchease_out(1)%profiles_1d%rho_vol(IS) &
          & + (eqchease_out(1)%profiles_1d%rho_vol(IS+1) - eqchease_out(1)%profiles_1d%rho_vol(IS)) &
          & * (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZDI(J1)   = RDI(IS) + (RDI(IS+1) - RDI(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZSY(J1)   = RSY(IS) + (RSY(IS+1) - RSY(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZMERC(J1) = SMERCI(IS) + (SMERCI(IS+1) - SMERCI(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZRINT(J1) = SMERCR(IS) + (SMERCR(IS+1) - SMERCR(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZHGG(J1)  = HMERCR(IS) + (HMERCR(IS+1) - HMERCR(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZELL(J1)  = RELL(IS) + (RELL(IS+1) - RELL(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZDEDR(J1) = RDEDR(IS) + (RDEDR(IS+1) - RDEDR(IS)) * &
          &               (APLACE(J1) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     !
  END DO
  !
  CALL RARRAY(' RATIONAL SURFACE Q',ZQS,NPOIDA)
  CALL RARRAY(' SMISOP1(Q)',APLACE,NPOIDA)
  CALL RARRAY(' POLOIDAL BETA(Q)',ZBETQ,NPOIDA)
  CALL RARRAY(' SHEAR(Q)',ZSHR,NPOIDA)
  CALL RARRAY(' SQRT(V/V-TOTAL)(Q)',ZVOL,NPOIDA)
  CALL RARRAY(' ELLIPTICITY',ZELL,NPOIDA)
  CALL RARRAY(' D(E) / DR',ZDEDR,NPOIDA)
  CALL RARRAY(' INDUCTANCE',ZLI,NPOIDA)
  CALL RARRAY(' DELTA PRIME (LOWEST A/R0 ORDER)',ZDPRIM,NPOIDA)
  CALL RARRAY(' MERCIER BY A/R0 EXPANSION WITH E',ZDI,NPOIDA)
  CALL RARRAY(' MERCIER BY SHAFRANOV YOURCHENKO',ZSY,NPOIDA)
  !
  IF (NBAL .EQ. 1) THEN
     !
     CALL RARRAY(' MERCIER(Q)',ZMERC,NPOIDA)
     CALL RARRAY(' RESISTIVE INTERCHANGE(Q)',ZRINT,NPOIDA)
     CALL RARRAY(' H OF GGG(Q)',ZHGG,NPOIDA)
     !
  ENDIF
  !
2 CONTINUE 
  !
  IF (NBAL .EQ. 1) THEN
     !
     WRITE(6,902)
     !
     IMERC = 0
     IRINT = 0
     IBAL  = 0
     !
     DO J3=1,NPSI
        !
        IF (SMERCI(J3) * SMERCI(J3+1) .LT. 0._RKIND) THEN
           !
           IMERC         = IMERC + 1
           ZSMERC(IMERC) = SMISOP1(J3) + SMERCI(J3) * &
                &                         (SMISOP1(J3) - SMISOP1(J3+1)) / &
                &                         (SMERCI(J3+1) - SMERCI(J3))
           ZRMERC(IMERC) = eqchease_out(1)%profiles_1d%rho_vol(J3) &
                & + SMERCI(J3) / (SMERCI(J3+1) - SMERCI(J3)) &
                & * (eqchease_out(1)%profiles_1d%rho_vol(J3) - eqchease_out(1)%profiles_1d%rho_vol(J3+1))
           !
        ENDIF
        !
        IF (SMERCR(J3) * SMERCR(J3+1) .LT. 0._RKIND) THEN
           !
           IRINT         = IRINT + 1
           ZSRINT(IRINT) = SMISOP1(J3) + SMERCR(J3) * &
                &                         (SMISOP1(J3) - SMISOP1(J3+1)) / &
                &                         (SMERCR(J3+1) - SMERCR(J3))
           ZRRINT(IRINT) = eqchease_out(1)%profiles_1d%rho_vol(J3) + SMERCR(J3) * &
                & (eqchease_out(1)%profiles_1d%rho_vol(J3) - eqchease_out(1)%profiles_1d%rho_vol(J3+1)) / &
                & (SMERCR(J3+1) - SMERCR(J3))
           !
        ENDIF
        !
        IF (NCBAL(J3) + NCBAL(J3+1) .EQ. 1) THEN
           !
           IBAL        = IBAL + 1
           ZSBAL(IBAL) = .5_RKIND * (SMISOP1(J3) + SMISOP1(J3+1))
           ZRBAL(IBAL) = .5_RKIND * (eqchease_out(1)%profiles_1d%rho_vol(J3) + eqchease_out(1)%profiles_1d%rho_vol(J3+1))
           !
        ENDIF
        !
     END DO
     !
     IF (IMERC .GT. 0) THEN
        !
        CALL RARRAY('S VALUES FOR WHICH -DI = 0',ZSMERC,IMERC)
        CALL RARRAY('R VALUES FOR WHICH -DI = 0',ZRMERC,IMERC)
        !
     ENDIF
     !
     IF (IRINT .GT. 0) THEN
        !
        CALL RARRAY('S VALUES FOR WHICH -DR = 0',ZSRINT,IRINT)
        CALL RARRAY('R VALUES FOR WHICH -DR = 0',ZRRINT,IRINT)
        !
     ENDIF
     !
     IF (IBAL .GT. 0) THEN
        !
        CALL RARRAY('S VALUES WHERE BAL. CRIT. = 0',ZSBAL,IBAL)
        CALL RARRAY('R VALUES WHERE BAL. CRIT. = 0',ZRBAL,IBAL)
        !
     ENDIF
  ENDIF
  !
  ZSTEP = REAL(1,RKIND) / REAL(10,RKIND)
  !
  DO J4=1,11
     !
     ZS(J4) = (J4 - 1._RKIND) * ZSTEP
     !
     IS = ISRCHFGE(NISO1EFF1,SMISOP1,1,ZS(J4))
     IR = ISRCHFGE(NISO1EFF1,eqchease_out(1)%profiles_1d%rho_vol,1,ZS(J4))
     !
     IF (IS .LT. 1)    IS = 1
     IF (IS .GT. NISO1EFF) IS = NISO1EFF
     IF (IR .LT. 1)    IR = 1
     IF (IR .GT. NISO1EFF) IR = NISO1EFF
     !
     ZQS(J4) = eqchease_out(1)%profiles_1d%q(IS) &
          & + (eqchease_out(1)%profiles_1d%q(IS+1) - eqchease_out(1)%profiles_1d%q(IS)) &
          & * (ZS(J4) - SMISOP1(IS)) / (SMISOP1(IS+1) - SMISOP1(IS))
     ZQR(J4) = eqchease_out(1)%profiles_1d%q(IR) &
          & + (eqchease_out(1)%profiles_1d%q(IR+1) - eqchease_out(1)%profiles_1d%q(IR)) &
          & * (ZS(J4) - eqchease_out(1)%profiles_1d%rho_vol(IR)) / &
          & (eqchease_out(1)%profiles_1d%rho_vol(IR+1) - eqchease_out(1)%profiles_1d%rho_vol(IR))
     ZELL(J4) = RELL(IR) + (RELL(IR+1) - RELL(IR)) &
          & * (ZS(J4) - eqchease_out(1)%profiles_1d%rho_vol(IR)) / &
          & (eqchease_out(1)%profiles_1d%rho_vol(IR+1) - eqchease_out(1)%profiles_1d%rho_vol(IR))
     ZDEDR(J4) = RDEDR(IR) + (RDEDR(IR+1) - RDEDR(IR)) &
          & * (ZS(J4) - eqchease_out(1)%profiles_1d%rho_vol(IR)) / &
          & (eqchease_out(1)%profiles_1d%rho_vol(IR+1) - eqchease_out(1)%profiles_1d%rho_vol(IR))
     !
  END DO
  !
  WRITE(6,903)
  !
  CALL RARRAY('S-VALUES',ZS,11)
  CALL RARRAY('Q-VALUES',ZQS,11)    
  !
  WRITE(6,904)
  !
  CALL RARRAY('R-VALUES',ZS,11)
  CALL RARRAY('Q-VALUES',ZQR,11)    
  CALL RARRAY('ELLIPTICITY',ZELL,11)    
  CALL RARRAY('D(E) / DR',ZDEDR,11)    
  !
901 FORMAT(///,1X,'******************************', &
       &           //,1X,'OUTPUT ON RATIONAL Q SURFACES ', &
       &           //,1X,'******************************')
902 FORMAT(///,1X,'****************************************', &
       &           //,1X,'ZEROES OF LOCAL AND BALLOONING CRITERIA ', &
       &           //,1X,'****************************************')
903 FORMAT(///,1X,'****************************', &
       &           //,1X,'Q VALUES FOR GIVEN S-VALUES ', &
       &           //,1X,'****************************')
904 FORMAT(///,1X,'****************************', &
       &           //,1X,'Q VALUES FOR GIVEN R-VALUES ', &
       &           //,1X,'****************************')
  !
  RETURN
END SUBROUTINE PRIQQU
