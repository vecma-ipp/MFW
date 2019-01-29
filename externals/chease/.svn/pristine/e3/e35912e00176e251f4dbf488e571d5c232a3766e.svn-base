!*DECK C2SM03
!*CALL PROCESS
SUBROUTINE CHIPSI(NPMAX,KP)
  !        #########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM03 INTERPOLATE THETA(S,CHI) AND SIGMA(S,CHI) WITH CUBIC         *
  !        SPLINES, WHERE (S,CHI) IS THE ERATO STABILITY MESH           *
  !                                                                     *
  !**********************************************************************
  !
  ! chin(ipsi,1:nt+2): chi values calculated in surface.f90 on each smiso(ipsi) surface and nt+1 intervals (i.e. chin(nt+2)=chin(1))
  ! tetmap(ipsi,1:nt+2): theta values at each (smiso,chin(ipsi,:)) points with respect to sigma,theta equilibrium mesh
  ! sigmap(ipsi,1:nt+2): sigma values at each (smiso,chin(ipsi,:)) points with respect to sigma,theta equilibrium mesh
  ! therefore we have:
  ! sigmap(niso1eff)=1. is on smiso(niso1eff)=1. on plasma edge
  ! (tetmap(niso1eff,:),sigmap(niso1eff,:)) are on the plasma surface, thus calculating the rho values with:
  ! call bound(nt+2,tetmap(niso1eff,:),rhomap) gives the rho_bound values at each tetmap
  !
  ! In this routine, the values on the CHIM mesh are calculated in order to be able to evaluate values on mapping mesh (psi,chim)
  ! TETCHI(nchi,ipsi): theta values at each (smiso(1:niso1eff),chim(1:nchi)) points w.r. to sigma,theta equilibrium mesh
  ! SIGCHI(nchi,ipsi): sigma values at each (smiso(1:niso1eff),chim(1:nchi)) points w.r. to sigma,theta equilibrium mesh
  ! In this way, we have with:
  ! call BOUND(nchi,TETCHI(nchi,npmax),RHO_bound) gives the values of rho_plasma_bound at each tetchi
  ! Then we have for example: R(chim,psi)=sigchi(ichi,ipsi)*RHO_bound_fit(ichi)*cos(tetchi(ichi,ipsi))
  ! by interpolating ala RHO_bound_fit = interpos(TETCHI(nchi,npmax),RHO_bound,tetchi(ichi,ipsi),-1,2*pi)
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZZM
  REAL(RKIND)      ::     ZRM
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  INTEGER          ::     J15
  INTEGER          ::     ICHIO
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  INTEGER          ::     ICHIM
  INTEGER          ::     J4
  INTEGER          ::     IT1
  INTEGER          ::     NPMAX
  INTEGER          ::     IT0
  INTEGER          ::     JT
  INTEGER          ::     JG
  INTEGER          ::     IC1
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZD2TET
  REAL(RKIND)      ::     ZD2SIG
  REAL(RKIND)      ::     ZD2BCO
  REAL(RKIND)      ::     ZD2BCN
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZD2CHO
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZTET
  INTEGER          ::     KP, KP1
  DIMENSION &
       &   IC(NPCHI),    IC1(NPCHI),   IT0(NPCHI),   IT1(NPCHI), &
       &   ZD2BCN(NTP2), ZD2BCO(NTP2), ZD2CHO(NTP2), &
       &   ZD2SIG(NTP2), ZD2TET(NTP2), ZTET(NTP2), &
       &   ZA1(NTP2),    ZB1(NTP2),    ZC1(NTP2)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  ! now computes on effectively surfaces (2:NISO1EFF1) so save result on KP+1 so that can insert on-axis values later (same as for eqchease_out in surface)
  KP1 = KP + 1
  ! 
  CHI(1) = CHI(1) + 2._RKIND * CPI
  !
  CALL DCOPY(NT2,TETMAP(1,KP),1,ZTET,1)
  !
  DO J1=2,NT2
     !
     IF (ZTET(J1) .LT. ZTET(J1-1)) THEN
        !
        ZTET(J1) = ZTET(J1) + 2._RKIND * CPI * (1._RKIND + &
             &                 INT(.5_RKIND * ABS(ZTET(J1) - ZTET(J1-1)) / CPI))       
        !
     ENDIF
     !
  END DO
  !
  CALL SPLCYP(CHIN(1,KP),CHIO(1,KP),NT1,RC2PI,RC2PI, &
       &              ZD2CHO,ZA1,ZB1,ZC1)
  CALL SPLCY(CHIN(1,KP),BCHIN(1,KP),NT1,RC2PI, &
       &              ZD2BCN,ZA1,ZB1,ZC1)
  CALL SPLCY(CHIN(1,KP),BCHIO(1,KP),NT1,RC2PI, &
       &              ZD2BCO,ZA1,ZB1,ZC1)
  CALL SPLCY(CHIN(1,KP),SIGMAP(1,KP),NT1,RC2PI, &
       &              ZD2SIG,ZA1,ZB1,ZC1)
  CALL SPLCYP(CHIN(1,KP),ZTET,NT1,RC2PI,RC2PI, &
       &               ZD2TET,ZA1,ZB1,ZC1)
  !
  ZD2CHO(NT2) = ZD2CHO(1)
  ZD2BCN(NT2) = ZD2BCN(1) 
  ZD2BCO(NT2) = ZD2BCO(1) 
  ZD2SIG(NT2) = ZD2SIG(1) 
  ZD2TET(NT2) = ZD2TET(1) 
  !
  CALL RESETI(IC,NCHI,1)
  CALL RESETI(IC1,NCHI,1)
  DO JG=1,NCHI
     DO JT = 1,NT2
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (CHIN(JT,KP).GE.CHIM(JG)) IC(JG)  = 0
        ENDIF
        IF (KP.EQ.NPMAX) THEN
           IF (IC1(JG).EQ.1) THEN
              IT1(JG) = JT-1
              IF (CHIN(JT,KP).GE.CHI(JG)) IC1(JG) = 0
           ENDIF
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,NCHI
     !
     ICHIM = IT0(J4)
     !
     IF (ICHIM .LT. 1)   ICHIM = 1
     IF (ICHIM .GT. NT1) ICHIM = NT1
     !
     ZH = CHIN(ICHIM+1,KP) - CHIN(ICHIM,KP)
     ZA = (CHIN(ICHIM+1,KP) - CHIM(J4)) / ZH
     ZB = (CHIM(J4) - CHIN(ICHIM,KP)) / ZH
     ZC = (ZA + 1) * (ZA - 1) * ZH * &
          &        (CHIN(ICHIM+1,KP) - CHIM(J4)) / 6._RKIND
     ZD = (ZB + 1) * (ZB - 1) * ZH * &
          &        (CHIM(J4) - CHIN(ICHIM,KP)) / 6._RKIND
     ! 
     EQ13(J4,KP1)  = ZA*BCHIN(ICHIM,KP) + ZB*BCHIN(ICHIM+1,KP) + &
          &                   ZC*ZD2BCN(ICHIM)   + ZD*ZD2BCN(ICHIM+1)
     EQ22(J4,KP1)  = ZA*BCHIO(ICHIM,KP) + ZB*BCHIO(ICHIM+1,KP) + &
          &                   ZC*ZD2BCO(ICHIM)   + ZD*ZD2BCO(ICHIM+1)
     EQ24(J4,KP1)  = ZA*CHIO(ICHIM,KP)  + ZB*CHIO(ICHIM+1,KP) + &
          &                   ZC*ZD2CHO(ICHIM)   + ZD*ZD2CHO(ICHIM+1)
     TETCHI(J4,KP) = ZA*ZTET(ICHIM)     + ZB*ZTET(ICHIM+1) + &
          &                   ZC*ZD2TET(ICHIM)   + ZD*ZD2TET(ICHIM+1)
     !
     IF (TETCHI(J4,KP) .LT. CT(1)) &
          &                   TETCHI(J4,KP) = TETCHI(J4,KP) + 2._RKIND*CPI
     IF (TETCHI(J4,KP) .GT. CT(NT1)) &
          &                   TETCHI(J4,KP) = TETCHI(J4,KP) - 2._RKIND*CPI
     !
     IF (KP .EQ. NPMAX) THEN
        !
        SIGCHI(J4,KP) = 1._RKIND
        !
     ELSE
        !
        SIGCHI(J4,KP) = ZA*SIGMAP(ICHIM,KP) + ZB*SIGMAP(ICHIM+1,KP) + &
             &                     ZC*ZD2SIG(ICHIM)    + ZD*ZD2SIG(ICHIM+1)
        !
     ENDIF
     !
     IF (KP .EQ. NPMAX) THEN
        !
        ICHIO = IT1(J4)
        !
        IF (ICHIO .LT. 1)   ICHIO = 1
        IF (ICHIO .GT. NT1) ICHIO = NT1
        !
        ZH = CHIN(ICHIO+1,KP) - CHIN(ICHIO,KP)
        ZA = (CHIN(ICHIO+1,KP) - CHI(J4)) / ZH
        ZB = (CHI(J4) - CHIN(ICHIO,KP)) / ZH
        ZC = (ZA + 1) * (ZA - 1) * ZH * &
             &           (CHIN(ICHIO+1,KP) - CHI(J4)) / 6._RKIND
        ZD = (ZB + 1) * (ZB - 1) * ZH * &
             &           (CHI(J4) - CHIN(ICHIO,KP)) / 6._RKIND
        ! 
        TETVAC(J4) = ZA*ZTET(ICHIO)    + ZB*ZTET(ICHIO+1) + &
             &                   ZC*ZD2TET(ICHIO)  + ZD*ZD2TET(ICHIO+1)
        CHIOLD(J4) = ZA*CHIO(ICHIO,KP) + ZB*CHIO(ICHIO+1,KP) + &
             &                   ZC*ZD2CHO(ICHIO)  + ZD*ZD2CHO(ICHIO+1)
        !     
     ENDIF
     !
  END DO
  !
  CHI(1)        = CHI(1) - 2._RKIND* CPI
  CHIOLD(NCHI1) = CHIOLD(1)
  CHIOLD(1)     = CHIOLD(1) - 2._RKIND * CPI
  !
!!$  write(33,*) KP, npmax, nchi
!!$  write(33,*) (tetchi(j1,kp),j1=1,nchi)
!!$  write(33,*) (sigchi(j1,kp),j1=1,nchi)
!!$  if (KP .EQ. npmax) then
!!$    CALL BOUND(nchi,tetchi(:,npmax),RHOVACM)
!!$    write(33,*) (RHOVACM(j1),j1=1,nchi)
!!$  endif
  !
  IF (NIDEAL .EQ. 2 .AND. KP .EQ. NPMAX) THEN
     !
     ! PLASMA BOUNDARY QUANTITIES REQUIRED BY LION ONLY
     !
     TETVAC(NCHI1) = TETVAC(1)
     !
     CALL DCOPY(NCHI,TETCHI(1,NPMAX),1,TETVACM,1)
     !
     TETVACM(NCHI1) = TETVACM(1)
     !
     CALL BOUND(NCHI1,TETVAC ,RHOVAC)
     CALL BOUND(NCHI1,TETVACM,RHOVACM)
     !
     DO J15=1,NCHI1
        !
        ZR  = RHOVAC(J15) * COS(TETVAC(J15)) + R0
        ZZ  = RHOVAC(J15) * SIN(TETVAC(J15)) + RZ0
        ZRM = RHOVACM(J15) * COS(TETVACM(J15)) + R0
        ZZM = RHOVACM(J15) * SIN(TETVACM(J15)) + RZ0
        !
        RHOVAC(J15)  = SQRT((ZR - RMAG)**2 + (ZZ - RZMAG)**2)
        TETVAC(J15)  = ATAN2(ZZ - RZMAG,ZR - RMAG)
        RHOVACM(J15) = SQRT((ZRM - RMAG)**2 + (ZZM - RZMAG)**2)
        TETVACM(J15) = ATAN2(ZZM - RZMAG,ZRM - RMAG)
        !
        IF (J15 .NE. 1 .AND. TETVAC(J15) .LT. 0._RKIND) THEN
           !
           TETVAC(J15) = TETVAC(J15) + 2._RKIND * CPI
           !
        ENDIF
        !
        IF (J15 .NE. 1 .AND. TETVACM(J15) .LT. 0._RKIND) THEN
           !
           TETVACM(J15) = TETVACM(J15) + 2._RKIND * CPI
           !
        ENDIF
        !
     END DO
     !
     TETVACM(NCHI1) = TETVACM(1) + 2._RKIND * CPI
     !
     CALL VLION
     !
  ENDIF
  !
  RETURN
END SUBROUTINE CHIPSI
