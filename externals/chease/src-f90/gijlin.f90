!*DECK C2SM08
!*CALL PROCESS
SUBROUTINE GIJLIN(KPSI,PS)
  !        ##########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM08 COMPUTE EQ'S FOR MARS IN [1], TABLE 3 AT GAUSSIAN QUADRATURE *
  !        POINTS ALONG CONSTANT POLOIDAL FLUX SURFACES.                *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !
  !ab
  !ab      abs(dpsi/ds)
  !ab
  REAL(RKIND)      ::     DFLEXPDC
  REAL(RKIND)      ::     DFLEXPDS
  REAL(RKIND)      ::     FLEXP
  REAL(RKIND)      ::     DOM2RTDS
  REAL(RKIND)      ::     OM2RT
  REAL(RKIND)      ::     ZDOMDS
  REAL(RKIND)      ::     ZOMEG
  REAL(RKIND)      ::     ZDRHODS
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZGMUNU
  REAL(RKIND)      ::     ZDJACDS
  REAL(RKIND)      ::     ZDGDSC
  REAL(RKIND)      ::     ZDGDCS
  REAL(RKIND)      ::     ZDRDSC
  REAL(RKIND)      ::     ZDRDCS
  REAL(RKIND)      ::     ZBZ
  REAL(RKIND)      ::     ZBR
  REAL(RKIND)      ::     ZB2
  REAL(RKIND)      ::     ZDCDZ
  REAL(RKIND)      ::     ZDCDR
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDSDR
  REAL(RKIND)      ::     ZDPDZ
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZGCHI2
  REAL(RKIND)      ::     ZBSCHI
  REAL(RKIND)      ::     ZGRADS
  REAL(RKIND)      ::     ZJAC
  REAL(RKIND)      ::     ZR
  INTEGER          ::     J1
  INTEGER          ::     KPSI
  REAL(RKIND)      ::     PS
  REAL(RKIND)      ::     ZDPSIS
  ZDPSIS = 2._RKIND * PS * CPSRF
  CALL GCHI(KPSI)
  !ab
  DO J1=1,NMGAUS*NT1
     !
     ZR = RRISO(J1,KPSI)
     !
     ! ZJAC = [GRAD-S; GRAD-CHI;GRAD-PHI]
     ! ZGRADS = |GRAD-S|
     ! ZBSCHI = BETA_{S,CHI}
     ! ZGCHI2 = |GRAD-CHI|**2
     !
     ZJAC   = ZDPSIS * CP(KPSI) * ZR**NER * &
          &            GPISO(J1,KPSI)**NEGP
     ZGRADS = GPISO(J1,KPSI) / ZDPSIS
     ZBSCHI = BCHISO(J1)
     ZGCHI2 = (ZBSCHI * ZGRADS)**2 + (ZR / (ZJAC * ZGRADS))**2
     !
     ! SEE DEFINITIONS OF EQL'S IN H.LUETJENS ET AL., COMPUTER PHYSICS
     ! COMMUNICATIONS 69, 287 (1992)
     !
     EQL(J1, 1) = ZJAC*(ZBSCHI*ZGRADS/ZR)**2 + 1._RKIND/(ZJAC*ZGRADS**2)
     EQL(J1, 2) = ZJAC * (ZGRADS / ZR)**2
     EQL(J1, 3) = ZR**2 / ZJAC
     EQL(J1, 4) = - ZBSCHI * ZJAC * (ZGRADS / ZR)**2
     EQL(J1, 5) = EQL(J1, 1) * ZJAC**2
     EQL(J1, 6) = EQL(J1, 2) * ZJAC**2
     EQL(J1, 7) = EQL(J1, 3) * ZJAC**2
     EQL(J1, 8) = EQL(J1, 4) * ZJAC**2
     EQL(J1, 9) = ZJAC
     EQL(J1,10) = - ZDPSIS * TTP(KPSI) / TMF(KPSI)
     EQL(J1,11) = -ZJAC * (CPPR(KPSI)+TTP(KPSI)/ZR**2)
     EQL(J1,12) = ZDPSIS
     EQL(J1,13) = ZJAC * TMF(KPSI) / ZR**2
     EQL(J1,14) = CPR(KPSI)
     EQL(J1,15) = ZDPSIS * CPPR(KPSI)
     !
     ! ZDPDR = D(PSI) / D(R) AT Z CONSTANT
     ! ZDPDZ = D(PSI) / D(Z) AT R CONSTANT
     !
     ZDPDR = DPRISO(J1,KPSI)
     ZDPDZ = DPZISO(J1,KPSI)
     !
     ! ZDSDR = D(S)/D(R) AT Z CONSTANT
     ! ZDSDZ = D(S)/D(Z) AT R CONSTANT
     ! ZDCDR = D(CHI)/D(R) AT Z CONSTANT
     ! ZDCDZ = D(CHI)/D(Z) AT R CONSTANT
     !
     ZDSDR = ZDPDR/ZDPSIS
     ZDSDZ = ZDPDZ/ZDPSIS
     ZDCDR = ZDSDR*ZBSCHI-ZDSDZ*ZR/(ZJAC*ZGRADS**2)
     ZDCDZ = ZDSDZ*ZBSCHI+ZDSDR*ZR/(ZJAC*ZGRADS**2)
     !
     ! ZB2 = B**2
     ! ZBR = B_R
     ! ZBZ = B_Z
     !
     ZB2 = (TMF(KPSI)**2+GPISO(J1,KPSI)**2) / ZR**2
     ZBR = -ZDPDZ/ZR
     ZBZ =  ZDPDR/ZR
     !ab
     !ab      EQL(*,16) IS D(CHI)/DZ
     !ab      EQL(*,17) IS DS/DZ 
     !ab      EQL(*,18) IS BZ
     !ab      EQL(*,19) IS BR
     !ab
     EQL(J1,16) = ZDCDZ
     EQL(J1,17) = ZDSDZ
     EQL(J1,18) = ZBZ
     EQL(J1,19) = ZBR
     !
     ! QUANTITIES FOR SHEARED FLOWS, INHOMOGEN DENSITY, ETC...
     !
     ! ZDRDCS = D(R)/D(CHI) AT S   CONSTANT
     ! ZDRDSC = D(R)/D(S)   AT CHI CONSTANT
     ! ZDGDCS = D(GRAD-PSI**2)/D(CHI) AT S   CONSTANT
     ! ZDGDSC = D(GRAD-PSI**2)/D(S)   AT CHI CONSTANT
     ! ZDJACDS = D(JACOBIAN)/DS AT CHI CONSTANT
     !
     ZDRDCS = - ZJAC * ZDSDZ / ZR
     ZDRDSC = ZDPSIS*DRNISO(J1,KPSI) -  ZBSCHI * ZDRDCS
     ZDGDCS = ZJAC*(ZDSDR * DGZISO(J1,KPSI) - &
          &                  ZDSDZ * DGRISO(J1,KPSI))/ZR
     ZDGDSC = ZDPSIS*DGNISO(J1,KPSI) - ZBSCHI * ZDGDCS
     !
     ZDJACDS = ZJAC*(1._RKIND/PS+ZDPSIS*CPDP(KPSI)/CP(KPSI)+ &
          &             NER*ZDRDSC/ZR+.5_RKIND*NEGP*ZDGDSC/GPISO(J1,KPSI)**2)
     !
     ! OM2RT(KPSI) = OMEGA(S)**2/(K-BOLTZMANN*TEMPERATURE(S))
     ! DOM2RTDP(KPSI) = D(OM2RT(KPSI))/D(S)
     !
     ZGMUNU = 1._RKIND
     ZRHO   = 1._RKIND
     ZDRHODS= 0._RKIND
     ZOMEG  = 0._RKIND
     ZDOMDS = 0._RKIND
     OM2RT  = 0._RKIND
     DOM2RTDS = 0._RKIND
     !
     ! FLEXP = EXP(R**2*OMEGA**2/(2.*K-BOLTZMANN*TEMPERATURE(S)))
     ! DFLEXPDS = D(FLEXP)/D(S)   AT CHI CONSTANT
     ! DFLEXPDC = D(FLEXP)/D(CHI) AT S   CONSTANT
     !
     FLEXP  = EXP(.5_RKIND*ZR**2*OM2RT)
     DFLEXPDS = FLEXP*ZR*(ZDRDSC*OM2RT+ &
          &                        .5_RKIND*ZR*DOM2RTDS)
     DFLEXPDC = FLEXP*ZR*ZDRDCS*OM2RT
     !
     EQI(J1, 1) = FLEXP * TMF(KPSI) * ZJAC / ZB2
     EQI(J1, 2) = -FLEXP*ZJAC*EQL(J1,2)*EQL(J1,12)/ZB2
     EQI(J1, 3) = -FLEXP*EQL(J1,4)*EQL(J1,12)**2/ZB2
     EQI(J1, 4) = -FLEXP*EQL(J1,4)*EQL(J1,13)*EQL(J1,12)/ZB2
     EQI(J1, 5) = FLEXP*ZJAC*(ZJAC*EQL(J1,1)- &
          &                            (EQL(J1,4)*EQL(J1,12))**2/ZB2)
     EQI(J1, 6) = FLEXP*TMF(KPSI)*ZJAC**2*EQL(J1,4)/ZB2
     EQI(J1, 7) = FLEXP*(ZJAC*ZR)**2*EQL(J1,2)/ZB2
     EQI(J1, 8) = FLEXP*ZJAC*ZB2
     EQI(J1, 9) = EQL(J1,4)*EQL(J1,12)*EQL(J1,15)/ZB2
     EQI(J1,10) = -ZJAC*(TTP(KPSI)/TMF(KPSI)+ &
          &                       TMF(KPSI)*CPPR(KPSI)/ZB2)
     EQI(J1,11) = - EQI(J1,3)
     EQI(J1,12) = - EQI(J1,4)
     EQI(J1,13) = - EQI(J1,1)
     EQI(J1,14) = - EQI(J1,2)
     EQI(J1,15) = FLEXP*ZDOMDS*TMF(KPSI)*ZJAC*EQL(J1,4)* &
          &                EQL(J1,12)/ZB2
     EQI(J1,16) = 2._RKIND*FLEXP*ZOMEG*ZJAC**2*ZDCDZ*TMF(KPSI)/ZR**2
     EQI(J1,17) = FLEXP*ZDOMDS*ZR**2*ZJAC* &
          &                EQL(J1,2)*EQL(J1,12)/ZB2
     EQI(J1,18) = 2._RKIND*FLEXP*ZOMEG*ZJAC**2*ZBZ/ZB2
     EQI(J1,19) = -FLEXP*ZDOMDS*TMF(KPSI)*ZJAC
     EQI(J1,20) = 2*FLEXP*ZOMEG*ZJAC**2*ZDSDZ
     EQI(J1,21) = FLEXP*ZJAC*EQL(J1,4)*EQL(J1,12)
     EQI(J1,22) = FLEXP*ZJAC**2*TMF(KPSI)/EQL(J1,12)
     EQI(J1,23) = -ZGMUNU*ZJAC*(ZJAC*ZDCDZ/ZR- &
          &                 ZBR*EQL(J1,12)*EQL(J1,4)/ZB2)**2
     EQI(J1,24) = ZJAC*ZGMUNU*(ZJAC*ZDCDZ/ZR- &
          &                ZBR*EQL(J1,12)*EQL(J1,4)/ZB2)* &
          &                (ZDSDZ*EQL(J1,13)*ZR/ZB2)
     EQI(J1,25) = EQI(J1,24)
     EQI(J1,26) = -ZGMUNU*ZJAC*(EQL(J1,13)*ZR*ZDSDZ/ZB2)**2
     EQI(J1,27) = ZJAC*EQL(J1,2)*EQL(J1,3)*EQL(J1,13)/ZB2
     EQI(J1,28) = ZJAC*EQL(J1,2)*EQL(J1,3)*EQL(J1,12)/ZB2
     EQI(J1,29) = EQL(J1,3)*EQL(J1,4)*EQL(J1,13)**2/ZB2
     EQI(J1,30) = EQL(J1,3)*EQL(J1,4)*EQL(J1,12)*EQL(J1,13)/ZB2
     EQI(J1,31) = ZJAC*EQL(J1,3)*EQL(J1,4)*EQL(J1,13)/ZB2
     EQI(J1,32) = ZJAC*EQL(J1,1)-(EQL(J1,4)*EQL(J1,12))**2/ZB2
     !
     EQ3(J1, 1) = ZRHO
     EQ3(J1, 2) = ZDRHODS
     EQ3(J1, 3) = ZOMEG
     EQ3(J1, 4) = ZDOMDS
     EQ3(J1, 5) = FLEXP
     EQ3(J1, 6) = ZR*ZOMEG**2*ZJAC*(EQL(J1,5)*ZDSDR+ &
          &                EQL(J1,8)*ZDCDR-ZJAC*ZBR*EQL(J1,12)*EQL(J1,4)/ZB2)
     EQ3(J1, 7) = ZR*ZOMEG**2*TMF(KPSI)*(EQL(J1,6)*ZDCDR+ &
          &                                       EQL(J1,8)*ZDSDR)/ZB2
     EQ3(J1, 8) = ZR*ZOMEG**2*ZJAC*ZBR
     EQ3(J1, 9) = -EQL(J1,15)
     EQ3(J1,10) = ZJAC*FLEXP
     EQ3(J1,11) = FLEXP*EQL(J1,12)
     EQ3(J1,12) = FLEXP*EQL(J1,13)
     EQ3(J1,13) = ZJAC*DFLEXPDS
     EQ3(J1,14) = DFLEXPDC*EQL(J1,12)**2*EQL(J1,4)/ZB2
     EQ3(J1,15) = ZJAC*DFLEXPDC*TMF(KPSI)/ZB2
     EQ3(J1,16) = DFLEXPDC*EQL(J1,12)
     EQ3(J1,17) = FLEXP*ZDJACDS
     !
  END DO
  !
  RETURN
END SUBROUTINE GIJLIN
