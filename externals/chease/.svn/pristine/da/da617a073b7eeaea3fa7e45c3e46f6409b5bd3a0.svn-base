SUBROUTINE GLOADD(PSISMP1,PJBS2,PBET,KPSISMP1,KOPT)
  !        ##################################################
  !
  !                                        AUTHORS:
  !                                        O. SAUTER,  CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !   CALLED AT END OF GLOQUA. ENABLES ONE TO ADD DIAGNOSTIC CALCULATIONS
  !   WHEN LOCAL VALUES ARE NEEDED, add to cheaseout_add..., but try to compute it from here
  !   and other values. SURFADD has been suppressed
  !
  !     KOPT = 1: DIAGNOSTICS RELATED TO EQUILIBRIUM, PRETOR => RBDIAG
  !     .    = 2: DIAGNOSTICS RELATED TO SAWTEETH CRITERIA => DIAGARS
  !     .         AND DIAG ON RATIONAL Q SURFACES
  !     .    = 3: DIAGNOSTICS RELATED TO NEOCLASSICAL TEARING MODE STUFF
  !     .         => GLOBNEO, QVALNEO, VALSNEO, ETC. IN COMNEO
  !     .    = 4: DIAG. FOR MERCIER WITH PALPHA, THERMAL IONS. READ FROM FILE
  !
  !     FOR EACH CALL, THE FLAG NFLGADDIA(KOPT) IS SET TO 1 OR RELATED VALUE
  !     WHICH IS THEN RECOGNIZED IN ROUTINE OUTPUT FOR APPROPRIATE OUTPUT
  !     CALLS TO OUTGLOAD(NFLGADDIA). THUS CHANGE ROUTINES SURFADD, OUTGLOAD ACCORDINGLY
  !     WHEN MODIFYING THIS ROUTINE. (SIMILAR WITH SURFADD AND OUSURFLG(NOT YET))
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  USE interpos_module
!!$         USE interpos_module
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZDPADPS
  REAL(RKIND)      ::     ZKAPZSM
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZSQREPS
  INTEGER          ::     JEFF
  INTEGER          ::     JP1
  REAL(RKIND)      ::     ZRMIN
  REAL(RKIND)      ::     Z1OBZSM
  REAL(RKIND)      ::     ZFACP
  INTEGER          ::     ISTART
  INTEGER          ::     ISHFTPAL
  INTEGER          ::     ISHFTPI
  INTEGER          ::     II
  INTEGER          ::     IDIANEW
  INTEGER          ::     IDIAQI
  REAL(RKIND)      ::     ZDATPALF
  INTEGER          ::     INBPALF
  REAL(RKIND)      ::     ZDATPI
  INTEGER          ::     INBPI
  INTEGER          ::     INFO
  REAL(RKIND)      ::     ZDATPSI
  INTEGER          ::     INBPSI
  REAL(RKIND)      ::     ZGMX
  REAL(RKIND)      ::     ZGMSTA
  REAL(RKIND)      ::     ZGM
  REAL(RKIND)      ::     ZINORM
  REAL(RKIND)      ::     ZEPSRS
  REAL(RKIND)      ::     ZDPSICSM
  REAL(RKIND)      ::     ZPTILDE
  REAL(RKIND)      ::     ZDPSI
  REAL(RKIND)      ::     ZRJS
  REAL(RKIND)      ::     ZRJSP1
  REAL(RKIND)      ::     ZPTILDPR
  REAL(RKIND)      ::     ZWMAGJP1
  REAL(RKIND)      ::     ZWMAGJS
  REAL(RKIND)      ::     ZWMAGJM1
  REAL(RKIND)      ::     ZDPSIJS1
  REAL(RKIND)      ::     ZDPSIJS
  INTEGER          ::     JSM1
  REAL(RKIND)      ::     ZBTQ
  REAL(RKIND)      ::     ZAGEOQ
  REAL(RKIND)      ::     ZRGEOQ
  INTEGER          ::     I
  INTEGER          ::     JS
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZCSIQ
  REAL(RKIND)      ::     ZCSJP1
  REAL(RKIND)      ::     ZCSJ
  REAL(RKIND)      ::     ZQRAT
  INTEGER          ::     IQ
  REAL(RKIND)      ::     ZQSJP1
  REAL(RKIND)      ::     ZQSJ
  INTEGER          ::     J11
  INTEGER          ::     JJ
  INTEGER          ::     IQSMAX
  INTEGER          ::     IQSMIN
  INTEGER          ::     ISMAX
  REAL(RKIND)      ::     ZQSMAX
  INTEGER          ::     ISMIN
  REAL(RKIND)      ::     ZQSMIN
  REAL(RKIND)      ::     ZDPSIDRO
  INTEGER          ::     JSP1
  INTEGER          ::     IJ4P1
  REAL(RKIND)      ::     ZDPSIDROA
  REAL(RKIND)      ::     ZDPPSI
  REAL(RKIND)      ::     ZBT
  REAL(RKIND)      ::     ZRLFS
  REAL(RKIND)      ::     ZTMFCS
  REAL(RKIND)      ::     PBET
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZAKAPEDG
  INTEGER          ::     IDIAGLAS
  REAL(RKIND)      ::     ZDUMMY
  REAL(RKIND)      ::     PJBS2
  REAL(RKIND)      ::     PSISMP1
  REAL(RKIND)      ::     ZSM
  INTEGER          ::     J
  INTEGER          ::     KOPT
  REAL(RKIND)      ::     ZKAPEDGE
  REAL(RKIND)      ::     ZAEDGE
  REAL(RKIND)      ::     ZRMJ
  REAL(RKIND)      ::     ZEPS08
  REAL(RKIND)      ::     ZDA
  REAL(RKIND)      :: FIT0CUB, XX(1:4), YY(1:4), XXX
  INTEGER          ::     KPSISMP1
  DIMENSION &
       &   PBET(KPSISMP1),   PJBS2(KPSISMP1), &
       &   PSISMP1(KPSISMP1), ZSM(KPSISMP1), ZDUMMY(KPSISMP1,2) &
       &  ,ZDATPSI(NPBPS), ZDATPI(NPBPS), ZDATPALF(NPBPS)
  !
  FIT0CUB(XXX) = FCCCC0(YY(1),YY(2),YY(3),YY(4),XX(1),XX(2),XX(3),XX(4),XXX)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !     0. INITIALIZE SOME LOCAL DATA AS IN GLOQUA
  !
  ZEPS08 = 1.0E-08_RKIND
  ZRMJ = EQCHEASE_OUT_ADD_1D(KPSISMP1,IIRGEO)
  ZAEDGE = EQCHEASE_OUT_ADD_1D(KPSISMP1,iiamin)
  ZKAPEDGE = eqchease_out(1)%profiles_1d%elongation(KPSISMP1)
  ZSM(1:KPSISMP1) = sqrt(PSISMP1(1:KPSISMP1)/CPSRF)
  !
  GO TO (100, 200, 300, 400, 500) KOPT
  !
  !.......................................................................
  !
  !     1. EQUILIBRIUM DIAGNOSTICS RELATED TO COMPARISON WITH PRETOR AND OTHERS
  !     .  RBDIAG ARRAY:    => DEFINES NFLGADDIA(1) = NB OF RBDIAG COLUMNS
  !     RBDIAG(.,1) = RHO_PRETOR
  !     RBDIAG(.,2) = BPO_PRETOR
  !     RBDIAG(.,3) = DVOL/DRHO_TOR
  !     RBDIAG(.,4) = SHEAR_RHOTOR
  !
100 CONTINUE
  !
  RBDIAGTX(1) = 'B1: RHO_PRETOR=r/a'
  RBDIAGTX(2) = 'B2: BPO_PRETOR=1/R0 dpsi/d(r/a) (1/2pi)'
  RBDIAGTX(3) = 'B3: DVOL/DRHO_TOR'
  RBDIAGTX(4) = 'B4: shear(with rhotor)'
  !     SET FLAG TO NUMBER OF RBDIAG DEFINED
  NFLGADDIA(1) = 4
  !
  RBDIAG(1:KPSISMP1,1) = EQCHEASE_OUT_ADD_1D(1:KPSISMP1,iiamin) / ZAEDGE
  call interpos(RBDIAG(1:KPSISMP1,1),eqchease_out(1)%profiles_1d%psi(1:KPSISMP1),KPSISMP1,YOUTP=RBDIAG(1:KPSISMP1,2))
  RBDIAG(1:KPSISMP1,2) = RBDIAG(1:KPSISMP1,2) / ZRMJ
  ! now vprime is dV/dpsi since V09a
  RBDIAG(1:KPSISMP1,3) = eqchease_out(1)%profiles_1d%vprime(1:KPSISMP1) * eqchease_out(1)%profiles_1d%dpsidrho_tor(1:KPSISMP1)
  call interpos(eqchease_out(1)%profiles_1d%rho_tor(1:KPSISMP1),eqchease_out(1)%profiles_1d%q(1:KPSISMP1),KPSISMP1, &
    & YOUTP=RBDIAG(1:KPSISMP1,4))
  RBDIAG(1:KPSISMP1,4) = eqchease_out(1)%profiles_1d%rho_tor(1:KPSISMP1) / eqchease_out(1)%profiles_1d%q(1:KPSISMP1) &
       & * RBDIAG(1:KPSISMP1,4)
  !
  !
  GO TO 999
  !.......................................................................
  !
  !     2. DIAGNOSTIC ARRAYS AND VALUES ON RATIONAL Q SURFACES.
  !     DIAGARS, title given by DIAGARTX(J), LAST ARRAYS ARE VALUES AT Q SURFACES
  !     IDIAGLAS = NDIAGLAS/100 ; JQVALUES = NDIAGLAS - IDIAGLAS*100
  !
  !     PROFILES FOR DIAGARS(I,J), I=1,NPSI1, J=1,IDIAGLAS DEFINED IN 2.1
  !
  !     SPECIFIC VALUES ON Q VALUES JQ=1,JQVALUES IN DIAGARS(II,IDIAGLAS+JQ), II=1,NDIAPERQ
  !     DEFINED IN 2.2
  !
  !     => DEFINES NFLGADDIA(2) = IDIAGLAS (NOTE, DIAGARS ARE EXTENDED AT 400)
  !
200 CONTINUE
  !
  !     2.1 PROFILE VALUES IN DIAGARS(.,J), J=1,IDIAGLAS
  !
  DIAGARTX(1) = 'D1: CONVF'
  DIAGARTX(2) = 'D2: BETABUS_GA=(int pdV - p(rho) V)/V/(Bpavga**2/2mu0)'
  DIAGARTX(3) = 'D3: BETAPLOCALGA=p/(Bpavga**2/2mu0)'
  DIAGARTX(4) = 'D4: L_I_GA'
  DIAGARTX(5)=  'D5: RBAR=RHO_Vnorm*a*kappa**0.5'
  DIAGARTX(6)=  'D6: rhotor*BT/Rgeom/BPO_hinton*2pi'
  DIAGARTX(7)=  'D7: IBS2 => jBStild=dIBS2/dpsi / dA/dpsi'
  IDIAGLAS = 7
  NDIAGLAS = 100*IDIAGLAS
  !     SET FLAG TO NUMBER OF PROFILES
  NFLGADDIA(2) = IDIAGLAS
  !
  !     1: CONVF = (int dlp)**2 / (2 V/Rgeom)
  DIAGARS(2:KPSISMP1,1) = rleng(1:KPSISMP1-1)**2 / &
       & (2._rkind*eqchease_out(1)%profiles_1d%volume(2:KPSISMP1) / EQCHEASE_OUT_ADD_1D(2:KPSISMP1,IIRGEO))
  XX(1:4)=ZSM(2:5)
  YY(1:4)=DIAGARS(2:5,1)
  DIAGARS(1,1) = FIT0CUB(RC0P)
  !     2: BETABUS_GA(int pdV - p(rho) V)/Bpavga**2 * 2mu0
  !     2: BETAB_GA(PSI): INT(P DV) = PV(PSI) - INT(PPRIME*V*DPSI)
  !     => BETAB PROP. TO INT(P DV)-PV= -INT(PPRIME*V*DPSI)
  DIAGARS(1:KPSISMP1,2) = (/0._rkind, betab(1:KPSISMP1-1) /) * DIAGARS(1:KPSISMP1,1)
  !     3: BETAPLOCALGA
  DIAGARS(2:KPSISMP1,3) = 2._rkind * eqchease_out(1)%profiles_1d%pressure(2:KPSISMP1) / eqchease_out_add_1d(2:KPSISMP1,iiIplas)**2 &
       & * rleng(1:KPSISMP1-1)**2
  DIAGARS(1,3) = RC0P
  !     4: li_ga
  DIAGARS(1:KPSISMP1,4) =  eqchease_out(1)%profiles_1d%li(1:KPSISMP1)*DIAGARS(1:KPSISMP1,1)
  !     5: RBAR=RHO_Vnorm*a*(kap)**1/2
  DIAGARS(1:KPSISMP1,5) =  eqchease_out(1)%profiles_1d%rho_vol(1:KPSISMP1) * &
       & EQCHEASE_OUT_ADD_1D(1:KPSISMP1,iiamin) * sqrt(eqchease_out(1)%profiles_1d%elongation(1:KPSISMP1))
  !   6: rhotor*BT/Rgeom/BPO_hinton*2pi=q to check
  DIAGARS(2:KPSISMP1,6) =  eqchease_out(1)%profiles_1d%rho_tor(2:KPSISMP1) * eqchease_out(1)%profiles_1d%F_dia(2:KPSISMP1) &
       & / eqchease_out_add_1d(2:KPSISMP1,iirgeo) / eqchease_out(1)%profiles_1d%dpsidrho_tor(2:KPSISMP1) * TWOPI
  XX(1:4)=ZSM(2:5)
  YY(1:4)=DIAGARS(2:5,6)
  DIAGARS(1,6) = FIT0CUB(RC0P)
  !   7: jBStild=dIBS2/dpsi / dA/dpsi
  DIAGARS(1:KPSISMP1,7) =  PJBS2(1:KPSISMP1)
  !
  !     2.2 DIAGNOSTICS ON INTEGER Q SURFACES + EDGE + CENTER + 1.5
  !
  !     USE ARRAY DIAGARS(II,IDIAGLAS+J), J=1,NB. OF INTEGER Q
  !     (II, DIAGARS VALUE): (1,CS), (2,QS), (3,RBAR), (4,BETAB),
  !     (5,BETABGA), (6,BETALOC), (7,LI), (8,EPSILON), (9,SHEAR),
  !     (10,RMID), (11,ELONGATION), (12,RHO_V), (13, DELTA_AVR)
  !     RBAR = RHO_V * SQRT(KAPPA_EDGE) * A_EDGE
  !
  IF (NVERBOSE .GE. 2) WRITE(DIAGARTX(IDIAGLAS+1),'(14(A9,1X))') 'S','QS','RBAR', &
    &     'BETABU','BETABUGA','BETAP_LOC','LI','EPSILON','SHEAR', &
    &     'RMID','ELONG.','RVOL','DELTA_AVR','ALPHA'
  NDIAPERQ = 14
  ZQSMIN = eqchease_out(1)%profiles_1d%q(ISMIN(KPSISMP1,eqchease_out(1)%profiles_1d%q,1))
  ZQSMAX = eqchease_out(1)%profiles_1d%q(ISMAX(KPSISMP1,eqchease_out(1)%profiles_1d%q,1))
  IQSMIN = INT(ZQSMIN) + 1
  IQSMAX = INT(ZQSMAX)
  JJ = IDIAGLAS
  !
  !     CENTRAL VALUES
  JJ = JJ+1
  DIAGARS(1,JJ)  = 0.0_RKIND
  DIAGARS(2,JJ)  = eqchease_out(1)%profiles_1d%q(1)
  DIAGARS(3,JJ)  = 0.0_RKIND
  DIAGARS(4,JJ)  = DIAGARS(1,2)/DIAGARS(1,1)
  DIAGARS(5,JJ)  = DIAGARS(1,2)
  DIAGARS(6,JJ)  = DIAGARS(1,3)
  DIAGARS(7,JJ)  = eqchease_out(1)%profiles_1d%li(1)
  DIAGARS(8,JJ)  = 0.0_RKIND
  DIAGARS(9,JJ)  = eqchease_out_add_1d(1,iishear)
  DIAGARS(10,JJ) = eqchease_out_add_1d(1,iirgeo)
  DIAGARS(11,JJ) = eqchease_out(1)%profiles_1d%elongation(1)
  DIAGARS(12,JJ) = 0.0_RKIND
  DIAGARS(13,JJ) = 0.5_rkind*(eqchease_out(1)%profiles_1d%tria_lower(1)+eqchease_out(1)%profiles_1d%tria_upper(1))
  DIAGARS(14,JJ) = eqchease_out_add_1d(1,iialpha)
  DO J11=1,KPSISMP1-1
    ZQSJ = eqchease_out(1)%profiles_1d%q(J11)
    ZQSJP1 = eqchease_out(1)%profiles_1d%q(J11+1)
    DO IQ=IQSMIN,IQSMAX
      IF ((IQ-ZQSJ)*(IQ-ZQSJP1).LE.0._RKIND .OR. &
           &         (1.5_RKIND-ZQSJ)*(1.5_RKIND-ZQSJP1).LE.0._RKIND) THEN
        ZQRAT = IQ * 1._RKIND
        IF ((1.5_RKIND-ZQSJ)*(1.5_RKIND-ZQSJP1) .LE. 0._RKIND) ZQRAT = 1.5_RKIND
        JJ = JJ + 1
        IF (JJ .GT. NPDIAG1-1) THEN
          IF (NVERBOSE .GE. 2) PRINT *,'********************************'
          IF (NVERBOSE .GE. 2) PRINT *,' JJ= ',JJ,' NB OF RATIONAL SURFACES FOR DIAGNOSTICS > NPDIAG1=',NPDIAG1
          GO TO 12
        ENDIF
        ZCSJ = ZSM(J11)
        ZCSJP1 = ZSM(J11+1)
        ZCSIQ = FLINEAR(ZQSJ,ZQSJP1,ZCSJ,ZCSJP1,ZQRAT)
        DIAGARS(1,JJ) = ZCSIQ
        DIAGARS(2,JJ) = ZQRAT
        DIAGARS(3,JJ) = FLINEAR(ZCSJ,ZCSJP1, &
             &           DIAGARS(J11,5),DIAGARS(J11+1,5),ZCSIQ)
        DIAGARS(5,JJ) = FLINEAR(ZCSJ,ZCSJP1, &
             &           DIAGARS(J11,2),DIAGARS(J11+1,2),ZCSIQ)
        DIAGARS(4,JJ) = DIAGARS(5,JJ) * 0.5_RKIND*(DIAGARS(J11,1)+DIAGARS(J11+1,1))
        DIAGARS(6,JJ) = FLINEAR(ZCSJ,ZCSJP1, &
             &           DIAGARS(J11,3),DIAGARS(J11+1,3),ZCSIQ)
        DIAGARS(7,JJ) = FLINEAR(ZCSJ,ZCSJP1, &
             &           DIAGARS(J11,4),DIAGARS(J11+1,4),ZCSIQ)
        DIAGARS(8,JJ) = FLINEAR(ZCSJ,ZCSJP1, &
             &           eqchease_out_add_1d(J11,iiamin)/eqchease_out_add_1d(J11,iirgeo), &
             & eqchease_out_add_1d(J11,iiamin)/eqchease_out_add_1d(J11+1,iirgeo),ZCSIQ)
        DIAGARS(9,JJ) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(J11,iishear), &
             &           eqchease_out_add_1d(J11+1,iishear),ZCSIQ)
        DIAGARS(10,JJ) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(J11,iirgeo), &
             &           eqchease_out_add_1d(J11+1,iirgeo),ZCSIQ)
        DIAGARS(11,JJ) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out(1)%profiles_1d%elongation(J11), &
             &           eqchease_out(1)%profiles_1d%elongation(J11+1),ZCSIQ)
        DIAGARS(12,JJ) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out(1)%profiles_1d%RHO_VOL(J11), &
             &           eqchease_out(1)%profiles_1d%RHO_VOL(J11+1),ZCSIQ)
        DIAGARS(13,JJ) = FLINEAR(ZCSJ,ZCSJP1, &
             & 0.5_rkind*(eqchease_out(1)%profiles_1d%tria_lower(J11)+eqchease_out(1)%profiles_1d%tria_upper(J11)), &
             & 0.5_rkind*(eqchease_out(1)%profiles_1d%tria_lower(J11+1)+eqchease_out(1)%profiles_1d%tria_upper(J11+1)), &
             &           ZCSIQ)
        DIAGARS(14,JJ) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(J11,iialpha), &
             &           eqchease_out_add_1d(J11+1,iialpha),ZCSIQ)
        GO TO 11
      ENDIF
    ENDDO
11  CONTINUE
  END DO
12 CONTINUE
  !
  !     EDGE VALUES
  JJ = JJ+1
  IF (JJ .GT. NPDIAG1) THEN
    IF (NVERBOSE .GE. 2) PRINT *,'********************************'
    IF (NVERBOSE .GE. 2) PRINT *,' JJ= ',JJ, &
      &       ' NB OF RATIONAL SURFACES FOR DIAGNOSTICS > NPDIAG1=' &
      &       ,NPDIAG1
    !%OS           STOP 'GLOADD 2'
  ENDIF
  DIAGARS(1,JJ)  = ZSM(KPSISMP1)
  DIAGARS(2,JJ)  = eqchease_out(1)%profiles_1d%q(KPSISMP1)
  DIAGARS(3,JJ)  = DIAGARS(KPSISMP1,5)
  DIAGARS(4,JJ)  = DIAGARS(KPSISMP1,2)/DIAGARS(KPSISMP1,1)
  DIAGARS(5,JJ)  = DIAGARS(KPSISMP1,2)
  DIAGARS(6,JJ)  = DIAGARS(KPSISMP1,3)
  DIAGARS(7,JJ)  = eqchease_out(1)%profiles_1d%li(KPSISMP1)
  DIAGARS(8,JJ)  = eqchease_out_add_1d(KPSISMP1,iiamin)/eqchease_out_add_1d(KPSISMP1,iirgeo)
  DIAGARS(9,JJ)  = eqchease_out_add_1d(KPSISMP1,iishear)
  DIAGARS(10,JJ) = eqchease_out_add_1d(KPSISMP1,iirgeo)
  DIAGARS(11,JJ) = eqchease_out(1)%profiles_1d%elongation(KPSISMP1)
  DIAGARS(12,JJ) = eqchease_out(1)%profiles_1d%RHO_VOL(KPSISMP1)
  DIAGARS(13,JJ) = 0.5_rkind*(eqchease_out(1)%profiles_1d%tria_lower(KPSISMP1)+eqchease_out(1)%profiles_1d%tria_upper(KPSISMP1))
  DIAGARS(14,JJ) = eqchease_out_add_1d(KPSISMP1,iialpha)
  !     VALUE OF NDIAGLAS = 100*(NB OF PROFILES) + NB OF INTEGER Q
  NDIAGLAS = 100*IDIAGLAS + (JJ-IDIAGLAS)
  !
  GO TO 999
  !.......................................................................
  !
  !     3. PARAMETERS RELATED TO NEOCLASSICAL TEARING MODE
  !     .  VALUES ON Q SURFACES GIVEN IN QVALNEO(I=1,NEONBQS), WITH I=1 FOR AXIS
  !     .  AND I=NEONBQS FOR EDGE. MIDDLE Q VALUES SET IN PRESET AND NAMELIST
  !
  !     .  => DEFINE NFLGADDIA(3) = 100*NEONBVAL + NEONBQS
  !
300 CONTINUE
  !
  !     VALUES TO BE SAVED DEFINED IN SURFADD AS NEED NEONBVAL DEFINED
  !
  NFLGADDIA(3) = NEONBVAL*100 + NEONBQS
  !
  !     KOPT=2 SHOULD ALWAYS BE CALLED BEFORE
  !
  IF (NFLGADDIA(2) .EQ. 0) THEN
    IF (NVERBOSE .GE. 0) WRITE(0,*) 'IF WANT GLOADD SERIES 3 SHOULD CALL KOPT=2 FIRST'
    RETURN
  END IF

  !
  !     Q_AXIS
  I = 1
  VALSNEO(2,I) = DIAGARS(I,2)
  VALSNEO(3,I) = eqchease_out(1)%profiles_1d%li(I)
  VALSNEO(4,I) = DIAGARS(I,1)
  VALSNEO(5,I) = DIAGARS(I,3)
  VALSNEO(6,I) = eqchease_out_add_1d(I,iishear)
  ! Note: value of (1) is at smiso(1) and not on-axis, but difference not so important for these values
  VALSNEO(7,I) = VALSNEO(6,I) / VALSNEO(8,I)
  VALSNEO(9,I) = 0._RKIND
  VALSNEO(21,I)= - SMERCI(I)
  VALSNEO(22,I)= - SMERCR(I)
  VALSNEO(23,I)= HMERCR(I)
  VALSNEO(36,I) = eqchease_out_add_1d(I,iiIplas) / RLENG(I)
  VALSNEO(37,I) = EQCHEASE_OUT_ADD_1D(I,IIRGEO) * RBPOL0(I)
  ZBT = TMF(I) / EQCHEASE_OUT_ADD_1D(I,IIRGEO)
  VALSNEO(38,I) = 0.0_RKIND
  VALSNEO(39,I) = 0.0_RKIND
  VALSNEO(40,I) = 0.0_RKIND
  VALSNEO(41,I) = 0.0_RKIND
  VALSNEO(42,I) = 0.5_RKIND*ZBT**2 + CPR(I)
  VALSNEO(43,I) = 0.0_RKIND
  VALSNEO(44,I) = - SMERCI(I)
  VALSNEO(45,I) = - SMERCR(I)
  VALSNEO(47,I) = EQCHEASE_OUT_ADD_1D(I,IIRGEO)
  VALSNEO(48,I) = 0.0_RKIND
  VALSNEO(49,I) = 0.0_RKIND
  VALSNEO(50,I) = 0.0_RKIND
  !
  ! could have problem for arrays constructed only on smiso mesh without extra axis point
  ! so avoid JS=1-2 interval
  DO JS=2,KPSISMP1-1
    ZQSJ = eqchease_out(1)%profiles_1d%q(JS)
    ZQSJP1 = eqchease_out(1)%profiles_1d%q(JS+1)
    DO I=2,NEONBQS-1
      IF ((ZQSJ-QVALNEO(I))*(ZQSJP1-QVALNEO(I)) .LE. 0._RKIND) THEN
        ZCSJ = SMISOP1(JS)
        ZCSJP1 = SMISOP1(JS+1)
        ZCSIQ = VALSNEO(1,I)
        VALSNEO(2,I) = FLINEAR(ZCSJ,ZCSJP1,DIAGARS(JS,2),DIAGARS(JS+1,2),ZCSIQ)
        VALSNEO(3,I) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out(1)%profiles_1d%li(JS), &
             & eqchease_out(1)%profiles_1d%li(JS+1),ZCSIQ)
        VALSNEO(4,I) = FLINEAR(ZCSJ,ZCSJP1,DIAGARS(JS,1),DIAGARS(JS+1,1),ZCSIQ)
        VALSNEO(5,I) = FLINEAR(ZCSJ,ZCSJP1,DIAGARS(JS,3),DIAGARS(JS+1,3),ZCSIQ)
        VALSNEO(6,I) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(JS,iishear), &
             & eqchease_out_add_1d(JS+1,iishear),ZCSIQ)
        VALSNEO(7,I) = VALSNEO(6,I) / VALSNEO(8,I)
        ZRHO = FLINEAR(ZCSJ,ZCSJP1,eqchease_out(1)%profiles_1d%rho_vol(JS), &
             & eqchease_out(1)%profiles_1d%rho_vol(JS+1),ZCSIQ) * ZAEDGE
        VALSNEO(9,I) = ZRHO
        VALSNEO(21,I)= - FLINEAR(ZCSJ,ZCSJP1,SMERCI(JS-1),SMERCI(JS),ZCSIQ)
        VALSNEO(22,I)= - FLINEAR(ZCSJ,ZCSJP1,SMERCR(JS-1),SMERCR(JS),ZCSIQ)
        VALSNEO(23,I)= FLINEAR(ZCSJ,ZCSJP1,HMERCR(JS-1),HMERCR(JS),ZCSIQ)
        VALSNEO(36,I) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(JS,iiIplas)/RLENG(JS-1), &
             & eqchease_out_add_1d(JS+1,iiIplas)/RLENG(JS),ZCSIQ)
        ZRGEOQ = FLINEAR(ZCSJ,ZCSJP1,EQCHEASE_OUT_ADD_1D(JS,IIRGEO), &
             &           EQCHEASE_OUT_ADD_1D(JS+1,IIRGEO),ZCSIQ)
        ZAGEOQ = FLINEAR(ZCSJ,ZCSJP1,EQCHEASE_OUT_ADD_1D(JS,IIAMIN) &
             &           ,EQCHEASE_OUT_ADD_1D(JS+1,IIAMIN),ZCSIQ)
        VALSNEO(37,I) = (ZRGEOQ+ZAGEOQ) * FLINEAR(ZCSJ,ZCSJP1, &
             &           RBPOL0(JS-1),RBPOL0(JS),ZCSIQ)
        ZBTQ =FLINEAR(ZCSJ,ZCSJP1,eqchease_out(1)%profiles_1d%F_DIA(JS), &
             & eqchease_out(1)%profiles_1d%f_dia(JS+1),ZCSIQ) / (ZRGEOQ+ZAGEOQ)
        VALSNEO(38,I) = ZRHO * ZBTQ / QVALNEO(I)
        VALSNEO(39,I) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(JS,iiIplas),eqchease_out_add_1d(JS+1,iiIplas),ZCSIQ)
        VALSNEO(40,I) = FLINEAR(ZCSJ,ZCSJP1,eqchease_out_add_1d(JS,iialpha),eqchease_out_add_1d(JS+1,iialpha),ZCSIQ)
        VALSNEO(41,I) = VALSNEO(40,I) / FLINEAR(ZCSJ,ZCSJP1,eqchease_out(1)%profiles_1d%dpsidrho_tor(JS), &
             &           eqchease_out(1)%profiles_1d%dpsidrho_tor(JS+1),ZCSIQ)
        ZWMAGJS  = eqchease_out(1)%profiles_1d%pressure(JS)+0.5_rkind*eqchease_out(1)%profiles_1d%gm5(JS)
        ZWMAGJP1 = eqchease_out(1)%profiles_1d%pressure(JS+1)+0.5_rkind*eqchease_out(1)%profiles_1d%gm5(JS+1)
        VALSNEO(42,I) = FLINEAR(ZCSJ,ZCSJP1,ZWMAGJS,ZWMAGJP1,ZCSIQ)
        ZPTILDPR = (ZWMAGJP1-ZWMAGJS) / (PSISMP1(JS+1) - PSISMP1(JS))
        ! well = 2 V / dVdpsi * dPtild/dpsi / <B^2>
        VALSNEO(43,I) = ZPTILDPR * &
             & (eqchease_out(1)%profiles_1d%volume(JS)/RIVOL(JS-1)/eqchease_out(1)%profiles_1d%gm5(JS) &
             & + eqchease_out(1)%profiles_1d%volume(JS+1)/RIVOL(JS)/eqchease_out(1)%profiles_1d%gm5(JS+1))
        VALSNEO(44,I) = - FLINEAR(ZCSJ,ZCSJP1,SMERCI(JS-1),SMERCI(JS),ZCSIQ)
        VALSNEO(45,I) = - FLINEAR(ZCSJ,ZCSJP1,SMERCR(JS-1),SMERCR(JS),ZCSIQ)
        VALSNEO(47,I) = ZRGEOQ + ZAGEOQ
        !   48: DR/DRHOA
        ZDA = (EQCHEASE_OUT_ADD_1D(JS+1,IIAMIN)-EQCHEASE_OUT_ADD_1D(JS,IIAMIN)) * ZAEDGE
        VALSNEO(48,I) = (EQCHEASE_OUT_ADD_1D(JS+1,IIRGEO)+EQCHEASE_OUT_ADD_1D(JS+1,IIAMIN) - &
             & EQCHEASE_OUT_ADD_1D(JS,IIRGEO)-EQCHEASE_OUT_ADD_1D(JS,IIAMIN)) / ZDA
        !   49: DPSI/DRHOA
        ZDPSI = PSISMP1(JS+1) - PSISMP1(JS)
        VALSNEO(49,I) = ZDPSI / ZDA
        !   50: D(rhotor*BT/Rgeom/BPO_hinton*2pi) / DPSI
        VALSNEO(50,I) = (DIAGARS(JS+1,6)-DIAGARS(JS,6)) / ZDPSI
      ENDIF
    END DO
  END DO
  !     QEDGE
  I = NEONBQS
  VALSNEO(2,I) = DIAGARS(KPSISMP1,2)
  VALSNEO(3,I) = eqchease_out(1)%profiles_1d%li(KPSISMP1)
  VALSNEO(4,I) = DIAGARS(KPSISMP1,1)
  VALSNEO(5,I) = DIAGARS(KPSISMP1,3)
  VALSNEO(6,I) = eqchease_out_add_1d(KPSISMP1,iishear)
  VALSNEO(7,I) = VALSNEO(6,I) / VALSNEO(8,I)
  ZRHO = eqchease_out(1)%profiles_1d%rho_vol(KPSISMP1) * ZAEDGE
  VALSNEO(9,I) = ZRHO
  VALSNEO(21,I)= - SMERCI(KPSISMP1)
  VALSNEO(22,I)= - SMERCR(KPSISMP1)
  VALSNEO(23,I)= HMERCR(KPSISMP1)
  VALSNEO(36,I) = eqchease_out_add_1d(KPSISMP1,iiIplas) / RLENG(KPSISMP1-1)
  ZRLFS = EQCHEASE_OUT_ADD_1D(KPSISMP1,IIRGEO) + EQCHEASE_OUT_ADD_1D(KPSISMP1,IIAMIN)
  VALSNEO(37,I) = ZRLFS * RBPOL0(KPSISMP1)
  ZBT = TMF(KPSISMP1-1) / ZRLFS
  VALSNEO(38,I) = ZRHO * ZBT / eqchease_out(1)%profiles_1d%q(KPSISMP1)
  VALSNEO(39,I) = eqchease_out_add_1d(KPSISMP1,iiIplas)
  VALSNEO(40,I) = -2._RKIND * RMAG * QVALNEO(I)**2 / ZBT**2 * &
       &     VALSNEO(7,I) * MAX(ZEPS08,CPR(KPSISMP1)) / eqchease_out(1)%profiles_1d%volume(KPSISMP1) / ZAEDGE
  VALSNEO(41,I) = -2._RKIND * RMAG * ZRHO * QVALNEO(I) / ZBT * CPPR(KPSISMP1-1)
  ZPTILDE = eqchease_out(1)%profiles_1d%pressure(KPSISMP1)+0.5_rkind*eqchease_out(1)%profiles_1d%gm5(KPSISMP1)
  VALSNEO(42,I) = ZPTILDE
  ZPTILDPR=(ZPTILDE - eqchease_out(1)%profiles_1d%pressure(KPSISMP1-1) &
       & -0.5_rkind*eqchease_out(1)%profiles_1d%gm5(KPSISMP1-1)) &
       & / (PSISMP1(KPSISMP1) - PSISMP1(KPSISMP1-1))
  VALSNEO(43,I) = ZPTILDPR * &
             & (eqchease_out(1)%profiles_1d%volume(KPSISMP1-1)/RIVOL(KPSISMP1-2)/eqchease_out(1)%profiles_1d%gm5(KPSISMP1-1) &
             & + eqchease_out(1)%profiles_1d%volume(KPSISMP1)/RIVOL(KPSISMP1-1)/eqchease_out(1)%profiles_1d%gm5(KPSISMP1))
  VALSNEO(44,I) = - SMERCI(KPSISMP1-1)
  VALSNEO(45,I) = - SMERCR(KPSISMP1-1)
  VALSNEO(47,I) = ZRLFS
  VALSNEO(48,I) = 0.0_RKIND
  VALSNEO(49,I) = 0.0_RKIND
  VALSNEO(50,I) = 0.0_RKIND

  !     RELATED VALUES AND COMPLETE COMPUTATION OF SHEART'S
  DO I=1,NEONBQS
    IF (VALSNEO(1,I) .GE. 0._RKIND) THEN
      ZEPSRS = VALSNEO(9,I)/EQCHEASE_OUT_ADD_1D(KPSISMP1,IIRGEO)
      IF (VALSNEO(6,I) .NE. 0.0_RKIND) THEN
        !     D=2*betap*eps^2/sq*(Lq/-Lp)*(1-1/q^2)
        VALSNEO(25,I)= -2._RKIND*VALSNEO(2,I)*ZEPSRS**2/VALSNEO(6,I) &
             &           / VALSNEO(8,I) * (1._RKIND-1._RKIND/QVALNEO(I)**2)
        !     SQRT(EPS)*RHOIPOL, MASS=2
        VALSNEO(32,I)= SQRT(ZEPSRS)*1.44E-04_RKIND &
             &           *SQRT(VALSNEO(12,I))/(VALSNEO(36,I)*B0EXP)**2
        !     DELTA_POL=EPS^1.5*(L_Q/L_P)**2*rhoipol^2 (from Sauter PoP 1997, but a3=1)
        VALSNEO(30,I)= VALSNEO(32,I)**2 * sqrt(ZEPSRS) / VALSNEO(8,I)**2
      ELSE
        VALSNEO(25,I)= 0.0_RKIND
        VALSNEO(30,I)= 0.0_RKIND
        VALSNEO(32,I)= 0.0_RKIND
      ENDIF
      VALSNEO(14,I)= VALSNEO(6,I) * VALSNEO(14,I)
      VALSNEO(15,I)= VALSNEO(6,I) * VALSNEO(15,I)
      VALSNEO(16,I)= VALSNEO(6,I) * VALSNEO(16,I)
      VALSNEO(20,I)= VALSNEO(11,I) / VALSNEO(12,I)
      VALSNEO(24,I)= VALSNEO(21,I) - VALSNEO(23,I) + 0.25_RKIND
      !     DELTA_BS(coeff as PoP, Sauter 97 with full BS, a2=2.6), Te'/Te=etae/(etae+1)*p'/p
      VALSNEO(29,I)= - 2.6_RKIND * VALSNEO(18,I)/(VALSNEO(18,I)+1._RKIND) * &
           &                    (VALSNEO(26,I)+VALSNEO(27,I)+VALSNEO(28,I)) / (-VALSNEO(8,I))
      !     DELTA_GGJ= 6*D (*W)
      VALSNEO(31,I)= 6._RKIND*VALSNEO(24,I) / (VALSNEO(2,I) + 1.0E-14_RKIND)
      !     FT_APP(PLETZER)= 1-[(1-EP)^2/(1-EP^2)^1/2/(1+1.46EP^1/2)
      VALSNEO(34,I)= 1._RKIND - (1._RKIND-ZEPSRS)**2/SQRT(1._RKIND-ZEPSRS*ZEPSRS) &
           &         / (1._RKIND+1.46_RKIND*SQRT(ZEPSRS))
    ENDIF
  END DO
  I=1
  VALSNEO(31,I)= 0._RKIND
  !
  !   GLOBAL QUANTITIES
  !
  !
  !   GLOBAL PARAMETERS, CHANGED TO MKSA
  !   GLOBNEO(5 AND 6) COMPUTED IN SURFADD
  !
  GLOBNEO(1) = R0EXP
  GLOBNEO(2) = B0EXP
  GLOBNEO(3) = EQCHEASE_OUT_ADD_1D(KPSISMP1,IIRGEO) * GLOBNEO(1)
  GLOBNEO(4) = ZAEDGE * GLOBNEO(1)
  GLOBNEO(5) = eqchease_out(1)%profiles_1d%elongation(KPSISMP1)
  GLOBNEO(6) = 0.5_rkind * (eqchease_out(1)%profiles_1d%tria_lower(KPSISMP1) + &
       & eqchease_out(1)%profiles_1d%tria_upper(KPSISMP1))
  GLOBNEO(7) = RITOT*GLOBNEO(1)*GLOBNEO(2)/4.E-07_RKIND/3.141593_RKIND
  ZINORM = RINOR/1.256_RKIND
  ZGM    = 100._RKIND * BETA / ZINORM
  ZGMSTA = 100._RKIND * BETAS / ZINORM
  ZGMX   = 100._RKIND * BETAX / ZINORM
  GLOBNEO(8) = ZGM
  GLOBNEO(9) = ZGMSTA
  GLOBNEO(10) = ZGMX
  GLOBNEO(11) = ZGMX * CONVF
  GLOBNEO(12) = VALSNEO(3,NEONBQS)
  GLOBNEO(13) = eqchease_out(1)%profiles_1d%li(KPSISMP1) * CONVF
  !
  GO TO 999
  !
  !     4. MERCIER WITH KINETIC EFFECTS, READ PALPHA, PIONS FROM FILE
  !     .  ASSUMES KOPT=2 PREVIOUSLY CALLED AND ADDS IDIANEW DIAGARS TERMS
  !
  !     .  => CHANGED NFLGADDIA(2) TO NFLGADDIA(2) + IDIANEW
  !
400 CONTINUE
  !
  !     CHECK THAT KOPT=2 WAS CALLED
  IF (NFLGADDIA(2) .EQ. 0) THEN
    IF (NVERBOSE .GE. 0) WRITE(0,*) ' GLOADD(KOPT = 4) ASSUMES THAT KOPT=2 IS CALLED', &
      &       ' BEFORE'
    STOP 'GLOADD 400'
  ENDIF
  !
  !     4.1 READ PALPHA AND PIONS FROM FILE VS. PSI (NEED PSI FOR DPALPHA/DPSI)
  !
  CALL GDATAEXT('EXPDATA',1,'S-MESH@',INBPSI,ZDATPSI,1,NVERBOSE+1,-1,INFO)
  INBPSI = ABS(INBPSI)
  CALL GDATAEXT('EXPDATA',1,'PRESS_ION@',INBPI,ZDATPI,1,NVERBOSE+1,0,INFO)
  INBPI = ABS(INBPI)
  CALL GDATAEXT('EXPDATA',1,'PRESS_ALPHA@',INBPALF,ZDATPALF,1,NVERBOSE+1,0,INFO)
  INBPALF = ABS(INBPALF)
  IF (INBPSI.NE.INBPI .OR. INBPSI.NE.INBPALF) THEN
    IF (NVERBOSE .GE. 0) WRITE(0,*) ' PROBLEM IN GLOADD 410, INBPSI= ',INBPSI,' INBPI= ', &
         &       INBPI,' INBPALF= ',INBPALF,' SHOULD BE EQUAL'
    ! STOP 'GLOADD 410'
  ENDIF
  !
  !     4.2 DEFINE NEW ARRAYS AND MOVE Q DIAGNOSTICS DOWN
  !
  IDIAGLAS = NFLGADDIA(2)
  IDIAQI = NDIAGLAS - 100*IDIAGLAS
  IDIANEW = 17
  IF (IDIAGLAS+IDIAQI+IDIANEW .GT. NPDIAG1) THEN
    IF (NVERBOSE .GE. 0) WRITE(0,*) '********************************'
    PRINT *,' IDIAGLAS+IDIAQI+IDIANEW= ',IDIAGLAS,'+',IDIAQI, &
      &       '+',IDIANEW,' = ',IDIAGLAS+IDIAQI+IDIANEW, &
      &       ' NB OF DIAGNOSTICS DIAGARS > NPDIAG1=',NPDIAG1
    STOP 'GLOADD 3'
  ENDIF
  DIAGARTX(IDIAGLAS+IDIANEW+1) = DIAGARTX(IDIAGLAS+1)
  DO IQ=1,IDIAQI
    DO II=1,NDIAPERQ
      DIAGARS(II,IDIAGLAS+IDIANEW+IQ) = DIAGARS(II,IDIAGLAS+IQ)
    END DO
    DO II=1,NDIAPERQ
      DIAGARS(II,IDIAGLAS+IQ) = 0.0_RKIND
    END DO
  END DO
  !
  DIAGARTX(IDIAGLAS+1) = 'D14: DM_MHD=-(-DI)'
  DIAGARTX(IDIAGLAS+2) = 'D15: DM_ANA_COEFF RPP/S2/B2'
  DIAGARTX(IDIAGLAS+3) = 'D16: DM_MHD/COEFF'
  DIAGARTX(IDIAGLAS+4) = 'D17: COEFF_in_RHO'
  DIAGARTX(IDIAGLAS+5) = 'D18: DM_ANA_KAP_NOCOF'
  DIAGARTX(IDIAGLAS+6) = 'D19: DM_ANA_KAP'
  DIAGARTX(IDIAGLAS+7) = 'D20: DM_ANA_KO_NOCOF'
  DIAGARTX(IDIAGLAS+8) = 'D21: DM_ANA_HOT_NOCOF'
  DIAGARTX(IDIAGLAS+9) = 'D22: DMM_ANA_NOCOF'
  DIAGARTX(IDIAGLAS+10)= 'D23: DMM_ANA_TOT'
  DIAGARTX(IDIAGLAS+11)= 'D24: P_TOT'
  ISHFTPI = 12
  ISHFTPAL = 13
  DIAGARTX(IDIAGLAS+ISHFTPI)= 'D25: P_ION'
  DIAGARTX(IDIAGLAS+ISHFTPAL)= 'D26: P_ALPHA'
  DIAGARTX(IDIAGLAS+14)= 'D27: DP_ALPHA/DPSI'
  DIAGARTX(IDIAGLAS+15)= 'D28: RDI'
  DIAGARTX(IDIAGLAS+16)= 'D29: RDI/COEFF'
  !%OS         DIAGARTX(IDIAGLAS+17)= 'D30: DM_MHD/COEFF(RMIN)'
  DIAGARTX(IDIAGLAS+17)= 'D30: RMIN[M]'
  !     CHECK THAT IDIANEW ABOVE CORRESPONDS TO THE +II JUST DEFINED
  !
  NDIAGLAS = 100*(IDIAGLAS+IDIANEW) + IDIAQI
  !     SET FLAG TO NUMBER OF PROFILES
  NFLGADDIA(2) = NFLGADDIA(2) + IDIANEW
  !
  !     4.3 COMPUTE RELATED ARRAYS IN MKSA, BUT MPA, MA, ...
  !
  !     4.3.1 INTERPOLATE PI AND PALPHA ON SMISO
  !
  ZSM(1:KPSISMP1) = sqrt(PSISMP1/CPSRF)
  ISTART = 1
  DO J=1,KPSISMP1
    !     FIND INTERVAL
    DO I=ISTART,INBPSI-1
      IF (ZDATPSI(I+1) .GE. ZSM(J)**2) GO TO 4311
    END DO
4311 CONTINUE
    !     +12: ION PRESSURE
    DIAGARS(J,IDIAGLAS+ISHFTPI) = FLINEAR(ZDATPSI(I), &
         &       ZDATPSI(I+1),ZDATPI(I),ZDATPI(I+1),ZSM(J)**2)
    !     +13: ALPHA PRESSURE
    DIAGARS(J,IDIAGLAS+ISHFTPAL) = FLINEAR(ZDATPSI(I), &
         &       ZDATPSI(I+1),ZDATPALF(I),ZDATPALF(I+1),ZSM(J)**2)
    ISTART = I
  END DO
  !
  !     4.3.2 OTHER ARRAYS
  !     P'=CPPR(ZSM), S=CDRQ(CS), B=SQRT(RB2AV(ZSM)) or TMF(ZSM)/EQCHEASE_OUT_ADD_1D(1:KPSISMP1,IIRGEO)
  !     NOTE: SHOULD FLINEAR TMF, BUT OK FOR NOW
  !
  ZFACP = B0EXP**2 / (4.E-07_RKIND * CPI)
  DO J=2,KPSISMP1
    Z1OBZSM = EQCHEASE_OUT_ADD_1D(J,IIRGEO) / eqchease_out(1)%profiles_1d%f_dia(J)
    !     +1: FULL MERCIER TERM FROM GLOQUA: DM = 1/4 - (-DI)
    DIAGARS(J,IDIAGLAS+1) = 0.25_RKIND - SMERCI(J-2)
    !     +2: COEFF = -2MU0*R*DP/DR / S^2 / B^2 =~ -2 R^2 *DP/DPSI / S^2 / B / Q
    !     SHOULD KEEP DERIVATIVES WITH RESPECT TO PSI, AS IN IDEAL MERCIER, IN 
    !     PARTICULAR FOR (Q')**2.
    !     THUS COEFF = - 2*(MU0*DP/DPSI)/(DQ/DPSI)**2 * Q**3/RHO**2/B**3
    ZRMIN = eqchease_out(1)%profiles_1d%r_inboard(j)
    JP1 = MIN(J+1,KPSISMP1)
    JEFF = JP1 - 1
    ZRHO = ZAEDGE * eqchease_out(1)%profiles_1d%volume(j)
    ZSQREPS = SQRT(EQCHEASE_OUT_ADD_1D(j,iiamin)/EQCHEASE_OUT_ADD_1D(j,IIRGEO))
    DIAGARS(J,IDIAGLAS+2) = -2._RKIND * CPPR(J-1) / (CDQ(J-1)+1.0E-13_RKIND)**2 &
         &       * QPSI(J-1)**3 / ZRHO**2 * Z1OBZSM**3
    !     +3: FULL MERCIER TERM FROM GLOQUA DIVIDED BY COEFF TERM
    DIAGARS(J,IDIAGLAS+3) = DIAGARS(J,IDIAGLAS+1) / &
         &       (DIAGARS(J,IDIAGLAS+2)+1.E-13_RKIND)
    !     +4: COEFF AS +2, BUT IN RHO
    ZDPDR = eqchease_out(1)%profiles_1d%pprime(j)*eqchease_out(1)%profiles_1d%dpsidrho_tor(j)
    DIAGARS(J,IDIAGLAS+4) = -2._RKIND * eqchease_out(1)%profiles_1d%volume(j) * ZDPDR * &
         &       (Z1OBZSM/(CDRQ(J-1)+1.E-13_RKIND))**2
    !     +5: DM_ANA_KAP_NOCOF, EQ(2) WITH KAPPA-1 TERM, WITHOUT COEFF
    ZKAPZSM = eqchease_out(1)%profiles_1d%elongation(j)
    DIAGARS(J,IDIAGLAS+5) = 1._RKIND &
         &       - eqchease_out(1)%profiles_1d%q(j)**2 * (1._RKIND-0.75_RKIND*(ZKAPZSM-1._RKIND))
    !     +6: DM_ANA_KAP, EQ(2) WITH KAPPA-1 TERM, WITH COEFF
    DIAGARS(J,IDIAGLAS+6) = DIAGARS(J,IDIAGLAS+2) * DIAGARS(J,IDIAGLAS+5)
    !     +7: DM_ANA_KO, KRUSKAL-OBERMAN TERM EQ(6), = 0.47*SQRT(EPS)*PION*Q/R^2/B/DP/DPSI
    DIAGARS(J,IDIAGLAS+7) = 0.47_RKIND * ZSQREPS * eqchease_out(1)%profiles_1d%q(j) &
         &       * (DIAGARS(J,IDIAGLAS+ISHFTPI)/ZFACP)/ZRHO**2 &
         &       * Z1OBZSM / eqchease_out(1)%profiles_1d%pprime(j)
    !     +8: DM_ANA_HOT, EQ(7), = - (Q**2/SQRT(2)/PI*R0/RBAR*SQRT(EPS)*(DPALF/DPSI)/(DP/DPSI))
    ZDPADPS = (DIAGARS(JP1,IDIAGLAS+ISHFTPAL) &
         &       - DIAGARS(J-1,IDIAGLAS+ISHFTPAL)) &
         &       / (ZSM(JP1)**2 - ZSM(J-1)**2) * (- 1._RKIND / SPSIM)
    DIAGARS(J,IDIAGLAS+8) = - eqchease_out(1)%profiles_1d%q(j)**2 / SQRT(2._RKIND)/CPI &
         &       * EQCHEASE_OUT_ADD_1D(J,IIRGEO)/ZRHO * ZSQREPS * (ZDPADPS/ZFACP) / eqchease_out(1)%profiles_1d%pprime(j)
    !     +9: DMM_ANA_NOCOF, EQ(8), WITHOUT COEFF TERM
    DIAGARS(J,IDIAGLAS+9) = DIAGARS(J,IDIAGLAS+3) &
         &       + DIAGARS(J,IDIAGLAS+7) + DIAGARS(J,IDIAGLAS+8)
    !     +10: DMM_ANA_TOT, EQ(8)
    DIAGARS(J,IDIAGLAS+10) = DIAGARS(J,IDIAGLAS+2) &
         &       * DIAGARS(J,IDIAGLAS+9)
    !     +11: PTOT IN MKSA
    DIAGARS(J,IDIAGLAS+11) = eqchease_out(1)%profiles_1d%pressure(j) * ZFACP
    !     +14: DPALPHA/DPSI [1E-6, MKSA]
    DIAGARS(J,IDIAGLAS+14) = ZDPADPS/1.E+06_RKIND/B0EXP/R0EXP**2
    !     +15: LUTJENS FORMULA RDI(ZSM)
    DIAGARS(J,IDIAGLAS+15) = 0.25_RKIND - RDI(J)
    !     +16: LUTJENS FORMULA RDI(ZSM)2
    DIAGARS(J,IDIAGLAS+16) = &
         &       (1._RKIND - eqchease_out(1)%profiles_1d%q(j)**2 * (1._RKIND + 1.5_RKIND*DPRIME(J-1)*RDEDR(J-1) - &
         &       .75_RKIND * (2*RELL(J-1) + eqchease_out(1)%profiles_1d%volume(j) * RDEDR(J-1))))
    !     +17: COEFF WITH EPS INSTEAD OF RBAR/R0
    DIAGARS(J,IDIAGLAS+17) = DIAGARS(J,IDIAGLAS+3) / (ZRHO/ZRMIN)**2
  END DO
  !     
  GO TO 999
  !
  !     5. NOT YET
  !
500 CONTINUE
  !
  IF (NVERBOSE .GE. 0) WRITE(0,*) ' OPTION 5 NOT YET IMPLEMENTED IN GLOADD'
  STOP 'GLOADD 500'
  !
999 CONTINUE
  !
  RETURN
END SUBROUTINE GLOADD
