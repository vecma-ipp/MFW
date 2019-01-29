!*DECK C3SA01
!*CALL PROCESS
SUBROUTINE OUTPUT(K)
  !        ####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SA01  INPUT / OUTPUT                                              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  USE interpos_module
  IMPLICIT NONE
  INCLUDE 'COMDAT.inc'
  !
  INTERFACE
     SUBROUTINE RARRAY(KNAME,PA,KDIM)
       USE globals
       IMPLICIT NONE
       INTEGER, INTENT(IN)       :: KDIM
       REAL(RKIND), INTENT(IN)   :: PA(KDIM)
       CHARACTER*(*), INTENT(IN) :: KNAME
     END SUBROUTINE RARRAY
  END INTERFACE
  !
  INTEGER          ::     J172
  INTEGER          ::     J171
  INTEGER          ::     ISSUM
  INTEGER          ::     IBL
  INTEGER          ::     J151
  INTEGER          ::     NGA
  REAL(RKIND)      ::     ZRGEOMCS
  INTEGER          ::     I
  REAL(RKIND)      ::     ZDUMMY, ZDUMMY2
  REAL(RKIND)      ::     ZF3
  REAL(RKIND)      ::     ZS3
  REAL(RKIND)      ::     ZF2
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZF1
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZJDIFF
  INTEGER          ::     J111
  REAL(RKIND)      ::     ZBBS
  REAL(RKIND)      ::     ZCBS2
  REAL(RKIND)      ::     ZCBS1
  REAL(RKIND)      ::     ZBSF
  REAL(RKIND)      ::     ZLI1
  REAL(RKIND)      ::     ZBPOL1
  REAL(RKIND)      ::     ZGMX
  REAL(RKIND)      ::     ZGMSTA
  REAL(RKIND)      ::     ZGM
  REAL(RKIND)      ::     ZIBSNO
  REAL(RKIND)      ::     ZINORM
  REAL(RKIND)      ::     ZBXPER
  REAL(RKIND)      ::     ZBSPER
  REAL(RKIND)      ::     ZBAXEPER
  REAL(RKIND)      ::     ZBPERC
  REAL(RKIND)      ::     ZMU0
  REAL(RKIND)      ::     ZD2PST
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  REAL(RKIND)      ::     ZCPSI
  REAL(RKIND)      ::     FIT0CUB, XX(1:4), YY(1:4), XXX
  INTEGER          ::     K
  DIMENSION &
       &   ZCPSI(NSNT),   ZDPDS(NSNT),   ZDPDT(NSNT),   ZD2PST(NSNT), &
       &   ZJDIFF(NPISOEFF), ZDUMMY(NPISOEFF+1), ZDUMMY2(NPISOEFF+1)
  !
  FIT0CUB(XXX) = FCCCC0(YY(1),YY(2),YY(3),YY(4),XX(1),XX(2),XX(3),XX(4),XXX)
  !-----------------------------------------------------------------------
  !
  GOTO (10,20,30,40,50,60,70,80,90,100) K
  !
  !     PRINT OUT NAMELIST
10 CONTINUE
  !
  !        CALL WNLLONG(132)
  WRITE(6,1001)
  WRITE (6,EQDATA)
  RETURN
  !
  !     RESIDU OUTPUT
20 CONTINUE
  !
  IF (NPRPSI .EQ. 1) THEN
     CALL DCOPY(NSTMAX,CPSICL(1),4,ZCPSI,1)
     CALL DCOPY(NSTMAX,CPSICL(2),4,ZDPDS,1)
     CALL DCOPY(NSTMAX,CPSICL(3),4,ZDPDT,1)
     CALL DCOPY(NSTMAX,CPSICL(4),4,ZD2PST,1)
  ENDIF
  !
  WRITE(6,1100)
  !
  IF (NSURF .NE. 1) CALL RVAR('SCALE                   ',SCALE)
  !
  CALL RVAR('PSI - MIN               ',SPSIM)
  CALL RVAR('R OF MAGAXE             ',RRAXIS)
  CALL RVAR('Z OF MAGAXE             ',RZAXIS)
  !
  IF (NOPT .EQ. 0) THEN
     CALL RVAR('CHECK OF SOLUTION       ',SCHECK)
     CALL RVAR('POLOIDAL MAGNETIC ENERGY',WMAGP)
  ENDIF
  !
  IF (NTEST .EQ. 0 .AND. NPRPSI .EQ. 1) THEN
     CALL RARRAY('PSI NUMERIQUE',ZCPSI,NSTMAX)
     CALL RARRAY('DPSI/DS',ZDPDS,NSTMAX)
     CALL RARRAY('DPSI/DT',ZDPDT,NSTMAX)
     CALL RARRAY('D2P/DST',ZD2PST,NSTMAX)
     !
  ELSE IF (NTEST .EQ. 1) THEN
     !
     IF (NPRPSI .EQ. 1) THEN
        CALL RARRAY('PSI THEORIQUE',CPSI1T,NSTMAX)
        CALL RARRAY('PSI NUMERIQUE',ZCPSI,NSTMAX)
        CALL RARRAY('ABSOLUT DIFFERENCES OF PSI ON NODES',DIFFP,NSTMAX)
     ENDIF
     CALL RVAR('RESIDU OF PSI ON NODES        ',RESPSI)
     !
     IF (NPRPSI .EQ. 1) THEN
        CALL RARRAY('DPSI/DS THEORIQUE',DPDSTH,NSTMAX)
        CALL RARRAY('DPSI/DS NUMERIQUE',DPDSNU,NSTMAX)
        CALL RARRAY('ABSOLUT DIFFERENCES OF DPSI / DS ON NODES',DIFFDS,NSTMAX)
     ENDIF
     CALL RVAR('RESIDU OF DPSI / DS ON NODES  ',RESDPS)
     !
     IF (NPRPSI .EQ. 1) THEN
        CALL RARRAY('DPSI/DT THEORIQUE',DPDTTH,NSTMAX)
        CALL RARRAY('DPSI/DT NUMERIQUE',DPDTNU,NSTMAX)
        CALL RARRAY('ABSOLUT DIFFERENCES OF DPSI / DT ON NODES',DIFFDT,NSTMAX)
     ENDIF
     CALL RVAR('RESIDU OF DPSI / DT ON NODES  ',RESDPT)
     !
     IF (NPRPSI .EQ. 1) THEN
        CALL RARRAY('D2PSI/DST THEORIQUE',D2PSTT,NSTMAX)
        CALL RARRAY('D2PSI/DST NUMERIQUE',D2PSTN,NSTMAX)
        CALL RARRAY('ABSOLUT DIFFERENCES OF D2PSI / DST ON NODES',DIFFST,NSTMAX)
     ENDIF
     CALL RVAR('RESIDU OF D2PSI / DST ON NODES ',RESDST)
  ENDIF
  !
  RETURN
  !
30 CONTINUE
  !
  IF (NSTTP .LT. 2) THEN
     IF (NSURF .NE. 1) THEN
        WRITE(6,1201) NS,NT,NPSI,NCHI,NINSCA
     ELSE
        WRITE(6,1202) NS,NT,NPSI,NCHI
     ENDIF
  ELSE
     WRITE(6,1203) NS,NT,NISO,NPSI,NCHI,NINSCA,NINMAP
  ENDIF
  !
  RETURN
  !
40 CONTINUE
  !
  WRITE(6,1250) SPSIM,RMAG,RZMAG
  RETURN
  !
  !     PRINT OUT MESH
50 CONTINUE
  !
  CALL RARRAY('SIGMA - MESH',CSIG,NS1)
  CALL RARRAY('THETA - MESH',CT,NT1)
  CALL RARRAY('RHOS - MESH',RHOS,NT1)
  RETURN
  !
60 CONTINUE
  !
  !     SHIFT OF THE MAGNETIC AXIS
  WRITE(6,2050) R0,RZ0
  CALL BLINES(1)
  RETURN
  !
  !     FLUX SURFACES AVERAGED QUANTITIES AND BETAS
70 CONTINUE
  !
  WRITE(6,2300)
  WRITE(6,2301)
  ZMU0 = 1.25664_RKIND
  ZBPERC = 100._RKIND * BETA
  ZBSPER = 100._RKIND * BETAS
  ZBXPER = 100._RKIND * BETAX
  ZBAXEPER = 2._rkind*100._rkind*cp0*( eqchease_out_add_1d(NISO1EFF1,iirgeo)/T0 )**2
  ZINORM = RINOR/ZMU0
  ZIBSNO = RIBSNOR/ZMU0
  ZGM    = ZBPERC/ZINORM
  ZGMSTA = ZBSPER/ZINORM
  ZGMX   = ZBXPER/ZINORM
  ZBPOL1 = BETAP*CONVF
  ZLI1   = eqchease_out(1)%profiles_1d%li(NISO1EFF1)*CONVF
  ZBSF   = RITBS/RITOT
  ZCBS1  = 0._RKIND
  ZCBS2  = 0._RKIND
  IF (BETAX .GT. 0._RKIND) ZCBS1 = ZIBSNO*ZINORM/(ZBXPER*SQRT(ASPCT))
  IF (BETAP .GT. 0._RKIND) ZCBS2 = ZBSF/(SQRT(ASPCT)*ZBPOL1)
  ZBBS  = ZBXPER * ZBSF
  !
  CALL RVAR('AVERAGED PRESSURE      ',CPBAR)
  CALL RVAR('TOTAL CURRENT          ',RITOT)
  CALL RVAR('NORMALIZED CURRENT     ',RINOR)
  CALL RVAR('IN    (MA,T,M)         ',ZINORM)
  CALL RVAR('PRESSURE PEAKING FACTOR',CPPF)
  CALL RVAR('CONVERSION FACTOR      ',CONVF)
  CALL RVAR('POLOIDAL BETA          ',BETAP)
  CALL RVAR('POLOIDAL BETA (GA)     ',ZBPOL1)
  CALL RVAR('LI                     ',eqchease_out(1)%profiles_1d%li(NISO1EFF1))
  CALL RVAR('LI (GA)                ',ZLI1  )
  CALL RVAR('BETA  [%]              ',ZBPERC)
  CALL RVAR('BETA* [%]              ',ZBSPER)
  CALL RVAR('BETAX [%]              ',ZBXPER)
  CALL RVAR('BETA AXE [%]           ',ZBAXEPER)
  CALL RVAR('G     (MA,T,M)         ',ZGM   )
  CALL RVAR('G*    (MA,T,M)         ',ZGMSTA)
  CALL RVAR('GEXP  (MA,T,M)         ',ZGMX  )
  CALL RVAR('F0=IB.S./ITOT (NUE*=0, all ne,Te)',ZBSF)
  CALL RVAR('IB.S./ITOT (NUE*.NE.0, all ne,Te)',RITBSC/RITOT)
  CALL RVAR('IB.S./ITOT (NUE*, p*ne''/ne,..)',RITBSC2/RITOT)
  CALL RVAR('IB.S./ITOT (NUE*,L31p'',..)',RITBSC3/RITOT)
  CALL RVAR('CBS1=IB.S.(0)/(G*SQRT(E)) ',ZCBS1 )
  CALL RVAR('CBS2=F0/(BP(1)*SQR(E)) ',ZCBS2 )
  CALL RVAR('BBS(NUE*=0)            ',ZBBS  )
  !
  IF (NRFP .EQ. 1) THEN
     !
     CALL RVAR('F                      ',RFPF )
     CALL RVAR('THETA                  ',RFPT )
     !
  ENDIF
  !
  WRITE(6,2302)
  !
  CALL RVAR('T(PSI)ON AXIS         ',T0   )
  CALL RVAR('TT-PRIME ON AXIS      ',DTTP0)
  CALL RVAR('Q(PSI) ON AXIS        ',Q0   )
  CALL RVAR('DQ/DPSI ON AXIS       ',DQDP0)
  CALL RVAR('CP(PSI) ON AXIS       ',CPSI0)
  CALL RVAR('DCP/DPSI ON AXIS      ',CPDP0)
  CALL RVAR('PRESSURE ON AXIS      ',CP0  )
  CALL RVAR('DP/DPSI ON AXIS       ',DPDP0)
  CALL RVAR('I - PRIME ON AXIS     ',RIPR0)
  CALL RVAR('<J.B>/<T/R**2> ON AXIS',RJDTB0)
  CALL RVAR('TOROIDAL FLUX(EDGE)   ',CTORSRF)
  ! assume PHI normalized by R0^2 B0 and rho_tor=sqrt(Phi/pi/B0) with same B0, thus do not use Bvacuum(Rgeom(edge)) (from revision 905)
  CALL RVAR('TOROIDAL RHOTOR(EDGE)=sqrt(PHI/PI)   ', &
       & sqrt(CTORSRF/CPI))
  CALL RVAR('TOROIDAL RHOTOR(EDGE)=eqchease_out(1)%profiles_1d%rho_tor', &
       & eqchease_out(1)%profiles_1d%rho_tor(NISO1EFF1))
  !
  IF (NPROFZ .EQ. 1) THEN
     !
     CALL RVAR('DENSITY ON AXIS       ',DENS0)
     CALL RVAR('TEMPERATURE ON AXIS   ',TEMP0)
     !
  ENDIF
  !
  WRITE(6,2350)

  DO J111=1,NISO1EFF
     ZJDIFF(J111) = RJPAR(J111) - RJBSOS(J111,2)
  ENDDO
  !
  CALL RVAR('MINIMUM Q VALUE',QMIN)
  CALL RVAR('S VALUE OF QMIN',CSQMIN)
  CALL RVAR('Q AT 95% FLUX SURFACE',Q95)
!
  CALL RARRAY('S-MESH',SMISOP1,NISO1EFF1)
  CALL RARRAY('BETA-POLOIDAL',eqchease_out(1)%profiles_1d%beta_pol,NISO1EFF1)
  CALL RARRAY('PSIchease=psi/2pi',eqchease_out(1)%profiles_1d%psi,NISO1EFF1)
  CALL RARRAY('T=RBphi',eqchease_out(1)%profiles_1d%F_dia,NISO1EFF1)
  CALL RARRAY('T*DT/DPSI',eqchease_out(1)%profiles_1d%ffprime,NISO1EFF1)
  CALL RARRAY('Pressure',eqchease_out(1)%profiles_1d%pressure,NISO1EFF1)
  CALL RARRAY('Pprime=dp/dpsi',eqchease_out(1)%profiles_1d%pprime,NISO1EFF1)
  CALL RARRAY('Q profile',eqchease_out(1)%profiles_1d%q,NISO1EFF1)
  CALL RARRAY('DQ/DPSI profile',eqchease_out_add_1d(:,iidqdpsi),NISO1EFF1)
  CALL RARRAY('SHEAR profile',eqchease_out_add_1d(:,iishear),NISO1EFF1)
  CALL RARRAY('I-STAR=<jphi/R>/<1/R>',eqchease_out(1)%profiles_1d%jphi,NISO1EFF1)
  ! CALL RARRAY('I//=<J . B> / <T / R**2>',(/ RJDTB0, RJDOTB /),NISO1EFF1)
  !%OS to have correct dimension. R0 supposed to be 1 in CHEASE but may not be exactly
  CALL RARRAY('I//=<J . B>/<T/R**2>/RGEOM(a)', &
       & (/ RJDTB0, RJDOTB(1:NISO1EFF) /)/eqchease_out_add_1d(NISO1EFF1,iirgeo),NISO1EFF1)
  CALL RARRAY('j//=<J . B>/B0',eqchease_out(1)%profiles_1d%jparallel,NISO1EFF1)
  CALL RARRAY('FTRAP',(/ RC0P, 1._RKIND-RFCIRC /),NISO1EFF1)
!!$  CALL RARRAY('C1,before scale',CIDR,NISO1EFF)
!!$  CALL RARRAY('C2,before scale',CIDQ,NISO1EFF)
!!$  IF (NSTTP .LE. 2) THEN
!!$     CALL RARRAY('C0/C2',CID0,NISO1EFF)
!!$     CALL RARRAY('C1/C2',CID2,NISO1EFF)
!!$  ELSE IF (NSTTP .EQ. 3) THEN
!!$     CALL RARRAY('C1/C2',CID0,NISO1EFF)
!!$     CALL RARRAY('C3/C2',CID2,NISO1EFF)
!!$  ENDIF
  ! useful flux surface averaged quantities. Print int(dlp/Bp), Bp=|grad(psi)|/R and then all <.> from RJ1-RJ7
  ! Note TWOPI.*RJ5 and C1 differ by SCALE factor since C1 calculated before final scaling
  ! keep same length arrays, NISO1EFF1, to easily extracts output as columns
  CALL RARRAY('Int(R dlp/|grad(psi)|)=Int(J dchi)',(/ eqchease_out(1)%profiles_1d%vprime(1)/TWOPI, TWOPI*RJ5(1:NISO1EFF) /),NISO1EFF1)
  ! goes like 1/rho^2 on-axis, thus undefined thus leave 0 for central value
  CALL RARRAY('<1/R**2/|grad(psi)|**2>',(/ RC0P, RJ1(1:NISO1EFF)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<1/|grad(psi)|**2>',(/ RC0P, RJ2(1:NISO1EFF)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<1/Bp**2>',(/ RC0P, RJ3(1:NISO1EFF)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<1/R**2>',eqchease_out(1)%profiles_1d%gm1,NISO1EFF1)
  CALL RARRAY('<Bp**2>',(/ RC0P, RJ6(1:NISO1EFF)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<Bp>',(/ RC0P, RJ7s(1:NISO1EFF,9)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<|grad(psi)|**2>',(/ RC0P, RJ7s(1:NISO1EFF,1)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<|grad(psi)|>',(/ RC0P, RJ7s(1:NISO1EFF,2)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<1/Bp>',(/ RC0P, RJ7s(1:NISO1EFF,7)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<1/R> profile',eqchease_out(1)%profiles_1d%gm9,NISO1EFF1)
  CALL RARRAY('<R>',eqchease_out(1)%profiles_1d%gm8,NISO1EFF1)
  CALL RARRAY('<R**2>',(/ RMAG**2, RJ7s(1:NISO1EFF,5)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<(|grad(psi)|/B)**2>',(/ RC0P, RJ7s(1:NISO1EFF,6)/RJ5(1:NISO1EFF) /),NISO1EFF1)
  !
  CALL RARRAY('<1/B**2>',eqchease_out(1)%profiles_1d%gm4,NISO1EFF1)
  CALL RARRAY('<B>',(/ eqchease_out(1)%profiles_1d%f_dia(1)/eqchease_out_add_1d(1,iirgeo), &
    & RIB(1:NISO1EFF)/RIVOL(1:NISO1EFF) /),NISO1EFF1)
  CALL RARRAY('<B**2>',eqchease_out(1)%profiles_1d%gm5,NISO1EFF1)
  CALL RARRAY('Bpol0=|grad psi|/R at Bmin',(/ RC0P, RBPOL0 /),NISO1EFF1)
  CALL RARRAY('<j.B>BS-CURRENT (ZERO COLL. and all from from ne, etc)',(/ RC0P, RJBSOS(1:NISO1EFF,1) /),NISO1EFF1)
  CALL RARRAY('<j.B>BS-CURRENT, all from ne, Te, Zeff, Ti ',(/ RC0P, RJBSOS(1:NISO1EFF,2) /),NISO1EFF1)
  CALL RARRAY('<j.B>BS-CURRENT, p from equil, gradients from ne'', etc',(/ RC0P, RJBSOS(1:NISO1EFF,3) /),NISO1EFF1)
  CALL RARRAY('<j.B>BS-CURRENT, p and p'' L31 from equil, rest from Te, Ti',(/ RC0P, RJBSOS(1:NISO1EFF,4) /),NISO1EFF1)
  CALL RARRAY('NUESTAR (WITHOUT ZEFF)',(/ RC0P, RNUSTAR /),NISO1EFF1)
  XX=SMISOP1(2:5)
  YY=RJPAR(2:5)
  CALL RARRAY('<j.B> (Eq.44)',(/ FIT0CUB(RC0P), RJPAR /),NISO1EFF1)
  CALL RARRAY('<j.B> - <j.B>-BS(all from ne, Te, ...)',(/ FIT0CUB(RC0P), ZJDIFF /),NISO1EFF1)
  XX=SMISOP1(2:5)
  YY=RELL(2:5)
  CALL RARRAY('ELLIPTICITY',(/ FIT0CUB(RC0P), RELL/),NISO1EFF1)
  XX=SMISOP1(2:5)
  YY=RDEDR(2:5)
  CALL RARRAY('D(ELL.)/Drhovolnorm',(/ FIT0CUB(RC0P), RDEDR /),NISO1EFF1)
  CALL RARRAY('MERCIER BY L.A.TH. WITH E ONLY ',(/ RC0P, RDI /),NISO1EFF1)
  CALL RARRAY('MERCIER SHAFRANOV YOURCHENKO ',(/ RC0P, RSY /),NISO1EFF1)
  CALL RARRAY('Rgeom profile',eqchease_out_add_1d(:,iirgeo),NISO1EFF1)
  CALL RARRAY('a/Rgeom',eqchease_out_add_1d(:,iiamin)/eqchease_out_add_1d(:,iirgeo),NISO1EFF1)
  CALL RARRAY('VOLUME profile',eqchease_out(1)%profiles_1d%volume,NISO1EFF1)
  CALL RARRAY('RHO_VOL_NORM',eqchease_out(1)%profiles_1d%rho_vol,NISO1EFF1)
  CALL RARRAY('RHO_TOR_NORM',(/RC0P, CSMTOR /),NISO1EFF1)
  CALL RARRAY('RHO_TOR=sqrt(Phi/pi/B0)',eqchease_out(1)%profiles_1d%rho_tor,NISO1EFF1)
  CALL RARRAY('1/Rgeom(a) dpsi/drhotor', &
       & eqchease_out(1)%profiles_1d%dpsidrho_tor(1:NISO1EFF1)/eqchease_out_add_1d(NISO1EFF1,iirgeo),NISO1EFF1)
  CALL RARRAY('Ip profile',eqchease_out_add_1d(:,iiIplas),NISO1EFF1)
  CALL RARRAY('li profile',eqchease_out(1)%profiles_1d%li,NISO1EFF1)
  CALL RARRAY('ALPHA1=-2 MU0 Rgeom(edge) Q(rho)**2 / Bphi(edge)**2 * DP/DRHOTOR(rho)',eqchease_out_add_1d(:,iialpha),NISO1EFF1)
  zdummy(1:NISO1EFF1) = -2._rkind * eqchease_out(1)%profiles_1d%q(1:NISO1EFF1)**2 * (/ RC0P, RJ7s(1:NISO1EFF,8)/RJ5(1:NISO1EFF) /) &
       & * eqchease_out(1)%profiles_1d%pprime(1:NISO1EFF1)
  CALL RARRAY('ALPHA2=-2 q**2 <R |grad psi|/B**2> dp/dpsi',zdummy,NISO1EFF1)
  call interpos(eqchease_out(1)%profiles_1d%r_outboard,eqchease_out(1)%profiles_1d%pressure,NISO1EFF1,YOUTP=zdummy)
  zdummy(1:NISO1EFF1)=-2._rkind * eqchease_out(1)%profiles_1d%area(1:NISO1EFF1)/pi &
    & * (/ RC0P, RJ7s(1:NISO1EFF,7)/RJ5(1:NISO1EFF) /) * eqchease_out(1)%profiles_1d%pprime(1:NISO1EFF1)
  CALL RARRAY('ALPHA3=-2 Area/pi <1/Bpol> dp/dpsi',zdummy,NISO1EFF1)
  call interpos(eqchease_out(1)%profiles_1d%r_inboard(NISO1EFF1:1:-1),eqchease_out(1)%profiles_1d%pressure,NISO1EFF1,YOUTP=zdummy)
  zdummy(1:NISO1EFF1)=-2._rkind * eqchease_out(1)%profiles_1d%area(1:NISO1EFF1)/pi &
    & / sqrt((/ RC1P, RJ6(1:NISO1EFF)/RJ5(1:NISO1EFF) /)) * eqchease_out(1)%profiles_1d%pprime(1:NISO1EFF1)
  CALL RARRAY('ALPHA4=ALPHA3 with 1/sqrt(<Bpol**2>)',zdummy,NISO1EFF1)
  zdummy(1:NISO1EFF1)=-eqchease_out(1)%profiles_1d%pprime(1:NISO1EFF1) /pi * (/RIVOL(1), RIVOL(1:NISO1EFF)/) * &
       & sqrt(eqchease_out(1)%profiles_1d%volume(1:NISO1EFF1)/twopi/pi/eqchease_out_add_1d(1:NISO1EFF1,iirgeo))
  CALL RARRAY('alphaGroebner=-dp/dpsi * dV/dpsi /2/pi**2 * sqrt(V/2pi**2/Rgeom)',zdummy,NISO1EFF1)

  CALL RARRAY('dVdpsi',eqchease_out(1)%profiles_1d%vprime,NISO1EFF1)
  CALL RARRAY('dVdrho_tor',eqchease_out(1)%profiles_1d%dvdrho,NISO1EFF1)
  zdummy(1:NISO1EFF1)=eqchease_out(1)%profiles_1d%vprime(1:NISO1EFF1) * eqchease_out(1)%profiles_1d%q(1:NISO1EFF1) / &
       & eqchease_out(1)%profiles_1d%rho_tor(1:NISO1EFF1) * eqchease_out(1)%profiles_1d%dpsidrho_tor / &
       & (eqchease_out(1)%profiles_1d%f_dia(1:NISO1EFF1)/eqchease_out_add_1d(NISO1EFF1,iirgeo))
  zdummy(1)=RC0p
  CALL RARRAY('dVdrhotor*qoB0rhotor',zdummy,NISO1EFF1)

  call interpos(eqchease_out(1)%profiles_1d%r_outboard,eqchease_out_add_1d(:,iirgeo)-eqchease_out_add_1d(NISO1EFF1,iirgeo), &
    & NISO1EFF1,YOUTP=zdummy)
  CALL RARRAY('shiftprime=dD/dRLFS',zdummy,NISO1EFF1)
  CALL RARRAY('Bmin profile',eqchease_out_add_1d(:,iiBmin),NISO1EFF1)
  CALL RARRAY('Bmax',eqchease_out_add_1d(:,iiBmax),NISO1EFF1)
  CALL RARRAY('R_INBOARD',eqchease_out(1)%profiles_1d%r_inboard,NISO1EFF1)
  CALL RARRAY('R_OUTBOARD',eqchease_out(1)%profiles_1d%r_outboard,NISO1EFF1)
  CALL RARRAY('elongation',eqchease_out(1)%profiles_1d%elongation,NISO1EFF1)
  CALL RARRAY('delta_bottom',eqchease_out(1)%profiles_1d%tria_lower,NISO1EFF1)
  CALL RARRAY('delta_upper',eqchease_out(1)%profiles_1d%tria_upper,NISO1EFF1)
  CALL RARRAY('area profile',eqchease_out(1)%profiles_1d%area,NISO1EFF1)
  !
  !     EXTRA DIAGNOSTICS OUTPUT LINKED TO ROUTINES GLOADD AND OUTGLOAD
  !
  if (NDIAGOP .GE. 0) then
    DO I=1,3
      CALL OUTGLOAD(I,NFLGADDIA(I))
    END DO
  end if
  !
  !------- SECTION ADDED TO MATCH WITH GA'S EQUILIBRIUM CODE ---------

  NGA = 3
  OPEN(NGA,FILE='NGA',FORM='FORMATTED')
  REWIND(NGA)
  WRITE(NGA,*) NISO1EFF
  WRITE(NGA,*) SPSIM
  CALL OARRAY(NGA,'CSM - MESH',SMISO,NISO1EFF)
  CALL OARRAY(NGA,'P ',CPR,NISO1EFF)
  CALL OARRAY(NGA,'DP/DPSI',CPPR,NISO1EFF)
  CALL OARRAY(NGA,'Q ',QPSI,NISO1EFF)
  CLOSE(NGA)
  !
  !-------------------------------------------------------------------
  !
  IF (NRFP .EQ. 1) THEN
     !
     CALL RARRAY('SURFACE AVERAGED POLOIDAL MAGNETIC FIELD',RFPBP,NISO1EFF)
     !
  ENDIF
  !
  IF (NPROFZ .EQ. 1) THEN 
     !
     CALL RARRAY('PLASMA DENSITY',DENSTY,NISO1EFF)
     CALL RARRAY('PLASMA TEMPERATURE',TEMPER,NISO1EFF)
     !
  ENDIF
  !
  RETURN
  !
80 CONTINUE
  !
  WRITE(6,2500)
  !
  CALL RARRAY('CHI - VALUES',CHI,NCHI1)
  CALL RARRAY('CHIM - VALUES',CHIM,NCHI)
  !
  RETURN
  !
90 CONTINUE
  !
  CALL RARRAY('IDEAL MERCIER',(/RC0P, SMERCI /),NISO1EFF1)
  CALL RARRAY('RESISTIVE INTERCHANGE ',(/RC0P, SMERCR /),NISO1EFF1)
  CALL RARRAY('H OF GLASSER, GREENE & JOHNSON ',(/RC0P, HMERCR /),NISO1EFF1)
  CALL RARRAY('integral[B**2/R**2/Bp**2 dlp/Bp]',(/ RC0P, RJ7s(1:NISO1EFF,10) /),NISO1EFF1)
  ! jB=-G dp/dpsi - dG/dpsi B**2 / mu0
  zdummy2(1:NISO1EFF1) = - eqchease_out(1)%profiles_1d%f_dia(1:NISO1EFF1) * eqchease_out(1)%profiles_1d%pprime(1:NISO1EFF1) &
    & * (/ RC0P, RJ2(1:NISO1EFF)/) - eqchease_out(1)%profiles_1d%ffprime(1:NISO1EFF1) / &
    & eqchease_out(1)%profiles_1d%f_dia(1:NISO1EFF1) *(/ RC0P, RJ7s(1:NISO1EFF,10) /)
  CALL RARRAY('int(Connor)=int(jB/|grad psi|**2 dlp/Bp)',zdummy2,NISO1EFF1)
  zdummy(1:NISO1EFF1) = RC1P + RC1P/CPI/(/DQDP0, CDQ(1:NISO1EFF)/) * zdummy2(1:NISO1EFF1)
  CALL RARRAY('1+1/pi/qprime*int(Connor)',zdummy,NISO1EFF1)
  zdummy(1)=RC0P
  DO I=2,NISO1EFF1
    if (RC1P - 4._rkind*SMERCI(I-1) .GE. RC0P) then
      zdummy(I) = sqrt(RC1P - 4._rkind*SMERCI(i-1))
    else
      zdummy(I)=-RC1P
    end if
  end DO
  CALL RARRAY('sqrt(1-4 DI)',zdummy,NISO1EFF1)
  CALL IVAR('NTURN',NTURN)
  CALL IARRAY('NCBAL',(/0, NCBAL /),NISO1EFF1)
  CALL RARRAY('CHI0 VALUES',CHI0,NBLC0)
  !
  IF (NBLC0 .LE. 1) RETURN
  IF (.TRUE.) RETURN
  !
  DO J151=1,NBLC0
     !
     IBL = ISSUM(NISO1EFF,NCBLNS(1,J151),1)
     !
     IF (IBL .NE. 0) THEN
        !
        CALL RVAR('CHI0',CHI0(J151))
        CALL IARRAY('NCBAL(CHI0)',NCBLNS(1,J151),NISO1EFF)
        !
     ENDIF
     !
  END DO
  !
  RETURN
  !
100 CONTINUE
  !
  WRITE(6,2301)
  !
  CALL RVAR('POLOIDAL BETA      ',BETAP)
  CALL RVAR('TOROIDAL BETA      ',BETA )
  CALL RVAR('BETA STAR          ',BETAS)
  CALL RVAR('TOROIDAL BETA EXP. ',BETAX)
  !
  WRITE(6,2302)
  !
  CALL RVAR('PRESSURE ON AXIS   ',CP0  )
  CALL RVAR('DP/DPSI ON AXIS    ',DPDP0)
  CALL RVAR('IB.S./ITOT (NUE*=0, all ne,Te)',RITBS/RITOT)
  CALL RVAR('IB.S./ITOT (NUE*.NE.0, all ne,Te)',RITBSC/RITOT)
  CALL RVAR('IB.S./ITOT (NUE*, p*ne''/ne,..)',RITBSC2/RITOT)
  CALL RVAR('IB.S./ITOT (NUE*, L31 p'')',RITBSC3/RITOT)
  !
  CALL RARRAY('DP/DPSI',CPPR,NPPR+1)
  CALL RARRAY('Q',QPSI,NPPR+1)
  CALL RARRAY('DQ/DPSI',CDQ,NPPR+1)
  !
  IF (NBLOPT .NE. 0) THEN
     !
     CALL IARRAY('N2BAL',N2BAL,NPPR+1)
     CALL RARRAY('MERCIER',SMERCI,NPPR+1)
     CALL IARRAY('NCBAL',NCBAL,NPPR+1)
     CALL RARRAY('CHI0 VALUES',CHI0,NBLC0)
     !
     IF (NBLC0 .LE. 1) RETURN
     !
     DO J171=1,NBLC0
        IBL = ISSUM(NPPR+1,NCBLNS(1,J171),1)
        IF (IBL .NE. 0) THEN
           CALL RVAR('CHI0',CHI0(J171))
           CALL IARRAY('NCBAL(CHI0)',NCBLNS(1,J171),NPPR+1)
        ENDIF
     END DO
  ENDIF
  !
  IF (NBSOPT .NE. 0) THEN
     DO J172=1,NPPR+1
        ZJDIFF(J172) = RJPAR(J172) - RJBSOS(J172,2)
     END DO
     CALL RARRAY('RFCIRC',RFCIRC,NPPR+1)
     CALL RARRAY('RB2AV',RB2AV,NPPR+1)
     CALL RARRAY('BS-CURRENT (ZERO COLL. and all from from ne, etc)',RJBSOS(1,1),NISO1EFF)
     CALL RARRAY('BS-CURRENT, all from ne, Te, Zeff, Ti ',RJBSOS(1,2),NISO1EFF)
     CALL RARRAY('BS-CURRENT, p from equil, gradients from ne'', etc',RJBSOS(1,3),NISO1EFF)
     CALL RARRAY('BS-CURRENT, p and p'' L31 from equil, rest from Te, Ti',RJBSOS(1,4),NISO1EFF)
     CALL RARRAY('J-PARALLEL (<j.B>Eq.43)',RJPAR,NPPR+1)
     CALL RARRAY('J-PAR - J-BS(all from ne, Te,...)',ZJDIFF,NPPR+1)
  ENDIF
  !
  RETURN
  !
1001 FORMAT(///,1X,'******************', &
       &          //,1X,'NAMELIST VARIABLES', &
       &          //,1X,'******************',//)
1100 FORMAT(///,1X,'******************************', &
       &           //,1X,'FINAL OUTPUT : PSI SOLUTION   ', &
       &           //,1X,'VALUES ON (SIGMA, THETA) NODES', &
       &           //,1X,'******************************')
1201 FORMAT(//,1X,' NS = ',I4,4X,' NT = ',I4,4X, &
       &                ' NPSI = ',I4,4X,' NCHI = ',I4,4X, &
       &                ' NINSCA = ',I4)
1202 FORMAT(//,1X,' NS = ',I4,4X,' NT = ',I4,4X, &
       &                ' NPSI = ',I4,4X,' NCHI = ',I4)
1203 FORMAT(//,1X,' NS = ',I4,4X,' NT = ',I4,4X,' NISO = ',I4,4X, &
       &                ' NPSI = ',I4,4X,' NCHI = ',I4,4X, &
       &                ' NINSCA = ',I4,4X,' NINMAP = ',I4)
1250 FORMAT(6X,'PSIMIN = ',1PE20.12,3X,'RMAG =   ',1PE20.12, &
       &          3X,'ZMAG =   ',1PE20.12)
1300 FORMAT(73X,'MITER = ',I3,3X,'RESIDU = ',F13.10,3X, &
       &              'EPSLON = ',F13.10)
1400 FORMAT(/,1X,'CHECK THE FIRST POINT ON MAGNETIC FLUX SURFACES')
2050 FORMAT(///,1X,'*****************************************', &
       &           //,1X,'POSITION OF THE CALCULATION MESH CENTER :', &
       &              1X,'R0 = ',1PE16.8,3X,'Z0 = ',1PE16.8, &
       &           //,1X,'*****************************************')
2100 FORMAT(/,1X,'     *****   ITERATION OVER THE SHIFT ', &
       &               'NOT CONVERGED')
2150 FORMAT(/,1X,'COMPARE CURRENTS :', &
       &          /,35X,'(1) FROM SOURCE TERM   =  ',1PE13.5, &
       &          /,35X,'(2) FROM LINE INTEGRAL =  ',1PE13.5, &
       &            10X,'--->  RATIO  (1)/(2)  :  ',1PE13.5)
2300 FORMAT(///,1X,'*******************************************', &
       &           //,1X,'QUANTITIES COMPUTED IN MAPPIN FROM SOLUTION', &
       &           //,1X,'*******************************************')
2301 FORMAT(///,1X,'**************************', &
       &           //,1X,'VOLUME AVERAGED QUANTITIES', &
       &           //,1X,'**************************')
2302 FORMAT(///,1X,'****************************************', &
       &           //,1X,'QUANTITIES EXTRAPOLATED ON MAGNETIC AXIS', &
       &           //,1X,'****************************************')
2350 FORMAT(///,1X,'*********************************', &
       &         //,1X,'FUNCTIONS OF S = SQRT(PSI/PSIMIN) ', &
       &         //,1X,'VALUES AT MIDDLE POINTS IN S MESH', &
       &         //,1X,'*********************************')
2400 FORMAT(///,1X,'**************************', &
       &         //,1X,'SIZES OF COMMONS FOR CLEAR', &
       &         //,1X,'**************************')
2500 FORMAT(///,1X,'*******************************************', &
       &           //,1X,'QUANTITIES COMPUTED IN MAPPIN TO FEED ERATO', &
       &           //,1X,'*******************************************')
  !
END SUBROUTINE OUTPUT
