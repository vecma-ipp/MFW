SUBROUTINE GLOQUA(PSM,KPSM,KCASE)
  !        ##################################
  !
  !**********************************************************************
  !
  ! GLOBAL EQUILIBRIUM QUANTITIES IN TABLE 1 OF PUBLICATION
  ! VARIOUS QUANTITITES ON PSM MESH WHICH HAVE NOT BEEN CALCULATED IN CHIPSIMETRICS
  ! COMPUTE ON WHOLE PROFILE AND DO INTERPOLATION ON PS MESH IF NEEDED
  !
  ! THIS HAS BEEN CHANGED SO THAT QUANTITIES ARE CALCULATED ON THE WHOLE MESH
  ! SAME AS THE ONE USED IN SURFACE AND CHIPSIMETRICS (FOR MOST NIDEAL CASES)
  !
  ! routine SURFACE computes on mesh psiiso and SMISO(1:NISO1EFF)
  ! This routine computes on mesh [0 SMISO]=SMISOP1(1:NISO1EFF1)
  !
  ! CS REMOVED SINCE ALL COMPUTED ON SINGLE PSM MESH
  !**********************************************************************
  !
  USE globals
  USE interpol
  use interpos_module
  IMPLICIT NONE
  !
  integer :: IEXTRA_NT
  INTEGER, INTENT(IN) :: KPSM, KCASE
  REAL(RKIND), INTENT(IN) :: PSM(KPSM)
  !
  REAL(RKIND) :: ZPSISM(KPSM) &
       & ,ZDUMMY1(KPSM+1), ZDUMMY2(KPSM+1), ZPSISMP1(KPSM+1), ZIP(KPSM+1) &
       & ,SIGMA(KPSM+1), ZPHICS(KPSM+1), ZJBS2(KPSM+1), ZBET(KPSM+1), &
       & ZPPR(KPSM+1)
  REAL(RKIND) :: TENS_DEF, ZIP2, ZIB2, ZS1, ZS2, ZS3, ZF1, ZF2, ZF3, ZS95, ZBMAG
  INTEGER :: I, J10, JQMIN, J1, JQ95, KPSM1

  REAL(RKIND)      ::     FIT0CUB, XX(1:4), YY(1:4), XXX
  FIT0CUB(XXX) = FCCCC0(YY(1),YY(2),YY(3),YY(4),XX(1),XX(2),XX(3),XX(4),XXX)

  !     
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !
  DO J10=1,KPSM
    !     1) INTERPOLATE J5 TO OBTAIN J5' REQUIRED FOR LOCAL INTERCHANGE
    !     STABILITY CRITERIA.     
    !         I1 = J10
    !         IF (j10.eq.1)  I1 = 2
    !         IF (j10.eq.KP) I1 = KP-1
    !         RJ5P(J10) = FQQQ1(RJ5(I1-1),RJ5(I1),RJ5(I1+1),
    !     ,                     PSM(I1-1),PSM(I1),PSM(I1+1),
    !     ,                     PSM(J10))
    !         RJ5P(J10) = RJ5P(J10) / (2.*PSM(J10)*CPSRF)
    !         CDQ(J10) = FQQQ1(QPSI(I1-1),QPSI(I1),QPSI(I1+1),
    !     ,                    PSM(I1-1),PSM(I1),PSM(I1+1),
    !     ,                    PSM(J10))
    !         CDQ(J10) = CDQ(J10) / (2.*PSM(J10)*CPSRF)
    !     2) COMPUTE MERCIER PARAMETER -D_I (EQ.(19) IN CHEASE PAPER)
    SMERCI(J10) = (CPPR(J10)*TMF(J10)*RJ2(J10)/CDQ(J10) - .5_RKIND)**2 + &
         &                  CPPR(J10) * (RJ5P(J10) - CPPR(J10) * RJ3(J10)) * &
         &                 (TMF(J10)**2 * RJ1(J10) + RJ4(J10)) / CDQ(J10)**2
    !     3) COMPUTE H OF GREENE, GLASSER, JOHNSON (EQ.(21) IN CHEASE PAPER)
    HMERCR(J10) = TMF(J10) * CPPR(J10) / CDQ(J10) * &
         &                 (RJ2(J10) - RJ5(J10) * (RJ4(J10) + TMF(J10)**2 * &
         &                  RJ1(J10)) / (RJ6(J10) + TMF(J10)**2 * RJ4(J10)))
    !     4) COMPUTE RESISTIVE INTERCHANGE PARAMETER -D_R 
    !        (EQ.(20) IN CHEASE PAPER)
    SMERCR(J10) = SMERCI(J10) - (HMERCR(J10) - .5_RKIND)**2
  END DO
  !
  IF (KCASE .EQ. 2) return
  !
  KPSM1 = KPSM + 1
  !
  ! Quantities which can be computed directly from surface results (and where in chipsimetrics before)
  eqchease_out(1)%profiles_1d%psi = eqchease_out(1)%coord_sys%grid%dim1
  eqchease_out(1)%profiles_1d%F_dia = (/ T0, tmf(1:KPSM) /)
  eqchease_out(1)%profiles_1d%pressure = (/ CP0, cpr(1:KPSM) /)
  eqchease_out(1)%profiles_1d%ffprime = (/ DTTP0, ttp(1:KPSM) /)
  eqchease_out(1)%profiles_1d%pprime = (/ DPDP0, cppr(1:KPSM) /)
  eqchease_out(1)%profiles_1d%q = (/ q0, qpsi(1:KPSM) /)
  ! jphi=<jphi/R>/<1/R>
  eqchease_out(1)%profiles_1d%jphi = (/ RIPR0, ripr(1:KPSM) /)
  eqchease_out(1)%profiles_1d%ftrap = (/ 0._rkind, (1._rkind - RFCIRC(1:KPSM)) /)
  eqchease_out(1)%profiles_1d%gm1 = (/ 1./RMAG**2, RJ4(1:KPSM)/RJ5(1:KPSM) /)
  ! eqchease_jparallel = <j.B>/B0=<j.Bchease>/(TMF(a)/Rgeom(a)), Iparallel=<j.B>/R0/<Bphi/R>=<j.Bchease>/TMF(:)/<1/R**2>
  eqchease_out(1)%profiles_1d%jparallel = (/ RJDTB0, rjdotb(1:KPSM) /) * eqchease_out(1)%profiles_1d%F_dia &
       & * eqchease_out(1)%profiles_1d%gm1 / (eqchease_out(1)%profiles_1d%F_dia(KPSM1)/eqchease_out_add_1d(KPSM1,iirgeo))
  ZBMAG = T0/RMAG
  eqchease_out(1)%profiles_1d%gm4 = (/ 1._RKIND/ZBMAG**2, RIBI2(1:KPSM)/RIVOL(1:KPSM) /)
  eqchease_out(1)%profiles_1d%gm5 = (/ ZBMAG**2, RB2AV(1:KPSM) /)
  eqchease_out(1)%profiles_1d%gm8 = (/ RMAG, RJ7s(1:KPSM,4)/RJ5(1:KPSM) /)
  eqchease_out(1)%profiles_1d%gm9 = (/ 1._RKIND/RMAG, RJ7s(1:KPSM,3)/RJ5(1:KPSM) /)
  eqchease_out(1)%profiles_1d%b_av = (/ ZBMAG, RIB(1:KPSM)/RIVOL(1:KPSM) /)
  !
  !  extra 1D quantities: for ogyropsi or other use
  eqchease_out_add_1d(:,iidqdpsi) = (/ DQDP0, cdq(1:KPSM) /)     !dqdpsi
  ! YC: note that the array above is overwritten below by interpos output
  !
  ! Quantities integrated and/or fitted on psi mesh. Use interpos for integral, in this way accurate and integrals on same mesh points
  !
  ZPSISM(1:KPSM) = CPSRF * PSM(1:KPSM)**2
  ZPSISMP1(1:KPSM1) = (/0._rkind, ZPSISM(1:KPSM)/)
  !
  !
  SIGMA=(/1000._RKIND, (REAL(1.,RKIND),I=1,KPSM) /)
  TENS_DEF = -0.1_RKIND
  !
  ! dIBS/dpsi = <j.B> / <B**2> * 2 pi q(psi), as jBS = gam(psi)*B, IBS=int(dpsi) int(dlp/(RBp)) <j.B> / <B**2> * G/R
  ZDUMMY1(2:KPSM1) = TWOPI * QPSI(1:KPSM) / RB2AV(1:KPSM)
  ZDUMMY1(1) = 0._RKIND
  CALL INTERPOS(ZPSISMP1,(/0._RKIND, RJBSOS(1:KPSM,1)/)*ZDUMMY1(1:KPSM1),N=KPSM1,TENSION=TENS_DEF,xscal=ZPSISM(KPSM),& 
       & yscalint=RITBS,sigma=sigma,nbcscal=(/2, 0/), ybcscal=(/0._rkind, 0._rkind/))
  CALL INTERPOS(ZPSISMP1,(/0._RKIND, RJBSOS(1:KPSM,2)/)*ZDUMMY1(1:KPSM1),NIN=KPSM1,TENSION=TENS_DEF,YOUTINT=ZJBS2(1:KPSM1), &
       & sigma=sigma,nbc=(/2, 0/), ybc=(/0._rkind, 0._rkind/))
  RITBSC = ZJBS2(KPSM1)
  ! Assumes ZPSISM(KPSM) is edge value (like cs(npsi1) before
  CALL INTERPOS(ZPSISMP1,(/0._RKIND, RJBSOS(1:KPSM,3)/)*ZDUMMY1(1:KPSM1),N=KPSM1,TENSION=TENS_DEF,xscal=ZPSISM(KPSM), &
       & yscalint=RITBSC2,sigma=sigma,nbcscal=(/2, 0/), ybcscal=(/0._rkind, 0._rkind/))
  CALL INTERPOS(ZPSISMP1,(/0._RKIND, RJBSOS(1:KPSM,4)/)*ZDUMMY1(1:KPSM1),N=KPSM1,TENSION=TENS_DEF,xscal=ZPSISM(KPSM), &
       & yscalint=RITBSC3,sigma=sigma,nbcscal=(/2, 0/), ybcscal=(/0._rkind, 0._rkind/))
  !
  CALL INTERPOS(ZPSISMP1,eqchease_out(1)%profiles_1d%q,KPSM1,TENSION=TENS_DEF,YOUT=ZDUMMY1(1:KPSM1), &
       & YOUTPP=ZDUMMY2(1:KPSM1),YOUTINT=eqchease_out(1)%profiles_1d%phi(1:KPSM1), &
       & nbc=(/2, 2/), ybc=(/ eqchease_out(1)%profiles_1d%q(1), eqchease_out(1)%profiles_1d%q(KPSM1) /) )
  eqchease_out(1)%profiles_1d%phi(1:KPSM1) = eqchease_out(1)%profiles_1d%phi(1:KPSM1) * TWOPI
  CTORSRF = eqchease_out(1)%profiles_1d%phi(KPSM1)
  CSMTOR(1:KPSM-1) = SQRT(eqchease_out(1)%profiles_1d%phi(2:KPSM)/CTORSRF)
  CSMTOR(KPSM) = 1._RKIND
  ! assume PHI normalized by R0^2 B0 and rho_tor=sqrt(Phi/pi/B0) with same B0, thus do not use Bvacuum(Rgeom(edge)) as before version 788
  eqchease_out(1)%profiles_1d%rho_tor(1:KPSM1) = sqrt(eqchease_out(1)%profiles_1d%phi(1:KPSM1)  / PI )
  !dpsicheasedrhotor=B0*rhotor/q (B0 disappears in CHEASE units) (to convert d/dpsi in dprime = d/drho_tor=d/dpsi * dpsi/drho_tor)
  eqchease_out(1)%profiles_1d%dpsidrho_tor(1:KPSM1) = eqchease_out(1)%profiles_1d%rho_tor(1:KPSM1) / eqchease_out(1)%profiles_1d%q(1:KPSM1)
  CALL INTERPOS(eqchease_out(1)%profiles_1d%rho_tor(1:KPSM1),eqchease_out(1)%profiles_1d%dpsidrho_tor(1:KPSM1), &
       & nin=KPSM1,TENSION=-1._rkind,YOUT=eqchease_out(1)%profiles_1d%dpsidrho_tor(1:KPSM1),nbc=(/2, 0/), ybc=(/RC0P, RC0P/))
  eqchease_out(1)%profiles_1d%dpsidrho_tor(1) = RC0P ! Avoid round-off errors

  ! alpha= -2 MU0 Rgeom(edge) Q(rho)**2 / B0**2 * DP/DRHOTOR(rho)
  eqchease_out_add_1d(:,iialpha) = - 2._rkind * eqchease_out_add_1d(KPSM1,iirgeo) * eqchease_out(1)%profiles_1d%q**2 &
       & / (TMF(KPSM)/eqchease_out_add_1d(KPSM1,iirgeo))**2 * (/DPDP0, CPPR(1:KPSM) /) * eqchease_out(1)%profiles_1d%dpsidrho_tor(:)

  ! Calculate dV/dpsi=Vprime. Add point at s=0 since int(0)=0, but dV/dpsi(0) is not 0.
  IF (NVERBOSE .GE. 4) print *,'ZPSISMP1, RIVOL, profiles_1d%vprime, profiles_1d%volume'
  CALL INTERPOS(ZPSISMP1,(/RIVOL(1), RIVOL(1:KPSM)/),nin=KPSM1,TENSION=TENS_DEF,YOUT=eqchease_out(1)%profiles_1d%vprime, &
       & YOUTPP=ZDUMMY2(1:KPSM1),YOUTINT=eqchease_out(1)%profiles_1d%volume,SIGMA=SIGMA,nbc=(/0, 2/), ybc=(/RC0P, RIVOL(KPSM)/))
  ! CHEASE misses 2 pi in RIVOL definition
  eqchease_out(1)%profiles_1d%vprime = TWOPI * eqchease_out(1)%profiles_1d%vprime
  eqchease_out(1)%profiles_1d%volume = TWOPI * eqchease_out(1)%profiles_1d%volume(1:KPSM1)
  ! Calculate dV/drho_tor (use dV/drho_tor(0)=0)
  CALL INTERPOS(eqchease_out(1)%profiles_1d%rho_tor(1:KPSM1),eqchease_out(1)%profiles_1d%volume(1:KPSM1), &
       & nin=KPSM1,TENSION=TENS_DEF,YOUTP=eqchease_out(1)%profiles_1d%dvdrho, &
       & nbc=(/1, 2/), ybc=(/RC0P, eqchease_out(1)%profiles_1d%volume(KPSM1)/))
  IF (NVERBOSE .GE. 4) write(*,'(i3,1p4e15.6)') &
       & (i,ZPSISMP1(i),RIVOL(i),eqchease_out(1)%profiles_1d%vprime(i),eqchease_out(1)%profiles_1d%volume(i),i=1,1)
  IF (NVERBOSE .GE. 4) write(*,'(i3,1p4e15.6)') &
       & (i,ZPSISMP1(i),RIVOL(i-1),eqchease_out(1)%profiles_1d%vprime(i),eqchease_out(1)%profiles_1d%volume(i),i=2,KPSM1)
  eqchease_out(1)%profiles_1d%rho_vol(1:KPSM) = &
    & sqrt(eqchease_out(1)%profiles_1d%volume(1:KPSM)/eqchease_out(1)%profiles_1d%volume(KPSM1))
  eqchease_out(1)%profiles_1d%rho_vol(KPSM1) = 1._RKIND
  !
  CALL INTERPOS(ZPSISMP1,(/RARE(1), RARE(1:KPSM)/),nin=KPSM1,TENSION=TENS_DEF,YOUT=eqchease_out(1)%profiles_1d%aprime, &
       & YOUTINT=eqchease_out(1)%profiles_1d%area,SIGMA=SIGMA,nbc=(/0, 2/), ybc=(/RC0P, RARE(KPSM)/))
  AREA = eqchease_out(1)%profiles_1d%area(KPSM1)
  !
  CALL INTERPOS(ZPSISMP1,(/0._rkind, CPPR(1:KPSM)/)*eqchease_out(1)%profiles_1d%area(1:KPSM1) &
       & ,KPSM1,TENSION=TENS_DEF,YOUTINT=ZBET(1:KPSM1),SIGMA=SIGMA,nbc=(/2, 0/), ybc=(/0._rkind, 0._rkind/))
  !
  eqchease_out_add_1d(1:KPSM1,iiIplas) = (/RC0P, RIIR(1:KPSM)/)
  eqchease_out(1)%profiles_1d%beta_pol(1:KPSM1)  = -8._RKIND * cpi * ZBET(1:KPSM1) &
       & / eqchease_out_add_1d(1:KPSM1,iiIplas)**2 / eqchease_out_add_1d(1:KPSM1,iirgeo)
  XX=SMISOP1(2:5)
  YY=eqchease_out(1)%profiles_1d%beta_pol(2:5)
  eqchease_out(1)%profiles_1d%beta_pol(1) = FIT0CUB(RC0P)
  !
  CALL INTERPOS(ZPSISMP1,(/RC0P, RIP2(1:KPSM)/),KPSM1,TENSION=TENS_DEF,xscal=ZPSISM(KPSM),yscalint=ZIP2,SIGMA=SIGMA, &
       & nbcscal=(/2, 2/), ybcscal=(/0._rkind, RIP2(KPSM)/))
  CALL INTERPOS(ZPSISMP1,(/RC0P, RIP(1:KPSM)/),KPSM1,TENSION=TENS_DEF,YOUTINT=ZIP(1:KPSM1),SIGMA=SIGMA, &
       & nbc=(/2, 2/), ybc=(/0._rkind, RIP(KPSM)/),option=-12)
  CALL INTERPOS(ZPSISMP1,(/RC0P, RIB2(1:KPSM)/),KPSM1,TENSION=TENS_DEF,xscal=ZPSISM(KPSM),yscalint=ZIB2,SIGMA=SIGMA, &
       & nbcscal=(/2, 2/), ybcscal=(/0._rkind, RIB2(KPSM)/))
  !
  CALL INTERPOS(ZPSISMP1,eqchease_out_add_1d(1:KPSM1,iiIplas),nin=KPSM1,TENSION=TENS_DEF,YOUTINT=eqchease_out(1)%profiles_1d%li &
       & ,SIGMA=SIGMA,nbc=(/2, 2/), ybc=(/eqchease_out_add_1d(1,iiIplas), eqchease_out_add_1d(KPSM1,iiIplas)/))
  eqchease_out(1)%profiles_1d%li(2:KPSM1) = 4._RKIND*CPI * eqchease_out(1)%profiles_1d%li(2:KPSM1) &
       & / eqchease_out_add_1d(2:KPSM1,iirgeo) / eqchease_out_add_1d(2:KPSM1,iiIplas)**2
  XX=SMISOP1(2:5)
  YY=eqchease_out(1)%profiles_1d%li(2:5)
  eqchease_out(1)%profiles_1d%li(1)=FIT0CUB(RC0P)
  CALL INTERPOS(sqrt(ZPSISMP1),eqchease_out(1)%profiles_1d%li,nin=KPSM1,TENSION=TENS_DEF,YOUT=eqchease_out(1)%profiles_1d%li, &
       & YOUTPP=ZDUMMY2(1:KPSM1),SIGMA=SIGMA,nbc=(/1, 2/),ybc=(/0._rkind, eqchease_out(1)%profiles_1d%li(KPSM1) /))
  ! To check how to get value on-axis, with interpos or else.
  eqchease_out(1)%profiles_1d%gm2(2:KPSM1) = RJ6(1:KPSM)/RJ5(1:KPSM) / eqchease_out(1)%profiles_1d%dpsidrho_tor(2:KPSM1)**2
  eqchease_out(1)%profiles_1d%gm3(2:KPSM1) = RJ7s(1:KPSM,1)/RJ5(1:KPSM) / eqchease_out(1)%profiles_1d%dpsidrho_tor(2:KPSM1)**2
  eqchease_out(1)%profiles_1d%gm6(2:KPSM1) = RJ7s(1:KPSM,6)/RJ5(1:KPSM) / eqchease_out(1)%profiles_1d%dpsidrho_tor(2:KPSM1)**2
  eqchease_out(1)%profiles_1d%gm7(2:KPSM1) = RJ7s(1:KPSM,2)/RJ5(1:KPSM) / eqchease_out(1)%profiles_1d%dpsidrho_tor(2:KPSM1)
  XX=SMISOP1(2:5)
  YY=eqchease_out(1)%profiles_1d%gm2(2:5)
  eqchease_out(1)%profiles_1d%gm2(1) = FIT0CUB(RC0P)
  YY=eqchease_out(1)%profiles_1d%gm3(2:5)
  eqchease_out(1)%profiles_1d%gm3(1) = FIT0CUB(RC0P)
  YY=eqchease_out(1)%profiles_1d%gm6(2:5)
  eqchease_out(1)%profiles_1d%gm6(1) = FIT0CUB(RC0P)
  YY=eqchease_out(1)%profiles_1d%gm7(2:5)
  eqchease_out(1)%profiles_1d%gm7(1) = FIT0CUB(RC0P)

  VOLUME  = eqchease_out(1)%profiles_1d%volume(KPSM1)
  CPBAR   = ZIP(KPSM1) / VOLUME * TWOPI
  RITOT   = eqchease_out_add_1d(KPSM1,iiIplas)
  RINOR   = RITOT / (ASPCT * TMF(KPSM))
  BETA    = 2._RKIND * ZIP(KPSM1) / ZIB2
  
  BETAP   = eqchease_out(1)%profiles_1d%beta_pol(KPSM1)
  BETAS   = 2._RKIND * SQRT(ZIP2 * VOLUME/TWOPI) /  ZIB2
  BETAX   = 2._RKIND * CPBAR * ( eqchease_out_add_1d(KPSM1,iirgeo)/TMF(KPSM) )**2
  CONVF   = 0.5_RKIND * RLENG(KPSM)**2 *  eqchease_out_add_1d(KPSM1,iirgeo)/ VOLUME
  RIBSNOR = RITBS / (ASPCT * TMF(KPSM))
  !
  IF (CPBAR .EQ. 0._RKIND) THEN
    CPPF   = 0._RKIND
  ELSE
    CPPF   = CP0 / CPBAR
  ENDIF
  !
  DPRIME(1:KPSM) = 0.5_RKIND * eqchease_out(1)%profiles_1d%li(2:KPSM1) + eqchease_out(1)%profiles_1d%beta_pol(2:KPSM1)
  !
  ! ITM shear is with rhotor, historically CHEASE closer to shear with rhovol but changed to rhotor since March 2015 and version 4.10b of CPO including shear
  CALL INTERPOS(eqchease_out(1)%profiles_1d%rho_tor,eqchease_out(1)%profiles_1d%q,KPSM1,TENSION=TENS_DEF, &
       & YOUT=ZDUMMY1(1:KPSM1),YOUTP=eqchease_out_add_1d(:,iishear), &
       & nbc=(/2, 2/), ybc=(/eqchease_out(1)%profiles_1d%q(1), eqchease_out(1)%profiles_1d%q(KPSM1)/))
  eqchease_out_add_1d(:,iishear) = eqchease_out(1)%profiles_1d%rho_tor * eqchease_out_add_1d(:,iishear) &
    & / eqchease_out(1)%profiles_1d%q
  CDRQ(1:KPSM) = eqchease_out_add_1d(2:KPSM1,iishear)
  !
  ! eqchease_out(1)%profiles_1d%shear(1:KPSM1) = eqchease_out_add_1d(1:KPSM1,iishear)
  !
  CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out_add_1d(:,iishear),KPSM1,TENSION=TENS_DEF, &
       & YOUTP=eqchease_out_add_1d(:,iidsheardpsi))
  !
  CALL INTERPOS(eqchease_out(1)%profiles_1d%psi,eqchease_out(1)%profiles_1d%q,KPSM1,TENSION=TENS_DEF, &
       & YOUTP=eqchease_out_add_1d(:,iidqdpsi),YOUTPP=eqchease_out_add_1d(:,iid2qdpsi2), &
       & nbc=(/2, 2/), ybc=(/eqchease_out(1)%profiles_1d%q(1), eqchease_out(1)%profiles_1d%q(KPSM1) /))
  !
  CALL INTERPOS(eqchease_out(1)%profiles_1d%rho_vol,(/RELL(1), RELL(1:KPSM)/),KPSM1,TENSION=TENS_DEF, &
       & YOUT=ZDUMMY2(1:KPSM1),YOUTP=RDEDR(1:KPSM1),SIGMA=SIGMA)
  !
  CALL INTERPOS(eqchease_out(1)%profiles_1d%rho_vol,(/CPR(1), CPR(1:KPSM)/),NIN=KPSM1,TENSION=TENS_DEF,YOUTP=ZPPR(1:KPSM1), &
    & SIGMA=SIGMA)
  RDI(1:KPSM) = .25_RKIND + 2._RKIND * eqchease_out(1)%profiles_1d%rho_vol(2:KPSM1) * ZPPR(1:KPSM) / (T0*CDRQ(1:KPSM))**2 * &
    &           (1._RKIND - eqchease_out(1)%profiles_1d%q(2:KPSM1)**2 * (1._RKIND + 1.5_RKIND*DPRIME(1:KPSM)*RDEDR(1:KPSM) - &
    &           .75_RKIND * (2._RKIND*ZDUMMY2(2:KPSM1) + eqchease_out(1)%profiles_1d%rho_vol(2:KPSM1) * RDEDR(1:KPSM))))
  RSY(1:KPSM) = .25_RKIND + 2._RKIND * eqchease_out(1)%profiles_1d%rho_vol(2:KPSM1) * ZPPR(2:KPSM1) / (T0*CDRQ(1:KPSM))**2 * &
    &              (1._RKIND - ZDUMMY1(2:KPSM1)**2)
  !
  !     DIAGNOSTICS TYPE 1-3
  if (NDIAGOP .GE. 0) then
    CALL GLOADD(ZPSISMP1(1:KPSM1),ZJBS2(1:KPSM1),ZBET(1:KPSM1),KPSM1,1)
    CALL GLOADD(ZPSISMP1(1:KPSM1),ZJBS2(1:KPSM1),ZBET(1:KPSM1),KPSM1,2)
    CALL GLOADD(ZPSISMP1(1:KPSM1),ZJBS2(1:KPSM1),ZBET(1:KPSM1),KPSM1,3)
!!$    CALL GLOADD(ZPSISMP1(1:KPSM1),ZJBS2(1:KPSM1),ZBET(1:KPSM1),KPSM1,4) ! Note: could not check if works with new smisop1 mesh (0., csm(:)), only if EXPDATA given (to implement new flag)
  end if
  !
  JQMIN = 1
  QMIN = Q0
  ZS95 = SQRT(0.95_RKIND)
  !
  DO J1=1,KPSM
     IF ( QPSI(J1) .LT. QMIN ) THEN
        JQMIN = J1
        QMIN = QPSI(JQMIN)
     ENDIF
     IF ( SMISO(J1) .LT. ZS95 ) THEN
        JQ95 = J1
     ENDIF
  enddo
  !
  CSQMIN=SMISO(JQMIN)
  !
  !     QUADRATIC INTERPOLATION FOR QMIN
  !
  IF (JQMIN.ne.1 .and. JQMIN.ne.KPSM) then
     ZS1 = SMISO(JQMIN-1)
     ZF1 = QPSI(JQMIN-1)
     ZS2 = SMISO(JQMIN)
     ZF2 = QPSI(JQMIN)
     ZS3 = SMISO(JQMIN+1)
     ZF3 = QPSI(JQMIN+1)
     CSQMIN = - FB1(ZF1,ZF2,ZF3,ZS1,ZS2,ZS3) / FB2(ZF1,ZF2,ZF3,ZS1,ZS2,ZS3) * 0.5_RKIND
     QMIN = FQQQ0(ZF1,ZF2,ZF3,ZS1,ZS2,ZS3,CSQMIN)
  endif
  !
  !     QUADRATIC INTERPOLATION FOR Q95
  !
  Q95 = FQQQ0(QPSI(JQ95-1),QPSI(JQ95),QPSI(JQ95+1),SMISO(JQ95-1),SMISO(JQ95),SMISO(JQ95+1),ZS95)
  !
  RETURN
END SUBROUTINE GLOQUA
