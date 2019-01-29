SUBROUTINE chipsimetrics(KP,ps,npmax)
  !        #####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! Compute flux surface quantities, etc
  !                                                                     *
  !**********************************************************************
  ! (Same comments as in chipsi.f90, note that chipsi is called only before doing the calculation of ERATO or others quantities. Now chipsimetrics is called directly after surface.f90 in mappin.f90, so calculated on each surface. Therefore tetchi(j) for given kp is the same as tetchi(j,kp) after call of chipsi. Thus use same variable
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
  ! TETCHI(nchi): theta values at each (smiso(kp),chim(1:nchi)) points w.r. to sigma,theta equilibrium mesh
  ! SIGCHI(nchi): sigma values at each (smiso(kp),chim(1:nchi)) points w.r. to sigma,theta equilibrium mesh
  ! In this way, we have with:
  ! call BOUND(nchi,TETCHI(1:nchi),RHO_bound) gives the values of rho_plasma_bound at each tetchi of this flux surface kp
  ! Then we have for example: R(chim,psi)=sigchi(ichi)*RHO_bound(ichi)*cos(tetchi(ichi))
  ! Note that only for the last call at kp=npmax do we have the tetchi values on the plasma boundary since sigchi=1.
  !
  ! It is better to use tetchi, sigchi for further interpolation than tetmap and sigmap since nchi>>nt usually.
  !
  ! Note: TETPSI, SIGPSI on IGMAX=NMGAUS*(NT+1) points in surface so could use this also
  !
  USE globals
  USE interpos_module
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZZ2
  REAL(RKIND)      ::     ZR2
  REAL(RKIND)      ::     ZRHO2
  REAL(RKIND)      ::     ZZ1
  REAL(RKIND)      ::     ZR1
  REAL(RKIND)      ::     ZRHO1
  INTEGER          ::     ICHIM
  INTEGER          ::     J7
  INTEGER          ::     J6
  INTEGER          ::     JU
  INTEGER          ::     npmax
  REAL(RKIND)      ::     ZITJ0
  REAL(RKIND)      ::     ZCURV
  REAL(RKIND)      ::     ZTMF2
  REAL(RKIND)      ::     Z4
  REAL(RKIND)      ::     Z3
  REAL(RKIND)      ::     ZDGDPC
  REAL(RKIND)      ::     ZDGDCP
  REAL(RKIND)      ::     ZDZDPC
  REAL(RKIND)      ::     ZDRDPC
  REAL(RKIND)      ::     ZDZDCP
  REAL(RKIND)      ::     ZDRDCP
  REAL(RKIND)      ::     ZDZDPN
  REAL(RKIND)      ::     ZDRDPN
  REAL(RKIND)      ::     ZDGDPN
  REAL(RKIND)      ::     ZDFDZ
  REAL(RKIND)      ::     ZDFDR
  REAL(RKIND)      ::     ZDFDT
  REAL(RKIND)      ::     ZDFDS
  REAL(RKIND)      ::     ZDZ2DT
  REAL(RKIND)      ::     ZDZ2DS
  REAL(RKIND)      ::     Z2
  REAL(RKIND)      ::     ZDZ1DT
  REAL(RKIND)      ::     ZDZ1DS
  REAL(RKIND)      ::     Z1
  REAL(RKIND)      ::     ZDPDZ
  REAL(RKIND)      ::     ZDPDR
  REAL(RKIND)      ::     ZDCDR
  REAL(RKIND)      ::     ZDCDZ
  REAL(RKIND)      ::     ZDTDZ
  REAL(RKIND)      ::     ZDSDZ
  REAL(RKIND)      ::     ZDTDR
  REAL(RKIND)      ::     ZDSDR
  REAL(RKIND)      ::     ZJAC
  REAL(RKIND)      ::     ZJPHI
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZRHO
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  REAL(RKIND)      ::     ZGRADP
  REAL(RKIND)      ::     ZFP
  REAL(RKIND)      ::     ZD2PT2
  REAL(RKIND)      ::     ZD2PS2
  REAL(RKIND)      ::     ZD2PST
  REAL(RKIND)      ::     ZDPDT
  REAL(RKIND)      ::     ZDPDS
  REAL(RKIND)      ::     ZD2RST
  REAL(RKIND)      ::     ZDRSDT
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZPCEL
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  REAL(RKIND)      ::     ZS
  REAL(RKIND)      ::     ZT
  INTEGER          ::     J4
  INTEGER          ::     IS0
  INTEGER          ::     JS
  INTEGER          ::     IT0
  INTEGER          ::     JG
  INTEGER          ::     JT
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZBND
  INTEGER          ::     KP
  INTEGER          ::     KP1
  REAL(RKIND)      ::     ZTETA
  INTEGER          ::     J1, ipsi
  REAL(RKIND)      ::     ZEPS
  REAL(RKIND)      ::     PS
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  REAL(RKIND)      ::     ZTET
  REAL(RKIND)      ::     ZD2TET
  REAL(RKIND)      ::     ZD2SIG
  REAL(RKIND)      ::     ZD2BCN
  REAL(RKIND)      ::     ZC1
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     zbetchi
  REAL(RKIND)      ::     zdpsis
  REAL(RKIND)      ::     zmu0
  REAL(RKIND)      ::     dpsi
  REAL(RKIND)      ::     ZD2BT2
  REAL(RKIND)      ::     ZD2BS2
  REAL(RKIND)      ::     ZDBDST
  REAL(RKIND)      ::      IGDPS
  REAL(RKIND)      ::      IJAC
  REAL(RKIND)      ::      Irho
  REAL(RKIND)      ::      IR
  REAL(RKIND)      ::      dchi
  REAL(RKIND)      ::      ZAH, ZAHPR, ZAHPRCOR, ZBETCHITOT
  REAL(RKIND)      ::      Y2, WORK, tmp1, tmp2, TENSOS
  REAL(RKIND)      ::      ZDET, ZDRDPSI, ZDRDCHI, ZDZDPSI, ZDZDCHI, ZBETCHI0
  REAL(RKIND)      ::      ZDRHOTORDPSI, zthet3(401),zbnd3(401)

  DIMENSION &
       &   IS0(NPCHI),       IT0(NPCHI),       ic(npchi), &
       &   ZBND(NPCHI,5),    ZCURV(NPCHI1),    ZDBDS(NPCHI,16), &
       &   ZDBDT(NPCHI,16),  ZPCEL(NPCHI,16),  ZS(NPCHI), &
       &   ZS1(NPCHI),       ZS2(NPCHI),       ZTETA(NPCHI,5), &
       &   ZT(NPCHI),        ZT1(NPCHI),       ZT2(NPCHI), &
       &   ZD2BCN(NTP2),     ZD2SIG(NTP2),     ZD2TET(NTP2), &
       &   ZTET(NTP2),       ZA1(NTP2),        ZB1(NTP2), &
       &   ZC1(NTP2),        zbetchi(npchi1),  &
       &   ZDBDST(NPCHI,16), ZD2BS2(NPCHI,16), &
       &   ZD2BT2(NPCHI,16), ZAH(NCHI),        ZAHPR(NCHI), &
       &   Y2(NT2),          tmp1(NCHI),       tmp2(NCHI), &
       &   WORK(NT2),        ZAHPRCOR(NCHI)

  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  kp1=kp+1
  Zeps  = 1.E-3_RKIND
  ZMU0 = 4.E-07_RKIND * CPI
  !
  ZDPSIS = 2._RKIND * PS * CPSRF
  !
  ! eqchease_out should include all and only those variables needed to write on the ITM CPO equilibrium structure
  ! any additional information will be added to eqchease_out_add_xxx
  !
  ! equilibrium structure is bound to evolve so try to write data in same order, so it is easier to maintain
  ! Note that at this stage, eqchease_out is also written in outxt.f90, so do the same there
  !
  ! Since need to allocate all pointers, and it might depend on effective psiiso array, it is done in mappin
  ! So do it there as well in right order
  ! NOTE: values on-axis, at kp=1, psi surface are set in surf_metrics_onaxis after calling chipsimetrics on each other surfaces
  !
  ! note: within chease, keep chease units and transform to si units and sign just before writing data or leaving chease (eqchease_mksa)
  !
  ! This routine works for the generic psi, chi, phi mapping with generic Jacobian ala CHEASE with NER and NEGP
  ! XTOR uses different metric so arrays below defined in outxt for a (psi,theta,phi)
  !
  ! Note all quantities which can be calculated directly from existing arrays should be set in GLOQUA over whole profile directly
  ! Here only additional flux integrals are computed (may be should move all to SURFACE at some point?)
  !
  CALL DCOPY(NT2,TETMAP(1,KP),1,ZTET,1)
  DO J1=2,NT2
     IF (ZTET(J1) .LT. ZTET(J1-1)) THEN
        ZTET(J1) = ZTET(J1) + 2._RKIND * CPI * (1._RKIND + &
             &                   INT(.5_RKIND * ABS(ZTET(J1) - ZTET(J1-1)) / CPI))
     ENDIF
  ENDDO
  !
  ! Compute zbetchi with interpos to impose some smoothness
  TENSOS = -3._RKIND
  call interpos(CHIN(1:NT1,KP),BCHIN(1:NT1,KP),NT1,tension=TENSOS, &
    & yout=ZB1(1:NT1),nbc=-1,ybc=RC2PI)
  BCHIN(1:NT1,KP) = ZB1(1:NT1)
  BCHIN(NT2,KP) = ZB1(1)
  call interpos(CHIN(1:NT1,KP),BCHIN(1:NT1,KP),NT1,nout=nchi,xout=chim(1:nchi),tension=TENSOS, &
    & yout=zbetchi(1:Nchi),nbc=-1,ybc=RC2PI)
  !
  ! Compute zsigchi with interpos to impose little smoothness, can use small tension at this stage it seems.
  TENSOS = -0.01_RKIND
  call interpos(CHIN(1:NT1,KP),SIGMAP(1:NT1,KP),NT1,tension=TENSOS, &
        & yout=ZB1(1:NT1),youtp=ZC1(1:NT1),youtPP=ZA1(1:NT1),nbc=-1,ybc=RC2PI)
  IF (KP .EQ. NPMAX) THEN
    SIGCHI(1:nchi,KP) = 1._RKIND
  else
    call interpos(CHIN(1:NT1,KP),SIGMAP(1:NT1,KP),NT1,nout=nchi,xout=chim(1:nchi),tension=TENSOS, &
      & yout=SIGCHI(1:Nchi,KP),nbc=-1,ybc=RC2PI)
  end IF
  !
  CALL SPLCYP(CHIN(1,KP),ZTET,NT1,RC2PI,RC2PI,ZD2TET,ZA1,ZB1,ZC1)
  ZD2TET(NT2) = ZD2TET(1)
  !
  CALL RESETI(IC,NCHI,1)
  DO JG=1,NCHI
     DO JT = 1,NT2
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (CHIN(JT,KP).GE.CHIM(JG)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,NCHI
     ICHIM = IT0(J4)
     IF (ICHIM .LT. 1)   ICHIM = 1
     IF (ICHIM .GT. NT1) ICHIM = NT1
     !
     ZH = CHIN(ICHIM+1,KP) - CHIN(ICHIM,KP)
     ZA = (CHIN(ICHIM+1,KP) - CHIM(J4)) / ZH
     ZB = (CHIM(J4) - CHIN(ICHIM,KP)) / ZH
     ZC = (ZA + 1) * (ZA - 1) * ZH * &
          &          (CHIN(ICHIM+1,KP) - CHIM(J4)) / 6._RKIND
     ZD = (ZB + 1) * (ZB - 1) * ZH * &
          &          (CHIM(J4) - CHIN(ICHIM,KP)) / 6._RKIND
     !
     TETCHI(J4,KP) = ZA*ZTET(ICHIM)     + ZB*ZTET(ICHIM+1) + &
          &                   ZC*ZD2TET(ICHIM)   + ZD*ZD2TET(ICHIM+1)
     TETCHI_sorted(J4,KP) = TETCHI(J4,KP)
     IF (TETCHI(J4,KP) .LT. CT(1)) &
          &                   TETCHI(J4,KP) = TETCHI(J4,KP) + 2._RKIND*CPI
     IF (TETCHI(J4,KP) .GT. CT(NT1)) &
          &                   TETCHI(J4,KP) = TETCHI(J4,KP) - 2._RKIND*CPI
     !
  ENDDO
  !
  DO J1=1,NCHI
     ZTETA(J1,1) = TETCHI(J1,KP)
     ZTETA(J1,2) = TETCHI(J1,KP) - 2._RKIND * ZEPS
     ZTETA(J1,3) = TETCHI(J1,KP) -      ZEPS
     ZTETA(J1,4) = TETCHI(J1,KP) +      ZEPS
     ZTETA(J1,5) = TETCHI(J1,KP) + 2._RKIND * ZEPS
  ENDDO
  !
  CALL BOUND(NCHI,ZTETA(1,1),ZBND(1,1))
  CALL BOUND(NCHI,ZTETA(1,2),ZBND(1,2))
  CALL BOUND(NCHI,ZTETA(1,3),ZBND(1,3))
  CALL BOUND(NCHI,ZTETA(1,4),ZBND(1,4))
  CALL BOUND(NCHI,ZTETA(1,5),ZBND(1,5))
!!$  !
  ! Write arrays related to chi, to check calculation then in matlab of inverse of chi(sigma,theta), psi(sigma,theta) in to sigma(psi,chi), theta(psi,chi) or similar
  !
!!$  write(31,*) KP, ps, npmax, nchi, nbps
!!$  write(31,*) (tetchi_sorted(j1,KP),j1=1,nchi)
!!$  write(31,*) (sigchi(j1,KP),j1=1,nchi)
!!$  if (KP .EQ. npmax) then
!!$    ! zteta_bound of last surface
!!$    write(31,*) (ZBND(j1,1),j1=1,nchi)
!!$    write(31,*) (TETBPS(j1),j1=1,nbps)
!!$    write(31,*) (RRBPS(j1),j1=1,nbps)
!!$    write(31,*) (RZBPS(j1),j1=1,nbps)
!!$    write(31,*) (D2RBPS(j1),j1=1,nbps)
!!$    write(31,*) (D2ZBPS(j1),j1=1,nbps)
!!$    write(31,*) BPS(1), BPS(12)
!!$    ! write(31,*) (zthet3(j1),j1=1,401)
!!$  end if
!!$  if (KP .EQ. npmax) then
!!$    ! write 2D arrays only once at end
!!$    ! psi(sigma,theta)
!!$    write(32,*) ns, nt, npmax, nchi
!!$    write(32,*) (cpsicl(j1),j1=1,4*(ns+1)*nt,4)
!!$    ! chi(sigma,theta)
!!$    write(32,*) ((CHIN(j1,ipsi),j1=1,nt+2),ipsi=1,npmax)
!!$    ! theta(chi,psi)
!!$    write(32,*) ((TETMAP(j1,ipsi),j1=1,nt+2),ipsi=1,npmax)
!!$    ! sigma(chi,psi)
!!$    write(32,*) ((SIGMAP(j1,ipsi),j1=1,nt+2),ipsi=1,npmax)
!!$    ! psim, psiiso used for mapping (+ center)
!!$    write(32,*) (SMISO(j1),j1=1,npmax)
!!$    ! chim, effective chi used for mapping
!!$    write(32,*) (chim(j1),j1=1,nchi)
!!$    ! chi
!!$    write(32,*) (chi(j1),j1=1,nchi+1)
!!$    ! csig
!!$    write(32,*) (csig(j1),j1=1,ns+1)
!!$    ! ct
!!$    write(32,*) (ct(j1),j1=1,nt+1)
!!$    ! rhos
!!$    write(32,*) (rhos(j1),j1=1,nt+1)
!!$    ! R0, RZ0 to get R, Z from CSIG, CT, RHOS
!!$    write(32,*) R0, RZ0
!!$  end if
  !
  CALL RESETI(IC,NCHI,1)
  DO JT = 1,NT1
     DO JG=1,NCHI
        IF (IC(JG).EQ.1) THEN
           IT0(JG) = JT-1
           IF (TETCHI(JG,KP).LE.CT(JT)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  CALL RESETI(IC,NCHI,1)
  DO JS = 1,NS1
     DO JG=1,NCHI
        IF (IC(JG).EQ.1) THEN
           IS0(JG) = JS-1
           IF (SIGCHI(JG,KP).LE.CSIG(JS)) IC(JG)  = 0
        ENDIF
     ENDDO
  ENDDO
  !
  DO J4=1,NCHI
     ZT(J4) = TETCHI(J4,KP)
     ZS(J4) = SIGCHI(J4,KP)
     IF (IS0(J4) .GT. NS) IS0(J4) = NS
     IF (IS0(J4) .LT. 1)  IS0(J4) = 1
     IF (IT0(J4) .GT. NT) IT0(J4) = NT
     IF (IT0(J4) .LT. 1)  IT0(J4) = 1
     ZS1(J4) = CSIG(IS0(J4))
     ZS2(J4) = CSIG(IS0(J4)+1)
     ZT1(J4) = CT(IT0(J4))
     ZT2(J4) = CT(IT0(J4)+1)
  ENDDO
  !
  CALL PSICEL(IS0,IT0,NCHI,NPCHI,ZPCEL,CPSICL)
  ! old :  CALL BASIS2(NCHI,NPCHI,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT)
  CALL BASIS3(NCHI,NPCHI,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT, &
       &               ZDBDST,ZD2BS2,ZD2BT2)
  ! INITIALIZE quantities required for flux average integrals
  DCHI=CHI(2)-CHI(1)
  IGDPS=0
  IJAC=0
  Irho =0
  Ir=0
  ZBETCHI0=0._RKIND
  !
  DO J5=1,NCHI
     ZDRSDT = (ZBND(J5,2) + 8*(ZBND(J5,4) - ZBND(J5,3)) - &
          &               ZBND(J5,5)) / (12._RKIND * ZEPS)
     ZDPDS = ZDBDS(J5, 1) * ZPCEL(J5, 1) + &
          &             ZDBDS(J5, 2) * ZPCEL(J5, 2) + &
          &             ZDBDS(J5, 3) * ZPCEL(J5, 3) + &
          &             ZDBDS(J5, 4) * ZPCEL(J5, 4) + &
          &             ZDBDS(J5, 5) * ZPCEL(J5, 5) + &
          &             ZDBDS(J5, 6) * ZPCEL(J5, 6) + &
          &             ZDBDS(J5, 7) * ZPCEL(J5, 7) + &
          &             ZDBDS(J5, 8) * ZPCEL(J5, 8) + &
          &             ZDBDS(J5, 9) * ZPCEL(J5, 9) + &
          &             ZDBDS(J5,10) * ZPCEL(J5,10) + &
          &             ZDBDS(J5,11) * ZPCEL(J5,11) + &
          &             ZDBDS(J5,12) * ZPCEL(J5,12) + &
          &             ZDBDS(J5,13) * ZPCEL(J5,13) + &
          &             ZDBDS(J5,14) * ZPCEL(J5,14) + &
          &             ZDBDS(J5,15) * ZPCEL(J5,15) + &
          &             ZDBDS(J5,16) * ZPCEL(J5,16)
     ZDPDT = ZDBDT(J5, 1) * ZPCEL(J5, 1) + &
          &             ZDBDT(J5, 2) * ZPCEL(J5, 2) + &
          &             ZDBDT(J5, 3) * ZPCEL(J5, 3) + &
          &             ZDBDT(J5, 4) * ZPCEL(J5, 4) + &
          &             ZDBDT(J5, 5) * ZPCEL(J5, 5) + &
          &             ZDBDT(J5, 6) * ZPCEL(J5, 6) + &
          &             ZDBDT(J5, 7) * ZPCEL(J5, 7) + &
          &             ZDBDT(J5, 8) * ZPCEL(J5, 8) + &
          &             ZDBDT(J5, 9) * ZPCEL(J5, 9) + &
          &             ZDBDT(J5,10) * ZPCEL(J5,10) + &
          &             ZDBDT(J5,11) * ZPCEL(J5,11) + &
          &             ZDBDT(J5,12) * ZPCEL(J5,12) + &
          &             ZDBDT(J5,13) * ZPCEL(J5,13) + &
          &             ZDBDT(J5,14) * ZPCEL(J5,14) + &
          &             ZDBDT(J5,15) * ZPCEL(J5,15) + &
          &             ZDBDT(J5,16) * ZPCEL(J5,16)
     !------------ same as erdata.f90 ----------------------------
     ZD2RST = (- ZBND(J5,2) + 16._RKIND * ZBND(J5,3) - &
          &             30._RKIND * ZBND(J5,1) + 16._RKIND * ZBND(J5,4) - &
          &             ZBND(J5,5)) / (12._RKIND * ZEPS**2)
     !
     ZD2PST = ZDBDST(J5, 1) * ZPCEL(J5, 1) + &
          &            ZDBDST(J5, 2) * ZPCEL(J5, 2) + &
          &            ZDBDST(J5, 3) * ZPCEL(J5, 3) + &
          &            ZDBDST(J5, 4) * ZPCEL(J5, 4) + &
          &            ZDBDST(J5, 5) * ZPCEL(J5, 5) + &
          &            ZDBDST(J5, 6) * ZPCEL(J5, 6) + &
          &            ZDBDST(J5, 7) * ZPCEL(J5, 7) + &
          &            ZDBDST(J5, 8) * ZPCEL(J5, 8) + &
          &            ZDBDST(J5, 9) * ZPCEL(J5, 9) + &
          &            ZDBDST(J5,10) * ZPCEL(J5,10) + &
          &            ZDBDST(J5,11) * ZPCEL(J5,11) + &
          &            ZDBDST(J5,12) * ZPCEL(J5,12) + &
          &            ZDBDST(J5,13) * ZPCEL(J5,13) + &
          &            ZDBDST(J5,14) * ZPCEL(J5,14) + &
          &            ZDBDST(J5,15) * ZPCEL(J5,15) + &
          &            ZDBDST(J5,16) * ZPCEL(J5,16)
     !
     ZD2PS2 = ZD2BS2(J5, 1) * ZPCEL(J5, 1) + &
          &            ZD2BS2(J5, 2) * ZPCEL(J5, 2) + &
          &            ZD2BS2(J5, 3) * ZPCEL(J5, 3) + &
          &            ZD2BS2(J5, 4) * ZPCEL(J5, 4) + &
          &            ZD2BS2(J5, 5) * ZPCEL(J5, 5) + &
          &            ZD2BS2(J5, 6) * ZPCEL(J5, 6) + &
          &            ZD2BS2(J5, 7) * ZPCEL(J5, 7) + &
          &            ZD2BS2(J5, 8) * ZPCEL(J5, 8) + &
          &            ZD2BS2(J5, 9) * ZPCEL(J5, 9) + &
          &            ZD2BS2(J5,10) * ZPCEL(J5,10) + &
          &            ZD2BS2(J5,11) * ZPCEL(J5,11) + &
          &            ZD2BS2(J5,12) * ZPCEL(J5,12) + &
          &            ZD2BS2(J5,13) * ZPCEL(J5,13) + &
          &            ZD2BS2(J5,14) * ZPCEL(J5,14) + &
          &            ZD2BS2(J5,15) * ZPCEL(J5,15) + &
          &            ZD2BS2(J5,16) * ZPCEL(J5,16)
     !
     ZD2PT2 = ZD2BT2(J5, 1) * ZPCEL(J5, 1) + &
          &            ZD2BT2(J5, 2) * ZPCEL(J5, 2) + &
          &            ZD2BT2(J5, 3) * ZPCEL(J5, 3) + &
          &            ZD2BT2(J5, 4) * ZPCEL(J5, 4) + &
          &            ZD2BT2(J5, 5) * ZPCEL(J5, 5) + &
          &            ZD2BT2(J5, 6) * ZPCEL(J5, 6) + &
          &            ZD2BT2(J5, 7) * ZPCEL(J5, 7) + &
          &            ZD2BT2(J5, 8) * ZPCEL(J5, 8) + &
          &            ZD2BT2(J5, 9) * ZPCEL(J5, 9) + &
          &            ZD2BT2(J5,10) * ZPCEL(J5,10) + &
          &            ZD2BT2(J5,11) * ZPCEL(J5,11) + &
          &            ZD2BT2(J5,12) * ZPCEL(J5,12) + &
          &            ZD2BT2(J5,13) * ZPCEL(J5,13) + &
          &            ZD2BT2(J5,14) * ZPCEL(J5,14) + &
          &            ZD2BT2(J5,15) * ZPCEL(J5,15) + &
          &            ZD2BT2(J5,16) * ZPCEL(J5,16)
     !
     !-------------------------------------------------------------
     !
     ZFP    = (ZDPDS**2 + (ZDPDT / SIGCHI(J5,KP) - ZDPDS * ZDRSDT / &              ! g11
          &               ZBND(J5,1))**2) / ZBND(J5,1)**2
     ZGRADP = SQRT(ZFP)
     !
     ZCOST = COS(ZTETA(J5,1))
     ZSINT = SIN(ZTETA(J5,1))
     !
     ZRHO   = SIGCHI(J5,KP) * ZBND(J5,1)
     ZR     = ZRHO * ZCOST + R0
     ZZ     = ZRHO * ZSINT + RZ0
     ZJAC   = CP(KP) * ZR**NER * ZGRADP**NEGP
     !
     !   ---  Variables needed for ogyropsi ---
     ZDSDR = (ZDRSDT * ZSINT + ZBND(J5,1) * ZCOST) / ZBND(J5,1)**2
     ZDTDR = - ZSINT / ZRHO
     ZDSDZ = (ZBND(J5,1) * ZSINT - ZDRSDT * ZCOST) / ZBND(J5,1)**2
     ZDTDZ = ZCOST / ZRHO
     !
     ZDPDR = ZDPDS * ZDSDR + ZDPDT * ZDTDR
     ZDPDZ = ZDPDS * ZDSDZ + ZDPDT * ZDTDZ
     !
     Z1     = ZDPDS
     ZDZ1DS = ZD2PS2
     ZDZ1DT = ZD2PST
     Z2     = ZDPDT / SIGCHI(J5,KP) - ZDRSDT * ZDPDS / ZBND(J5,1)
     ZDZ2DS = (ZD2PST - ZDPDT / SIGCHI(J5,KP)) / SIGCHI(J5,KP) - &
          &             ZDRSDT * ZD2PS2 / ZBND(J5,1)
     ZDZ2DT = - ZD2PST*ZDRSDT/ZBND(J5,1) + ZD2PT2/SIGCHI(J5,KP) + &
          &              ZDPDS * (ZDRSDT**2-ZD2RST*ZBND(J5,1))/ZBND(J5,1)**2
     !
     ZDFDS = 2 * (Z1 * ZDZ1DS + Z2 * ZDZ2DS) / ZBND(J5,1)**2
     ZDFDT = - 2 * ZFP * ZDRSDT / ZBND(J5,1) + &
          &           2 * (Z1 * ZDZ1DT + Z2 * ZDZ2DT) / ZBND(J5,1)**2
     !
     ZDFDR = ZDFDS * ZDSDR + ZDFDT * ZDTDR
     ZDFDZ = ZDFDS * ZDSDZ + ZDFDT * ZDTDZ
     !
     ZDGDPN = (ZDFDR * ZDPDR + ZDFDZ * ZDPDZ) / ZFP
     ZDRDPN = ZDPDR / ZFP
     ! %YC Computes ZBETCHI0=beta_psi_chi(theta=0)
     ! ZBETCHITOT = beta_psi_chi(theta)
     ! ZBETCHI = beta_psi_chi(theta) - beta_psi_chi(theta=0)
     ! Remember that BCHIN is (mysteriously) scaled by zdpsis in surface.f90
     ! This is kept here for consistency, altough ZBETCHITOT/zdpsis is used everywhere afterwards
     IF (J5.EQ.1) THEN
       ZBETCHI0 = ZR * ZDPDZ * (1-ZDPDZ**2/ZFP)**(-0.5) / ZJAC / ZGRADP**3
       IF (ZTETA(J5,1).GE.ABS(ZTETA(J5+1,1)-ZTETA(J5,1))/2) THEN
         IF (NVERBOSE .GE. 0) write(0,*) &
              & 'WARNING, in chipsimetrics.f90, ZTETA(1)~=0, formula used for ZBETCHI0 can not be applied. ZTETA(1)=', & 
           & ZTETA(J5,1)
       END IF
     END IF
     ZBETCHITOT = ZBETCHI(J5) + ZBETCHI0*zdpsis
     ! %YC

     ZDRDCP = - ZJAC * ZDPDZ / ZR
     ZDRDPC = ZDRDPN - ZBETCHITOT/zdpsis * ZDRDCP
     ZDGDCP = ZJAC * (ZDPDR * ZDFDZ - ZDPDZ * ZDFDR) / ZR
     ZDGDPC = ZDGDPN - ZBETCHITOT/zdpsis * ZDGDCP
     ! Variables needed for ORB5
     ZDCDZ=(ZDRDPC*ZR)/ZJAC
     ZDCDR=1/(ZDPDR)*(ZBETCHITOT*ZFP/zdpsis-ZDPDZ*ZDCDZ)

     eqchease_out(1)%coord_sys%g_11(kp1,j5) = zfp
     eqchease_out(1)%coord_sys%g_22(kp1,j5) = ((ZBETCHITOT/zdpsis)**2*zfp + zr**2/zjac**2/zfp)
     eqchease_out(1)%coord_sys%g_33(kp1,j5) = 1._rkind/(ZR)**2
     eqchease_out(1)%coord_sys%g_12(kp1,j5) = ZBETCHITOT/zdpsis*zfp
     eqchease_out(1)%coord_sys%jacobian(kp1,j5) = ZJAC
     eqchease_out(1)%coord_sys%position%R(kp1,j5) = ZR
     eqchease_out(1)%coord_sys%position%Z(kp1,j5) = ZZ
     !
     ! Other quantities for ogyropsi.f90
     !
     eqchease_out_add_2d(kp1,j5,iiB) = sqrt( tmf(kp)**2 + zfp ) / ZR                                            ! B
     eqchease_out_add_2d(kp1,j5,iidBdpsi) = -1._rkind/ZR**2 *ZDRDPC * sqrt( tmf(kp)**2 + zfp ) + &
          & 1./ ZR * ( ttp(kp) + 0.5_rkind*ZDGDPC ) / sqrt( tmf(kp)**2 + zfp )                                 ! dBdpsi
     eqchease_out_add_2d(kp1,j5,iidBdchi) =  -1._rkind/ZR**2 * ZDRDCP*sqrt( tmf(kp)**2 + zfp ) + &
          & 0.5_rkind / ZR * ZDGDCP / sqrt( tmf(kp)**2 + zfp )                                                 ! dBdchi
     eqchease_out_add_2d(kp1,j5,iidpsidR) = ZDPDR                                                               ! dPsidr
     eqchease_out_add_2d(kp1,j5,iidpsidZ) = ZDPDZ                                                               ! dPsidz
     eqchease_out_add_2d(kp1,j5,iidchidR) = ZDCDR                                                               ! dChidr
     eqchease_out_add_2d(kp1,j5,iidchidZ) = ZDCDZ                                                               ! dChidz
     !
     ! Integrals required for  <| grd PSI |> = int GRD_PSI J dchi / Int J dchi
     IGDPS = IGDPS+sqrt(zfp)*ZJAC*dchi
     IJAC  = IJAC + ZJAC*dchi
     ! for < r >
     Irho = Irho + ZRHO*ZJAC*dchi
     ! for < R >
     IR   = IR + ZR*ZJAC*dchi
     !
     ! %YC for hamada.f90
     ZDET = ZR * SIGCHI(J5,KP) * ZBND(J5,1)**2 / ZJAC
     ZDRDCHI = (SIGCHI(J5,KP)*ZDRSDT*ZDPDS*ZCOST &
          &        -SIGCHI(J5,KP)*ZBND(J5,1)*ZDPDS*ZSINT &
          &        -ZBND(J5,1)*ZDPDT*ZCOST)/ZDET
     ZDRDPSI = ZDPDR/ZFP - ZBETCHITOT/ZDPSIS * ZDRDCHI
     ZDZDCHI = (SIGCHI(J5,KP)*ZDRSDT*ZDPDS*ZSINT &
          &        +SIGCHI(J5,KP)*ZBND(J5,1)*ZDPDS*ZCOST &
          &        -ZBND(J5,1)*ZDPDT*ZSINT)/ZDET
     ZDZDPSI = ZDPDZ/ZFP - ZBETCHITOT/ZDPSIS * ZDZDCHI
     !
     eqchease_out_add_2d(KP1,J5,iidRdpsi) = ZDRDPSI
     eqchease_out_add_2d(KP1,J5,iidRdchi) = ZDRDCHI
     eqchease_out_add_2d(KP1,J5,iidZdpsi) = ZDZDPSI
     eqchease_out_add_2d(KP1,J5,iidZdchi) = ZDZDCHI
     if (ZTETA(J5,1).GT.0._RKIND) then
        eqchease_out_add_2d(KP1,J5,iitheta) = ZTETA(J5,1)
     else
        eqchease_out_add_2d(KP1,J5,iitheta) = 2._RKIND * CPI + ZTETA(J5,1)
     end if
     ! %YC
     !
  ENDDO
  ! <| grd PSI |>
  eqchease_out_add_1d(kp1,iigradpsi_av) = IGDPS/IJAC
  ! < r >
  eqchease_out_add_1d(kp1,iia_av) = Irho/IJAC
  ! < R >
  eqchease_out_add_1d(kp1,iiR_av) = IR/IJAC
  !
  ! %YC for hamada.f90
  ! interpolation of AH and AHPR on the CHIM mesh (by construction, CHIN(i+1,KP)>CHIN(i,KP), so no need to rearrange it)
  CALL SPLINE(NT2,CHIN(1,KP),AH(1,KP),Y2,WORK)
  CALL PPSPLN2(NCHI,CHIM,NT2-1,CHIN(1,KP),AH(1,KP),Y2,ZAH,tmp1,tmp2)
  !
  CALL SPLINE(NT2,CHIN(1,KP),AHPR(1,KP),Y2,WORK)
  CALL PPSPLN2(NCHI,CHIM,NT2-1,CHIN(1,KP),AHPR(1,KP),Y2,ZAHPR,tmp1,tmp2)
  !
  CALL SPLINE(NT2,CHIN(1,KP),AHPRCOR(1,KP),Y2,WORK)
  CALL PPSPLN2(NCHI,CHIM,NT2-1,CHIN(1,KP),AHPRCOR(1,KP),Y2,ZAHPRCOR,tmp1,tmp2)
  !
  DO J5=1,NCHI
     ZAHPR(J5) = ZAHPR(J5) + 2*ZBETCHI0*ZAHPRCOR(J5)
     eqchease_out_add_2d(KP1,J5,iiAh) = ZAH(J5)
     eqchease_out_add_2d(KP1,J5,iidAhdpsi) = ZAHPR(J5)
  END DO
  ! %YC
  !
  !
  RETURN
END SUBROUTINE chipsimetrics
