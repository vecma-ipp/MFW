SUBROUTINE SURF_METRICS_ONAXIS
  !        ####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !
  ! LEAD MAPPINGS FOR DIFFERENT CODES LINKED TO CHEASE
  ! NEW FEATURE: WORK ON GENERIC MESH SMISO/PSIISO(1:NISO1EFF) CREATED IN PREMAP
  ! IN THIS WAY MOST DIFFERENCES BETWEEN NIDEAL CASES ARE IN PREMAP FOR THE MESH DEFINITION
  ! AND THEN AFTER THE CALCULATIONS OF THE VARIOUS QUANTITIES IN MAPPIN FOR THE CALUCLATIONS OF SPECIFIC
  ! OUTPUT VALUES FOR EACH MAPPING
  !
  !**********************************************************************
  !
  USE globals
  USE interpol
  use interpos_module
  IMPLICIT NONE
  !
  REAL(RKIND), ALLOCATABLE :: SIGMA(:), ZWORK(:)
  REAL(RKIND) :: ZBMAG, FIT0CUB, XX(1:4), YY(1:4), XXX, TENS_DEF
  INTEGER ::  I, ICHI
  !
  FIT0CUB(XXX) = FCCCC0(YY(1),YY(2),YY(3),YY(4),XX(1),XX(2),XX(3),XX(4),XXX)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  TENS_DEF = -0.1_RKIND
  ALLOCATE(SIGMA(NISO1EFF1))
  SIGMA=(/1000._RKIND, (REAL(1.,RKIND),I=1,NISO1EFF) /)
  !
  ! Could put 1./1e-14?
  ARATIO(1) = 0.0_RKIND
  !
  IF (NSURF .NE. 1) THEN
     !
     CP0   = FCCCC0(CPR(1),CPR(2),CPR(3),CPR(4), &
          &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
     DPDP0 = FCCCC0(CPPR(1),CPPR(2),CPPR(3),CPPR(4), &
          &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
     T0    = FCCCC0(TMF(1),TMF(2),TMF(3),TMF(4), &
          &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
     DTTP0 = FCCCC0(TTP(1),TTP(2),TTP(3),TTP(4), &
          &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
     !
  ELSE
    ! SOLOVEV
    T0    = 1._RKIND
    DTTP0 = 0._RKIND
    CP0   = CPP * SPSIM
    DPDP0 = CPP
    !
  ENDIF
  !
  Q0    = FCCCC0(QPSI(1),QPSI(2),QPSI(3),QPSI(4), &
       &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  RIPR0 = FCCCC0(RIPR(1),RIPR(2),RIPR(3),RIPR(4), &
       &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  RJDTB0= FCCCC0(RJDOTB(1),RJDOTB(2),RJDOTB(3),RJDOTB(4), &
       &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  CPSI0 = FCCCC0(CP(1),CP(2),CP(3),CP(4), &
       &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  DQDP0 = FCCCC0(CDQ(1),CDQ(2),CDQ(3),CDQ(4), &
       &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  CPDP0 = FCCCC0(CPDP(1),CPDP(2),CPDP(3),CPDP(4), &
       &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  !
  IF (NPROFZ .EQ. 1) THEN
    TEMP0 = FCCCC0(TEMPER(1),TEMPER(2),TEMPER(3),TEMPER(4), &
         &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
    DENS0 = FCCCC0(DENSTY(1),DENSTY(2),DENSTY(3),DENSTY(4), &
         &                  SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ENDIF
  !
  eqchease_out(1)%profiles_1d%surface(1) = 0._rkind
  XX=SMISOP1(2:5)
  YY=eqchease_out(1)%profiles_1d%elongation(2:5)
  eqchease_out(1)%profiles_1d%elongation(1) = FIT0CUB(RC0P)
  !
  eqchease_out(1)%profiles_1d%r_inboard(1) = RMAG
  eqchease_out(1)%profiles_1d%r_outboard(1) = RMAG
  eqchease_out_add_1d(1,iirgeo) = RMAG
  eqchease_out_add_1d(1,iiR_av) = RMAG
  eqchease_out_add_1d(1,iia_av) = 0._rkind
  eqchease_out_add_1d(1,iiamin) = 0._rkind
  YY=eqchease_out(1)%profiles_1d%tria_lower(2:5)
  eqchease_out(1)%profiles_1d%tria_lower(1) = FIT0CUB(RC0P)
  YY=eqchease_out(1)%profiles_1d%tria_upper(2:5)
  eqchease_out(1)%profiles_1d%tria_upper(1) = FIT0CUB(RC0P)
  ZBMAG = T0 / RMAG
  eqchease_out_add_1d(1,iiBmin) = ZBMAG
  eqchease_out_add_1d(1,iiBmax) = ZBMAG
  eqchease_out(1)%profiles_1d%b_min(1) = ZBMAG
  eqchease_out(1)%profiles_1d%b_max(1) = ZBMAG
  !
  eqchease_out(1)%coord_sys%g_11(1,:) = 0._rkind
  eqchease_out(1)%coord_sys%g_33(1,:) = 1._rkind/RMAG**2
  eqchease_out(1)%coord_sys%grid%dim1=(/0._rkind, psiiso(1:NISO1EFF) /)
  allocate(zwork(NISO1EFF1))
  do ichi=1,NCHI
    CALL INTERPOS(SMISOP1(2:NISO1EFF1),eqchease_out(1)%coord_sys%jacobian(2:NISO1EFF1,ichi),N=NISO1EFF1-1, &
         & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out(1)%coord_sys%jacobian(1,ichi), &
         & nbcscal=(/2, 2/), &
         & ybcscal=(/eqchease_out(1)%coord_sys%jacobian(2,ichi), eqchease_out(1)%coord_sys%jacobian(NISO1EFF1,ichi)/))
    CALL INTERPOS(SMISOP1(2:NISO1EFF1),eqchease_out(1)%coord_sys%g_12(2:NISO1EFF1,ichi),N=NISO1EFF1-1, &
         & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out(1)%coord_sys%g_12(1,ichi), &
         & nbcscal=(/2, 2/), ybcscal=(/eqchease_out(1)%coord_sys%g_12(2,ichi), eqchease_out(1)%coord_sys%g_12(NISO1EFF1,ichi)/))
    ! actually g_22 goes like (a1+a2*psi)/psi^2, so first interpolate on gtild_22=psi^2*g_22, but then divides by 0 for on-axis value, but smoothing better
    ! on psi_norm so that extrapolation distance near center is small
    CALL INTERPOS(SMISOP1(1:NISO1EFF1)**2, &
         & eqchease_out(1)%coord_sys%grid%dim1(1:NISO1EFF1)**2 * eqchease_out(1)%coord_sys%g_22(1:NISO1EFF1,ichi), &
         & NIN=NISO1EFF1-1,NOUT=NISO1EFF1,TENSION=10._rkind*TENS_DEF,XOUT=SMISOP1(1:NISO1EFF1)**2, &
         & YOUT=zwork, &
         & NBC=(/2, 2/), YBC=(/RC0P, &
         & eqchease_out(1)%coord_sys%grid%dim1(NISO1EFF1)**2 * eqchease_out(1)%coord_sys%g_22(NISO1EFF1,ichi)/))
    CALL INTERPOS(SMISOP1(2:NISO1EFF1)**2,zwork(2:NISO1EFF1)/eqchease_out(1)%coord_sys%grid%dim1(2:NISO1EFF1)**2,N=NISO1EFF1-1, &
      & OPTION=-11,XSCAL=SMISOP1(1),yscal=eqchease_out(1)%coord_sys%g_22(1,ichi))
  end do
  eqchease_out(1)%coord_sys%position%R(1,:) = RMAG
  eqchease_out(1)%coord_sys%position%Z(1,:) = RZMAG
  !
  eqchease_out_add_2d(1,:,iiB) = ZBMAG
  !
  ! Profiles on axis. Uses interpos to get d/dpsi as well.
  ! Uses fact that d/drho=0 to fix extrapolation towards 0, with large error bar
  ! Then uses value on axis to interpolate on psi and get d/dpsi in center
  !
  ! Obtain Te(0) using dTe/drho=0 as constraint
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iiTe), eqchease_out_add_1d(2:NISO1EFF1,iiTe)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iiTe), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iiTe)/))
  ! fit on psi to get dTe(0)/dpsi
  CALL INTERPOS((/SPSIM, PSIISO(1:NISO1EFF)/),eqchease_out_add_1d(1:NISO1EFF1,iiTe),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SPSIM,YSCALP=eqchease_out_add_1d(1,iidTedpsi), &
       & nbcscal=(/2, 2/), ybcscal=(/eqchease_out_add_1d(1,iiTe), eqchease_out_add_1d(NISO1EFF1,iiTe)/))
  ! ne, dnedpsi
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iine), eqchease_out_add_1d(2:NISO1EFF1,iine)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iine), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iine)/))
  CALL INTERPOS((/SPSIM, PSIISO(1:NISO1EFF)/),eqchease_out_add_1d(1:NISO1EFF1,iine),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SPSIM,YSCALP=eqchease_out_add_1d(1,iidnedpsi), &
       & nbcscal=(/2, 2/), ybcscal=(/eqchease_out_add_1d(1,iine), eqchease_out_add_1d(NISO1EFF1,iine)/))
  ! Ti, dTidpsi
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iiTi), eqchease_out_add_1d(2:NISO1EFF1,iiTi)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iiTi), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iiTi)/))
  CALL INTERPOS((/SPSIM, PSIISO(1:NISO1EFF)/),eqchease_out_add_1d(1:NISO1EFF1,iiTi),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SPSIM,YSCALP=eqchease_out_add_1d(1,iidTidpsi), &
       & nbcscal=(/2, 2/), ybcscal=(/eqchease_out_add_1d(1,iiTi), eqchease_out_add_1d(NISO1EFF1,iiTi)/))
  ! ni, dnidpsi
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iini), eqchease_out_add_1d(2:NISO1EFF1,iini)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iini), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iini)/))
  CALL INTERPOS((/SPSIM, PSIISO(1:NISO1EFF)/),eqchease_out_add_1d(1:NISO1EFF1,iini),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SPSIM,YSCALP=eqchease_out_add_1d(1,iidnidpsi), &
       & nbcscal=(/2, 2/), ybcscal=(/eqchease_out_add_1d(1,iini), eqchease_out_add_1d(NISO1EFF1,iini)/))
  ! zeff
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iizeff), eqchease_out_add_1d(2:NISO1EFF1,iizeff)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iizeff), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iizeff)/))
  ! nuestar
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iinuestar), eqchease_out_add_1d(2:NISO1EFF1,iinuestar)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iinuestar), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iinuestar)/))
  ! signeo
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iisigneo), eqchease_out_add_1d(2:NISO1EFF1,iisigneo)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iisigneo), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iisigneo)/))
  ! jbsBav
  CALL INTERPOS(SMISOP1(1:NISO1EFF1),(/eqchease_out_add_1d(2,iijbsBav), eqchease_out_add_1d(2:NISO1EFF1,iijbsBav)/),N=NISO1EFF1, &
       & TENSION=TENS_DEF,XSCAL=SMISOP1(1),yscal=eqchease_out_add_1d(1,iijbsBav), &
       & SIGMA=SIGMA,nbcscal=(/1, 2/), ybcscal=(/RC0P, eqchease_out_add_1d(NISO1EFF1,iijbsBav)/))
  !
  RETURN
END SUBROUTINE SURF_METRICS_ONAXIS
