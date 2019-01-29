SUBROUTINE metrictoitm(Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
  !
  ! chipsimetrics was made to compute eqchease_out, etc
  ! It computes them now in CHEASE units, so they can be used within CHEASE
  ! But they need to be transformed into MKSA and ITM convention (psi_chease*2pi) before leaving CHEASE
  ! This is done here for eqchease_out(1). Do not touch eqchease_out_add_1d, etc as they are used in specific output routines
  ! Use R0EXP and B0EXP
  !
  ! Added calculation of profiles_2d(2), (3) on (psi,theta) and (rho_tor_norm, theta) using interpolation inside LCFS
  ! Since this takes time, namelist variable nprof2d determines how many fields are calculated (none if do not write on ITM database, cf auxval.f90)
  ! (should calculate these fields directly as in psibox from CHEASE, but needed to test these 2d interpolation in any case...)
  !
  USE globals
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
  !
  interface
     subroutine prof2d_rz_to_fluxtheta(RR,ZZ,f2darray_in,Rbnd,Zbnd,Raxis,Zaxis,flux_norm_out,theta_out, &
          & rhopolar_out,Rout,Zout,farray_out,tension_in)
       !
       use itm_types
       use interpos_module
       !
       IMPLICIT NONE
       real(R8), allocatable :: RR(:), ZZ(:), f2darray_in(:,:,:), Rbnd(:), Zbnd(:), flux_norm_out(:), theta_out(:)
       real(R8), intent(IN) :: Raxis, Zaxis
       real(R8), intent(IN), optional :: tension_in
       !
       real(R8), allocatable :: rhopolar_out(:,:), Rout(:,:), Zout(:,:), farray_out(:,:,:)
     end subroutine prof2d_rz_to_fluxtheta
  end interface
  !
  CHARACTER  ZDATE*8
  REAL(RKIND) :: ZMU0, ZCOF, ZDX, tension_def
  REAL(RKIND), allocatable :: RR(:), ZZ(:), Rbnd(:), Zbnd(:), theta_out(:), rhopolar_out(:,:), Rout(:,:), Zout(:,:)
  REAL(RKIND), allocatable :: zflux_norm(:), f2darray_RZ(:,:,:), f2darray_psitheta(:,:,:)
  INTEGER :: i, ii, ndim1, ndim2, inin, I_compute_only_psitheta_surfaces
  !
  ZMU0 = 4.E-07_RKIND * CPI
  !
  ! NORMALIZATION AND SIGNS, USE COCOS_OUT INDEX AND R0EXP, B0EXP, SIGNIPXP, SIGNB0XP
  ! THEN USE SEC. III OF COCOS PAPER TO TRANSFORM OUTPUTS (O. Sauter, S. Yu. Medvedev, submitted to CPC 2012, in chease svn trunk)
  !
  !
  ! datainfo
  allocate(eqchease_out(1)%codeparam%codename(1))
  eqchease_out(1)%codeparam%codename(1) = 'CHEASE'
  allocate(eqchease_out(1)%codeparam%codeversion(1))
  eqchease_out(1)%codeparam%codeversion(1) = &
       & 'Id: $Id: metrictoitm_afterfpp.f90 978 2015-07-01 18:48:08Z osauter $ ; Global svnversion ' // "exported"
  write(6,'(A)') trim(eqchease_out(1)%codeparam%codeversion(1))

  CALL DATE_AND_TIME(ZDATE)
  allocate(eqchease_out(1)%datainfo%putdate(1))
  eqchease_out(1)%datainfo%putdate(1) = ZDATE
  allocate(eqchease_out(1)%datainfo%dataprovider(1))
  eqchease_out(1)%datainfo%dataprovider(1)='chease user'
  ! put 4 lines in comments(1:4)
  allocate(eqchease_out(1)%datainfo%comment(size(comments)))
  eqchease_out(1)%datainfo%comment(:) = comments(:)
  ! do i=1,size(equil_out(1)%datainfo%comment)
  !    PRINT *,trim(equil_out(1)%datainfo%comment(i))
  ! end do
  eqchease_out(1)%datainfo%cocos = COCOS_OUT
  ! eqgeometry
  eqchease_out(1)%eqgeometry%boundarytype = 0 ! limiter (CHEASE cannot deal formally with X points)
  allocate(eqchease_out(1)%eqgeometry%boundary(1)%r(NBPSOUT))
  allocate(eqchease_out(1)%eqgeometry%boundary(1)%z(NBPSOUT))
  ! (not in 410a anymore) eqchease_out(1)%eqgeometry%boundary(1)%npoints = NBPSOUT
  do i=1,NBPSOUT
    eqchease_out(1)%eqgeometry%boundary(1)%r(i) = RRBPSOU(i)*R0EXP
    eqchease_out(1)%eqgeometry%boundary(1)%z(i) = (RZBPSOU(i)-(RZMAG-RZMGEQD))*R0EXP
  enddo
  eqchease_out(1)%eqgeometry%geom_axis%r = eqchease_out_add_1d(NISO1EFF1,iirgeo) * R0EXP
  eqchease_out(1)%eqgeometry%geom_axis%z = RZMGEQD * R0EXP
  eqchease_out(1)%eqgeometry%a_minor = eqchease_out_add_1d(NISO1EFF1,iiamin) * R0EXP
  eqchease_out(1)%eqgeometry%elongation = eqchease_out(1)%profiles_1d%elongation(NISO1EFF1)
  ! new fields only in eqgeometry so far
  eqchease_out(1)%eqgeometry%elong_upper = &
       & (maxval(eqchease_out(1)%eqgeometry%boundary(1)%z)-eqchease_out(1)%eqgeometry%geom_axis%z) &
       & / eqchease_out(1)%eqgeometry%a_minor
  eqchease_out(1)%eqgeometry%elong_lower = &
       & (eqchease_out(1)%eqgeometry%geom_axis%z-minval(eqchease_out(1)%eqgeometry%boundary(1)%z)) &
       & / eqchease_out(1)%eqgeometry%a_minor
  !
  eqchease_out(1)%eqgeometry%tria_upper = eqchease_out(1)%profiles_1d%tria_upper(NISO1EFF1)
  eqchease_out(1)%eqgeometry%tria_lower = eqchease_out(1)%profiles_1d%tria_lower(NISO1EFF1)
  ! profiles_1d
  eqchease_out(1)%profiles_1d%psi = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp * &
       & eqchease_out(1)%profiles_1d%psi * R0EXP**2 * B0EXP
  eqchease_out(1)%profiles_1d%phi = SIGNB0XP * eqchease_out(1)%profiles_1d%phi * R0EXP**2 * B0EXP
  eqchease_out(1)%profiles_1d%pressure = eqchease_out(1)%profiles_1d%pressure * B0EXP**2 / ZMU0
  eqchease_out(1)%profiles_1d%F_dia = SIGNB0XP * eqchease_out(1)%profiles_1d%F_dia * R0EXP * B0EXP
  eqchease_out(1)%profiles_1d%pprime = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out(1)%profiles_1d%pprime * B0EXP / ZMU0 / R0EXP**2
  eqchease_out(1)%profiles_1d%ffprime = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out(1)%profiles_1d%ffprime * B0EXP
  eqchease_out(1)%profiles_1d%jphi = SIGNIPXP * eqchease_out(1)%profiles_1d%jphi * B0EXP/zmu0/R0EXP
  eqchease_out(1)%profiles_1d%jparallel = SIGNIPXP * eqchease_out(1)%profiles_1d%jparallel * &
       & B0EXP/zmu0/R0EXP
  eqchease_out(1)%profiles_1d%q = SIGNIPXP * SIGNB0XP * Ksigma_rhothetaphi * eqchease_out(1)%profiles_1d%q
  eqchease_out(1)%profiles_1d%r_inboard = eqchease_out(1)%profiles_1d%r_inboard * R0EXP
  eqchease_out(1)%profiles_1d%r_outboard = eqchease_out(1)%profiles_1d%r_outboard * R0EXP
  eqchease_out(1)%profiles_1d%rho_tor = eqchease_out(1)%profiles_1d%rho_tor * R0EXP
  eqchease_out(1)%profiles_1d%dpsidrho_tor = SIGNIPXP*Ksigma_Bp*twopi**Kexp_Bp * &
       & eqchease_out(1)%profiles_1d%dpsidrho_tor &
       & * R0EXP*B0EXP
  ! eqchease_out(1)%profiles_1d%rho_vol is normalized
  eqchease_out(1)%profiles_1d%volume = eqchease_out(1)%profiles_1d%volume * R0EXP**3
  eqchease_out(1)%profiles_1d%vprime = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out(1)%profiles_1d%vprime * R0EXP / B0EXP
  ! assume drho -> drho_tor not normalized, in [m]
  eqchease_out(1)%profiles_1d%dvdrho = eqchease_out(1)%profiles_1d%dvdrho * R0EXP**2
  eqchease_out(1)%profiles_1d%area = eqchease_out(1)%profiles_1d%area * R0EXP**2
  eqchease_out(1)%profiles_1d%aprime = SIGNIPXP * Ksigma_Bp / twopi**Kexp_Bp * &
       & eqchease_out(1)%profiles_1d%aprime / B0EXP
  eqchease_out(1)%profiles_1d%surface = eqchease_out(1)%profiles_1d%surface * R0EXP**2
  eqchease_out(1)%profiles_1d%gm1 = eqchease_out(1)%profiles_1d%gm1 / R0EXP**2
  eqchease_out(1)%profiles_1d%gm2 = eqchease_out(1)%profiles_1d%gm2 / R0EXP**2
  eqchease_out(1)%profiles_1d%gm4 = eqchease_out(1)%profiles_1d%gm4 / B0EXP**2
  eqchease_out(1)%profiles_1d%gm5 = eqchease_out(1)%profiles_1d%gm5 * B0EXP**2
  eqchease_out(1)%profiles_1d%gm6 = eqchease_out(1)%profiles_1d%gm6 / B0EXP**2
  eqchease_out(1)%profiles_1d%gm8 = eqchease_out(1)%profiles_1d%gm8 * R0EXP
  eqchease_out(1)%profiles_1d%gm9 = eqchease_out(1)%profiles_1d%gm9 / R0EXP
  eqchease_out(1)%profiles_1d%b_av = eqchease_out(1)%profiles_1d%b_av * B0EXP
  eqchease_out(1)%profiles_1d%b_min = eqchease_out(1)%profiles_1d%b_min * B0EXP
  eqchease_out(1)%profiles_1d%b_max = eqchease_out(1)%profiles_1d%b_max * B0EXP
  ! global_param (after profiles_1d since related to it and needs to unnormalize only once)
  eqchease_out(1)%global_param%beta_pol = BETAP    ! POLOIDAL BETA
  eqchease_out(1)%global_param%beta_tor   = BETAX   ! BETA=<P>_V * 2*MU0/B0^2
  eqchease_out(1)%global_param%beta_normal = 100._rkind*BETAX/(rinor*1.e-06_RKIND/ZMU0)  ! bet_normal [MA,T,M]
  eqchease_out(1)%global_param%i_plasma = SIGNIPXP * RITOT * R0EXP*B0EXP/ZMU0  ! TOTAL CURRENT --> [A]
  eqchease_out(1)%global_param%li = eqchease_out(1)%profiles_1d%li(NISO1EFF1)
  eqchease_out(1)%global_param%volume = eqchease_out(1)%profiles_1d%volume(NISO1EFF1)
  eqchease_out(1)%global_param%area   = eqchease_out(1)%profiles_1d%area(NISO1EFF1)
  eqchease_out(1)%global_param%psi_ax = eqchease_out(1)%profiles_1d%psi(1)
  eqchease_out(1)%global_param%psi_bound = eqchease_out(1)%profiles_1d%psi(NISO1EFF1)
  eqchease_out(1)%global_param%mag_axis%position%r = RMAG * R0EXP            ! R OF MAGAXE --> [M]
  eqchease_out(1)%global_param%mag_axis%position%z = RZMGEQD * R0EXP ! Z OF MAGAXE --> [M]
  eqchease_out(1)%global_param%mag_axis%bphi = SIGNB0XP * T0/RMAG * B0EXP    ! b_phi at MAGAXE --> [T]
  eqchease_out(1)%global_param%mag_axis%q = eqchease_out(1)%profiles_1d%q(1) ! q at magnetic axis
  eqchease_out(1)%global_param%q_95 = SIGNIPXP * SIGNB0XP * Ksigma_rhothetaphi * q95 ! q at 95% of poloidal magnetic flux
  eqchease_out(1)%global_param%q_min = SIGNIPXP * SIGNB0XP * Ksigma_rhothetaphi * qmin ! minimum q
  eqchease_out(1)%global_param%toroid_field%R0 = R0EXP
  eqchease_out(1)%global_param%toroid_field%B0 = SIGNB0XP * B0EXP
  eqchease_out(1)%global_param%w_mhd = 1.5_rkind * CPBAR*B0EXP**2/zmu0 * &
       & eqchease_out(1)%global_param%volume/R0EXP**3
  !
  ! profiles_2d
  !
  ! profiles_2d(1): R, Z grids already computed but in CHEASE units, need to use directly with SI units since profiles_1d etc have already been modified
  eqchease_out(1)%profiles_2d(1)%grid%dim1 = eqchease_out(1)%profiles_2d(1)%grid%dim1 * R0EXP
  eqchease_out(1)%profiles_2d(1)%grid%dim2 = eqchease_out(1)%profiles_2d(1)%grid%dim2 * R0EXP
  !
  allocate(eqchease_out(1)%profiles_2d(1)%grid_type(4))
  eqchease_out(1)%profiles_2d(1)%grid_type(1) = '1'
  eqchease_out(1)%profiles_2d(1)%grid_type(2) = 'rectangular, (R, phi, Z) rectangular, COCOS=13'
  eqchease_out(1)%profiles_2d(1)%grid_type(3) = '-1'
  eqchease_out(1)%profiles_2d(1)%grid_type(4) = 'NA since rectangular mesh'
  ! note that box dimensions are already multiplied by R0EXP
  ! allocate(eqchease_out(1)%profiles_2d(1)%grid%dim1(NRBOX))
  zcof=1._rkind/real(nrbox-1,rkind)
  allocate(eqchease_out(1)%profiles_2d(1)%R(NRBOX,NZBOX))
  do i=1,nrbox
    ! eqchease_out(1)%profiles_2d(1)%grid%dim1(i)=RBOXLFT+real(i-1,rkind)*RBOXLEN*zcof
    eqchease_out(1)%profiles_2d(1)%R(i,:)=eqchease_out(1)%profiles_2d(1)%grid%dim1(i)
  enddo
  ! allocate(eqchease_out(1)%profiles_2d(1)%grid%dim2(NZBOX))
  zcof=1._rkind/real(nzbox-1,rkind)
  allocate(eqchease_out(1)%profiles_2d(1)%Z(NRBOX,NZBOX))
  do i=1,nzbox
    ! eqchease_out(1)%profiles_2d(1)%grid%dim2(i)=ZBOXMID-0.5_rkind*ZBOXLEN+real(i-1,rkind)*ZBOXLEN*zcof
    eqchease_out(1)%profiles_2d(1)%Z(:,i)=eqchease_out(1)%profiles_2d(1)%grid%dim2(i)
  enddo

  allocate(eqchease_out(1)%profiles_2d(1)%psi(NRBOX,NZBOX))
  eqchease_out(1)%profiles_2d(1)%psi = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp * EQDSPSI(1:NRBOX,1:NZBOX) &
       & * R0EXP**2 * B0EXP
  ! EQDSPSI is shifted to have psi_edge=0, so shift it to given psi_edge
  eqchease_out(1)%profiles_2d(1)%psi = eqchease_out(1)%profiles_2d(1)%psi + &
       & eqchease_out(1)%global_param%psi_bound
  ! BR = -1/R dpsi/dZ
  ! BZ = 1/R dpsi/dR
  ! Bphi = F_dia/R
  eqchease_out(1)%profiles_2d(1)%phi(1:NRBOX,1:NZBOX) = SIGNB0XP * &
       & eqchease_out(1)%profiles_2d(1)%phi(1:NRBOX,1:NZBOX) * R0EXP**2 * B0EXP
  eqchease_out(1)%profiles_2d(1)%jphi(1:NRBOX,1:NZBOX) = SIGNIPXP * &
       & eqchease_out(1)%profiles_2d(1)%jphi(1:NRBOX,1:NZBOX) * B0EXP/zmu0/R0EXP
  eqchease_out(1)%profiles_2d(1)%jpar(1:NRBOX,1:NZBOX) = SIGNIPXP * &
       & eqchease_out(1)%profiles_2d(1)%jpar(1:NRBOX,1:NZBOX) * B0EXP/zmu0/R0EXP
  eqchease_out(1)%profiles_2d(1)%br(1:NRBOX,1:NZBOX) = - SIGNIPXP * Ksigma_RphiZ * &
       & eqchease_out(1)%profiles_2d(1)%br(1:NRBOX,1:NZBOX) * B0EXP
  eqchease_out(1)%profiles_2d(1)%bz(1:NRBOX,1:NZBOX) = - SIGNIPXP * Ksigma_RphiZ * &
       & eqchease_out(1)%profiles_2d(1)%bz(1:NRBOX,1:NZBOX) * B0EXP
  eqchease_out(1)%profiles_2d(1)%bphi(1:NRBOX,1:NZBOX) = SIGNB0XP * &
       & eqchease_out(1)%profiles_2d(1)%bphi(1:NRBOX,1:NZBOX) * B0EXP
  eqchease_out(1)%profiles_2d(1)%pressure(1:NRBOX,1:NZBOX) = SIGNIPXP * &
       & eqchease_out(1)%profiles_2d(1)%pressure(1:NRBOX,1:NZBOX) * B0EXP**2 / ZMU0
  !
  ! Calculate profiles_2d(2) on (rho_psi, theta) from profiles_2d(1) to check interpolation routines for CRONOS
  ! Depends on nprof2d:
  ! nprof2d = 0: no calculation
  !         = 1: compute flux surface on psi=profiles_1d%psi(:) and on a theta mesh (NZBOX intervals)
  !              Fill in only grid, psi, theta, R(psi,theta), Z(psi,theta) and rho_polar(psi,theta) in %vtheta
  !         = 2: same as 1 and interpolates BR, BZ, BPHI, PHI, jphi, jpar and pressure
  !        = 11: performs as well same as (1) but for rho_tor_norm,theta mesh and saves into profiles_2d(3)
  !        = 12: same as 2 for profiles_2d(2) and same fields on (rho_tor_norm,theta) in profiles_2d(3)
  !
  if (nprof2d .GT. 0) THEN
    call runtim
    do ii=2,int(nprof2d/10)+2
      ! ii = 2 for filling in profiles_2d(ii) with (psi,theta) grid
      ! ii = 3 for filling in profiles_2d(ii) with (rho_tor_norm,theta) grid
      !
      allocate(eqchease_out(1)%profiles_2d(ii)%grid_type(4))
      eqchease_out(1)%profiles_2d(ii)%grid_type(1) = '2'
      if (ii .EQ. 2) write(eqchease_out(1)%profiles_2d(ii)%grid_type(ii),*) 'inverse, (psi,theta), COCOS=',cocos_out
      if (ii .EQ. 3) write(eqchease_out(1)%profiles_2d(ii)%grid_type(ii),*) 'inverse, (rho_tor_norm,theta), COCOS=',cocos_out
      eqchease_out(1)%profiles_2d(ii)%grid_type(3) = '3'
      eqchease_out(1)%profiles_2d(ii)%grid_type(4) = 'polar'
      !
      if (ii .EQ. 2) then
        ndim1 = size(eqchease_out(1)%profiles_1d%psi)
        allocate(eqchease_out(1)%profiles_2d(ii)%grid%dim1(ndim1))
        eqchease_out(1)%profiles_2d(ii)%grid%dim1(1:ndim1) = eqchease_out(1)%profiles_1d%psi(1:ndim1)
      elseif (ii .EQ. 3) then
        ndim1 = size(eqchease_out(1)%profiles_1d%rho_tor)
        allocate(eqchease_out(1)%profiles_2d(ii)%grid%dim1(ndim1))
        eqchease_out(1)%profiles_2d(ii)%grid%dim1(1:ndim1) = eqchease_out(1)%profiles_1d%rho_tor(1:ndim1) &
             & / eqchease_out(1)%profiles_1d%rho_tor(ndim1)
      else
        write(6,*) 'ii = ',ii,' should not happen in nprof2d part of metrictoitm, skip further part'
        call flush(6)
        go to 222
      end if
      ndim2 = NZBOX+1 ! use NZBOX for 2nd dimension: nb theta intervals
      allocate(theta_out(ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%grid%dim2(ndim2))
      zdx = twopi / real(NZBOX,RKIND)
      theta_out(:) = (/0._rkind, (real(i,rkind)*zdx, i=1,NZBOX) /)
      eqchease_out(1)%profiles_2d(ii)%grid%dim2(1:ndim2) = theta_out(1:ndim2)
      allocate(eqchease_out(1)%profiles_2d(ii)%theta(ndim1,ndim2))
      do i=1,ndim2
        eqchease_out(1)%profiles_2d(ii)%theta(1:ndim1,i) = eqchease_out(1)%profiles_2d(ii)%grid%dim2(i)
      end do
      ! All flux surface quantities in profiles_2d can now be copied from profiles_1d since they do not depend on theta
      allocate(eqchease_out(1)%profiles_2d(ii)%psi(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%phi(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%pressure(ndim1,ndim2))
      do i=1,ndim1
        eqchease_out(1)%profiles_2d(ii)%psi(i,1:ndim2) = eqchease_out(1)%profiles_1d%psi(i)
        eqchease_out(1)%profiles_2d(ii)%phi(i,1:ndim2) = eqchease_out(1)%profiles_1d%phi(i)
        eqchease_out(1)%profiles_2d(ii)%pressure(i,1:ndim2) = eqchease_out(1)%profiles_1d%pressure(i)
      end do
      !
      if (mod(nprof2d,10) .EQ. 1) THEN
        ININ = 1
      elseif (mod(nprof2d,10) .EQ. 2) THEN
        ININ = 6
      else
        write(6,*) 'mod(nprof2d,10) = ',mod(nprof2d,10), &
             & ' should not happen in nprof2d part of metrictoitm, skip further part'
        call flush(6)
        go to 222
      end if
      allocate(zflux_norm(ndim1))
      allocate(f2darray_RZ(size(eqchease_out(1)%profiles_2d(1)%grid%dim1), &
           & size(eqchease_out(1)%profiles_2d(1)%grid%dim2),ININ))
      if (ii .eq. 2) THEN
        ! interpolate on normalized psi flux
        f2darray_RZ(:,:,1) = &
             & (eqchease_out(1)%profiles_2d(1)%psi(:,:)-eqchease_out(1)%profiles_2d(ii)%grid%dim1(1))/ &
             & (eqchease_out(1)%profiles_2d(ii)%grid%dim1(ndim1)-eqchease_out(1)%profiles_2d(ii)%grid%dim1(1))
        zflux_norm = (eqchease_out(1)%profiles_2d(ii)%grid%dim1-eqchease_out(1)%profiles_2d(ii)%grid%dim1(1))/ &
             & (eqchease_out(1)%profiles_2d(ii)%grid%dim1(ndim1)-eqchease_out(1)%profiles_2d(ii)%grid%dim1(1))
      else
        ! interpolate on normalized Phi flux, radial mesh corresponding to rho_tor_norm
        f2darray_RZ(:,:,1) = eqchease_out(1)%profiles_2d(1)%phi(:,:)/eqchease_out(1)%profiles_1d%phi(ndim1)
        zflux_norm = eqchease_out(1)%profiles_2d(ii)%grid%dim1**2 ! or profiles_1d%phi(:)/profiles_1d%phi(ndim1)
      end if
      if (mod(nprof2d,10) .EQ. 2) THEN
        f2darray_RZ(:,:,2) = eqchease_out(1)%profiles_2d(1)%BR(:,:)
        f2darray_RZ(:,:,3) = eqchease_out(1)%profiles_2d(1)%BZ(:,:)
        f2darray_RZ(:,:,4) = eqchease_out(1)%profiles_2d(1)%BPHI(:,:)
        f2darray_RZ(:,:,5) = eqchease_out(1)%profiles_2d(1)%JPHI(:,:)
        f2darray_RZ(:,:,6) = eqchease_out(1)%profiles_2d(1)%JPAR(:,:)
      end if
      allocate(f2darray_psitheta(ndim1,ndim2,ININ))
      tension_def = -0.1_RKIND
      allocate(RR(size(eqchease_out(1)%profiles_2d(1)%grid%dim1)))
      RR(:) = eqchease_out(1)%profiles_2d(1)%grid%dim1(:)
      allocate(ZZ(size(eqchease_out(1)%profiles_2d(1)%grid%dim2)))
      ZZ(:) = eqchease_out(1)%profiles_2d(1)%grid%dim2(:)
      allocate(Rbnd(size(eqchease_out(1)%eqgeometry%boundary(1)%r)))
      Rbnd(:) = eqchease_out(1)%eqgeometry%boundary(1)%r(:)
      allocate(Zbnd(size(eqchease_out(1)%eqgeometry%boundary(1)%z)))
      Zbnd(:) = eqchease_out(1)%eqgeometry%boundary(1)%z(:)
      allocate(rhopolar_out(ndim1,ndim2))
      allocate(Rout(ndim1,ndim2))
      allocate(Zout(ndim1,ndim2))
      ! The interpolation assumes one gives a "flux" radial quantity and it interpolates on sqrt(radial flux)
      ! Thus gives the normalized flux here and then
      call prof2d_rz_to_fluxtheta(RR,ZZ,f2darray_RZ,Rbnd,Zbnd, &
        & eqchease_out(1)%global_param%mag_axis%position%r,eqchease_out(1)%global_param%mag_axis%position%z, &
        & zflux_norm,theta_out,rhopolar_out,Rout,Zout,f2darray_psitheta,tension_def)
      !
      allocate(eqchease_out(1)%profiles_2d(ii)%vtheta(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%R(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%Z(ndim1,ndim2))
      if (nverbose .ge. 1) write(6,*) 'profiles_2d(ii)%vtheta contains polar rho value(dim1,dim2) ', &
           & 'of each flux surface (dim1) at theta values (dim2)'
      eqchease_out(1)%profiles_2d(ii)%vtheta(:,:) = rhopolar_out(:,:)
      eqchease_out(1)%profiles_2d(ii)%R(:,:) = Rout(:,:)
      eqchease_out(1)%profiles_2d(ii)%Z(:,:) = Zout(:,:)
      !
      allocate(eqchease_out(1)%profiles_2d(ii)%BR(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%BZ(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%BPHI(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%JPHI(ndim1,ndim2))
      allocate(eqchease_out(1)%profiles_2d(ii)%JPAR(ndim1,ndim2))
      if (mod(nprof2d,10) .EQ. 2) THEN
        eqchease_out(1)%profiles_2d(ii)%BR(:,:) = f2darray_psitheta(:,:,2)
        eqchease_out(1)%profiles_2d(ii)%BZ(:,:) = f2darray_psitheta(:,:,3)
        eqchease_out(1)%profiles_2d(ii)%BPHI(:,:) = f2darray_psitheta(:,:,4)
        eqchease_out(1)%profiles_2d(ii)%JPHI(:,:) = f2darray_psitheta(:,:,5)
        eqchease_out(1)%profiles_2d(ii)%JPAR(:,:) = f2darray_psitheta(:,:,6)
      else
        eqchease_out(1)%profiles_2d(ii)%BR(:,:) = RC0P
        eqchease_out(1)%profiles_2d(ii)%BZ(:,:) = RC0P
        eqchease_out(1)%profiles_2d(ii)%BPHI(:,:) = RC0P
        eqchease_out(1)%profiles_2d(ii)%JPHI(:,:) = RC0P
        eqchease_out(1)%profiles_2d(ii)%JPAR(:,:) = RC0P
     end if
      !
      deallocate(zflux_norm,f2darray_RZ,f2darray_psitheta,RR,ZZ,Rbnd,Zbnd,rhopolar_out,Rout,Zout,theta_out)
      !
    end do
    call runtim
222 continue
  end if
  !
  ! coord_sys
  allocate(eqchease_out(1)%coord_sys%grid_type(4))
  eqchease_out(1)%coord_sys%grid_type(1) = '2'
  if (NIDEAL .eq. 5) THEN
    ! XTOR uses equidistant polar theta and not J=c(psi) R^NER grad(psi)^NEGP jacobian
    write(eqchease_out(1)%coord_sys%grid_type(2),'(A,I2)') &
         & 'inverse, (psi,theta,phi), equidistant polar, cocos= ',COCOS_OUT
    eqchease_out(1)%coord_sys%grid_type(3) = '3'
    eqchease_out(1)%coord_sys%grid_type(4) = 'equidistant polar angle'
  elseif (NER.EQ.2 .AND. NEGP.EQ.0) THEN
    write(eqchease_out(1)%coord_sys%grid_type(2),'(A,I2)') &
         & 'inverse, (psi,chi,phi), straight field line, cocos= ',COCOS_OUT
    eqchease_out(1)%coord_sys%grid_type(3) = '1'
    eqchease_out(1)%coord_sys%grid_type(4) = &
         & 'straight field line, NER=2, NEGP=0, Jac=c(psi) R^NER grad(psi)^NEGP'
  elseif (NER.EQ.1 .AND. NEGP.EQ.-1) THEN
    write(eqchease_out(1)%coord_sys%grid_type(2),'(A,I2)') &
         & 'inverse, (psi,chi,phi), equal arc, cocos= ',COCOS_OUT
    eqchease_out(1)%coord_sys%grid_type(3) = '2'
    eqchease_out(1)%coord_sys%grid_type(4) = 'equal arc, NER=-1, NEGP=1, Jac=c(psi) R^NER grad(psi)^NEGP'
  elseif (NER.EQ.0 .AND. NEGP.EQ.0) THEN
    write(eqchease_out(1)%coord_sys%grid_type(2),'(A,I2)') &
         & 'inverse, (psi,chi,phi), Jac is psi function, cocos= ',COCOS_OUT
    eqchease_out(1)%coord_sys%grid_type(3) = '2'
    eqchease_out(1)%coord_sys%grid_type(4) = 'ala Hamada, NER=0, NEGP=0, Jac=c(psi) R^NER grad(psi)^NEGP'
  else
    write(eqchease_out(1)%coord_sys%grid_type(2),'(A,I2)') &
         & 'inverse, (psi,chi,phi), general chi, cocos= ',COCOS_OUT
    eqchease_out(1)%coord_sys%grid_type(3) = '99'
    write(eqchease_out(1)%coord_sys%grid_type(4),'(A,I2,A,I2)') &
      & 'chi determined by Jac=c(psi) R^NER grad(psi)^NEGP with NER=',NER,' and NEGP=',NEGP
  end if
  eqchease_out(1)%coord_sys%grid%dim1 = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp &
    & * eqchease_out(1)%coord_sys%grid%dim1 * R0EXP**2 * B0EXP
  eqchease_out(1)%coord_sys%jacobian = eqchease_out(1)%coord_sys%jacobian * R0EXP / TWOPI**Kexp_Bp / B0EXP ! ~ 1/sqrt(det(g_ij))
  eqchease_out(1)%coord_sys%g_11 = twopi**(2*Kexp_Bp) * eqchease_out(1)%coord_sys%g_11 * (R0EXP * B0EXP)**2
  ! Adding Ksigma_rhothetaphi makes test NIDEAL10 fail, so discuss with Yann first
  !  eqchease_out(1)%coord_sys%g_12 = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp * eqchease_out(1)%coord_sys%g_12 * B0EXP * Ksigma_rhothetaphi
  eqchease_out(1)%coord_sys%g_12 = SIGNIPXP * Ksigma_Bp * twopi**Kexp_Bp * eqchease_out(1)%coord_sys%g_12 &
       & * B0EXP
  eqchease_out(1)%coord_sys%g_22 = eqchease_out(1)%coord_sys%g_22 / R0EXP**2
  eqchease_out(1)%coord_sys%g_33 = eqchease_out(1)%coord_sys%g_33 / R0EXP**2
  eqchease_out(1)%coord_sys%position%R = eqchease_out(1)%coord_sys%position%R * R0EXP
  eqchease_out(1)%coord_sys%position%Z = (eqchease_out(1)%coord_sys%position%Z-(RZMAG-RZMGEQD)) * R0EXP
  !
  ! Extra values and "Special" values
  ! Be cautious to specify effective indices on right-hand side at least otherwise takes full size which migh be larger than left-side allocated
  !
  ! D_I: IDEAL MERCIER
  eqchease_out(1)%profiles_1d%phi_flow(1:NISO1EFF1) = (/RC0P, SMERCI(1:NISO1EFF) /)
  ! D_R: RESISTIVE INTERCHANGE
  eqchease_out(1)%profiles_1d%s_flow(1:NISO1EFF1) = (/RC0P, SMERCR(1:NISO1EFF) /)
  ! NCBAL: Ballooning (0 if stable)

  eqchease_out(1)%profiles_1d%h_flow(1:NISO1EFF1) = real((/0, NCBAL(1:NISO1EFF) /),rkind)
  !
  return
end SUBROUTINE metrictoitm
