subroutine COCOStransform(equilibrium_in,COCOS_in, COCOS_out, IPsign_out, B0sign_out, equilibrium_out)
!
! Transform input equilibrium CPO with COCOS_in (from input or from CPO .cocos index if exists) into output equil_CPO with COCOS_out
!
! Can ask for a specific sign of Ip and/or B0 in output as well
!
! It follows the general transformation rules specified in Appendix C of COCOS paper (by O. Sauter and S. Yu. Medvevdev, Comp. Phys. Comm. 2012)
! without the normalization factors since assume CPO_in and CPO_out are in SI units
!
  use itm_types

  use euITM_schemas
  use copy_structures

  IMPLICIT NONE
  REAL(R8), PARAMETER :: TWOPI=6.283185307179586476925286766559005768394_R8

  interface
     SUBROUTINE COCOS(KCOCOS,Kexp_Bp,Ksigma_Bp,Ksigma_RphiZ,Ksigma_rhothetaphi,Ksign_q_pos,Ksign_pprime_pos)
       !
       ! return values of exp_Bp, sigma_Bp, sigma_rhothetaphi, sign_q_pos, sign_pprime_pos
       ! from the input value KCOCOS and according to O. Sauter and S. Yu. Medvevdev paper and Table I
       ! (see paper in chease directory)
       !
       IMPLICIT NONE
       INTEGER, intent(in) :: KCOCOS
       INTEGER, intent(out) :: Kexp_Bp, Ksigma_Bp, Ksigma_RphiZ, Ksigma_rhothetaphi, Ksign_q_pos, Ksign_pprime_pos
     END SUBROUTINE COCOS
  end interface

  type (type_equilibrium), pointer  ::  equilibrium_in(:) ! should not use intent(in) or out with pointers
  type (type_equilibrium), pointer  ::  equilibrium_out(:)
  type (type_equilibrium), pointer  ::  equilibrium_in2(:)
  type (type_equilibrium), pointer  ::  equilibrium_out2(:)
  integer, optional, intent(IN) :: COCOS_in
  integer, optional, intent(IN) :: COCOS_out
  integer, optional, intent(IN) :: IPsign_out
  integer, optional, intent(IN) :: B0sign_out
  !
  integer :: inequi, nb1D, i, it, i_gridtype, j
  integer :: icocos_in, iexp_Bp_in,isigma_Bp_in,isigma_RphiZ_in,isigma_rhothetaphi_in,isign_q_pos_in,isign_pprime_pos_in
  integer :: icocos_out, iexp_Bp_out,isigma_Bp_out,isigma_RphiZ_out,isigma_rhothetaphi_out,isign_q_pos_out,isign_pprime_pos_out
  integer :: iIPsign_out, iB0sign_out
  real(R8) :: z_one, sigma_Ip_in, sigma_B0_in, pedge_rel, sigma_Ip_out, sigma_B0_out, sigma_IP_eff, sigma_B0_eff
  real(R8) :: sigma_Bp_eff, exp_Bp_eff, sigma_rhothetaphi_eff, sigma_RphiZ_eff, fact_psi, fact_q

  !
  icocos_in = 13
  if (present(COCOS_in)) THEN
    if (COCOS_in .NE. itm_int_invalid) icocos_in = COCOS_in
  end if
  icocos_out = icocos_in
  if (present(COCOS_out)) THEN
    if (COCOS_out .NE. itm_int_invalid) icocos_out = COCOS_out
  end if
  ! Ipsign_out undefined means use input value transformed to new COCOS
  iIPsign_out = itm_int_invalid
  if (present(IPsign_out)) THEN
    iIPsign_out = IPsign_out
  end if
  iB0sign_out = itm_int_invalid
  if (present(B0sign_out)) THEN
    iB0sign_out = B0sign_out
  end if
  !
  ! Get COCOS related parameters
  call COCOS(icocos_in,iexp_Bp_in,isigma_Bp_in,isigma_RphiZ_in,isigma_rhothetaphi_in,isign_q_pos_in,isign_pprime_pos_in)
  call COCOS(icocos_out,iexp_Bp_out,isigma_Bp_out,isigma_RphiZ_out,isigma_rhothetaphi_out,isign_q_pos_out,isign_pprime_pos_out)
  !
  ! Check for COCOS consistency (cf Sec. V of paper)
  !
  z_one = 1._R8
  inequi = size(equilibrium_in)
  ! check only 1st time
  nb1D = size(equilibrium_in(1)%profiles_1d%q)
  sigma_Ip_in = sign(z_one,equilibrium_in(1)%global_param%i_plasma)
  sigma_B0_in = sign(z_one,equilibrium_in(1)%global_param%toroid_field%b0)
  if (sign(z_one,equilibrium_in(1)%profiles_1d%q(nb1D))*isigma_rhothetaphi_in*sigma_Ip_in*sigma_B0_in .le. 0._r8) THEN
    write(*,*) 'WARNING: sign(q) is not consistent with COCOS_in=',icocos_in,' value'
    write(*,*) 'qedge = ',equilibrium_in(1)%profiles_1d%q(nb1D)
    write(*,*) 'sig_rhothetaphi*sign(Ip)*sign(B0) = ',isigma_rhothetaphi_in, ' * ',sigma_Ip_in,' * ',sigma_B0_in,' = ', &
      & isigma_rhothetaphi_in*sigma_Ip_in*sigma_B0_in
  end if
  if (sign(z_one,equilibrium_in(1)%profiles_1d%F_dia(nb1D))*sigma_B0_in .LE. 0) THEN
    write(*,*) 'WARNING: Signs of F and B0 are not consistent'
  end if
  IF (sign(z_one,equilibrium_in(1)%global_param%psi_bound-equilibrium_in(1)%global_param%psi_ax)*isigma_Bp_in*sigma_Ip_in &
       & .LE. 0._R8) THEN
    IF (sign(z_one,equilibrium_in(1)%global_param%psi_bound-equilibrium_in(1)%global_param%psi_ax) .LE. 0._R8) THEN
      write(*,*) 'WARNING: psi should be increasing with : sign(Ip)=',sigma_Ip_in,' and sigma_Bp=',isigma_Bp_in, &
           & ' for COCOS=',icocos_in
    ELSE
      write(*,*) 'WARNING: psi should be decreasing with : sign(Ip)=',sigma_Ip_in,' and sigma_Bp=',isigma_Bp_in, &
           & ' for COCOS=',icocos_in
    END if
  ELSE
    ! check sign of pprime
    IF (associated(equilibrium_in(1)%profiles_1d%pprime)) THEN
      pedge_rel=0._R8
      DO i=2,nb1D
        pedge_rel = pedge_rel + equilibrium_in(1)%profiles_1d%pprime(i) * &
             & (equilibrium_in(1)%profiles_1d%psi(i)-equilibrium_in(1)%profiles_1d%psi(i-1))
      END DO
      IF (pedge_rel .GE. R8) THEN
        write(*,*) 'WARNING: pprime has wrong sign'
      END IF
    END IF
  END IF
  !
  ! Define effective variables: sigma_Ip_eff, sigma_B0_eff, sigma_Bp_eff, exp_Bp_eff as in Appendix C
  ! sign(Ip) in output:
  !
  sigma_RphiZ_eff  = REAL(isigma_RphiZ_out * isigma_RphiZ_in,R8)
  !
  IF (iIPsign_out .LE. -2) THEN
    sigma_IP_eff = sigma_RphiZ_eff
  ELSE
    sigma_IP_eff = sigma_Ip_in * REAL(iIPsign_out,R8)
  END IF
  sigma_Ip_out = sigma_Ip_in * sigma_IP_eff
  !
  ! sign(B0) in output:
  IF (iB0sign_out .LE. -2) THEN
    sigma_B0_eff = REAL(isigma_RphiZ_in * isigma_RphiZ_out,R8)
  ELSE
    sigma_B0_eff = sigma_B0_in * REAL(iB0sign_out,R8)
  END IF
  sigma_B0_out = sigma_B0_in * sigma_B0_eff
  !
  sigma_Bp_eff = REAL(isigma_Bp_out * isigma_Bp_in,R8)
  exp_Bp_eff = REAL(iexp_Bp_out - iexp_Bp_in,R8)
  sigma_rhothetaphi_eff  = REAL(isigma_rhothetaphi_out * isigma_rhothetaphi_in,R8)
  !
  ! Copy equilibrium_in to _out and then transform relevant quantities according to desired sign(Ip), sign(B0)
  !
  call copy_arr_type_equilibrium(equilibrium_in,equilibrium_out)
  !
  ! eqconstraint: NOT TOUCHED AT THIS STAGE SINCE MIGHT DEPEND ON TOKAMAK?
  !
  ! eqgeometry: only "lengths" so would only be changed if l_d normalization changes
  !
  ! flush: NOT TOUCHED AT THIS STAGE
  !
  fact_psi = sigma_Ip_eff * sigma_Bp_eff * TWOPI**exp_Bp_eff
  fact_q = sigma_Ip_eff * sigma_B0_eff * sigma_rhothetaphi_eff
  do it=1,size(equilibrium_out)
    !
    ! datainfo
    !
    equilibrium_out(it)%datainfo%cocos = icocos_out
    !
    ! In global_param:
    !
    equilibrium_out(it)%global_param%i_plasma = sigma_Ip_eff * equilibrium_out(it)%global_param%i_plasma
    equilibrium_out(it)%global_param%psi_ax = fact_psi * equilibrium_out(it)%global_param%psi_ax
    equilibrium_out(it)%global_param%psi_bound = fact_psi * equilibrium_out(it)%global_param%psi_bound
    equilibrium_out(it)%global_param%mag_axis%bphi = sigma_B0_eff * equilibrium_out(it)%global_param%mag_axis%bphi
    equilibrium_out(it)%global_param%mag_axis%q = fact_q * equilibrium_out(it)%global_param%mag_axis%q
    equilibrium_out(it)%global_param%q_95 = fact_q * equilibrium_out(it)%global_param%q_95
    equilibrium_out(it)%global_param%q_min = fact_q * equilibrium_out(it)%global_param%q_min
    equilibrium_out(it)%global_param%toroid_field%b0 = sigma_B0_eff * equilibrium_out(it)%global_param%toroid_field%b0
    !
    ! profiles_1d:
    !
    equilibrium_out(it)%profiles_1d%psi = fact_psi * equilibrium_out(it)%profiles_1d%psi
    equilibrium_out(it)%profiles_1d%phi = sigma_B0_eff * equilibrium_out(it)%profiles_1d%phi
    equilibrium_out(it)%profiles_1d%F_dia = sigma_B0_eff * equilibrium_out(it)%profiles_1d%F_dia
    equilibrium_out(it)%profiles_1d%pprime = sigma_Ip_eff * sigma_Bp_eff / TWOPI**exp_Bp_eff &
         & * equilibrium_out(it)%profiles_1d%pprime
    equilibrium_out(it)%profiles_1d%ffprime = sigma_Ip_eff * sigma_Bp_eff / TWOPI**exp_Bp_eff &
         & * equilibrium_out(it)%profiles_1d%ffprime
    equilibrium_out(it)%profiles_1d%jphi = sigma_Ip_eff * equilibrium_out(it)%profiles_1d%jphi
    equilibrium_out(it)%profiles_1d%jparallel = sigma_Ip_eff * equilibrium_out(it)%profiles_1d%jparallel
    equilibrium_out(it)%profiles_1d%q = fact_q * equilibrium_out(it)%profiles_1d%q
    equilibrium_out(it)%profiles_1d%dpsidrho_tor = fact_psi * equilibrium_out(it)%profiles_1d%dpsidrho_tor
    equilibrium_out(it)%profiles_1d%vprime = equilibrium_out(it)%profiles_1d%vprime / fact_psi
    ! no change for equilibrium_out(it)%profiles_1d%dvdrho
    equilibrium_out(it)%profiles_1d%aprime = equilibrium_out(it)%profiles_1d%aprime / fact_psi
    equilibrium_out(it)%profiles_1d%b_av = sigma_B0_eff * equilibrium_out(it)%profiles_1d%b_av
    equilibrium_out(it)%profiles_1d%b_min = sigma_B0_eff * equilibrium_out(it)%profiles_1d%b_min
    equilibrium_out(it)%profiles_1d%b_max = sigma_B0_eff * equilibrium_out(it)%profiles_1d%b_max
    equilibrium_out(it)%profiles_1d%omega = sigma_RphiZ_eff * equilibrium_out(it)%profiles_1d%omega
    equilibrium_out(it)%profiles_1d%omegaprime = sigma_RphiZ_eff * equilibrium_out(it)%profiles_1d%omegaprime / fact_psi
    ! not touched so far:
    ! mach_a
    ! phi_flow
    ! s_flow
    ! h_flow
    !
    ! profiles_2d(j):
    !
    do j=1,size(equilibrium_out(it)%profiles_2d)
      equilibrium_out(it)%profiles_2d(j)%psi = fact_psi * equilibrium_out(it)%profiles_2d(j)%psi
      equilibrium_out(it)%profiles_2d(j)%theta = sigma_RphiZ_eff * sigma_rhothetaphi_eff &
           & * equilibrium_out(it)%profiles_2d(j)%theta
      equilibrium_out(it)%profiles_2d(j)%jphi = sigma_Ip_eff * equilibrium_out(it)%profiles_2d(j)%jphi
      equilibrium_out(it)%profiles_2d(j)%jpar = sigma_Ip_eff * equilibrium_out(it)%profiles_2d(j)%jpar
      ! if sign(Ip_out(it)) follows from sign(Ip_in), then sigma_Ip_eff=sigma_RphiZ_eff and BR_out(it)=BR_in as it should
      ! if sign(Ip) is changed on demand fro inputs, then BR should follow
      equilibrium_out(it)%profiles_2d(j)%br = sigma_RphiZ_eff * sigma_Ip_eff * equilibrium_out(it)%profiles_2d(j)%br
      equilibrium_out(it)%profiles_2d(j)%bz = sigma_RphiZ_eff * sigma_Ip_eff * equilibrium_out(it)%profiles_2d(j)%bz
      equilibrium_out(it)%profiles_2d(j)%bphi = sigma_B0_eff * equilibrium_out(it)%profiles_2d(j)%bphi
      equilibrium_out(it)%profiles_2d(j)%vphi = sigma_RphiZ_eff * equilibrium_out(it)%profiles_2d(j)%vphi
      equilibrium_out(it)%profiles_2d(j)%vtheta = sigma_RphiZ_eff * sigma_rhothetaphi_eff &
           & * equilibrium_out(it)%profiles_2d(j)%vtheta
    end do
    !
    ! coord_sys: Assumed to be a flux surface coordinate system ala f(psi), f(theta), phi, thus follows sigma_rhothetaphi
    ! If "1" is psi, then fact_psi should be included, but if rho is used, then it should not
    ! 
    i_gridtype = -99
    IF (associated(equilibrium_out(it)%coord_sys%grid_type)) THEN
      read(equilibrium_out(it)%coord_sys%grid_type(1),*,ERR=99) i_gridtype
      IF (i_gridtype .EQ. 2) THEN
        ! Assumes grid_type inverse ala psi, chi, phi, so should rescale using psi_fac for g_1j and rhothetphi for J
        equilibrium_out(it)%coord_sys%grid%dim1 = fact_psi * equilibrium_out(it)%coord_sys%grid%dim1
        equilibrium_out(it)%coord_sys%jacobian = equilibrium_out(it)%coord_sys%jacobian / abs(fact_psi)
        equilibrium_out(it)%coord_sys%g_11 = fact_psi**2 * equilibrium_out(it)%coord_sys%g_11
        equilibrium_out(it)%coord_sys%g_12 = sigma_rhothetaphi_eff * fact_psi * equilibrium_out(it)%coord_sys%g_12
        equilibrium_out(it)%coord_sys%g_13 = fact_psi * equilibrium_out(it)%coord_sys%g_13
        equilibrium_out(it)%coord_sys%g_23 = sigma_rhothetaphi_eff * equilibrium_out(it)%coord_sys%g_23
      ELSE
        print *,'equilibrium_out(it)%coord_sys%grid_type(1) = ',equilibrium_out(it)%coord_sys%grid_type(1), &
             & ' assumes no effect of COCOS required in CPOequilibrium%coord_sys'
      END IF
99    IF (i_gridtype .EQ. -99) THEN
        print *,' Cannot read grid_type index, thus assumes no effect of COCOS required in CPOequilibrium%coord_sys'
        print *,'equilibrium_out(it)%coord_sys%grid_type(1) = ',equilibrium_out(it)%coord_sys%grid_type(1)
      END IF
    ELSE
      print *,'equilibrium_out(it)%coord_sys%grid_type not ASSOCIATED => ', &
           & 'assumes no effect of COCOS required in CPOequilibrium%coord_sys'
    END IF
    !
  END do
  !
  return
  !
end subroutine COCOStransform
