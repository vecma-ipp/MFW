subroutine copy_ids_to_itm_equilibrium(equil_in_ids,equil_out_itm,flag_status)
  !
  ! copy ids version DD3_0.0PUAL3_0.0 equilibrium structure to eu-itm version 4_10a CPO structure.
  !
  ! flag_status = 0 if OK, 1 if not
  !
  USE globals, ONLY : NVERBOSE, RC1P
  use euITM_schemas ! CPO definitions, note should be linked onto euitm_schemas_4.10a.3_R1.1.2.f90
  use ids_schemas   ! IDS definitions, note should be linked onto ids_schemas_vDD3_0.0PUAL3_0.0.f90
  !
  IMPLICIT NONE
  !
  type(ids_equilibrium)     :: equil_in_ids
  type(type_equilibrium),pointer      :: equil_out_itm(:)
  integer :: flag_status
  !
  CHARACTER  ZDATE*8
  integer :: i, it, nb_times, npsi_1d, nb_profiles_2d, ndim1_coord_sys, ndim2_coord_sys, nb_points, &
       & nb_dim_codename, nb_dim_codeversion, nb_lines_parameters, nb_lines_output_diag, &
       & is_homogeneous_time, nb_top_time, nb_lines_output_flag, ndim1_prof2d, ndim2_prof2d
  !
  !**********************************************************************
  !
  flag_status = 1
  !
  ! check ids_properties/homogeneous_time to know which "time" to use (note only for time_slice(:)/time)
  is_homogeneous_time = equil_in_ids%ids_properties%homogeneous_time
  !
  nb_times = 1
  if (associated(equil_in_ids%time_slice)) then
    if (is_homogeneous_time .eq. 1) then
      nb_times = size(equil_in_ids%time)
    else
      nb_times = size(equil_in_ids%time_slice)
    end if
  else
    ! not associated, return
    return
  end if
  if (nb_times .lt. 1) then
    ! problem with size of input
    return
  end if
  !
  allocate(equil_out_itm(nb_times))
  !
  nb_top_time = size(equil_in_ids%time) ! for code and vacuum_toroidal_field
  if (nb_top_time .ne. nb_times) then
    if (nverbose .ge. 1) then
      write(0,*) 'Note that size(equil_in_ids%time) is not the same as size(equil_in_ids%time_slice).', &
        & ' Not sure how to copy code parameters and vacuum_toroidal_field'
    end if
  end if
  !
  ! loop over time for time_slice parts and rest if nb_top_time = nb_times
  ! for code parameters use (:) of time for line copy of input
  !
  do it=1,nb_times
    npsi_1d = size(equil_in_ids%time_slice(it)%profiles_1d%psi)
    nb_profiles_2d = size(equil_in_ids%time_slice(it)%profiles_2d)
    ndim1_coord_sys = size(equil_in_ids%time_slice(it)%coordinate_system%grid%dim1,1)
    ndim2_coord_sys = size(equil_in_ids%time_slice(it)%coordinate_system%grid%dim2,1)
    !
    if (is_homogeneous_time .eq. 1) then
      equil_out_itm(it)%time = equil_in_ids%time(it)
    else
      equil_out_itm(it)%time = equil_in_ids%time_slice(it)%time
    end if
    !
    ! datainfo
    !
    allocate(equil_out_itm(it)%datainfo%dataprovider(1))
    equil_out_itm(it)%datainfo%dataprovider(1) = 'copy_ids_vDD3_0.0PUAL3_0.0_to_itm_v4_10a_equilibrium.f90'
    ! should include date
    CALL DATE_AND_TIME(ZDATE)
    allocate(equil_out_itm(it)%datainfo%putdate(1))
    equil_out_itm(it)%datainfo%putdate = ZDATE
    if ( associated(equil_in_ids%ids_properties%comment) ) then
      allocate(equil_out_itm(it)%datainfo%comment(size(equil_in_ids%ids_properties%comment)))
      equil_out_itm(it)%datainfo%comment = equil_in_ids%ids_properties%comment
    end if
    equil_out_itm(it)%datainfo%cocos = equil_in_ids%ids_properties%cocos
    !
    ! eqgeometry
    !
    equil_out_itm(it)%eqgeometry%boundarytype = equil_in_ids%time_slice(it)%boundary%type
    nb_points = size(equil_in_ids%time_slice(it)%boundary%lcfs%r)
    allocate(equil_out_itm(it)%eqgeometry%boundary(1))
    allocate(equil_out_itm(it)%eqgeometry%boundary(1)%r(nb_points))
    allocate(equil_out_itm(it)%eqgeometry%boundary(1)%z(nb_points))
    equil_out_itm(it)%eqgeometry%boundary(1)%r(1:nb_points) = equil_in_ids%time_slice(it)%boundary%lcfs%r(1:nb_points)
    equil_out_itm(it)%eqgeometry%boundary(1)%z(1:nb_points) = equil_in_ids%time_slice(it)%boundary%lcfs%z(1:nb_points)
    equil_out_itm(it)%eqgeometry%geom_axis%r = equil_in_ids%time_slice(it)%boundary%geometric_axis%r
    equil_out_itm(it)%eqgeometry%geom_axis%z = equil_in_ids%time_slice(it)%boundary%geometric_axis%z
    equil_out_itm(it)%eqgeometry%a_minor = equil_in_ids%time_slice(it)%boundary%a_minor
    equil_out_itm(it)%eqgeometry%elongation = equil_in_ids%time_slice(it)%boundary%elongation
    equil_out_itm(it)%eqgeometry%elong_upper = equil_in_ids%time_slice(it)%boundary%elongation_upper
    equil_out_itm(it)%eqgeometry%elong_lower = equil_in_ids%time_slice(it)%boundary%elongation_lower
    equil_out_itm(it)%eqgeometry%tria_upper = equil_in_ids%time_slice(it)%boundary%triangularity_upper
    equil_out_itm(it)%eqgeometry%tria_lower = equil_in_ids%time_slice(it)%boundary%triangularity_lower
    !
    ! global_param
    equil_out_itm(it)%global_param%beta_pol = equil_in_ids%time_slice(it)%global_quantities%beta_pol
    equil_out_itm(it)%global_param%beta_tor = equil_in_ids%time_slice(it)%global_quantities%beta_tor
    equil_out_itm(it)%global_param%beta_normal = equil_in_ids%time_slice(it)%global_quantities%beta_normal
    equil_out_itm(it)%global_param%i_plasma = equil_in_ids%time_slice(it)%global_quantities%ip
    equil_out_itm(it)%global_param%li = equil_in_ids%time_slice(it)%global_quantities%li_3
    equil_out_itm(it)%global_param%volume = equil_in_ids%time_slice(it)%global_quantities%volume
    equil_out_itm(it)%global_param%area = equil_in_ids%time_slice(it)%global_quantities%area
    equil_out_itm(it)%global_param%psi_ax = equil_in_ids%time_slice(it)%global_quantities%psi_axis
    equil_out_itm(it)%global_param%psi_bound = equil_in_ids%time_slice(it)%global_quantities%psi_boundary
    equil_out_itm(it)%global_param%mag_axis%position%r = equil_in_ids%time_slice(it)%global_quantities%magnetic_axis%r
    equil_out_itm(it)%global_param%mag_axis%position%z = equil_in_ids%time_slice(it)%global_quantities%magnetic_axis%z
    equil_out_itm(it)%global_param%mag_axis%bphi = equil_in_ids%time_slice(it)%global_quantities%b_tor_axis
    equil_out_itm(it)%global_param%mag_axis%q = equil_in_ids%time_slice(it)%global_quantities%q_axis
    equil_out_itm(it)%global_param%q_95 = equil_in_ids%time_slice(it)%global_quantities%q_95
    equil_out_itm(it)%global_param%q_min = equil_in_ids%time_slice(it)%global_quantities%q_min%value
    equil_out_itm(it)%global_param%toroid_field%r0 = equil_in_ids%vacuum_toroidal_field%r0
    if (nb_top_time .eq. nb_times) then
      equil_out_itm(it)%global_param%toroid_field%b0 = equil_in_ids%vacuum_toroidal_field%b0(it)
    elseif (nb_top_time .eq. 1) then
      equil_out_itm(it)%global_param%toroid_field%b0 = equil_in_ids%vacuum_toroidal_field%b0(1)
    else
      write(0,*) 'size of equil%time_slice .ne. size equil%vacuum_... and latter not 1'
      write(0,*) 'Should do interpolation? discuss with O. Sauter'
      write(0,*) 'nb slices = ',nb_times,' size/equil%time)= ',nb_top_time, &
        & ' size(vacuum...b0)= ',size(equil_in_ids%vacuum_toroidal_field%b0)
    end if
    equil_out_itm(it)%global_param%w_mhd = equil_in_ids%time_slice(it)%global_quantities%w_mhd
    !
    ! profiles_1d
    allocate(equil_out_itm(it)%profiles_1d%psi(npsi_1d))
    equil_out_itm(it)%profiles_1d%psi(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%psi(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%phi(npsi_1d))
    equil_out_itm(it)%profiles_1d%phi(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%phi(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%pressure(npsi_1d))
    equil_out_itm(it)%profiles_1d%pressure(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%pressure(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%f_dia(npsi_1d))
    equil_out_itm(it)%profiles_1d%f_dia(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%f(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%pprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%pprime(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%dpressure_dpsi(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%ffprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%ffprime(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%f_df_dpsi(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%jphi(npsi_1d))
    equil_out_itm(it)%profiles_1d%jphi(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%j_tor(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%jparallel(npsi_1d))
    equil_out_itm(it)%profiles_1d%jparallel(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%j_parallel(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%q(npsi_1d))
    equil_out_itm(it)%profiles_1d%q(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%q(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%r_inboard(npsi_1d))
    equil_out_itm(it)%profiles_1d%r_inboard(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%r_inboard(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%r_outboard(npsi_1d))
    equil_out_itm(it)%profiles_1d%r_outboard(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%r_outboard(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%rho_tor(npsi_1d))
    equil_out_itm(it)%profiles_1d%rho_tor(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%rho_tor(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%dpsidrho_tor(npsi_1d))
    equil_out_itm(it)%profiles_1d%dpsidrho_tor(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%dpsi_drho_tor(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%rho_vol(npsi_1d))
    equil_out_itm(it)%profiles_1d%rho_vol(1:npsi_1d) = sqrt(equil_in_ids%time_slice(it)%profiles_1d%volume(1:npsi_1d) &
      & /equil_in_ids%time_slice(it)%profiles_1d%volume(npsi_1d))
    equil_out_itm(it)%profiles_1d%rho_vol(npsi_1d) = RC1P
    allocate(equil_out_itm(it)%profiles_1d%elongation(npsi_1d))
    equil_out_itm(it)%profiles_1d%elongation(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%elongation(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%tria_upper(npsi_1d))
    equil_out_itm(it)%profiles_1d%tria_upper(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%triangularity_upper(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%tria_lower(npsi_1d))
    equil_out_itm(it)%profiles_1d%tria_lower(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%triangularity_lower(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%volume(npsi_1d))
    equil_out_itm(it)%profiles_1d%volume(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%volume(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%vprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%vprime(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%dvolume_dpsi(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%dvdrho(npsi_1d))
    equil_out_itm(it)%profiles_1d%dvdrho(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%dvolume_drho_tor(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%area(npsi_1d))
    equil_out_itm(it)%profiles_1d%area(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%area(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%aprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%aprime(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%darea_dpsi(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%surface(npsi_1d))
    equil_out_itm(it)%profiles_1d%surface(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%surface(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%ftrap(npsi_1d))
    equil_out_itm(it)%profiles_1d%ftrap(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%trapped_fraction(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm1(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm1(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm1(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm2(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm2(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm2(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm3(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm3(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm3(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm4(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm4(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm4(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm5(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm5(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm5(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm6(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm6(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm6(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm7(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm7(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm7(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm8(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm8(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm8(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%gm9(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm9(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%gm9(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%b_av(npsi_1d))
    equil_out_itm(it)%profiles_1d%b_av(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%b_average(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%b_min(npsi_1d))
    equil_out_itm(it)%profiles_1d%b_min(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%b_min(1:npsi_1d)
    allocate(equil_out_itm(it)%profiles_1d%b_max(npsi_1d))
    equil_out_itm(it)%profiles_1d%b_max(1:npsi_1d) = equil_in_ids%time_slice(it)%profiles_1d%b_max(1:npsi_1d)
    !
    ! profiles_2d
    allocate(equil_out_itm(it)%profiles_2d(nb_profiles_2d))
    do i=1,nb_profiles_2d
      allocate(equil_out_itm(it)%profiles_2d(i)%grid_type(4))
      equil_out_itm(it)%profiles_2d(i)%grid_type(1) = equil_in_ids%time_slice(it)%profiles_2d(i)%grid_type%name(1)
      write(equil_out_itm(it)%profiles_2d(i)%grid_type(2),*) equil_in_ids%time_slice(it)%profiles_2d(i)%grid_type%index,' index'
      equil_out_itm(it)%profiles_2d(i)%grid_type(3) = equil_in_ids%time_slice(it)%profiles_2d(i)%grid_type%description(1)
      write(equil_out_itm(it)%profiles_2d(i)%grid_type(4),*) equil_in_ids%time_slice(it)%profiles_2d(i)%grid_type%index,' index'
      !
      ndim1_prof2d = size(equil_in_ids%time_slice(it)%profiles_2d(i)%grid%dim1)
      ndim2_prof2d = size(equil_in_ids%time_slice(it)%profiles_2d(i)%grid%dim2)
      allocate(equil_out_itm(it)%profiles_2d(i)%grid%dim1(ndim1_prof2d))
      equil_out_itm(it)%profiles_2d(i)%grid%dim1(1:ndim1_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%grid%dim1(1:ndim1_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%grid%dim2(ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%grid%dim2(1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%grid%dim2(1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%r(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%r(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%r(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%z(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%z(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%z(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%psi(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%psi(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%psi(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%theta(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%theta(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%theta(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%phi(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%phi(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%phi(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%jphi(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%jphi(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%j_tor(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%jpar(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%jpar(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%j_parallel(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%br(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%br(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%b_r(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%bz(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%bz(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%b_z(1:ndim1_prof2d,1:ndim2_prof2d)
      allocate(equil_out_itm(it)%profiles_2d(i)%bphi(1:ndim1_prof2d,1:ndim2_prof2d))
      equil_out_itm(it)%profiles_2d(i)%bphi(1:ndim1_prof2d,1:ndim2_prof2d) = equil_in_ids%time_slice(it)%profiles_2d(i)%b_tor(1:ndim1_prof2d,1:ndim2_prof2d)
    end do
    !
    ! coord_sys
    allocate(equil_out_itm(it)%coord_sys%grid_type(4))
    equil_out_itm(it)%coord_sys%grid_type(1) = equil_in_ids%time_slice(it)%coordinate_system%grid_type%name(1)
    write(equil_out_itm(it)%coord_sys%grid_type(2),*) equil_in_ids%time_slice(it)%coordinate_system%grid_type%index,' index'
    equil_out_itm(it)%coord_sys%grid_type(3) = equil_in_ids%time_slice(it)%coordinate_system%grid_type%description(1)
    write(equil_out_itm(it)%coord_sys%grid_type(4),*) equil_in_ids%time_slice(it)%coordinate_system%grid_type%index,' index'
    !
    allocate(equil_out_itm(it)%coord_sys%grid%dim1(ndim1_coord_sys))
    equil_out_itm(it)%coord_sys%grid%dim1(1:ndim1_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%grid%dim1(1:ndim1_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%grid%dim2(ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%grid%dim2(1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%grid%dim2(1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%position%r(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%position%r(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%r(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%position%z(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%position%z(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%z(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%jacobian(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%jacobian(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%jacobian(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%g_11(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_11(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%g_11(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%g_12(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_12(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%g_12(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%g_13(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_13(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%g_13(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%g_22(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_22(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%g_22(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%g_23(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_23(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%g_23(1:ndim1_coord_sys,1:ndim2_coord_sys)
    allocate(equil_out_itm(it)%coord_sys%g_33(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_33(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%time_slice(it)%coordinate_system%g_33(1:ndim1_coord_sys,1:ndim2_coord_sys)
    !
    ! codeparam: time dependent part
    if (nb_top_time .eq. nb_times) then
      equil_out_itm(it)%codeparam%output_flag = equil_in_ids%code%output_flag(it)
    elseif (nb_top_time .eq. 1) then
      equil_out_itm(it)%codeparam%output_flag = equil_in_ids%code%output_flag(1)
    else
      write(0,*) 'size of equil%time_slice .ne. size equil%time and latter not 1'
      write(0,*) 'Should do interpolation? discuss with O. Sauter'
      write(0,*) 'nb slices = ',nb_times,' size/equil%time)= ',nb_top_time, &
        & ' size(code%output_flag)= ',size(equil_in_ids%code%output_flag)
    end if
    !
  end do
  !
  nb_dim_codename = size(equil_in_ids%code%name)
  allocate(equil_out_itm(it)%codeparam%codename(nb_dim_codename))
  equil_out_itm(it)%codeparam%codename = equil_in_ids%code%name
  nb_dim_codeversion = size(equil_in_ids%code%version)
  allocate(equil_out_itm(it)%codeparam%codeversion(nb_dim_codeversion))
  equil_out_itm(it)%codeparam%codeversion = equil_in_ids%code%version
  nb_lines_parameters = size(equil_in_ids%code%parameters)
  if (nb_lines_parameters .GT. 0) then
    allocate(equil_out_itm(it)%codeparam%parameters(nb_lines_parameters))
    equil_out_itm(it)%codeparam%parameters = equil_in_ids%code%parameters
  end if
  !
  flag_status = 0
  !
END subroutine copy_ids_to_itm_equilibrium
