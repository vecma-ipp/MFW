subroutine copy_itm_to_ids_equilibrium(equil_in_itm,equil_out_ids,flag_status,extra_data_itm_to_ids,extra_option)
  !
  ! copy ITM CPO-equilibrium structure to IDS structure.
  ! copy whole times if timebase homogeneous, do not know if it could be otherwise in Equilibrium actually?
  !
  ! extra_option = 1: use extra_data_itm_to_ids to fill in extra arrays (return if expected keywords not present)
  !              = 0: do not use extra_data_itm_to_ids and copy only itm-cpo to ids (default)
  !
  ! flag_status = 0 if OK, 1 if not (equil_in_itm not associated or size<1)
  !
  use globals, ONLY : NPROF2D, R0EXP, NISO1EFF1, NVERBOSE, NER, NEGP
  use euITM_schemas ! CPO definitions, note should be linked onto euitm_schemas_4.10a.3_R1.1.2.f90
  use ids_schemas   ! IDS definitions, note should be linked onto ids_schemas_vDD3_0.0PUAL3_0.0.f90
  use itm_ids_utilities ! to give extra_data for ids
  !
  IMPLICIT NONE
  !
  type(type_equilibrium),pointer   :: equil_in_itm(:)
  type(ids_equilibrium)    :: equil_out_ids
  type(extra_data_gen),optional,pointer :: extra_data_itm_to_ids(:) ! scans through same time index as equil_in_itm
  integer :: flag_status
  integer, optional :: extra_option
  !
  integer :: i, it, iextra_option, nb_times, ndim1, ndim2, nb_2d, max_nb_points, nb_lines, nb_points, &
    & npsi_1d, ishear, iftrap, iprof2d, iextra, ilength_pol
  integer :: index_1, index_2, index_ids
  character*132 :: radial_label, radial_descr, theta_label, theta_descr
  CHARACTER  ZDATE*8
  !
  !**********************************************************************
  !
  flag_status = 1
  !
  iextra_option = 0
  if (present(extra_option)) iextra_option = extra_option
  !
  nb_times = 1
  if (associated(equil_in_itm)) then
    nb_times = size(equil_in_itm)
  else
    ! not associated, return
    return
  end if
  if (nb_times .lt. 1) then
    ! problem with size of input
    return
  end if
  if (iextra_option .EQ. 1) then
    if (present(extra_data_itm_to_ids) .AND. size(extra_data_itm_to_ids).lt.nb_times) then
      print *,'size(extra_data_itm_to_ids) <nb_times, do not use it'
      iextra_option = 0
    end if
  end if
  if (nverbose .ge. 3) write(*,*) 'nb_times = ',nb_times,' in copy_itm_to_ids'
  !
  ! find relevant keywords, assume same for all times
  ishear = 0
  ilength_pol = 0
  it = 1
  i=size(extra_data_itm_to_ids(it)%prof1d_list)
  ! print *,'salut1, i=',i
  do iextra=1,i
    ! print *,'salut2, iextra=',iextra
    if ( 'magnetic_shear' .eq. trim(extra_data_itm_to_ids(it)%prof1d_list(iextra)%name) ) then
      ishear = iextra
    end if
    if ( 'length_pol' .eq. trim(extra_data_itm_to_ids(it)%prof1d_list(iextra)%name) ) then
      ilength_pol = iextra
    end if
  end do
  !
  ! 1. Fill in NON-time dependent quantities, assume corresponds to equil_in_itm(nb_times) (last being most recent?)
  !    Assume not homogeneous time base, since do not know, but fill in both %time and %time_slice(:)%time
  !
  ! 1.1 IDS_properties
  !
  equil_out_ids%ids_properties%homogeneous_time = 1

  allocate(equil_out_ids%ids_properties%comment(size(equil_in_itm(nb_times)%datainfo%comment)))
  equil_out_ids%ids_properties%comment = equil_in_itm(nb_times)%datainfo%comment
  ! equil_out_ids%ids_properties%cocos = equil_in_itm(nb_times)%datainfo%cocos
  !
  ! Other than IDS_Properties
  !
  ! 2. Time dependent part
  !
  ! 2.1 Allocate first all arrays before do loop on time index
  ! Also set values when it is the same for all times
  !
  allocate(equil_out_ids%time(nb_times))
  allocate(equil_out_ids%vacuum_toroidal_field%b0(nb_times))
  allocate(equil_out_ids%time_slice(nb_times))
  allocate(equil_out_ids%code%output_flag(nb_times))
  !
  equil_out_ids%vacuum_toroidal_field%r0 = R0EXP
  if ( NISO1EFF1 .ne. size(equil_in_itm(1)%profiles_1d%psi) ) then
    write(0,*) 'problem with size(equil_in_itm(1)%profiles_1d%psi) which should be equal to NISO1EFF1'
    return
  end if
  do it=1,nb_times
    !
    equil_out_ids%time(it) = equil_in_itm(it)%time
    equil_out_ids%time_slice(it)%time = equil_in_itm(it)%time
    !
    npsi_1d = size(equil_in_itm(it)%profiles_1d%psi)  ! needed for some global_quantities as well (should be NISO1EFF1)
    !
    ! vacuum_toroidal_field
    equil_out_ids%vacuum_toroidal_field%b0(it) = equil_in_itm(it)%global_param%toroid_field%B0 * equil_in_itm(it)%global_param%toroid_field%R0 / R0EXP
    !
    ! time_slice(it)
    !
    ! boundary
    equil_out_ids%time_slice(it)%boundary%type = equil_in_itm(it)%eqgeometry%boundarytype
    nb_points = size(equil_in_itm(it)%eqgeometry%boundary(1)%r)
    allocate(equil_out_ids%time_slice(it)%boundary%lcfs%r(nb_points))
    equil_out_ids%time_slice(it)%boundary%lcfs%r(1:nb_points) = equil_in_itm(it)%eqgeometry%boundary(1)%r(1:nb_points)
    allocate(equil_out_ids%time_slice(it)%boundary%lcfs%z(nb_points))
    equil_out_ids%time_slice(it)%boundary%lcfs%z(1:nb_points) = equil_in_itm(it)%eqgeometry%boundary(1)%z(1:nb_points)
    equil_out_ids%time_slice(it)%boundary%geometric_axis%r = equil_in_itm(it)%eqgeometry%geom_axis%r
    equil_out_ids%time_slice(it)%boundary%geometric_axis%z = equil_in_itm(it)%eqgeometry%geom_axis%z
    equil_out_ids%time_slice(it)%boundary%minor_radius = equil_in_itm(it)%eqgeometry%a_minor
    equil_out_ids%time_slice(it)%boundary%elongation = equil_in_itm(it)%eqgeometry%elongation
    equil_out_ids%time_slice(it)%boundary%elongation_upper = equil_in_itm(it)%eqgeometry%elong_upper
    equil_out_ids%time_slice(it)%boundary%elongation_lower = equil_in_itm(it)%eqgeometry%elong_lower
    equil_out_ids%time_slice(it)%boundary%triangularity = &
      & 0.5_dp * (equil_in_itm(it)%eqgeometry%tria_upper + equil_in_itm(it)%eqgeometry%tria_lower)
    equil_out_ids%time_slice(it)%boundary%triangularity_upper = equil_in_itm(it)%eqgeometry%tria_upper
    equil_out_ids%time_slice(it)%boundary%triangularity_lower = equil_in_itm(it)%eqgeometry%tria_lower
    ! No X-point information from within CHEASE
    !
    ! Globals
    equil_out_ids%time_slice(it)%global_quantities%beta_pol = equil_in_itm(it)%global_param%beta_pol
    equil_out_ids%time_slice(it)%global_quantities%beta_tor = equil_in_itm(it)%global_param%beta_tor
    equil_out_ids%time_slice(it)%global_quantities%beta_normal = equil_in_itm(it)%global_param%beta_normal
    equil_out_ids%time_slice(it)%global_quantities%ip = equil_in_itm(it)%global_param%i_plasma
    equil_out_ids%time_slice(it)%global_quantities%li_3 = equil_in_itm(it)%global_param%li
    equil_out_ids%time_slice(it)%global_quantities%volume = equil_in_itm(it)%global_param%volume
    equil_out_ids%time_slice(it)%global_quantities%area = equil_in_itm(it)%global_param%area
    print *,'size(equil_out_ids%time_slice)= ',size(equil_out_ids%time_slice)
    print *,'size(equil_in_itm)= ',size(equil_in_itm)
    print *,'size(equil_in_itm(it)%profiles_1d%surface)= ',size(equil_in_itm(it)%profiles_1d%surface)
    equil_out_ids%time_slice(it)%global_quantities%surface = equil_in_itm(it)%profiles_1d%surface(npsi_1d)
    if (iextra_option .eq. 1 .and. ilength_pol .ne. 0) then
      equil_out_ids%time_slice(it)%global_quantities%length_pol = extra_data_itm_to_ids(it)%prof1d_list(ilength_pol)%values(npsi_1d)
    else
      equil_out_ids%time_slice(it)%global_quantities%length_pol = -1.e+40_dp
    end if
    equil_out_ids%time_slice(it)%global_quantities%psi_axis = equil_in_itm(it)%global_param%psi_ax
    equil_out_ids%time_slice(it)%global_quantities%psi_boundary = equil_in_itm(it)%global_param%psi_bound
    equil_out_ids%time_slice(it)%global_quantities%magnetic_axis%r = equil_in_itm(it)%global_param%mag_axis%position%r
    equil_out_ids%time_slice(it)%global_quantities%magnetic_axis%z = equil_in_itm(it)%global_param%mag_axis%position%z
    equil_out_ids%time_slice(it)%global_quantities%magnetic_axis%b_tor = equil_in_itm(it)%global_param%mag_axis%bphi
    equil_out_ids%time_slice(it)%global_quantities%q_axis = equil_in_itm(it)%global_param%mag_axis%q
    equil_out_ids%time_slice(it)%global_quantities%q_95 = equil_in_itm(it)%global_param%q_95
    equil_out_ids%time_slice(it)%global_quantities%q_min%value = equil_in_itm(it)%global_param%q_min
!!$    equil_out_ids%time_slice(it)%global_quantities%q_min%rho_tor_norm = 
    equil_out_ids%time_slice(it)%global_quantities%w_mhd = equil_in_itm(it)%global_param%w_mhd
    !
    ! equil_out_ids%profiles_1d:
    allocate(equil_out_ids%time_slice(it)%profiles_1d%psi(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%psi(1:npsi_1d) = equil_in_itm(it)%profiles_1d%psi(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%phi(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%phi(1:npsi_1d) = equil_in_itm(it)%profiles_1d%phi(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%pressure(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%pressure(1:npsi_1d) = equil_in_itm(it)%profiles_1d%pressure(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%f(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%f(1:npsi_1d) = equil_in_itm(it)%profiles_1d%f_dia(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%dpressure_dpsi(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%dpressure_dpsi(1:npsi_1d) = equil_in_itm(it)%profiles_1d%pprime(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%f_df_dpsi(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%f_df_dpsi(1:npsi_1d) = equil_in_itm(it)%profiles_1d%ffprime(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%j_tor(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%j_tor(1:npsi_1d) = equil_in_itm(it)%profiles_1d%jphi(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%j_parallel(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%j_parallel(1:npsi_1d) = equil_in_itm(it)%profiles_1d%jparallel(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%q(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%q(1:npsi_1d) = equil_in_itm(it)%profiles_1d%q(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%magnetic_shear(npsi_1d))
    if (iextra_option .eq. 1 .and. ishear .ne. 0) then
      equil_out_ids%time_slice(it)%profiles_1d%magnetic_shear(1:npsi_1d) = extra_data_itm_to_ids(1)%prof1d_list(ishear)%values(1:npsi_1d)
    else
      equil_out_ids%time_slice(it)%profiles_1d%magnetic_shear(1:npsi_1d) = -1.e+40_dp
    end if
    allocate(equil_out_ids%time_slice(it)%profiles_1d%r_inboard(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%r_inboard(1:npsi_1d) = equil_in_itm(it)%profiles_1d%r_inboard(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%r_outboard(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%r_outboard(1:npsi_1d) = equil_in_itm(it)%profiles_1d%r_outboard(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%rho_tor(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%rho_tor(1:npsi_1d) = equil_in_itm(it)%profiles_1d%rho_tor(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%rho_tor_norm(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%rho_tor_norm(1:npsi_1d) = equil_in_itm(it)%profiles_1d%rho_tor(1:npsi_1d) / equil_in_itm(it)%profiles_1d%rho_tor(npsi_1d)
    equil_out_ids%time_slice(it)%profiles_1d%rho_tor_norm(npsi_1d) = 1.0_dp
    allocate(equil_out_ids%time_slice(it)%profiles_1d%dpsi_drho_tor(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%dpsi_drho_tor(1:npsi_1d) = equil_in_itm(it)%profiles_1d%dpsidrho_tor(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%elongation(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%elongation(1:npsi_1d) = equil_in_itm(it)%profiles_1d%elongation(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%triangularity_upper(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%triangularity_upper(1:npsi_1d) = equil_in_itm(it)%profiles_1d%tria_upper(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%triangularity_lower(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%triangularity_lower(1:npsi_1d) = equil_in_itm(it)%profiles_1d%tria_lower(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%volume(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%volume(1:npsi_1d) = equil_in_itm(it)%profiles_1d%volume(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%dvolume_dpsi(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%dvolume_dpsi(1:npsi_1d) = equil_in_itm(it)%profiles_1d%vprime(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%dvolume_drho_tor(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%dvolume_drho_tor(1:npsi_1d) = equil_in_itm(it)%profiles_1d%dvdrho(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%area(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%area(1:npsi_1d) = equil_in_itm(it)%profiles_1d%area(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%darea_dpsi(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%darea_dpsi(1:npsi_1d) = equil_in_itm(it)%profiles_1d%aprime(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%surface(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%surface(1:npsi_1d) = equil_in_itm(it)%profiles_1d%surface(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%trapped_fraction(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%trapped_fraction(1:npsi_1d) = equil_in_itm(it)%profiles_1d%ftrap(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm1(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm1(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm1(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm2(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm2(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm2(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm3(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm3(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm3(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm4(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm4(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm4(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm5(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm5(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm5(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm6(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm6(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm6(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm7(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm7(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm7(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm8(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm8(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm8(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%gm9(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%gm9(1:npsi_1d) = equil_in_itm(it)%profiles_1d%gm9(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%b_average(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%b_average(1:npsi_1d) = equil_in_itm(it)%profiles_1d%b_av(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%b_min(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%b_min(1:npsi_1d) = equil_in_itm(it)%profiles_1d%b_min(1:npsi_1d)
    allocate(equil_out_ids%time_slice(it)%profiles_1d%b_max(npsi_1d))
    equil_out_ids%time_slice(it)%profiles_1d%b_max(1:npsi_1d) = equil_in_itm(it)%profiles_1d%b_max(1:npsi_1d)
    !
    ! profiles_2d
!!$    if (NPROF2D .gt. 0) then
    nb_2d = size(equil_in_itm(it)%profiles_2d)
    allocate(equil_out_ids%time_slice(it)%profiles_2d(nb_2d))
    do iprof2d=1,nb_2d
      ! grid type, so far use: index "name" 'description'
      ! 1 "rz" 'cylindrical R,Z ala eqdsk, within the corresponding COCOS convention'
      ! 2 "polar"       'true 2D polar coordinates rho, theta with magnetic axis as center of grid; theta and values following the corresponding COCOS convention'
      ! 11 "flux_psi_straight" 'flux surface type with psi as radial label, theta straight-field line (mod(index,10)=1), could be non-equidistant; magnetic axis as center of grid; following the corresponding COCOS convention'
      ! 12 "flux_psi_arc" 'same as 11 but theta to have equal arc (mod(index,10)=2)'
      ! 13 "flux_psi_polar" 'same as 11 but theta as polar angle, could be non-equidistant'
      ! 21 "flux_rhopolnorm_straight" 'same as 11 but radial label is sqrt[(psi-psi_axis)/(psi_edge-psi_axis)]'
      ! 22 "flux_rhopolnorm_arc" 'same as 12 but radial label is sqrt[(psi-psi_axis)/(psi_edge-psi_axis)]'
      ! 23 "flux_rhopolnorm_polar" 'same as 13 but radial label is sqrt[(psi-psi_axis)/(psi_edge-psi_axis)]'
      ! 31 to 33: same but "flux_rhotornorm_straight", "flux_rhotornorm_arc", "flux_rhotornorm_polar"
      !
      ! 91 "irregular_rz" 'irregular grid, thus give list of vertices in dim1(1:ndim1), dim2(1:ndim1) and then all fields are on values(1:ndim1,1); needs to know if R,Z, psi,theta for dim1, dim2?'
      !
      read(equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(1),*) index_1
      read(equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(3),*) index_2
      if ( index_1 .eq. 1) then
        ! should be R,Z case
        if ( (index_2 .gt. 0) .and. (nverbose.ge.3) ) write(*,*) &
          & 'index_1 of itm grid_type is 1, but index_2 is >0, not sure what it means, assume R,Z'
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%index = 1
        allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%name(1))
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%name(1) = 'tz'
        allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(1))
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(1) = &
          & 'cylindrical R,Z ala eqdsk, within the corresponding COCOS convention'
      elseif ( index_1 .eq. 2) then
        ! inverse flux-type, but do not know yet radial coordinate
        index_ids = 0
        do i=1,len(equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(2))-3
          if ( equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(2)(i:i+2) .eq. 'psi' ) index_ids = 10
        end do
        do i=1,len(equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(2))-11
          if ( equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(2)(i:i+11) .eq. 'rho_pol_norm' ) index_ids = 20
        end do
        do i=1,len(equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(2))-11
          if ( equil_in_itm(it)%profiles_2d(iprof2d)%grid_type(2)(i:i+11) .eq. 'rho_tor_norm' ) index_ids = 30
        end do
        if (index_2 .le. 0 .or.index_2 .ge. 4) then
          write(0,*) 'index_2 = ',index_2,' is unexpected, thus cannot set correctly profiles_2d, ask O. Sauter'
        end if
        index_ids = index_ids + index_2
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%index = index_ids
        if (int(index_ids/10) .eq. 1) then
          radial_label = 'psi'
          radial_descr = 'the radial label is poloidal flux psi'
        elseif (int(index_ids/10) .eq. 2) then
          radial_label = 'rhopolnorm'
          radial_descr = 'the radial label is sqrt[(psi-psi_axis)/(psi_edge-psi_axis)]'
        elseif (int(index_ids/10) .eq. 3) then
          radial_label = 'rhotornorm'
          radial_descr = 'the radial label is sqrt[phi/phi_edge], phi being the toroidal flux'
        else
          radial_label = 'unknown'
        end if
        if (index_2 .eq. 1) then
          theta_label = 'straight'
          theta_descr = 'theta straight-field line thetastar'
        elseif (index_2 .eq. 2) then
          theta_label = 'arc'
          theta_descr = 'theta chosen to have equal arc on last closed flux surface'
        elseif (index_2 .eq. 3) then
          theta_label = 'polar'
          theta_descr = 'theta is standard geometrical-polar coordinate'
        else
          theta_label = 'unknown'
          theta_descr = 'theta choice unknown'
        end if
        allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%name(1))
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%name(1) = 'flux_' // trim(radial_label) // '_' // trim(theta_label)
        allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(2))
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(1) = &
          & 'flux surface type where ' // trim(radial_descr) // ', and the ' // trim(theta_descr)
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(2) = &
          & ', could be non-equidistant; magnetic axis as center of grid; following the corresponding COCOS convention'
      else
        write(0,*) 'index_2 = ',index_2,' do not know what to do for grid_type, put dummy stuff'
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%index = 10*index_1 + index_2
        allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%name(1))
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%name(1) = 'unknown'
        allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(1))
        equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid_type%description(1) = 'index_2>2, do not know what to do for grid_type, put dummy stuff'
      end if
      !
      ndim1 = size(equil_in_itm(it)%profiles_2d(iprof2d)%grid%dim1)
      ndim2 = size(equil_in_itm(it)%profiles_2d(iprof2d)%grid%dim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid%dim1(ndim1))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid%dim1(1:ndim1) = equil_in_itm(it)%profiles_2d(iprof2d)%grid%dim1(1:ndim1)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid%dim2(ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%grid%dim2(1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%grid%dim2(1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%r(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%r(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%r(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%z(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%z(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%z(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%psi(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%psi(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%psi(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%theta(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%theta(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%theta(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%phi(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%phi(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%phi(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%j_tor(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%j_tor(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%jphi(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%j_parallel(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%j_parallel(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%jpar(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%b_r(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%b_r(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%br(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%b_z(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%b_z(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%bz(1:ndim1,1:ndim2)
      allocate(equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%b_tor(ndim1,ndim2))
      equil_out_ids%time_slice(it)%profiles_2d(iprof2d)%b_tor(1:ndim1,1:ndim2) = equil_in_itm(it)%profiles_2d(iprof2d)%bphi(1:ndim1,1:ndim2)
    end do
    !
    ! coordinate_system:
    equil_out_ids%time_slice(it)%coordinate_system%grid_type%index = 14
    allocate(equil_out_ids%time_slice(it)%coordinate_system%grid_type%name(1))
    equil_out_ids%time_slice(it)%coordinate_system%grid_type%name(1) = 'flux_psi_general'
    equil_out_ids%time_slice(it)%coordinate_system%grid_type%name(1) = 'flux_psi_general'
    allocate(equil_out_ids%time_slice(it)%coordinate_system%grid_type%description(2))
    equil_out_ids%time_slice(it)%coordinate_system%grid_type%description(1) = &
      & 'flux surface type (psi,theta) with jacobian~coeff |grad psi|^NEGP R^NER'
    write(equil_out_ids%time_slice(it)%coordinate_system%grid_type%description(2),*) 'NEGP=',NEGP,' NER=',NER
    ndim1 = size(equil_in_itm(1)%coord_sys%grid%dim1)
    ndim2 = size(equil_in_itm(1)%coord_sys%grid%dim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%grid%dim1(ndim1))
    equil_out_ids%time_slice(it)%coordinate_system%grid%dim1(1:ndim1) = equil_in_itm(it)%coord_sys%grid%dim1(1:ndim1)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%grid%dim2(ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%grid%dim2(1:ndim2) = equil_in_itm(it)%coord_sys%grid%dim2(1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%r(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%r(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%position%r(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%z(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%z(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%position%z(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%jacobian(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%jacobian(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%jacobian(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%g11_contravariant(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%g11_contravariant(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%g_11(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%g12_contravariant(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%g12_contravariant(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%g_12(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%g13_contravariant(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%g13_contravariant(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%g_13(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%g22_contravariant(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%g22_contravariant(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%g_22(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%g23_contravariant(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%g23_contravariant(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%g_23(1:ndim1,1:ndim2)
    allocate(equil_out_ids%time_slice(it)%coordinate_system%g33_contravariant(ndim1,ndim2))
    equil_out_ids%time_slice(it)%coordinate_system%g33_contravariant(1:ndim1,1:ndim2) = equil_in_itm(it)%coord_sys%g_33(1:ndim1,1:ndim2)
    !
    equil_out_ids%code%output_flag(it) = equil_in_itm(it)%codeparam%output_flag
  end do
  !
  ! code_parameters: at this stage, use array of structure of code%parameters to store input xml lines so use last equilibrium
  nb_lines = size(equil_in_itm(nb_times)%codeparam%codename)
  if ( nb_lines .gt. 0) then
    allocate(equil_out_ids%code%name(nb_lines))
    equil_out_ids%code%name(1:nb_lines) = equil_in_itm(nb_times)%codeparam%codename(1:nb_lines)
    print *,'size(equil_out_ids%code%name)= ',size(equil_out_ids%code%name)
    print *,'equil_out_ids%code%name(1) = ',equil_out_ids%code%name(1)
  end if
  nb_lines = size(equil_in_itm(nb_times)%codeparam%codeversion)
  if ( nb_lines .gt. 0) then
    allocate(equil_out_ids%code%version(nb_lines))
    equil_out_ids%code%version(1:nb_lines) = equil_in_itm(nb_times)%codeparam%codeversion(1:nb_lines)
    print *,'size(equil_out_ids%code%version)= ',size(equil_out_ids%code%version)
    print *,'equil_out_ids%code%version(1) = ',equil_out_ids%code%version(1)(70:100)
    print *,'len(equil_out_ids%code%version(1)) = ',len(equil_out_ids%code%version(1))
  end if
  nb_lines = size(equil_in_itm(nb_times)%codeparam%parameters)
  print *,'nb_lines for parameters = ',nb_lines
  if ( nb_lines .gt. 0) then
    allocate(equil_out_ids%code%parameters(nb_lines))
    equil_out_ids%code%parameters(1:nb_lines) = equil_in_itm(nb_times)%codeparam%parameters(1:nb_lines)
    print *,'equil_out_ids%code%parameters(1)= ',equil_out_ids%code%parameters(1)
  else
    allocate(equil_out_ids%code%parameters(1))
    equil_out_ids%code%parameters(1) = 'to have something'
  end if
  !
  flag_status = 0
  !
  return
END subroutine copy_itm_to_ids_equilibrium
