subroutine copy_ids_to_itm_equilibrium(equil_in_ids,equil_out_itm,flag_status)
  !
  ! copy ids equilibrium structure to eu-itm CPO structure.
  !
  ! flag_status = 0 if OK, 1 if not
  !
  use euITM_schemas ! CPO definitions
  use ids_schemas   ! IDS definitions
  !
  IMPLICIT NONE
  !
  type(ids_equilibrium)     :: equil_in_ids
  type(type_equilibrium),pointer      :: equil_out_itm(:)
  integer :: flag_status
  !
  integer :: i, it, nb_times, npsi_1d, nb_Profiles_2D, ndim1_coord_sys, ndim2_coord_sys, nb_points, &
       & nb_dim_codename, nb_dim_codeversion, nb_lines_parameters, nb_lines_output_diag
  integer, allocatable :: ndim1_prof2d(:), ndim2_prof2d(:)
  !
  !**********************************************************************
  !
  flag_status = 1
  !
  nb_times = 1
  if (associated(equil_in_ids%Timebase)) then
    nb_times = size(equil_in_ids%Timebase)
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
  ! need to loop over time for all CPO since time at top in CPO-ITM
  !
  ! dimensions identical for all times in IDS:
  npsi_1d = size(equil_in_ids%Profiles_1D%Psi,1)
  !
  nb_Profiles_2D = size(equil_in_ids%Profiles_2D)
  allocate(ndim1_prof2d(nb_Profiles_2D))
  allocate(ndim2_prof2d(nb_Profiles_2D))
  do i=1,nb_Profiles_2D
    ndim1_prof2d(i) = size(equil_in_ids%Profiles_2D(i)%Grid%Dim1,1)
    ndim2_prof2d(i) = size(equil_in_ids%Profiles_2D(i)%Grid%Dim2,1)
  end do
  !
  ndim1_coord_sys = size(equil_in_ids%Coordinate_System%Grid%Dim1,1)
  ndim2_coord_sys = size(equil_in_ids%Coordinate_System%Grid%Dim2,1)
  !
  nb_dim_codename = size(equil_in_ids%Code_Parameters%Code_Name)
  nb_dim_codeversion = size(equil_in_ids%Code_Parameters%Code_Version)
  nb_lines_parameters = size(equil_in_ids%Code_Parameters%Parameters)
  nb_lines_output_diag = size(equil_in_ids%Code_Parameters%Output_Diagnostics)
  do it=1,nb_times
    !
    equil_out_itm(it)%time = equil_in_ids%Timebase(it)
    !
    ! datainfo
    !
    allocate(equil_out_itm(it)%datainfo%dataprovider(size(equil_in_ids%IDS_Properties%Creator_of)))
    equil_out_itm(it)%datainfo%dataprovider = equil_in_ids%IDS_Properties%Creator_of
    allocate(equil_out_itm(it)%datainfo%putdate(size(equil_in_ids%IDS_Properties%Date_of)))
    equil_out_itm(it)%datainfo%putdate = equil_in_ids%IDS_Properties%Date_of
    allocate(equil_out_itm(it)%datainfo%source(size(equil_in_ids%IDS_Properties%Source_of)))
    equil_out_itm(it)%datainfo%source = equil_in_ids%IDS_Properties%Source_of
    if ( associated(equil_in_ids%IDS_Properties%Comment_of) ) then
      allocate(equil_out_itm(it)%datainfo%comment(size(equil_in_ids%IDS_Properties%Comment_of)))
      equil_out_itm(it)%datainfo%comment = equil_in_ids%IDS_Properties%Comment_of
    end if
    equil_out_itm(it)%datainfo%cocos = equil_in_ids%IDS_Properties%cocos
    !
    ! eqgeometry
    !
    equil_out_itm(it)%eqgeometry%boundarytype = equil_in_ids%Boundary%Type(it)
    nb_points = equil_in_ids%Boundary%N_Outline_Points(it)
    allocate(equil_out_itm(it)%eqgeometry%boundary(1))
    allocate(equil_out_itm(it)%eqgeometry%boundary(1)%r(nb_points))
    allocate(equil_out_itm(it)%eqgeometry%boundary(1)%z(nb_points))
    equil_out_itm(it)%eqgeometry%boundary(1)%r(1:nb_points) = equil_in_ids%Boundary%Outline_RZ(1,1:nb_points,it)
    equil_out_itm(it)%eqgeometry%boundary(1)%z(1:nb_points) = equil_in_ids%Boundary%Outline_RZ(2,1:nb_points,it)
    equil_out_itm(it)%eqgeometry%geom_axis%r = equil_in_ids%Boundary%Geometric_Axis_RZ(1,it)
    equil_out_itm(it)%eqgeometry%geom_axis%z = equil_in_ids%Boundary%Geometric_Axis_RZ(2,it)
    equil_out_itm(it)%eqgeometry%a_minor = equil_in_ids%Boundary%a_minor(it)
    equil_out_itm(it)%eqgeometry%elongation = equil_in_ids%Boundary%Elongation(it)
    equil_out_itm(it)%eqgeometry%elong_upper = equil_in_ids%Boundary%Elongation_Upper(it)
    equil_out_itm(it)%eqgeometry%elong_lower = equil_in_ids%Boundary%Elongation_Lower(it)
    equil_out_itm(it)%eqgeometry%tria_upper = equil_in_ids%Boundary%Triangularity_Upper(it)
    equil_out_itm(it)%eqgeometry%tria_lower = equil_in_ids%Boundary%Triangularity_Lower(it)
    !
    ! global_param
    equil_out_itm(it)%global_param%beta_pol = equil_in_ids%Global%Beta_Pol(it)
    equil_out_itm(it)%global_param%beta_tor = equil_in_ids%Global%Beta_Tor(it)
    equil_out_itm(it)%global_param%beta_normal = equil_in_ids%Global%Beta_Normal(it)
    equil_out_itm(it)%global_param%i_plasma = equil_in_ids%Global%Ip(it)
    equil_out_itm(it)%global_param%li = equil_in_ids%Global%li_3(it)
    equil_out_itm(it)%global_param%volume = equil_in_ids%Global%Volume(it)
    equil_out_itm(it)%global_param%area = equil_in_ids%Global%Area(it)
    equil_out_itm(it)%global_param%psi_ax = equil_in_ids%Global%Psi_Axis(it)
    equil_out_itm(it)%global_param%psi_bound = equil_in_ids%Global%Psi_Boundary(it)
    equil_out_itm(it)%global_param%mag_axis%position%r = equil_in_ids%Global%Magnetic_Axis%R(it)
    equil_out_itm(it)%global_param%mag_axis%position%z = equil_in_ids%Global%Magnetic_Axis%Z(it)
    equil_out_itm(it)%global_param%mag_axis%bphi = equil_in_ids%Global%B_Tor_Axis(it)
    equil_out_itm(it)%global_param%mag_axis%q = equil_in_ids%Global%q_Axis(it)
    equil_out_itm(it)%global_param%q_95 = equil_in_ids%Global%q_95(it)
    equil_out_itm(it)%global_param%q_min = equil_in_ids%Global%q_Min(it)
    equil_out_itm(it)%global_param%toroid_field%R0 = equil_in_ids%Global%Vacuum_Toroidal_Field%R0
    equil_out_itm(it)%global_param%toroid_field%B0 = equil_in_ids%Global%Vacuum_Toroidal_Field%B0(it)
    equil_out_itm(it)%global_param%w_mhd = equil_in_ids%Global%W_MHD(it)
    !
    ! profiles_1d
    allocate(equil_out_itm(it)%profiles_1d%psi(npsi_1d))
    equil_out_itm(it)%profiles_1d%psi(1:npsi_1d) = equil_in_ids%Profiles_1D%Psi(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%phi(npsi_1d))
    equil_out_itm(it)%profiles_1d%phi(1:npsi_1d) = equil_in_ids%Profiles_1D%Phi(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%pressure(npsi_1d))
    equil_out_itm(it)%profiles_1d%pressure(1:npsi_1d) = equil_in_ids%Profiles_1D%Pressure(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%F_dia(npsi_1d))
    equil_out_itm(it)%profiles_1d%F_dia(1:npsi_1d) = equil_in_ids%Profiles_1D%F(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%pprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%pprime(1:npsi_1d) = equil_in_ids%Profiles_1D%dPressure_dPsi(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%ffprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%ffprime(1:npsi_1d) = equil_in_ids%Profiles_1D%F_dF_dPsi(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%jphi(npsi_1d))
    equil_out_itm(it)%profiles_1d%jphi(1:npsi_1d) = equil_in_ids%Profiles_1D%J_Tor(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%jparallel(npsi_1d))
    equil_out_itm(it)%profiles_1d%jparallel(1:npsi_1d) = equil_in_ids%Profiles_1D%J_Parallel(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%q(npsi_1d))
    equil_out_itm(it)%profiles_1d%q(1:npsi_1d) = equil_in_ids%Profiles_1D%q(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%r_inboard(npsi_1d))
    equil_out_itm(it)%profiles_1d%r_inboard(1:npsi_1d) = equil_in_ids%Profiles_1D%R_Inboard(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%r_outboard(npsi_1d))
    equil_out_itm(it)%profiles_1d%r_outboard(1:npsi_1d) = equil_in_ids%Profiles_1D%R_Outboard(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%rho_tor(npsi_1d))
    equil_out_itm(it)%profiles_1d%rho_tor(1:npsi_1d) = equil_in_ids%Profiles_1D%Rho_Tor(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%dpsidrho_tor(npsi_1d))
    equil_out_itm(it)%profiles_1d%dpsidrho_tor(1:npsi_1d) = equil_in_ids%Profiles_1D%dPsi_dRho_Tor(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%rho_vol(npsi_1d))
    equil_out_itm(it)%profiles_1d%rho_vol(1:npsi_1d) = equil_in_ids%Profiles_1D%Rho_Vol_Norm(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%elongation(npsi_1d))
    equil_out_itm(it)%profiles_1d%elongation(1:npsi_1d) = equil_in_ids%Profiles_1D%Elongation(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%tria_upper(npsi_1d))
    equil_out_itm(it)%profiles_1d%tria_upper(1:npsi_1d) = equil_in_ids%Profiles_1D%Triangularity_Upper(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%tria_lower(npsi_1d))
    equil_out_itm(it)%profiles_1d%tria_lower(1:npsi_1d) = equil_in_ids%Profiles_1D%Triangularity_Lower(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%volume(npsi_1d))
    equil_out_itm(it)%profiles_1d%volume(1:npsi_1d) = equil_in_ids%Profiles_1D%Volume(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%vprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%vprime(1:npsi_1d) = equil_in_ids%Profiles_1D%dVolume_dPsi(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%dvdrho(npsi_1d))
    equil_out_itm(it)%profiles_1d%dvdrho(1:npsi_1d) = equil_in_ids%Profiles_1D%dVolume_dRho_Tor(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%area(npsi_1d))
    equil_out_itm(it)%profiles_1d%area(1:npsi_1d) = equil_in_ids%Profiles_1D%Area(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%aprime(npsi_1d))
    equil_out_itm(it)%profiles_1d%aprime(1:npsi_1d) = equil_in_ids%Profiles_1D%dArea_dPsi(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%surface(npsi_1d))
    equil_out_itm(it)%profiles_1d%surface(1:npsi_1d) = equil_in_ids%Profiles_1D%Surface(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%ftrap(npsi_1d))
    equil_out_itm(it)%profiles_1d%ftrap(1:npsi_1d) = equil_in_ids%Profiles_1D%Trapped_Fraction(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm1(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm1(1:npsi_1d) = equil_in_ids%Profiles_1D%gm1(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm2(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm2(1:npsi_1d) = equil_in_ids%Profiles_1D%gm2(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm3(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm3(1:npsi_1d) = equil_in_ids%Profiles_1D%gm3(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm4(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm4(1:npsi_1d) = equil_in_ids%Profiles_1D%gm4(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm5(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm5(1:npsi_1d) = equil_in_ids%Profiles_1D%gm5(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm6(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm6(1:npsi_1d) = equil_in_ids%Profiles_1D%gm6(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm7(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm7(1:npsi_1d) = equil_in_ids%Profiles_1D%gm7(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm8(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm8(1:npsi_1d) = equil_in_ids%Profiles_1D%gm8(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%gm9(npsi_1d))
    equil_out_itm(it)%profiles_1d%gm9(1:npsi_1d) = equil_in_ids%Profiles_1D%gm9(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%b_av(npsi_1d))
    equil_out_itm(it)%profiles_1d%b_av(1:npsi_1d) = equil_in_ids%Profiles_1D%B_Average(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%b_min(npsi_1d))
    equil_out_itm(it)%profiles_1d%b_min(1:npsi_1d) = equil_in_ids%Profiles_1D%B_Min(1:npsi_1d,it)
    allocate(equil_out_itm(it)%profiles_1d%b_max(npsi_1d))
    equil_out_itm(it)%profiles_1d%b_max(1:npsi_1d) = equil_in_ids%Profiles_1D%B_Max(1:npsi_1d,it)
    !
    ! profiles_2d
    allocate(equil_out_itm(it)%profiles_2d(nb_Profiles_2D))
    do i=1,nb_Profiles_2D
      allocate(equil_out_itm(it)%profiles_2d(i)%grid_type(size(equil_in_ids%Profiles_2D(i)%Grid_Type)))
      equil_out_itm(it)%profiles_2d(i)%grid_type = equil_in_ids%Profiles_2D(i)%Grid_Type
      allocate(equil_out_itm(it)%profiles_2d(i)%grid%dim1(ndim1_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%grid%dim1(1:ndim1_prof2d(i)) = equil_in_ids%Profiles_2D(i)%Grid%Dim1(1:ndim1_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%grid%dim2(ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%grid%dim2(1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%Grid%Dim2(1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%r(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%r(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%R(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%z(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%z(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%Z(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%psi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%psi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%Psi(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%theta(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%theta(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%Theta(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%phi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%phi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%Phi(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%jphi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%jphi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%J_Tor(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%jpar(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%jpar(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%J_Parallel(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%br(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%br(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%B_R(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%bz(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%bz(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%B_Z(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
      allocate(equil_out_itm(it)%profiles_2d(i)%bphi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)))
      equil_out_itm(it)%profiles_2d(i)%bphi(1:ndim1_prof2d(i),1:ndim2_prof2d(i)) = equil_in_ids%Profiles_2D(i)%B_Tor(1:ndim1_prof2d(i),1:ndim2_prof2d(i),it)
    end do
    !
    ! coord_sys
    allocate(equil_out_itm(it)%coord_sys%grid_type(size(equil_in_ids%Coordinate_System%Grid_Type)))
    equil_out_itm(it)%coord_sys%grid_type = equil_in_ids%Coordinate_System%Grid_Type
    allocate(equil_out_itm(it)%coord_sys%grid%dim1(ndim1_coord_sys))
    equil_out_itm(it)%coord_sys%grid%dim1(1:ndim1_coord_sys) = equil_in_ids%Coordinate_System%Grid%Dim1(1:ndim1_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%grid%dim2(ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%grid%dim2(1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%Grid%Dim2(1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%position%r(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%position%r(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%R(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%position%z(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%position%z(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%Z(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%jacobian(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%jacobian(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%Jacobian(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%g_11(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_11(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%g_11(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%g_12(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_12(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%g_12(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%g_13(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_13(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%g_13(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%g_22(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_22(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%g_22(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%g_23(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_23(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%g_23(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    allocate(equil_out_itm(it)%coord_sys%g_33(1:ndim1_coord_sys,1:ndim2_coord_sys))
    equil_out_itm(it)%coord_sys%g_33(1:ndim1_coord_sys,1:ndim2_coord_sys) = equil_in_ids%Coordinate_System%g_33(1:ndim1_coord_sys,1:ndim2_coord_sys,it)
    !
    ! codeparam
    allocate(equil_out_itm(it)%codeparam%codename(nb_dim_codename))
    equil_out_itm(it)%codeparam%codename = equil_in_ids%Code_Parameters%Code_Name
    allocate(equil_out_itm(it)%codeparam%codeversion(nb_dim_codeversion))
    equil_out_itm(it)%codeparam%codeversion = equil_in_ids%Code_Parameters%Code_Version
    if (nb_lines_parameters .GT. 0) then
      allocate(equil_out_itm(it)%codeparam%parameters(nb_lines_parameters))
      equil_out_itm(it)%codeparam%parameters = equil_in_ids%Code_Parameters%Parameters
    end if
    if (nb_lines_output_diag .gt. 0) then
      allocate(equil_out_itm(it)%codeparam%output_diag(nb_lines_output_diag))
      equil_out_itm(it)%codeparam%output_diag = equil_in_ids%Code_Parameters%Output_Diagnostics
    end if
    equil_out_itm(it)%codeparam%output_flag = equil_in_ids%Code_Parameters%Output_Flag(it)
    !
  end do
  !
  flag_status = 0
  !
END subroutine copy_ids_to_itm_equilibrium
