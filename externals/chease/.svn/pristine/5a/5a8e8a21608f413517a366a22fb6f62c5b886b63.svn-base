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
  use globals, ONLY : NPROF2D
  use euITM_schemas ! CPO definitions
  use ids_schemas   ! IDS definitions
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
  integer :: i, it, iextra_option, nb_times, ndim1, ndim2, nb_2D, itmax_2D, max_nb_points, max_nb_lines, nb_points, &
    & npsi_1d, ishear, iftrap, iprof2d
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
  !
  ! 1. Fill in NON-time dependent quantities, assume corresponds to equil_in_itm(nb_times) (last being most recent?)
  !
  ! 1.1 IDS_properties
  !
  equil_out_ids%IDS_Properties%Homogeneous_Timebase = 1
  ! allocate(equil_out_ids%IDS_Properties%Status_of(nb_times))
  ! equil_out_ids%IDS_Properties%Status_of = 


  allocate(equil_out_ids%IDS_Properties%Comment_of(size(equil_in_itm(nb_times)%datainfo%comment)))
  equil_out_ids%IDS_Properties%Comment_of = equil_in_itm(nb_times)%datainfo%comment
  allocate(equil_out_ids%IDS_Properties%Creator_of(size(equil_in_itm(nb_times)%datainfo%dataprovider))) ! should contain $USER
  equil_out_ids%IDS_Properties%Creator_of = equil_in_itm(nb_times)%datainfo%dataprovider
  allocate(equil_out_ids%IDS_Properties%Date_of(1))
  CALL DATE_AND_TIME(ZDATE)
  equil_out_ids%IDS_Properties%Date_of(1) = zdate
  allocate(equil_out_ids%IDS_Properties%Source_of(1))
!!$  if (NITMOPT.GT.0 .AND. MOD(NITMOPT,10) .EQ. 1) THEN
!!$    write(equil_out_ids%IDS_Properties%Source_of(1),*) 'IDS from rerunning CHEASE on shot/run= ',nitmshot(1),'/',nitmrun(1)
!!$  else
!!$    write(equil_out_ids%IDS_Properties%Source_of(1),*) 'IDS from running CHEASE from files'
!!$  end if
  ! allocate(equil_out_ids%IDS_Properties%Reference_of(1))
  ! equil_out_ids%IDS_Properties%Reference_of(1) = equil_in_itm(nb_times)%datainfo%whatref ! not sure what it is. Actually is a structure to fill in (by workflow?)
  equil_out_ids%IDS_Properties%Homogeneous_Timebase = 1
  equil_out_ids%IDS_Properties%cocos = equil_in_itm(nb_times)%datainfo%cocos
  !
  ! Other than IDS_Properties
  !
  ! find max length of profiles_2d arrays (versus time)
  nb_2D = size(equil_in_itm(1)%profiles_2d)
  itmax_2D = 1
  do it=2,nb_times
    if (nb_2D .lt. size(equil_in_itm(it)%profiles_2d)) then
      nb_2D = size(equil_in_itm(it)%profiles_2d)
      itmax_2D = it
    end if
  end do
  if (NPROF2D .gt. 0) then
    ! Seems that matlab read V1.1 has a bug with Profiles_2D if not empty so allow keeping it empty
    ! OK with fortran get
    ! print *,'nb_2D= ',nb_2D
    allocate(equil_out_ids%Profiles_2D(nb_2D))
    ! Note: grid_type depends on time in itm and not in ids, so use time with largest nb of elements, 1 otherwise
    do i=1,size(equil_in_itm(itmax_2D)%profiles_2d)
      allocate(equil_out_ids%Profiles_2D(i)%Grid_Type(size(equil_in_itm(itmax_2D)%profiles_2d(i)%grid_type)))
      equil_out_ids%Profiles_2D(i)%Grid_Type = equil_in_itm(itmax_2D)%profiles_2d(i)%grid_type
    end do
  end if
  ! Coordinate_System
  allocate(equil_out_ids%Coordinate_System%Grid_Type(size(equil_in_itm(nb_times)%coord_sys%grid_type)))
  equil_out_ids%Coordinate_System%Grid_Type = equil_in_itm(nb_times)%coord_sys%grid_type
  !
  ! 2. Time dependent part
  !
  ! 2.1 Allocate first all arrays before do loop on time index
  ! Also set values when it is the same for all times
  !
  allocate(equil_out_ids%Timebase(nb_times))
  allocate(equil_out_ids%Boundary%Type(nb_times))
  allocate(equil_out_ids%Boundary%N_Outline_Points(nb_times))
  max_nb_points = size(equil_in_itm(1)%eqgeometry%boundary(1)%r)
  do it=2,nb_times
    max_nb_points = max(max_nb_points,size(equil_in_itm(it)%eqgeometry%boundary(1)%r))
  end do
  allocate(equil_out_ids%Boundary%Outline_RZ(2,max_nb_points,nb_times))
  allocate(equil_out_ids%Boundary%Geometric_Axis_RZ(2,nb_times))
  allocate(equil_out_ids%Boundary%a_minor(nb_times))
  allocate(equil_out_ids%Boundary%Elongation(nb_times))
  allocate(equil_out_ids%Boundary%Elongation_Upper(nb_times))
  allocate(equil_out_ids%Boundary%Elongation_Lower(nb_times))
  allocate(equil_out_ids%Boundary%Triangularity_Upper(nb_times))
  allocate(equil_out_ids%Boundary%Triangularity_Lower(nb_times))
  allocate(equil_out_ids%Global%Beta_Pol(nb_times))
  allocate(equil_out_ids%Global%Beta_Tor(nb_times))
  allocate(equil_out_ids%Global%Beta_Normal(nb_times))
  allocate(equil_out_ids%Global%Ip(nb_times))
  allocate(equil_out_ids%Global%li_3(nb_times))
  allocate(equil_out_ids%Global%Volume(nb_times))
  allocate(equil_out_ids%Global%Area(nb_times))
  allocate(equil_out_ids%Global%Psi_Axis(nb_times))
  allocate(equil_out_ids%Global%Psi_Boundary(nb_times))
  allocate(equil_out_ids%Global%Magnetic_Axis%R(nb_times))
  allocate(equil_out_ids%Global%Magnetic_Axis%Z(nb_times))
  allocate(equil_out_ids%Global%B_Tor_Axis(nb_times))
  allocate(equil_out_ids%Global%q_Axis(nb_times))
  allocate(equil_out_ids%Global%q_95(nb_times))
  allocate(equil_out_ids%Global%q_Min(nb_times))
  ! allocate(equil_out_ids%Global%Vacuum_Toroidal_Field%R0(nb_times)) ! not a time array yet
  allocate(equil_out_ids%Global%Vacuum_Toroidal_Field%B0(nb_times))
  allocate(equil_out_ids%Global%W_MHD(nb_times))
  npsi_1d = size(equil_in_itm(1)%profiles_1d%psi)
  do it=2,nb_times
    npsi_1d = max(npsi_1d,size(equil_in_itm(it)%profiles_1d%psi))
  end do
  allocate(equil_out_ids%Profiles_1D%Psi(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Phi(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Pressure(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%F(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%dPressure_dPsi(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%F_dF_dPsi(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%J_Tor(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%J_Parallel(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%q(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Magnetic_Shear(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%R_Inboard(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%R_Outboard(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Rho_Tor(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%dPsi_dRho_Tor(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Rho_Vol_Norm(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Elongation(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Triangularity_Upper(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Triangularity_Lower(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Volume(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%dVolume_dPsi(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%dVolume_dRho_Tor(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Area(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%dArea_dPsi(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Surface(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%Trapped_Fraction(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm1(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm2(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm3(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm4(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm5(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm6(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm7(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm8(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%gm9(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%B_Average(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%B_Min(npsi_1d,nb_times))
  allocate(equil_out_ids%Profiles_1D%B_Max(npsi_1d,nb_times))
  !
  ! Profiles_2D
  if (NPROF2D .gt. 0) then
    do iprof2d=1,nb_2D
      ! find max_dimensions over time since time is extra dimension in ids, note profiles_2d is an array of structurein both itm and ids
      ndim1 = size(equil_in_itm(1)%profiles_2d(iprof2d)%grid%dim1)
      ndim2 = size(equil_in_itm(1)%profiles_2d(iprof2d)%grid%dim2)
      do it=2,nb_times
        if (size(equil_in_itm(it)%profiles_2d) .GE. iprof2d) then
          ndim1 = max(ndim1,size(equil_in_itm(it)%profiles_2d(iprof2d)%grid%dim1))
          ndim2 = max(ndim2,size(equil_in_itm(it)%profiles_2d(iprof2d)%grid%dim2))
        end if
      end do
      allocate(equil_out_ids%Profiles_2D(iprof2d)%Grid%Dim1(ndim1,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%Grid%Dim2(ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%R(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%Z(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%Psi(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%Theta(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%Phi(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%J_Tor(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%J_Parallel(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%B_R(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%B_Z(ndim1,ndim2,nb_times))
      allocate(equil_out_ids%Profiles_2D(iprof2d)%B_Tor(ndim1,ndim2,nb_times))
    end do
  end if
  !
  ndim1 = size(equil_in_itm(1)%coord_sys%grid%dim1)
  ndim2 = size(equil_in_itm(1)%coord_sys%grid%dim2)
  do it=2,nb_times
    ndim1 = max(ndim1,size(equil_in_itm(it)%coord_sys%grid%dim1))
    ndim2 = max(ndim2,size(equil_in_itm(it)%coord_sys%grid%dim2))
  end do
  allocate(equil_out_ids%Coordinate_System%Grid%Dim1(ndim1,nb_times))
  allocate(equil_out_ids%Coordinate_System%Grid%Dim2(ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%R(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%Z(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%Jacobian(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%g_11(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%g_12(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%g_13(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%g_22(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%g_23(ndim1,ndim2,nb_times))
  allocate(equil_out_ids%Coordinate_System%g_33(ndim1,ndim2,nb_times))
  !
  ! equil_out_ids%Code_Parameters:
  max_nb_lines = size(equil_in_itm(1)%codeparam%codename)
  itmax_2D = 1
  do it=2,nb_times
    if (nb_2D .lt. size(equil_in_itm(it)%codeparam%codename)) then
      max_nb_lines = size(equil_in_itm(it)%codeparam%codename)
      itmax_2D = it
    end if
  end do
  allocate(equil_out_ids%Code_Parameters%Code_Name(max_nb_lines))
  equil_out_ids%Code_Parameters%Code_Name = equil_in_itm(itmax_2D)%codeparam%codename
  !
  max_nb_lines = size(equil_in_itm(1)%codeparam%codeversion)
  itmax_2D = 1
  do it=2,nb_times
    if (nb_2D .lt. size(equil_in_itm(it)%codeparam%codeversion)) then
      max_nb_lines = size(equil_in_itm(it)%codeparam%codeversion)
      itmax_2D = it
    end if
  end do
  allocate(equil_out_ids%Code_Parameters%Code_Version(max_nb_lines))
  equil_out_ids%Code_Parameters%Code_Version = equil_in_itm(itmax_2D)%codeparam%codeversion
  !
  max_nb_lines = size(equil_in_itm(1)%codeparam%parameters)
  itmax_2D = 1
  do it=2,nb_times
    if (nb_2D .lt. size(equil_in_itm(it)%codeparam%parameters)) then
      max_nb_lines = size(equil_in_itm(it)%codeparam%parameters)
      itmax_2D = it
    end if
  end do
  if (max_nb_lines .gt. 0) then
    allocate(equil_out_ids%Code_Parameters%Parameters(max_nb_lines))
    equil_out_ids%Code_Parameters%Parameters = equil_in_itm(itmax_2D)%codeparam%parameters
  end if
  !
  max_nb_lines = size(equil_in_itm(1)%codeparam%output_diag)
  itmax_2D = 1
  do it=2,nb_times
    if (nb_2D .lt. size(equil_in_itm(it)%codeparam%output_diag)) then
      max_nb_lines = size(equil_in_itm(it)%codeparam%output_diag)
      itmax_2D = it
    end if
  end do
  if (max_nb_lines .gt. 0) then
    allocate(equil_out_ids%Code_Parameters%Output_Diagnostics(max_nb_lines))
    equil_out_ids%Code_Parameters%Output_Diagnostics = equil_in_itm(itmax_2D)%codeparam%output_diag
  end if
  allocate(equil_out_ids%Code_Parameters%Output_Flag(nb_times))  
  !
  ! 2.2 Fill in time-dependent data
  !
  ! find indices for profiles_1d extra_data first
  ishear = 0
  iftrap = 0
  if (iextra_option .EQ. 1) then
    do i=1,size(extra_data_itm_to_ids(1)%prof1d_list)
      if (trim(extra_data_itm_to_ids(1)%prof1d_list(i)%name) .EQ. "magnetic_shear") then
        ishear = i
      else if (trim(extra_data_itm_to_ids(1)%prof1d_list(i)%name) .EQ. "trapped_fraction") then
        iftrap = i
      end if
    end do
    if (ishear .EQ. 0) then
      print *,'copy_itm_to_ids_equilibrium: problem with magnetic_shear, put -1e40'
    end if
    if (iftrap .EQ. 0) then
      print *,'copy_itm_to_ids_equilibrium: problem with trapped_fraction, put -1e40'
    end if
  end if
  do it=1,nb_times
    !
    equil_out_ids%Timebase(it) = equil_in_itm(it)%time
    !
    !  Boundary: IDS is not an array, so use itm %boundary(1)
    equil_out_ids%Boundary%Type(it) = equil_in_itm(it)%eqgeometry%boundarytype
    nb_points = size(equil_in_itm(it)%eqgeometry%boundary(1)%r)
    equil_out_ids%Boundary%N_Outline_Points(it) = nb_points
    equil_out_ids%Boundary%Outline_RZ(1,1:nb_points,it) = equil_in_itm(it)%eqgeometry%boundary(1)%r(1:nb_points)
    equil_out_ids%Boundary%Outline_RZ(2,1:nb_points,it) = equil_in_itm(it)%eqgeometry%boundary(1)%z(1:nb_points)
    equil_out_ids%Boundary%Geometric_Axis_RZ(1,it) = equil_in_itm(it)%eqgeometry%geom_axis%r
    equil_out_ids%Boundary%Geometric_Axis_RZ(2,it) = equil_in_itm(it)%eqgeometry%geom_axis%z
    equil_out_ids%Boundary%a_minor(it) = equil_in_itm(it)%eqgeometry%a_minor
    equil_out_ids%Boundary%Elongation(it) = equil_in_itm(it)%eqgeometry%elongation
    equil_out_ids%Boundary%Elongation_Upper(it) = equil_in_itm(it)%eqgeometry%elong_upper
    equil_out_ids%Boundary%Elongation_Lower(it) = equil_in_itm(it)%eqgeometry%elong_lower
    equil_out_ids%Boundary%Triangularity_Upper(it) = equil_in_itm(it)%eqgeometry%tria_upper
    equil_out_ids%Boundary%Triangularity_Lower(it) = equil_in_itm(it)%eqgeometry%tria_lower
    ! No X-point information from within CHEASE
    !
    ! Globals
    equil_out_ids%Global%Beta_Pol(it) = equil_in_itm(it)%global_param%beta_pol
    equil_out_ids%Global%Beta_Tor(it) = equil_in_itm(it)%global_param%beta_tor
    equil_out_ids%Global%Beta_Normal(it) = equil_in_itm(it)%global_param%beta_normal
    equil_out_ids%Global%Ip(it) = equil_in_itm(it)%global_param%i_plasma
    equil_out_ids%Global%li_3(it) = equil_in_itm(it)%global_param%li
    equil_out_ids%Global%Volume(it) = equil_in_itm(it)%global_param%volume
    equil_out_ids%Global%Area(it) = equil_in_itm(it)%global_param%area
    equil_out_ids%Global%Psi_Axis(it) = equil_in_itm(it)%global_param%psi_ax
    equil_out_ids%Global%Psi_Boundary(it) = equil_in_itm(it)%global_param%psi_bound
    equil_out_ids%Global%Magnetic_Axis%R(it) = equil_in_itm(it)%global_param%mag_axis%position%r
    equil_out_ids%Global%Magnetic_Axis%Z(it) = equil_in_itm(it)%global_param%mag_axis%position%z
    equil_out_ids%Global%B_Tor_Axis(it) = equil_in_itm(it)%global_param%mag_axis%bphi
    equil_out_ids%Global%q_Axis(it) = equil_in_itm(it)%global_param%mag_axis%q
    equil_out_ids%Global%q_95(it) = equil_in_itm(it)%global_param%q_95
    equil_out_ids%Global%q_Min(it) = equil_in_itm(it)%global_param%q_min
    ! equil_out_ids%Global%Vacuum_Toroidal_Field%R0(it) = equil_in_itm(it)%global_param%toroid_field%R0
    equil_out_ids%Global%Vacuum_Toroidal_Field%R0 = equil_in_itm(it)%global_param%toroid_field%R0
    equil_out_ids%Global%Vacuum_Toroidal_Field%B0(it) = equil_in_itm(it)%global_param%toroid_field%B0
    equil_out_ids%Global%W_MHD(it) = equil_in_itm(it)%global_param%w_mhd
    !
    ! equil_out_ids%Profiles_1D:
    npsi_1d = size(equil_in_itm(it)%profiles_1d%psi)
    equil_out_ids%Profiles_1D%Psi(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%psi(1:npsi_1d)
    equil_out_ids%Profiles_1D%Phi(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%phi(1:npsi_1d)
    equil_out_ids%Profiles_1D%Pressure(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%pressure(1:npsi_1d)
    equil_out_ids%Profiles_1D%F(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%F_dia(1:npsi_1d)
    equil_out_ids%Profiles_1D%dPressure_dPsi(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%pprime(1:npsi_1d)
    equil_out_ids%Profiles_1D%F_dF_dPsi(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%ffprime(1:npsi_1d)
    equil_out_ids%Profiles_1D%J_Tor(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%jphi(1:npsi_1d)
    equil_out_ids%Profiles_1D%J_Parallel(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%jparallel(1:npsi_1d)
    equil_out_ids%Profiles_1D%q(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%q(1:npsi_1d)
    if (iextra_option .EQ. 1 .and. ishear .ne. 0) then
      equil_out_ids%Profiles_1D%Magnetic_Shear(1:npsi_1d,it) = extra_data_itm_to_ids(1)%prof1d_list(ishear)%values(1:npsi_1d)
    else
      equil_out_ids%Profiles_1D%Magnetic_Shear(1:npsi_1d,it) = -1.E+40_DP
    end if
    equil_out_ids%Profiles_1D%R_Inboard(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%r_inboard(1:npsi_1d)
    equil_out_ids%Profiles_1D%R_Outboard(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%r_outboard(1:npsi_1d)
    equil_out_ids%Profiles_1D%Rho_Tor(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%rho_tor(1:npsi_1d)
    equil_out_ids%Profiles_1D%dPsi_dRho_Tor(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%dpsidrho_tor(1:npsi_1d)
    equil_out_ids%Profiles_1D%Rho_Vol_Norm(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%rho_vol(1:npsi_1d)
    equil_out_ids%Profiles_1D%Elongation(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%elongation(1:npsi_1d)
    equil_out_ids%Profiles_1D%Triangularity_Upper(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%tria_upper(1:npsi_1d)
    equil_out_ids%Profiles_1D%Triangularity_Lower(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%tria_lower(1:npsi_1d)
    equil_out_ids%Profiles_1D%Volume(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%volume(1:npsi_1d)
    equil_out_ids%Profiles_1D%dVolume_dPsi(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%vprime(1:npsi_1d)
    equil_out_ids%Profiles_1D%dVolume_dRho_Tor(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%dvdrho(1:npsi_1d)
    equil_out_ids%Profiles_1D%Area(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%area(1:npsi_1d)
    equil_out_ids%Profiles_1D%dArea_dPsi(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%aprime(1:npsi_1d)
    equil_out_ids%Profiles_1D%Surface(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%surface(1:npsi_1d)
    equil_out_ids%Profiles_1D%Trapped_Fraction(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%ftrap(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm1(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm1(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm2(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm2(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm3(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm3(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm4(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm4(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm5(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm5(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm6(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm6(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm7(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm7(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm8(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm8(1:npsi_1d)
    equil_out_ids%Profiles_1D%gm9(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%gm9(1:npsi_1d)
    equil_out_ids%Profiles_1D%B_Average(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%b_av(1:npsi_1d)
    equil_out_ids%Profiles_1D%B_Min(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%b_min(1:npsi_1d)
    equil_out_ids%Profiles_1D%B_Max(1:npsi_1d,it) = equil_in_itm(it)%profiles_1d%b_max(1:npsi_1d)
    !
    ! equil_out_ids%Profiles_2D:
    if (NPROF2D .gt. 0) then
      do i=1,size(equil_in_itm(it)%profiles_2d)
        ndim1 = size(equil_in_itm(it)%profiles_2d(i)%grid%dim1)
        ndim2 = size(equil_in_itm(it)%profiles_2d(i)%grid%dim2)
        equil_out_ids%Profiles_2D(i)%Grid%Dim1(1:ndim1,it) = equil_in_itm(it)%profiles_2d(i)%grid%dim1(1:ndim1)
        equil_out_ids%Profiles_2D(i)%Grid%Dim2(1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%grid%dim2(1:ndim2)
        equil_out_ids%Profiles_2D(i)%R(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%r(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%Z(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%z(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%Psi(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%psi(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%Theta(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%theta(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%Phi(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%phi(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%J_Tor(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%jphi(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%J_Parallel(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%jpar(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%B_R(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%br(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%B_Z(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%bz(1:ndim1,1:ndim2)
        equil_out_ids%Profiles_2D(i)%B_Tor(1:ndim1,1:ndim2,it) = equil_in_itm(it)%profiles_2d(i)%bphi(1:ndim1,1:ndim2)
      end do
    end if
    !
    ! equil_out_ids%Coordinate_System:
    ndim1 = size(equil_in_itm(it)%coord_sys%grid%dim1)
    ndim2 = size(equil_in_itm(it)%coord_sys%grid%dim2)
    equil_out_ids%Coordinate_System%Grid%Dim1(1:ndim1,it) = equil_in_itm(it)%coord_sys%grid%dim1(1:ndim1)
    equil_out_ids%Coordinate_System%Grid%Dim2(1:ndim2,it) = equil_in_itm(it)%coord_sys%grid%dim2(1:ndim2)
    equil_out_ids%Coordinate_System%R(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%position%r(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%Z(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%position%z(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%Jacobian(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%jacobian(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%g_11(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%g_11(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%g_12(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%g_12(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%g_13(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%g_13(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%g_22(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%g_22(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%g_23(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%g_23(1:ndim1,1:ndim2)
    equil_out_ids%Coordinate_System%g_33(1:ndim1,1:ndim2,it) = equil_in_itm(it)%coord_sys%g_33(1:ndim1,1:ndim2)
    !
    equil_out_ids%Code_Parameters%Output_Flag(it) = equil_in_itm(it)%codeparam%output_flag
    !
  end do
  !
  flag_status = 0
  !
  return
END subroutine copy_itm_to_ids_equilibrium
