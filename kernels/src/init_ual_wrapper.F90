module init_ual_wrapper
  implicit none

  real(8), save :: n_addcoef, n_multcoef
  integer, save :: solve_ni, solve_ne, solve_psi

contains


#ifdef HAS_UAL
  subroutine init_ual2buf( &
       user, machine, version,&
       shot, run, time, &
       corep_out, &
       coret_out, &
       cores_out, &
       corei_out, &
       coref_out, &
       equil_out, &
       toroidf_out) 
    use iso_c_binding
    use string_binding
!    use itm_constants
    use read_structures
    use write_structures
    use deallocate_structures
    use c_tools
    use euitm_routines
    implicit none

    character(*) :: user, machine, version
    integer :: shot, run, idx
    real(EUITM_R8) :: time

    integer(kind=c_signed_char), pointer :: corep_out(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: cores_out(:)
    integer(kind=c_signed_char), pointer :: corei_out(:)
    integer(kind=c_signed_char), pointer :: coref_out(:)
    integer(kind=c_signed_char), pointer :: equil_out(:)
    integer(kind=c_signed_char), pointer :: toroidf_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)
    integer :: corep_size, coret_size, cores_size
    integer :: corei_size, equil_size, toroidf_size, coref_size

    character(F_STR_SIZE) :: cpo_file
    character(F_STR_SIZE) :: corep_file, coret_file, cores_file 
    character(F_STR_SIZE) :: corei_file, equil_file, toroidf_file
    character(F_STR_SIZE) :: coref_file

    integer :: ios

    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_coretransp), pointer :: coret(:) => NULL()
    type (type_coresource), pointer :: cores(:) => NULL()
    type (type_coreimpur), pointer :: corei(:) => NULL()
    type (type_corefast), pointer :: coref(:) => NULL()
    type (type_equilibrium), pointer :: equil(:) => NULL()
    type (type_toroidfield), pointer :: toroidf(:) => NULL()


    print *,'in init_ual_wrapper'

    !... allocate CPOs
    allocate(corep(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(coref(1))
    allocate(equil(1))
    allocate(toroidf(1))

    call euitm_open_env('euitm',shot,run,idx,user,machine,version)
    call euitm_get_slice(idx,'coreprof',corep(1),time,2)
    call euitm_get_slice(idx,'coretransp',coret(1),time,2)
    call euitm_get_slice(idx,'coresource',cores(1),time,2)
    call euitm_get_slice(idx,'coreimpur',corei(1),time,2)
    call euitm_get_slice(idx,'corefast',coref(1),time,2)
    call euitm_get_slice(idx,'equilibrium',equil(1),time,2)
    call euitm_get_slice(idx,'toroidfield',toroidf(1),time,2)


    ! modify n profile
    corep(1)%ne%value = corep(1)%ne%value * n_multcoef + n_addcoef
    corep(1)%ni%value = corep(1)%ni%value * n_multcoef + n_addcoef


!!! switch on/off these two lines with parameters in cxa
    corep(1)%ne%boundary%type = solve_ne
    if (.not.associated(corep(1)%ni%boundary%type)) then
       allocate(corep(1)%ni%boundary%type(1))
    end if
    corep(1)%ni%boundary%type(1) = solve_ni
    corep(1)%psi%boundary%type = solve_psi


    !... init names for each CPO ascii file
    corep_file = "init_coreprof.cpo"
    coret_file = "init_coretransp.cpo"
    cores_file = "init_coresource.cpo"
    corei_file = "init_coreimpur.cpo"
    coref_file = "init_corefast.cpo"
    equil_file = "init_equilibrium.cpo"
    toroidf_file = "init_toroidfield.cpo"


    !...  write each CPO in its own file
    call open_write_file(11,corep_file)
    call write_cpo(corep(1),'coreprof')
    call close_write_file

    call open_write_file(12,coret_file)
    call write_cpo(coret(1),'coretransp')
    call close_write_file

    call open_write_file(13,cores_file)
    call write_cpo(cores(1),'coresource')
    call close_write_file

    call open_write_file(14,corei_file)
    call write_cpo(corei(1),'coreimpur')
    call close_write_file

    call open_write_file(16,equil_file)
    call write_cpo(equil(1),'equilibrium')
    call close_write_file

    call open_write_file(17,toroidf_file)
    call write_cpo(toroidf(1),'toroidfield')
    call close_write_file

    call open_write_file(18,coref_file)
    call write_cpo(coref(1),'corefast')
    call close_write_file

    print *,'transfer CPO to buf'
    call file2byte(corep_file, corep_out, corep_size)
    call file2byte(coret_file, coret_out, coret_size)
    call file2byte(cores_file, cores_out, cores_size)
    call file2byte(corei_file, corei_out, corei_size)
    call file2byte(coref_file, coref_out, coref_size)
    call file2byte(equil_file, equil_out, equil_size)
    call file2byte(toroidf_file, toroidf_out, toroidf_size)

    print *,'deallocate cpos'
    call deallocate_cpo(corep)
    call deallocate_cpo(coret)
    call deallocate_cpo(cores)
    call deallocate_cpo(corei)
    call deallocate_cpo(coref)
    call deallocate_cpo(equil)
    call deallocate_cpo(toroidf)

    print *,'close ual db'
    call euitm_close(idx)
    call euitm_disconnect()

  end subroutine init_ual2buf
#endif



end module init_ual_wrapper
