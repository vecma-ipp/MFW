program ets_test_wrapper
  use euitm_schemas
  use iso_c_binding
  use string_binding
  use itm_constants
  use read_structures
  use write_structures
  use copy_structures
  use xml_file_reader
  use ets
  use equilibrium_input
  use deallocate_structures
!  use c_tools
  implicit none

!!$  interface
!!$     subroutine ITM_ETS(corep_old, corep_iter, corep_new, &
!!$          equil_old, equil_iter, coret,                   &
!!$          cores, corei,                                   &
!!$          control_integer, control_double,                &
!!$          code_parameters)
!!$       use euitm_schemas
!!$       use itm_types
!!$       type (type_coreprof), pointer :: corep_old(:), corep_iter(:), corep_new(:)
!!$       type (type_equilibrium), pointer ::  equil_old(:), equil_iter(:)
!!$       type (type_coretransp), pointer :: coret(:)
!!$       type (type_coresource), pointer :: cores(:)
!!$       type (type_coreimpur), pointer :: corei(:)
!!$       integer   :: control_integer(2)
!!$       real(R8)  :: control_double(6)  
!!$       type (type_param) :: code_parameters
!!$     end subroutine ITM_ETS
!!$
!!$     subroutine equil_input(corep, toroidf, equil, equil_new)
!!$       use euitm_schemas
!!$       type (type_coreprof), pointer :: corep(:)
!!$       type (type_toroidfield), pointer :: toroidf(:)
!!$       type (type_equilibrium), pointer :: equil(:), equil_new(:)
!!$     end subroutine equil_input
!!$  end interface
!!$
  



    integer  :: control_integer(2)  !integer control parameters
    real(R8) :: control_double(6)   !real control parameters

    integer(kind=c_signed_char), pointer :: corep_out(:)
    integer(kind=c_signed_char), pointer :: equil_out(:)

    integer :: ios
    integer :: tmpsize

    type (type_coreprof), pointer :: corep_old(:), corep_iter(:), corep_new(:)
    type (type_equilibrium), pointer :: equil_old(:), equil_iter(:), equil_new(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_coresource), pointer :: cores(:)
    type (type_coreimpur), pointer :: corei(:)
    type (type_toroidfield), pointer :: toroidf(:)
    type (type_param) :: code_parameters

    !integer   :: control_integer(2)  !integer control parameters
    !real(R8)  :: control_double(6)   !real control parameters
    real(R8)  :: tau                 !time step

    character(128) :: corep_in_file, corep_out_file
    character(128) :: equil_in_file, equil_out_file
    character(128) :: coret_in_file, cores_in_file
    character(128) :: corei_in_file, toroidf_in_file

    integer, save :: cpt = 0
    character(4)  :: cptstr

    real(R8) :: time_in


    tau = 0.1_R8
    !tau = control_double(1)

    control_integer(1) = 3 !SOLVER_TYPE      !number of numerical solver
    control_integer(2) = 0 !SIGMA_SOURCE     !plasma electrical conductivity 
    control_double(1)  = tau !TAU            !time step
    control_double(2)  = 1.0 !AMIX           !mixing factor for profiles
    control_double(3)  = 1.0 !AMIX**0.5 !mixing factor for transport coefficients
    control_double(4)  = 1.e0_R8             !actual convergence
    control_double(5)  = 0.0001_R8 !CONVREC  !required convergence
    control_double(6)  = 1.0_R8              !added by DPC, 2012-05-22


    !print *,"fortran ETS wrapper"

    !print *,"compute"

    !...  read inputs
    allocate(corep_old(1))
    allocate(corep_iter(1))
    allocate(equil_old(1))
    allocate(equil_iter(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(toroidf(1))


    corep_in_file = 'ets_coreprof_in.cpo'
    equil_in_file = 'ets_equilibrium_in.cpo'
    coret_in_file = 'ets_coretransp_in.cpo'
    cores_in_file = 'ets_coresource_in.cpo'
    corei_in_file = 'ets_coreimpur_in.cpo'
    toroidf_in_file = 'ets_toroidfield_in.cpo'
        
    ! read CPO file to CPO type
    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_old(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found:",corep_in_file
       STOP
    end if

    open (unit = 11, file = equil_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (11)
       call open_read_file(11, equil_in_file )
       call read_cpo(equil_old(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found:",equil_in_file
       STOP
    end if

    open (unit = 12, file = coret_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (12)
       call open_read_file(12, coret_in_file )
       call read_cpo(coret(1), 'coretransp' )
       call close_read_file
    else
       print *,"CPO file not found:",coret_in_file
       STOP
    end if

    open (unit = 13, file = cores_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (13)
       call open_read_file(13, cores_in_file )
       call read_cpo(cores(1), 'coresource' )
       call close_read_file
    else
       print *,"CPO file not found:",cores_in_file
       STOP
    end if

    open (unit = 14, file = corei_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (14)
       call open_read_file(14, corei_in_file )
       call read_cpo(corei(1), 'coreimpur' )
       call close_read_file
    else
       print *,"CPO file not found:",corei_in_file
       STOP
    end if

    open (unit = 15, file = toroidf_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (15)
       call open_read_file(15, toroidf_in_file )
       call read_cpo(toroidf(1), 'toroidfield' )
       call close_read_file
    else
       print *,"CPO file not found:",toroidf_in_file
       STOP
    end if    


    print *,'No convergence loop: copy old -> iter'
    call copy_cpo(corep_old(1),corep_iter(1))
    call copy_cpo(equil_old(1),equil_iter(1))


    time_in = equil_old(1)%time

    print *,"run ets"
    !...  run ETS
    !call get_code_parms(code_parameters, 'bdseq.xml', '', 'bdseq.xsd')
    call fill_param(code_parameters, 'ets.xml', '', 'ets.xsd')
    call ITM_ETS(corep_old, corep_iter, corep_new, &
         equil_old, equil_iter, coret,             &
         cores, corei,                             &
         control_integer, control_double,          &
         code_parameters)


    print *,"equil_input init a new equilibrium from coreprof"
    call equil_input(corep_new,toroidf,equil_old,equil_iter)

    equil_iter(1)%time = time_in + tau
    corep_new(1)%time = time_in + tau


    !print *,"time dependent cpo files:",cpt
    write(cptstr,'(I4.4)') cpt
    cpt = cpt+1

    corep_out_file = 'ets_coreprof_'//cptstr//'.cpo'
    equil_out_file = 'ets_equilibrium_'//cptstr//'.cpo'

    !print *,"write results in file"
    !...  write the results
    call open_write_file(20,corep_out_file)
    call write_cpo(corep_new(1),'coreprof')
    call close_write_file
    call open_write_file(21,equil_out_file)
    call write_cpo(equil_iter(1),'equilibrium')
    call close_write_file


    call deallocate_cpo(corep_old)
    call deallocate_cpo(corep_iter)
    call deallocate_cpo(corep_new)
    call deallocate_cpo(equil_old)
    call deallocate_cpo(equil_iter)
    call deallocate_cpo(coret)
    call deallocate_cpo(cores)
    call deallocate_cpo(corei)
    call deallocate_cpo(toroidf)

    !print *,"return from fortran wrapper"


  end program ets_test_wrapper



