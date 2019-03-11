! -*- coding: UTF-8 -*- 
!> @brief 
! Wrappers to call the Fortran functions from src/*_standalone.f90
!> @author
! Jalal Lakhlili (jlakhlili@gmail.com)
! -----------------------------------------------------------------

! -----------------------------------------------------------------
!> @brief     run the ets code 
!>
!> @param[in] data_path: the path to folder containnig cpo files, eg.: ../data/ETS/uq_8
!> @param[in] npar: the number of uncertain parameters      
!> @param[in] values: array (size is npar) of evaluation of uncertain values          
subroutine run_ets(data_path, npar, values)
    use allocate_deallocate
    
    use euitm_schemas, only: type_coreprof,    & 
                          &  type_equilibrium, &
                          &  type_coretransp,  &
                          &  type_coresource,  &
                          &  type_coreimpur,   &
                          &  type_toroidfield
    
    use read_structures, only: open_read_file,  &
                            &  close_read_file, &
                            &  read_cpo
    
    use write_structures, only: open_write_file,  &
                             &  close_write_file, &
                             &  write_cpo
    
    use deallocate_structures, only: deallocate_cpo
    
    use ets_standalone, only: ets_cpo
    
    implicit none
    
    ! Eg.: ../data/ETS/uq_8
    character(len=*), intent(in) :: data_path
    integer, intent(in) :: npar
    real(kind=8), dimension (npar), intent(in) :: values 

    
    ! Local args
    character(len=64) :: corep_in_file 
    character(len=64) :: cores_in_file 
    character(len=64) :: corei_in_file 
    character(len=64) :: equil_in_file 
    character(len=64) :: coret_in_file   
    character(len=64) :: toroidf_in_file 
    character(len=64) :: corep_out_file  
    character(len=64) :: equil_out_file  
    
    integer      :: ios
    
    ! CPO structures 
    type (type_coreprof)   , pointer :: corep(:)   => NULL()
    type (type_equilibrium), pointer :: equil(:)   => NULL()
    type (type_coretransp) , pointer :: coret(:)   => NULL()
    type (type_coresource) , pointer :: cores(:)   => NULL()
    type (type_coreimpur)  , pointer :: corei(:)   => NULL()
    type (type_toroidfield), pointer :: toroidf(:) => NULL()
    
    type (type_coreprof)   , pointer :: corep_new(:) => NULL()
    type (type_equilibrium), pointer :: equil_new(:) => NULL()
    
    
    ! CPO files
    corep_in_file   = data_path // "/ets_coreprof_in.cpo"
    equil_in_file   = data_path // "/ets_equilibrium_in.cpo"
    cores_in_file   = data_path // "/ets_coresource_in.cpo"
    corei_in_file   = data_path // "/ets_coreimpur_in.cpo"
    toroidf_in_file = data_path // "/ets_toroidfield_in.cpo"
    coret_in_file   = data_path //  "/ets_coretransp_in.cpo"
    corep_out_file  = data_path // "/ets_coreprof_out.cpo"
    equil_out_file  = data_path // "/ets_equilibrium_out.cpo"
    
    ! Allocate CPO strutures  
    allocate(corep(1))
    allocate(equil(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(toroidf(1))
    
    allocate(corep_new(1))
    allocate(equil_new(1))
    
    ! Read CPO file and write corresponding structures   
    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file)
       call read_cpo(corep(1), 'coreprof' )
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
       call read_cpo(equil(1), 'equilibrium' )
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
       call open_read_file(12, coret_in_file)
       call read_cpo(coret(1), 'coretransp')
       ! Update the transport coefficients
       coret(1)%values(1)%te_transp%diff_eff(1:npar)=values
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
    
    ! Call ets_standalone
    call ets_cpo(corep, equil, coret, cores, corei, toroidf, corep_new, equil_new)
    
    ! ... write output files
    call open_write_file(16, corep_out_file)
    call write_cpo(corep_new(1),'coreprof')
    call close_write_file
    
!    call open_write_file(17, equil_out_file)
!    call write_cpo(equil_new(1),'equilibrium')
!    call close_write_file
    
    ! ... deallocations
    call deallocate_cpo(equil_new)
    call deallocate_cpo(corep_new)  
    call deallocate_cpo(toroidf)
    call deallocate_cpo(corei)
    call deallocate_cpo(cores)
    call deallocate_cpo(coret)  
    call deallocate_cpo(equil)
    call deallocate_cpo(corep)
    
end subroutine run_ets
! -----------------------------------------------------------------------

