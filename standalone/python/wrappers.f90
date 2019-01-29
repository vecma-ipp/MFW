! -*- coding: UTF-8 -*- 
!> @brief 
! Wrappers to call the Fortran functions from src/*_standalone.f90
! -----------------------------------------------------------------------
subroutine run_ets(data_path, run_id)
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
    
    !... eg.: ../data/ETS/uq_8
    character(len=*), intent(in) :: data_path
    integer         , intent(in) :: run_id
    
    !... local args
    character(len=64) :: corep_in_file 
    character(len=64) :: cores_in_file 
    character(len=64) :: corei_in_file 
    character(len=64) :: equil_in_file 
    character(len=64) :: coret_in_file   
    character(len=64) :: toroidf_in_file 
    character(len=64) :: corep_out_file  
    character(len=64) :: equil_out_file  
    
    type (type_coreprof)   , pointer :: corep(:)   => NULL()
    type (type_equilibrium), pointer :: equil(:)   => NULL()
    type (type_coretransp) , pointer :: coret(:)   => NULL()
    type (type_coresource) , pointer :: cores(:)   => NULL()
    type (type_coreimpur)  , pointer :: corei(:)   => NULL()
    type (type_toroidfield), pointer :: toroidf(:) => NULL()
    
    type (type_coreprof)   , pointer :: corep_new(:) => NULL()
    type (type_equilibrium), pointer :: equil_new(:) => NULL()
    
    integer      :: ios
    character(4) :: str_run_id
    
    ! ... TODO: to be modified
    write (str_run_id,'(I0)')  run_id
    corep_in_file   = data_path // "/ets_coreprof_in.cpo"
    equil_in_file   = data_path // "/ets_equilibrium_in.cpo"
    cores_in_file   = data_path // "/ets_coresource_in.cpo"
    corei_in_file   = data_path // "/ets_coreimpur_in.cpo"
    toroidf_in_file = data_path // "/ets_toroidfield_in.cpo"
    coret_in_file  = data_path // "/tmp_"// Trim(str_run_id) // "/ets_coretransp_in.cpo"
    
    corep_out_file = data_path // "/tmp_"// Trim(str_run_id) // "/ets_coreprof_out.cpo"
   ! equil_out_file = data_path // "/tmp_"// Trim(str_run_id) // "/ets_equilibrium_out.cpo"
    
    print*, '>>> run_id, data_path: ',  run_id, data_path
    
    ! .. allocate CPO strutures  
    allocate(corep(1))
    allocate(equil(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(toroidf(1))
    
    allocate(corep_new(1))
    allocate(equil_new(1))
    
    ! ... read CPO file to CPO type    
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
    
    ! ... call ets_standalone
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

! -----------------------------------------------------------------------
subroutine update_coret_file(filename, npar, values)
    
    use euitm_schemas,   only: type_coretransp 
    
    use read_structures, only: open_read_file,  &
                            &  close_read_file, &
                            &  read_cpo
    
    use write_structures, only: open_write_file,  &
                             &  close_write_file, &
                             &  write_cpo
    
    use deallocate_structures, only: deallocate_cpo
    
    implicit none
    
    character(len=*), intent(in) :: filename   
    integer, intent(in) :: npar
    real(kind=8), dimension (npar), intent(in) :: values 
    
    integer :: ios
    type(type_coretransp) , pointer :: coret(:) => NULL()
    allocate(coret(1))
    
   print*, '>>> VALUES = ', values 
    ! ... 
    open (unit = 10, file = filename, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, filename)
       call read_cpo(coret(1), 'coretransp')
       call close_read_file
       coret(1)%values(1)%te_transp%diff_eff(1:npar)=values
    else
       print *,"CPO file not found:", filename
       STOP
    end if
    
    call open_write_file(10, filename)
    call write_cpo(coret(1), 'coretransp')
    call close_write_file
  
    call deallocate_cpo(coret)

end subroutine update_coret_file
! -----------------------------------------------------------------------
