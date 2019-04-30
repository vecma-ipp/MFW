program src_run
  use allocate_deallocate

  use euitm_schemas,   only: type_coreprof,    & 
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

  use sources_standalone, only: gaussian_source_cpo

  implicit none
  ! INPUTS (given by command arguments)
  character(len=128) :: cpo_dir  ! Path to the folder containing CPO files for ets
  character(len=128) :: in_fname ! NML file containing uncertain parameters values

  character(255) :: corep_file_in, equil_file_in, cores_file_in

  type(type_coreprof), pointer :: corep(:)
  type(type_equilibrium), pointer :: equil(:)
  type(type_coresource), pointer :: cores(:)

  integer :: ios

  allocate(corep(1))
  allocate(equil(1))
  
  ! CPO files
  call get_command_argument(1, cpo_dir)

  corep_file_in = trim(cpo_dir) //  '/ets_coreprof_in.cpo'
  equil_file_in = trim(cpo_dir) // '/ets_equilibrium_in.cpo'

  open (unit = 10, file = corep_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, corep_file_in )
     call read_cpo(corep(1), 'coreprof' )
     call close_read_file
  else
     print *,"CPO file not found ", corep_file_in
     STOP
  end if
  open (unit = 11, file = equil_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (11)
     call open_read_file(11, equil_file_in )
     call read_cpo(equil(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found ", equil_file_in
     STOP
  end if

  call gaussian_source_cpo(corep, equil, cores)

  ! transfer CPO to buf
  call open_write_file(12,'gaussian_coresource_out.cpo')
  call write_cpo(cores(1),'coresource')
  call close_write_file

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(cores)

end program src_run
