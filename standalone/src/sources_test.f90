! -*- coding: UTF-8 -*-
program sources_test

  use sources_standalone, only: gaussian_source_cpo

  use read_structures, only: open_read_file,  &
                          &  close_read_file, &
                          &  read_cpo
  use write_structures, only: open_write_file,  &
                           &  close_write_file, &
                          &  write_cpo
  use deallocate_structures, only: deallocate_cpo

implicit none

  ! CPO files and structures
  character(len=*), parameter :: corep_file_in   = "ets_coreprof_in.cpo"
  character(len=*), parameter :: equil_file_in   = "ets_equilibrium_in.cpo"
  character(len=*), parameter :: cores_file_out   = "ets_coresource_out.cpo"

  type(type_coreprof)   , pointer :: corep(:) => NULL()
  type(type_equilibrium), pointer :: equil(:) => NULL()
  type(type_coresource) , pointer :: cores(:) => NULL()

  integer :: ios

  allocate(corep(1))
  allocate(equil(1))
  
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
  call open_write_file(12, cores_file_out)
  call write_cpo(cores(1),'coresource')
  call close_write_file

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(cores)

end program sources_test
