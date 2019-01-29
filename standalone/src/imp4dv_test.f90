program imp4dv_test
  use imp4dv_standalone
  use read_structures
  use write_structures
  use deallocate_structures
  implicit none

  character(255) :: equil_file_in, corep_file_in, coret_file_in

  type(type_equilibrium), pointer :: equil(:)
  type(type_coreprof), pointer :: corep(:)
  type(type_coretransp), pointer :: coret(:), coret_dv(:)

  integer :: ios

  allocate(equil(1))
  allocate(corep(1))
  allocate(coret(1))
  
  equil_file_in = 'chease_equilibrium.cpo'
  corep_file_in = 'ets_coreprof.cpo'
  coret_file_in = 'gem_coretransp.cpo'

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
  open (unit = 12, file = coret_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, coret_file_in )
     call read_cpo(coret(1), 'coretransp' )
     call close_read_file
  else
     print *,"CPO file not found ", coret_file_in
     STOP
  end if

  call imp4dv_cpo(equil, corep, coret, coret_dv)

  ! transfer CPO to buf
  call open_write_file(12,'imp4dv_coretransp_out.cpo')
  call write_cpo(coret_dv(1),'coretransp')
  call close_write_file

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(coret)
  call deallocate_cpo(coret_dv)

end program imp4dv_test
