program equilupdate_test
  use equilupdate_standalone
  use read_structures
  use write_structures
  use deallocate_structures
  implicit none

  character(255) :: equil_file_in, corep_file_in, toroidf_file_in

  type(type_equilibrium), pointer :: equil(:), equil_out(:)
  type(type_coreprof), pointer :: corep(:)
  type(type_toroidfield), pointer :: toroidf(:)

  integer :: ios

  allocate(equil(1))
  allocate(corep(1))
  allocate(toroidf(1))
  
  equil_file_in = 'ets_equilibrium_in.cpo'
  corep_file_in = 'ets_coreprof_in.cpo'
  toroidf_file_in = 'ets_toroidfield_in.cpo'

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
  open (unit = 12, file = toroidf_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, toroidf_file_in )
     call read_cpo(toroidf(1), 'toroidfield' )
     call close_read_file
  else
     print *,"CPO file not found ", toroidf_file_in
     STOP
  end if

  call equilupdate2cpo(corep, toroidf, equil, equil_out)

  ! transfer CPO to buf
  call open_write_file(12,'equilupdate_equilibrium_out.cpo')
  call write_cpo(equil_out(1),'equilibrium')
  call close_write_file

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(toroidf)
  call deallocate_cpo(equil_out)

end program equilupdate_test
