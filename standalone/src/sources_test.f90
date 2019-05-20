program sources_test
  use sources_standalone
  use read_structures
  use write_structures
  use deallocate_structures
  implicit none

  character(255) :: corep_file_in, equil_file_in, cores_file_in

  type(type_coreprof), pointer :: corep(:)
  type(type_equilibrium), pointer :: equil(:)
  type(type_coresource), pointer :: cores(:)
  real(8) :: params(3)

  integer :: ios

  allocate(corep(1))
  allocate(equil(1))
  
  corep_file_in = 'ets_coreprof_in.cpo'
  equil_file_in = 'ets_equilibrium_in.cpo'

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
  params(1) = 906753.0485796847 
!  params(2) = 0.4533765242898424
!  params(3) = 0.18135060971593694

  params(2)=0.5466234757101576
  params(3)=0.21864939028406308
  
  call gaussian_source_cpo(corep, equil, params, cores)

  ! transfer CPO to buf
  call open_write_file(12,'gaussian_coresource_out.cpo')
  call write_cpo(cores(1),'coresource')
  call close_write_file

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(cores)

end program sources_test
