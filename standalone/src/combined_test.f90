program combined_test
  use euitm_schemas
  use read_structures
  use write_structures
  use deallocate_structures
  use imp4dv_standalone, only: imp4dv_cpo
  use ets_standalone, only: ets_cpo
  implicit none


  character(255) :: equilibrium_file_in, coreprof_file_in, coretransp_file_in, &
       coresource_file_in, coreimpur_file_in, toroidfield_file_in

  type(type_equilibrium), pointer :: equilibrium(:), equilibrium_out(:)
  type(type_coreprof), pointer :: coreprof(:), coreprof_out(:)
  type(type_coretransp), pointer :: coretransp(:), coretransp_dv(:)
  type(type_coresource), pointer :: coresource(:)
  type(type_coreimpur), pointer :: coreimpur(:)
  type(type_toroidfield), pointer :: toroidfield(:)

  integer :: ios

  print *,"##### READING INPUT FILES #####"
  allocate(equilibrium(1))
  allocate(coreprof(1))
  allocate(coretransp(1))
  allocate(coresource(1))
  allocate(coreimpur(1))
  allocate(toroidfield(1))
  
  equilibrium_file_in = 'chease_equilibrium.cpo'
  coreprof_file_in    = 'ets_coreprof.cpo'
  coretransp_file_in  = 'gem_coretransp.cpo'
  coreimpur_file_in   = 'ets_coreimpur_in.cpo'
  coresource_file_in  = 'ets_coresource_in.cpo'
  toroidfield_file_in = 'ets_toroidfield_in.cpo'

  open (unit = 10, file = coreprof_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, coreprof_file_in )
     call read_cpo(coreprof(1), 'coreprof' )
     call close_read_file
  else
     print *,"CPO file not found ", coreprof_file_in
     STOP
  end if
  open (unit = 11, file = equilibrium_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (11)
     call open_read_file(11, equilibrium_file_in )
     call read_cpo(equilibrium(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found ", equilibrium_file_in
     STOP
  end if
  open (unit = 12, file = coretransp_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, coretransp_file_in )
     call read_cpo(coretransp(1), 'coretransp' )
     call close_read_file
  else
     print *,"CPO file not found ", coretransp_file_in
     STOP
  end if
  open (unit = 13, file = coresource_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (13)
     call open_read_file(13, coresource_file_in )
     call read_cpo(coresource(1), 'coresource' )
     call close_read_file
  else
     print *,"CPO file not found ", coresource_file_in
     STOP
  end if
  open (unit = 14, file = coreimpur_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (14)
     call open_read_file(14, coreimpur_file_in )
     call read_cpo(coreimpur(1), 'coreimpur' )
     call close_read_file
  else
     print *,"CPO file not found ", coreimpur_file_in
     STOP
  end if
  open (unit = 15, file = toroidfield_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (15)
     call open_read_file(15, toroidfield_file_in )
     call read_cpo(toroidfield(1), 'toroidfield' )
     call close_read_file
  else
     print *,"CPO file not found ", toroidfield_file_in
     STOP
  end if


  print *,"##### CALLING IMP4DV #####"
  call imp4dv_cpo(equilibrium, coreprof, coretransp, coretransp_dv)

  ! write output CPO to file: un-comment lines below if needed
!!$  call open_write_file(16,'imp4dv_coretransp_out.cpo')
!!$  call write_cpo(coretransp_dv(1),'coretransp')
!!$  call close_write_file


  print *,"##### CALLING ETS #####"
  call ets_cpo(coreprof, &
       equilibrium, &
       coretransp_dv, &
       coresource, &
       coreimpur, &
       toroidfield, &
       coreprof_out, &
       equilibrium_out)


  ! write output CPO to file: un-comment lines below if needed
!!$  call open_write_file(17,'ets_coreprof_out.cpo')
!!$  call write_cpo(coreprof_out(1),'coreprof')
!!$  call close_write_file
!!$  call open_write_file(18,'ets_equilibrium_out.cpo')
!!$  call write_cpo(equilibrium_out(1),'equilibrium')
!!$  call close_write_file


  call deallocate_cpo(equilibrium)
  call deallocate_cpo(coreprof)
  call deallocate_cpo(coretransp)
  call deallocate_cpo(coresource)
  call deallocate_cpo(coreimpur)
  call deallocate_cpo(toroidfield)
  call deallocate_cpo(coretransp_dv)
  call deallocate_cpo(equilibrium_out)
  call deallocate_cpo(coreprof_out)

end program combined_test
