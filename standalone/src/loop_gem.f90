program loop_gem

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

  use copy_structures, only: copy_cpo

  use ets_standalone,         only: ets_cpo
  use equilupdate_standalone, only: equilupdate2cpo
  use chease_standalone,      only: chease_cpo
  use gem_standalone,         only: gem_cpo
  
  use mpi

  implicit none

  ! CPO files 
  character(len=*), parameter :: corep_file_in   = "ets_coreprof_in.cpo"
  character(len=*), parameter :: equil_file_in   = "ets_equilibrium_in.cpo"
  character(len=*), parameter :: cores_file_in   = "ets_coresource_in.cpo"
  character(len=*), parameter :: corei_file_in   = "ets_coreimpur_in.cpo"
  character(len=*), parameter :: coret_file_in   = "ets_coretransp_in.cpo"
  character(len=*), parameter :: toroidf_file_in = "ets_toroidfield_in.cpo"
  character(len=*), parameter :: coret_file_out  = "gem_coretransp_out.cpo"
  
  ! CPO structures 
  type(type_equilibrium), pointer :: equil_in(:)
  type(type_coreprof),    pointer :: corep_in(:)
  type(type_coretransp),  pointer :: coret_in(:)
  type(type_coresource),  pointer :: cores_in(:)
  type(type_coreimpur),   pointer :: corei_in(:)
  type(type_toroidfield), pointer :: toroidf_in(:)
  type(type_coreprof),    pointer :: corep_ets(:)
  type(type_equilibrium), pointer :: equil_update(:)
  type(type_equilibrium), pointer :: equil_chease(:)
  type(type_coretransp),  pointer :: coret_gem(:)

  integer :: ierr, npes, irank, ipe
  integer :: ios

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  allocate(equil_in(1))
  allocate(corep_in(1))
  allocate(coret_in(1))
  allocate(cores_in(1))
  allocate(corei_in(1))
  allocate(toroidf_in(1))
  
  allocate(corep_ets(1))
  allocate(equil_update(1))
  allocate(equil_chease(1))
  allocate(coret_gem(1))
  
  ! Read CPO file and write corresponding structures   
  open (unit = 10, file = equil_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
    close (10)
    call open_read_file(10, equil_file_in )
    call read_cpo(equil_in(1), 'equilibrium' )
    call close_read_file
  else
    print *,"CPO file not found ", equil_file_in
    STOP
  end if

  open (unit = 11, file = corep_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
    close (11)
    call open_read_file(11, corep_file_in )
    call read_cpo(corep_in(1), 'coreprof' )
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
    call open_read_file(12, coret_file_in)
    call read_cpo(coret_in(1), 'coretransp' )
    call close_read_file
  end if

  open (unit = 13, file = cores_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
    close (13)
    call open_read_file(13, cores_file_in)
    call read_cpo(cores_in(1), 'coresource' )
    call close_read_file
  end if

  open (unit = 14, file = corei_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
    close (14)
    call open_read_file(14, corei_file_in)
    call read_cpo(corei_in(1), 'coreimpur' )
    call close_read_file
  end if

  open (unit = 15, file = toroidf_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
    close (15)
    call open_read_file(15, toroidf_file_in)
    call read_cpo(toroidf_in(1), 'toroidfield' )
    call close_read_file
  end if

  call ets_cpo(corep_in, equil_in, coret_in, cores_in, corei_in, corep_ets)
  call equilupdate2cpo(corep_ets, toroidf_in, equil_in, equil_update)
  print*, "==== call CHEASE ====="
  call chease_cpo(equil_update, equil_chease)
  
  ! Transfer CPO to file
  print*, "==== call GEM ===="
  call gem_cpo(equil_in, corep_in, coret_gem)

  if (irank.eq.0) then
    ! Transfer CPO to file
    call open_write_file(16, coret_file_out)
    !call write_cpo(coret_gem(1), 'coretransp')
    call close_write_file
  end if

  call deallocate_cpo(equil_in)
  call deallocate_cpo(corep_in)
  call deallocate_cpo(coret_in)
  call deallocate_cpo(corei_in)
  call deallocate_cpo(toroidf_in)
  call deallocate_cpo(coret_gem)
  call deallocate_cpo(equil_chease)
  call deallocate_cpo(equil_update)
  call deallocate_cpo(corep_ets)

  call MPI_Finalize(ierr)

end program loop_gem
