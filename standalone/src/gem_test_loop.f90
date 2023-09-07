program gem_test

  use gem_standalone, only: gem_cpo

  use euitm_schemas,   only: type_coreprof,    & 
       &  type_equilibrium, &
       &  type_coretransp
  use read_structures, only: open_read_file,  &
       &  close_read_file, &
       &  read_cpo
  use write_structures, only: open_write_file,  &
       &  close_write_file, &
       &  write_cpo
  use deallocate_structures, only: deallocate_cpo

  use mpi
  
  implicit none

  ! CPO files 
  character(len=*), parameter :: equil_file_in  = "gem_equilibrium_in.cpo"
  character(len=*), parameter :: corep_file_in  = "gem_coreprof_in.cpo"
  character(len=*), parameter :: coret_file_out = "gem_coretransp_out.cpo"

  ! CPO structures 
  type(type_equilibrium), pointer :: equil(:)
  type(type_coreprof), pointer :: corep(:)
  type(type_coretransp), pointer :: coret(:)

  integer :: ierr, npes, irank
  integer :: ios
  integer :: it

  character(4) :: itstr

  integer, parameter :: STEPS = 10
  logical, parameter :: TIMETRACE = .TRUE.

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  allocate(equil(1))
  allocate(corep(1))

  ! Read CPO file and write corresponding structures   
  open (unit = 10, file = equil_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, equil_file_in )
     call read_cpo(equil(1), 'equilibrium' )
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
     call read_cpo(corep(1), 'coreprof' )
     call close_read_file
  else
     print *,"CPO file not found ", corep_file_in
     STOP
  end if

  do it=1, STEPS

    if (irank.eq.0) then
      write(itstr,'(I4.4)') it
      !print *,"**** iteration = "//itstr//" ****"
      print *, '*** iteration = ', it, ' ***'
     end if

    ! YY: Are next two lines necessary?     
    !call deallocate_cpo(coret_gem)
    !nullify(coret_gem)

    call gem_cpo(equil, corep, coret)

    if ((irank.eq.0).AND.(TIMETRACE)) then
      call open_write_file(23, 'gem_coretransp_'//itstr//'.cpo')
      call write_cpo(coret(1),'coretransp')
      call close_write_file
   end if

  end do 

  if ((irank.eq.0).AND.(.not.TIMETRACE)) then
     ! Transfer CPO to file
     call open_write_file(12, coret_file_out)
     call write_cpo(coret(1), 'coretransp')
     call close_write_file
  end if

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(coret)

  call MPI_Finalize(ierr)

end program gem_test
