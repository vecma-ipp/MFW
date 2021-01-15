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
  use mpi !, only: MPI_Init, MPI_Comm_size, MPI_Comm_rank, MPI_Bcast, MPI_Finalize, MPI_COMM_WORLD
!!$  use c_tools
  implicit none

  ! CPO files 
  character(len=*), parameter :: equil_file_in  = "gem_equilibrium_in.cpo"
  character(len=*), parameter :: corep_file_in  = "gem_coreprof_in.cpo"
  character(len=*), parameter :: coret_file_out = "gem_coretransp_out.cpo"

  ! CPO structures 
  type(type_equilibrium), pointer :: equil(:)
  type(type_coreprof), pointer :: corep(:)
  type(type_coretransp), pointer :: coret(:)

!!$  integer(kind=c_signed_char), pointer :: equilbuf(:), corepbuf(:), totbuf(:)
!!$  integer :: equilsize, corepsize, totsize
!!$  character(128) :: username, tmpdir
  integer :: ierr, npes, irank
  integer :: ios

  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  allocate(equil(1))
  allocate(corep(1))

!!$  ! master process read data from file as byte buffer
!!$  if (irank.eq.0) then
!!$     call file2byte(corep_file_in, corepbuf, corepsize)
!!$     call file2byte(equil_file_in, equilbuf, equilsize)
!!$     totsize = corepsize + equilsize
!!$     allocate(totbuf(totsize))
!!$     totbuf(1:corepsize) = corepbuf(1:corepsize)
!!$     totbuf(corepsize+1:corepsize+equilsize) = equilbuf(1:equilsize)
!!$  end if
!!$  
!!$  ! master broadcast sizes and total buffer then all processes extract CPOs
!!$  call MPI_Bcast(corepsize, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
!!$  call MPI_Bcast(equilsize, 1, MPI_INTEGER, 0, MPI_COMM_WORLD, ierr)
!!$  if (irank.ne.0) then
!!$     allocate(tobuf(corepsize+equilsize))
!!$  end if
!!$  call MPI_Bcast(totbuf, totsize, MPI_BYTE, 0, MPI_COMM_WORLD, ierr)
!!$  if (irank.ne.0) then
!!$     corepbuf => totbuf(1:corepsize)
!!$     equilbuf => totbuf(corepsize+1:corepsize+equilsize)
!!$  end if
!!$
!!$  ! flush buffer in local memory file
!!$  call getenv("USER",username)
!!$  call getenv("TMPDIR",tmpdir)
!!$  call byte2file(TRIM(tmpdir)//'/'//TRIM(username)//'_'//corep_file_in, corepbuf, corepsize)
!!$  call byte2file(TRIM(tmpdir)//'/'//TRIM(username)//'_'//equil_file_in, equilbuf, equilsize)
  
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

  call gem_cpo(equil, corep, coret)

  if (irank.eq.0) then
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
