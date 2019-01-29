PROGRAM main
  USE module_orb5, ONLY : orb5_cpo
  USE euitm_schemas
  USE read_structures
  USE write_structures
  USE deallocate_structures
#ifdef MPI2
  use mpi
#else
#ifdef MPI
  include "mpif.h"
#endif
#endif
  IMPLICIT NONE

  type (type_equilibrium), pointer :: equil(:) => NULL()
  type (type_coreprof), pointer :: corep(:) => NULL()
  type (type_coretransp), pointer :: coret(:) => NULL()

  integer :: ios
  integer :: ierr, npes, irank, ipe
  call MPI_Init(ierr)
  call MPI_Comm_size(MPI_COMM_WORLD, npes, ierr)
  call MPI_Comm_rank(MPI_COMM_WORLD, irank, ierr)

  allocate(corep(1))
  allocate(equil(1))    

  if (irank.eq.0) print *," *** READING INPUT CPOS *** "
  open (unit = 10, file = 'orb5_coreprof_in.cpo', &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, 'orb5_coreprof_in.cpo' )
     call read_cpo(corep(1), 'coreprof' )
     call close_read_file
  else
     print *,"CPO file not found for coreprof"
     STOP
  end if
  open (unit = 11, file = 'orb5_equilibrium_in.cpo', &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (11)
     call open_read_file(11, 'orb5_equilibrium_in.cpo' )
     call read_cpo(equil(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found for equilibrium"
     STOP
  end if

  if (irank.eq.0) print *," *** CALLING ORB5_CPO SUBROUTINE *** "
  CALL orb5_cpo(equil, corep, coret)


  if (irank.eq.0) then
     print *," *** WRITING OUTPUT CPO *** "
     call open_write_file(12, 'orb5_coretransp_out.cpo' )
     call write_cpo(coret(1), 'coretransp' )
     call close_write_file
  end if

  call deallocate_cpo(corep)
  call deallocate_cpo(equil)
  call deallocate_cpo(coret)

  call MPI_Finalize(ierr)

END PROGRAM main

