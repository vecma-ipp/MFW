! -*- coding: UTF-8 -*-
program gem0_test

use gem0_standalone, only: gem0_cpo

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

implicit none

  ! CPO files 
  character(len=*), parameter :: equil_file_in  = "ets_equilibrium_in.cpo"
  character(len=*), parameter :: corep_file_in  = "ets_coreprof_in.cpo"

  character(len=*), parameter :: coret_file_out = "gem0_coretransp_out.cpo"
  
  ! CPO structures 
  type(type_equilibrium), pointer :: equil(:)
  type(type_coreprof), pointer :: corep(:)
  type(type_coretransp), pointer :: coret(:)

  integer :: ios

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

  call gem0_cpo(equil, corep, coret)

  ! Transfer CPO to buf
  call open_write_file(12, coret_file_out)
  call write_cpo(coret(1), 'coretransp')
  call close_write_file

  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
  call deallocate_cpo(coret)

end program gem0_test
