! -*- coding: UTF-8 -*- 
program chease_test

use euitm_schemas,   only:type_equilibrium

use read_structures, only: open_read_file, &
                        &  close_read_file, &
                        &  read_cpo
use write_structures, only: open_write_file, &
                         &  close_write_file, &
                         &  write_cpo

use deallocate_structures, only: deallocate_cpo

use chease_standalone, only: chease_cpo

implicit none
  
  ! CPO files 
  character(len=*), parameter :: equil_in_file   = "chease_equilibrium_in.cpo" 
  character(len=*), parameter :: equil_out_file  = "chease_equilibrium_out.cpo"
  
  ! CPO structures 
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_equilibrium), pointer :: equil_new(:) => NULL()
  
  ! Other variables  
  integer :: ios

  ! Allocate CPO strutures  
  allocate(equil(1))
  allocate(equil_new(1))

  ! Read CPO file and write corresponding structure   
  open (unit = 10, file = equil_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, equil_in_file )
     call read_cpo(equil(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found:",equil_in_file
     STOP
  end if
  
  ! CHEASE 
  call chease_cpo(equil, equil_new) 
  
  ! Save the output file 
  call open_write_file(20, equil_out_file)
  call write_cpo(equil_new(1),'equilibrium')
  call close_write_file
  
  ! deallocations
  call deallocate_cpo(equil)
  call deallocate_cpo(equil_new)
 
end program chease_test
