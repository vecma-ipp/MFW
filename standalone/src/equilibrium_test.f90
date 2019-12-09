! -*- coding: UTF-8 -*- 
program equilibrium_test

  use euitm_schemas,   only: type_coreprof,    & 
                          &  type_equilibrium, &
                          &  type_toroidfield

use read_structures, only: open_read_file,  &
                        &  close_read_file, &
                        &  read_cpo
use write_structures, only: open_write_file,  &
                         &  close_write_file, &
                         &  write_cpo

use deallocate_structures, only: deallocate_cpo

use chease_standalone, only: chease_cpo
use equilupdate_standalone, only: equilupdate2cpo

implicit none
  
  ! CPO files 
  character(len=*), parameter :: corep_in_file   = "eq_coreprof_in.cpo" 
  character(len=*), parameter :: toroidf_in_file = "eq_toroidfield_in.cpo" 
  character(len=*), parameter :: equil_in_file   = "eq_equilibrium_in.cpo" 
  character(len=*), parameter :: equil_out_file  = "eq_equilibrium_out.cpo"
  
  ! CPO structures 
  type (type_coreprof),    pointer :: corep(:)     => NULL()
  type (type_toroidfield), pointer :: toroidf(:)   => NULL()
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_equilibrium), pointer :: equil_up(:)  => NULL()
  type (type_equilibrium), pointer :: equil_new(:) => NULL()
  
  ! Other variables  
  integer :: ios

  ! Allocate CPO strutures  
  allocate(corep(1))
  allocate(toroidf(1))
  allocate(equil(1))
  allocate(equil_up(1))
  allocate(equil_new(1))

  ! Read CPO files and write corresponding structures  
  open (unit = 10, file = corep_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, corep_in_file)
     call read_cpo(corep(1), 'coreprof')
     call close_read_file
  else
     print *,"CPO file not found ", corep_in_file
     STOP
  end if
  open (unit = 11, file = toroidf_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (11)
     call open_read_file(11, toroidf_in_file)
     call read_cpo(toroidf(1), 'toroidfield')
     call close_read_file
  else
     print *,"CPO file not found ", toroidf_in_file
     STOP
  end if
  open (unit = 12, file = equil_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, equil_in_file )
     call read_cpo(equil(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found:",equil_in_file
     STOP
  end if
  
  ! Update Equilibrium and call CHEASE 
  call equilupdate2cpo(corep, toroidf, equil, equil_up)
  call chease_cpo(equil_up, equil_new) 
  
  ! Save the output file 
  call open_write_file(20, equil_out_file)
  call write_cpo(equil_new(1),'equilibrium')
  call close_write_file
  
  ! deallocations
  call deallocate_cpo(corep)
  call deallocate_cpo(toroidf)
  call deallocate_cpo(equil)
  call deallocate_cpo(equil_up)
  call deallocate_cpo(equil_new)
 
end program equilibrium_test
