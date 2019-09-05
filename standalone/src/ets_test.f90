! -*- coding: UTF-8 -*-
program ets_test

use ets_standalone,         only: ets_cpo
use equilupdate_standalone, only: equilupdate2cpo

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

implicit none
  
  ! CPO files
  character(len=*), parameter :: corep_file_in   = "ets_coreprof_in.cpo"
  character(len=*), parameter :: equil_file_in   = "ets_equilibrium_in.cpo"
  character(len=*), parameter :: cores_file_in   = "ets_coresource_in.cpo"
  character(len=*), parameter :: corei_file_in   = "ets_coreimpur_in.cpo"
  character(len=*), parameter :: coret_file_in   = "ets_coretransp_in.cpo"
  character(len=*), parameter :: toroidf_file_in = "ets_toroidfield_in.cpo"

  character(len=*), parameter :: corep_file_out = "ets_coreprof.cpo"
  character(len=*), parameter :: equil_file_out = "updated_equilibrium.cpo"

  ! CPO structures 
  type (type_coreprof)   , pointer :: corep(:)     => NULL()
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_coretransp) , pointer :: coret(:)     => NULL()
  type (type_coresource) , pointer :: cores(:)     => NULL()
  type (type_coreimpur)  , pointer :: corei(:)     => NULL()
  type (type_toroidfield), pointer :: toroidf(:)   => NULL()
  type (type_coreprof)   , pointer :: corep_new(:) => NULL()
  type (type_equilibrium), pointer :: equil_up(:)  => NULL()
  
  integer :: ios 

  allocate(corep(1))
  allocate(equil(1))
  allocate(coret(1))
  allocate(cores(1))
  allocate(corei(1))
  allocate(toroidf(1))
  
  allocate(corep_new(1))
  allocate(equil_up(1))
  
  ! Read CPO file and write corresponding structures   
  open (unit = 10, file = corep_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
    close (10)
    call open_read_file(10, corep_file_in)
    call read_cpo(corep(1), 'coreprof' )
    call close_read_file
  else
     print *,"ERROR. CPO file not found:",corep_file_in
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
     print *,"CPO file not found:",equil_file_in
     STOP
  end if
  
  open (unit = 12, file = coret_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, coret_file_in)
     call read_cpo(coret(1), 'coretransp')
     call close_read_file
  else
     print *,"CPO file not found:",coret_file_in
     STOP
  end if
  
  open (unit = 13, file = cores_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (13)
     call open_read_file(13, cores_file_in )
     call read_cpo(cores(1), 'coresource' )
     call close_read_file
  else
     print *,"CPO file not found:",cores_file_in
     STOP
  end if
  
  open (unit = 14, file = corei_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (14)
     call open_read_file(14, corei_file_in )
     call read_cpo(corei(1), 'coreimpur' )
     call close_read_file
  else
     print *,"CPO file not found:",corei_file_in
     STOP
  end if
  
  open (unit = 15, file = toroidf_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (15)
     call open_read_file(15, toroidf_file_in )
     call read_cpo(toroidf(1), 'toroidfield' )
     call close_read_file
  else
     print *,"CPO file not found:",toroidf_file_in
     STOP
  end if 

  ! Call ets_standalone and update the equibrium
  call ets_cpo(corep, equil, coret, cores, corei, corep_new)
  call equilupdate2cpo(corep_new, toroidf, equil, equil_up)
 
  ! Save the output files 
  call open_write_file(20, corep_file_out)
  call write_cpo(corep_new(1),'coreprof')
  call close_write_file
  
  call open_write_file(21, equil_file_out)
  call write_cpo(equil_up(1),'equilibrium')
  call close_write_file

  ! CPO deallocations
  call deallocate_cpo(equil_up)
  call deallocate_cpo(corep_new)  
  call deallocate_cpo(toroidf)
  call deallocate_cpo(corei)
  call deallocate_cpo(cores)
  call deallocate_cpo(coret)  
  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
 
end program ets_test
