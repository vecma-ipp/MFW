! -*- coding: UTF-8 -*- 
!> @brief  run: ETS + Update EQ + CHEASE
!> @author Jalal Lakhlili 
!> Example of usage:
!>  ../bin/DRACO/full_run ../../data/AUG_28906_5/BGB_GEM_SPREAD/4FT
!

program ets_run

use allocate_deallocate

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

use ets_standalone,         only: ets_cpo
use equilupdate_standalone, only: equilupdate2cpo
use chease_standalone,      only: chease_cpo

implicit none
  
  ! INPUT: Path to the folder containing CPO files
  character(len=128) :: cpo_dir 
  
  ! CPO file names
  character(len=128) :: corep_in_file 
  character(len=128) :: cores_in_file 
  character(len=128) :: corei_in_file 
  character(len=128) :: equil_in_file 
  character(len=128) :: coret_in_file   
  character(len=128) :: toroidf_in_file
  character(len=128) :: corep_out_file  
  character(len=128) :: equil_up_file  
  character(len=128) :: equil_out_file  

  ! CPO structures 
  type (type_coreprof)   , pointer :: corep(:)     => NULL()
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_coretransp) , pointer :: coret(:)     => NULL()
  type (type_coresource) , pointer :: cores(:)     => NULL()
  type (type_coreimpur)  , pointer :: corei(:)     => NULL()
  type (type_toroidfield), pointer :: toroidf(:)   => NULL()
  type (type_coreprof)   , pointer :: corep_new(:) => NULL()
  type (type_equilibrium), pointer :: equil_up(:) => NULL()
  type (type_equilibrium), pointer :: equil_new(:) => NULL()
  
  ! To test CPO files presence in cpo_dir
  integer :: ios 
  
  ! Read the Path to CPO dir from the consol 
  call get_command_argument(1, cpo_dir)
  
  ! CPO files
  corep_in_file   = trim(cpo_dir) // "/ets_coreprof_in.cpo"
  equil_in_file   = trim(cpo_dir) // "/ets_equilibrium_in.cpo"
  cores_in_file   = trim(cpo_dir) // "/ets_coresource_in.cpo"
  corei_in_file   = trim(cpo_dir) // "/ets_coreimpur_in.cpo"
  coret_in_file   = trim(cpo_dir) //  "/ets_coretransp_in.cpo"
  toroidf_in_file  = trim(cpo_dir) // "/ets_toroidfield_in.cpo"
  
  corep_out_file  = "ets_coreprof_out.cpo"
  equil_up_file  = "ets_equilibrium_out.cpo"
  equil_out_file  = "chease_equilibrium_out.cpo"
  
  ! Allocate CPO strutures  
  allocate(corep(1))
  allocate(equil(1))
  allocate(coret(1))
  allocate(cores(1))
  allocate(corei(1))
  allocate(toroidf(1))
  allocate(corep_new(1))
  allocate(equil_up(1))
  allocate(equil_new(1))
  
  ! Read CPO files and write corresponding structures   
  open (unit = 10, file = corep_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, corep_in_file)
     call read_cpo(corep(1), 'coreprof' )
     call close_read_file
  else
     print *,"ERROR. CPO file not found:",corep_in_file
     STOP
  end if
  
  open (unit = 11, file = equil_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (11)
     call open_read_file(11, equil_in_file )
     call read_cpo(equil(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found:",equil_in_file
     STOP
  end if
  
  open (unit = 12, file = coret_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, coret_in_file)
     call read_cpo(coret(1), 'coretransp')
     call close_read_file
  else
     print *,"CPO file not found:",coret_in_file
     STOP
  end if
  
  open (unit = 13, file = cores_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (13)
     call open_read_file(13, cores_in_file )
     call read_cpo(cores(1), 'coresource' )
     call close_read_file
  else
     print *,"CPO file not found:",cores_in_file
     STOP
  end if
  
  open (unit = 14, file = corei_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (14)
     call open_read_file(14, corei_in_file )
     call read_cpo(corei(1), 'coreimpur' )
     call close_read_file
  else
     print *,"CPO file not found:",corei_in_file
     STOP
  end if
  
  open (unit = 15, file = toroidf_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (15)
     call open_read_file(15, toroidf_in_file )
     call read_cpo(toroidf(1), 'toroidfield' )
     call close_read_file
  else
     print *,"CPO file not found:",toroidf_in_file
     STOP
  end if 

  ! ETS
  call ets_cpo(corep, equil, coret, cores, corei, corep_new)
  
  ! Update EQ 
  call equilupdate2cpo(corep_new, toroidf, equil, equil_up)
  
  ! CHEASE
  call chease_cpo(equil_up, equil_new) 

  ! Write CPO output files
  call open_write_file(16, corep_out_file)
  call write_cpo(corep_new(1),'coreprof')
  call close_write_file

  call open_write_file(17, equil_up_file)
  call write_cpo(equil_up(1),'equilibrium')
  call close_write_file

  call open_write_file(18, equil_out_file)
  call write_cpo(equil_new(1),'equilibrium')
  call close_write_file

  ! CPO deallocations
  call deallocate_cpo(equil_new)
  call deallocate_cpo(equil_up)
  call deallocate_cpo(corep_new)  
  call deallocate_cpo(toroidf)
  call deallocate_cpo(corei)
  call deallocate_cpo(cores)
  call deallocate_cpo(coret)  
  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
 
end program ets_run
