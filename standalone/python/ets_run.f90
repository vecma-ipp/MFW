! -*- coding: UTF-8 -*- 
!> @brief 
! Wrappers to call the Fortran functions from src/*_standalone.f90
!> @author
! Jalal Lakhlili (jlakhlili@gmail.com)

program ets_run

!use csv_file 
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

use ets_standalone, only: ets_cpo

implicit none
  
  ! INPUTS
  !character(len=64) :: cpo_dir  ! Path to the folder containing CPO files
  character(len=*), parameter :: cpo_dir = '../data/SP2FT'
  character(len=64) :: in_fname ! NML file containing uncertain values
  
  ! CPO file names
  character(len=64) :: corep_in_file 
  character(len=64) :: cores_in_file 
  character(len=64) :: corei_in_file 
  character(len=64) :: equil_in_file 
  character(len=64) :: coret_in_file   
  character(len=64) :: corep_out_file  

  ! CPO structures 
  type (type_coreprof)   , pointer :: corep(:)     => NULL()
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_coretransp) , pointer :: coret(:)     => NULL()
  type (type_coresource) , pointer :: cores(:)     => NULL()
  type (type_coreimpur)  , pointer :: corei(:)     => NULL()
  type (type_coreprof)   , pointer :: corep_new(:) => NULL()
  
  ! Outputfile contraining values of interset (Te)
  character(len=64) :: out_file
  character(len=80) :: record

  ! Other local variables  
  integer :: i, n, ios, csv_file_unit
  logical :: path_exist, file_exists 
  real(kind=8) :: D1, D2 
  
  
  ! ...
!  if (command_argument_count() /=2) then
!    write(*,*) "ERROR: exactly 1 input argument is required"
!  STOP
!  end if
!  
!  call get_command_argument(1, cpo_dir)
!  inquire(file=trim(cpo_dir), exist=path_exist)
!  if (.not. path_exist) then
!    write(*,*) "ERROR: reference path '"//trim(cpo_dir)//"' does not exist"
!  STOP
!  end if

  call get_command_argument(1, in_fname)
  inquire(file=trim(in_fname), exist=file_exists)
  if (.not. file_exists) then
    write(*,*) "ERROR: reference file '"//trim(in_fname)//"' does not exist"
    stop
  end if

  ! Read uncertain paramters (cf. inputs/ets.template) 
  namelist /ets_input_file/  D1, &
                           & D2, &
                           & out_file  
  
  open(unit=20, file=trim(in_fname))
  read(20, ets_input_file)
  
  ! CPO files
  corep_in_file   = cpo_dir // "/ets_coreprof_in.cpo"
  equil_in_file   = cpo_dir // "/ets_equilibrium_in.cpo"
  cores_in_file   = cpo_dir // "/ets_coresource_in.cpo"
  corei_in_file   = cpo_dir // "/ets_coreimpur_in.cpo"
  coret_in_file   = cpo_dir //  "/ets_coretransp_in.cpo"
  corep_out_file  = cpo_dir // "/ets_coreprof_out.cpo"
  
  ! Allocate CPO strutures  
  allocate(corep(1))
  allocate(equil(1))
  allocate(coret(1))
  allocate(cores(1))
  allocate(corei(1))
  
  allocate(corep_new(1))
  
  ! Read CPO file and write corresponding structures   
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
     ! Update the transport coefficients
     coret(1)%values(1)%te_transp%diff_eff(1)=D1
     coret(1)%values(1)%te_transp%diff_eff(2)=D2
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
  
  ! Call ets_standalone
  call ets_cpo(corep, equil, coret, cores, corei, corep_new)
  
  ! Write output files
  call open_write_file(16, corep_out_file)
  call write_cpo(corep_new(1),'coreprof')
  call close_write_file
  
  ! Extract the quantity of interest and save them in the output file
  
  ! Write the header line.
  call csv_file_open_write (out_file, csv_file_unit)
  record = 'Te'
  call csv_file_record_write(out_file, csv_file_unit, record)
  
  ! Write corresponding values
  n =  size(corep_new(1)%te%value)
  do i = 1, n
    record = ''
    call csv_record_append_r8(corep_new(1)%te%value(i), record)
    call csv_file_record_write(out_file, csv_file_unit, record)
  end do

  call csv_file_close_write(out_file, csv_file_unit)
  ! ... deallocations
  call deallocate_cpo(corep_new)  
  call deallocate_cpo(corei)
  call deallocate_cpo(cores)
  call deallocate_cpo(coret)  
  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
 
end program ets_run
