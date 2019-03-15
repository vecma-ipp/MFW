! -*- coding: UTF-8 -*- 
!> @brief 
! UQ test for ets (4 uncertain params)
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
use csv_module

implicit none
  
  ! INPUTS
  character(len=128) :: cpo_dir  ! Path to the folder containing CPO files
  character(len=128) :: in_fname ! NML file containing uncertain values
  
  ! CPO file names
  character(len=128) :: corep_in_file 
  character(len=128) :: cores_in_file 
  character(len=128) :: corei_in_file 
  character(len=128) :: equil_in_file 
  character(len=128) :: coret_in_file   
  character(len=128) :: corep_out_file  

  ! CPO structures 
  type (type_coreprof)   , pointer :: corep(:)     => NULL()
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_coretransp) , pointer :: coret(:)     => NULL()
  type (type_coresource) , pointer :: cores(:)     => NULL()
  type (type_coreimpur)  , pointer :: corei(:)     => NULL()
  type (type_coreprof)   , pointer :: corep_new(:) => NULL()
  
  ! Outputfile contraining values of interset (Te, Ti, ...)
  character(len=128) :: out_file
  type(csv_file) :: csv_out_file

  ! Other local variables  
  integer :: ios, i, n_data, n_outputs
  logical :: infile_status, outfile_status 
  real(kind=8) :: D1, D2, D3, D4 
  
  ! ...
  if (command_argument_count() /=2) then
    write(*,*) "ERROR: exactly 2 input arguments are required"
  STOP
  end if
  
  call get_command_argument(1, cpo_dir)

  call get_command_argument(2, in_fname)
  inquire(file=trim(in_fname), exist=infile_status)
  if (.not. infile_status) then
    write(*,*) "ERROR: reference file '"//trim(in_fname)//"' does not exist"
    stop
  end if
 
  ! Read uncertain paramters (cf. inputs/ets.template) 
  namelist /ets_input_file/  D1, &
                           & D2, &
                           & D3, &
                           & D4, &
                           & out_file  
  
  open(unit=20, file=trim(in_fname))
  read(20, ets_input_file)
  
  ! CPO files
  corep_in_file   = trim(cpo_dir) // "/ets_coreprof_in.cpo"
  equil_in_file   = trim(cpo_dir) // "/ets_equilibrium_in.cpo"
  cores_in_file   = trim(cpo_dir) // "/ets_coresource_in.cpo"
  corei_in_file   = trim(cpo_dir) // "/ets_coreimpur_in.cpo"
  coret_in_file   = trim(cpo_dir) //  "/ets_coretransp_in.cpo"
  corep_out_file  = trim(cpo_dir) // "/ets_coreprof_out.cpo"
  
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
     coret(1)%values(1)%te_transp%diff_eff(3)=D3
     coret(1)%values(1)%te_transp%diff_eff(4)=D4
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
  
  !  ! Write CPO output files
  !  call open_write_file(16, corep_out_file)
  !  call write_cpo(corep_new(1),'coreprof')
  !  call close_write_file
  
  ! To collect outputs data, the quantity of interest is Te
  n_data    = size(corep_new(1)%te%value)
  n_outputs = 1  
  ! Open the CSV output file
  call csv_out_file%open(out_file, n_cols=n_outputs, status_ok=outfile_status)

  ! Add headers
  call csv_out_file%add('Te')
  call csv_out_file%next_row()
  
  ! Add data
  do i=1, n_data
    call csv_out_file%add(corep_new(1)%te%value(i))
    call csv_out_file%next_row()
  end do

  ! Finished
  call csv_out_file%close(outfile_status)

  ! CPO deallocations
  call deallocate_cpo(corep_new)  
  call deallocate_cpo(corei)
  call deallocate_cpo(cores)
  call deallocate_cpo(coret)  
  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
 
end program ets_run
