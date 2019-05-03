! -*- coding: UTF-8 -*- 
!> @brief  The code to run UQ script ets_test. It concerns the transport model

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

use csv_module

implicit none
  
  ! INPUTS (given by command arguments)
  character(len=128) :: cpo_dir  ! Path to the folder containing CPO files for ets
  character(len=128) :: in_fname ! NML file containing uncertain parameters values
  
  ! CPO file names
  character(len=128) :: corep_in_file 
  character(len=128) :: cores_in_file 
  character(len=128) :: corei_in_file 
  character(len=128) :: equil_in_file 
  character(len=128) :: coret_in_file   
  character(len=128) :: toroidf_in_file
  character(len=128) :: corep_out_file  
  character(len=128) :: equil_out_file  

  ! CPO structures 
  type (type_coreprof)   , pointer :: corep(:)     => NULL()
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_coretransp) , pointer :: coret(:)     => NULL()
  type (type_coresource) , pointer :: cores(:)     => NULL()
  type (type_coreimpur)  , pointer :: corei(:)     => NULL()
  type (type_toroidfield), pointer :: toroidf(:)   => NULL()
  type (type_coreprof)   , pointer :: corep_new(:) => NULL()
  type (type_equilibrium), pointer :: equil_new(:) => NULL()
  
  ! Uncertain parameters
  !real(kind=8), allocatable, dimension(:) :: params 
  real(kind=8) :: D1, D2, D3, D4 
  
  ! Output file contraining values of interset (te, ti, pressure ...)
  character(len=128) :: out_file
  type(csv_file)     :: csv_out_file

  ! Other local variables  
  integer :: ios, i, n_data, n_outputs
  logical :: infile_status, outfile_status 

  real(8) :: x(101)
  real(8) :: y(101)
  real(8) :: w(101)
  real(8), allocatable :: cx(:)
  real(8) , allocatable:: cy(:)
  real(8) , allocatable:: t(:)
  
  ! ...
  if (command_argument_count() /=2) then
    write(*,*) "ERROR: exactly 2 input arguments are required"
  STOP
  end if
  
  ! NML file
  call get_command_argument(2, in_fname)

  inquire(file=trim(in_fname), exist=infile_status)
  if (.not. infile_status) then
    write(*,*) "ERROR: reference file '"//trim(in_fname)//"' does not exist"
    stop
  end if
 
  ! Read uncertain paramters (cf. inputs/ets.template) 
  namelist /ets_input_file/  D1, D2, D3, D4, &
                           & out_file  
 
  open(unit=20, file=trim(in_fname))
  read(20, ets_input_file)
  
  ! CPO files
  call get_command_argument(1, cpo_dir)
  
  corep_in_file   = trim(cpo_dir) // "/ets_coreprof_in.cpo"
  equil_in_file   = trim(cpo_dir) // "/ets_equilibrium_in.cpo"
  cores_in_file   = trim(cpo_dir) // "/ets_coresource_in.cpo"
  corei_in_file   = trim(cpo_dir) // "/ets_coreimpur_in.cpo"
  coret_in_file   = trim(cpo_dir) // "/ets_coretransp_in.cpo"
  toroidf_in_file = trim(cpo_dir) // "/ets_toroidfield_in.cpo"
  
  corep_out_file  = "ets_coreprof_out.cpo"
  equil_out_file  = "ets_equilibrium_up.cpo"
  
  ! Allocate CPO strutures  
  allocate(corep(1))
  allocate(equil(1))
  allocate(coret(1))
  allocate(cores(1))
  allocate(corei(1))
  allocate(toroidf(1))
  
  allocate(corep_new(1))
  allocate(equil_new(1))
  
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

  ! Call ets_standalone and update the equibrium
  call ets_cpo(corep, equil, coret, cores, corei, corep_new)
  call equilupdate2cpo(corep_new, toroidf, equil, equil_new)
  
  ! ====== UQ for ETS + CHEASE
  ! Save ets_equilibrium_up.cpo and use it in chease_test
  ! TODO: fix XML conflict and use fus_run
  call open_write_file(16, equil_out_file)
  call write_cpo(equil_new(1),'equilibrium')
  call close_write_file

  ! ====== UQ for ETS
!  ! To collect outputs data, the quantity of interest is Te
!  n_data    = size(equil_new(1)%profiles_1d%pressure)
!  n_outputs = 1 
!  ! Open the CSV output file
!  call csv_out_file%open(out_file, n_cols=n_outputs, status_ok=outfile_status)
!
!  ! Add headers
!  call csv_out_file%add('p')
!  !call csv_out_file%add('ti')
!  call csv_out_file%next_row()
!  
!  ! Add data
!  do i=1, n_data
!    call csv_out_file%add(equil_new(1)%profiles_1d%pressure(i))
!    !call csv_out_file%add(corep_new(1)%ti%value(i, 1))
!    call csv_out_file%next_row()
!  end do
!
!  ! Finished
!  call csv_out_file%close(outfile_status)

  ! CPO deallocations
  call deallocate_cpo(equil_new)
  call deallocate_cpo(corep_new)  
  call deallocate_cpo(toroidf)
  call deallocate_cpo(corei)
  call deallocate_cpo(cores)
  call deallocate_cpo(coret)  
  call deallocate_cpo(equil)
  call deallocate_cpo(corep)
 
end program ets_run
