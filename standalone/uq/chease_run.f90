! -*- coding: UTF-8 -*- 
!> @brief 
! UQ test for chease 
!> @author
! Jalal Lakhlili (jlakhlili@gmail.com)

program chease_run


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

use chease_standalone,      only: chease2file

use csv_module

implicit none
  
  ! INPUTS
  character(len=128) :: cpo_dir
  character(len=128) :: in_fname
  
  ! CPO file names
  character(len=128) :: equil_in_file 
  
  ! CPO structures 
  type (type_equilibrium), pointer :: equil(:)  => NULL()
  
  ! Outputfile contraining values of interset (Q)
  character(len=128) :: out_file = "output.csv"
  type(csv_file) :: csv_out_file

  ! Q profile
  real(kind=8), allocatable, dimension(:) :: q

  ! Other local variables  
  integer :: ios, i 
  logical :: infile_status, outfile_status 
  real(kind=8) :: P0, P1, J0, J1
  
  ! ...
  if (command_argument_count() /=2) then
    write(*,*) "ERROR: exactly 1 input argument is required"
  STOP
  end if
  
  call get_command_argument(1, cpo_dir)
  
  call get_command_argument(2, in_fname)
  inquire(file=trim(in_fname), exist=infile_status)
  if (.not. infile_status) then
    write(*,*) "ERROR: reference file '"//trim(in_fname)//"' does not exist"
    stop
  end if

  ! Read uncertain paramters (cf. inputs/eq.template) 
  namelist /eq_input_file/  P0, &
                          & P1, &
                          & J0, &
                          & J1, &
                          & out_file  
  
  open(unit=20, file=trim(in_fname))
  read(20, eq_input_file)
  equil_in_file   = trim(cpo_dir) // "/ets_equilibrium_up.cpo"

  
  ! Allocate CPO strutures  
  allocate(equil(1))
  
  ! Read CPO file and write corresponding structures   
  open (unit = 19, file = equil_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (19)
     call open_read_file(19, equil_in_file )
     call read_cpo(equil(1), 'equilibrium' )
     equil(1)%profiles_1d%pressure(1)  = P0
     equil(1)%profiles_1d%pressure(51)  = P1
     equil(1)%profiles_1d%jparallel(1) = J0
     equil(1)%profiles_1d%jparallel(51) = J1
     call close_read_file
  else
     print *,"CPO file not found:",equil_in_file
     STOP
  end if
 
  allocate(q(101))

  call chease2file(equil, q) 
  ! Write CPO output files
!  call open_write_file(16, equil_out_file)
!  call write_cpo(equil_new(1),'equilibrium')
!  call close_write_file
  
  ! To collect outputs data, the quantity of interest is q
  ! Open the CSV output file
  call csv_out_file%open(out_file, n_cols=1, status_ok=outfile_status)

  ! Add headers
  call csv_out_file%add('q')
  call csv_out_file%next_row()
  
  ! Add data
  do i=1, 101
    call csv_out_file%add(q(i))
    call csv_out_file%next_row()
  end do

  ! Finished
  call csv_out_file%close(outfile_status)

  ! deallocations
 
  deallocate(q)
  call deallocate_cpo(equil)
 
end program chease_run
