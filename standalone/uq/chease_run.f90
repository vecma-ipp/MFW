! -*- coding: UTF-8 -*- 
!> @brief 
! UQ test for chease 
!> @author
! Jalal Lakhlili (jlakhlili@gmail.com)

program chease_run

use euitm_schemas,   only: type_coreprof, & 
                        &  type_equilibrium, &
                        &  type_coretransp, &
                        &  type_coresource, &
                        &  type_coreimpur, &
                        &  type_toroidfield
use read_structures, only: open_read_file, &
                        &  close_read_file, &
                        &  read_cpo
use write_structures, only: open_write_file, &
                         &  close_write_file, &
                         &  write_cpo

use deallocate_structures, only: deallocate_cpo

use chease_standalone, only: chease_cpo

use csv_module

implicit none
  
  ! INPUT: the ets_run output file
  character(len=128) :: equil_in_file =  "ets_equilibrium_up.cpo"
  
  ! CPO structures 
  type (type_equilibrium), pointer :: equil(:)  => NULL()
  type (type_equilibrium), pointer :: equil_new(:) => NULL()
  
  ! Outputfile contraining values of interset (Q)
  character(len=128) :: out_file = "q_output.csv"
  type(csv_file) :: csv_out_file

  ! Other local variables  
  integer :: ios, i, n_data 
  logical :: infile_status, outfile_status 
  
  ! Allocate CPO strutures  
  allocate(equil(1))
  allocate(equil_new(1))
  
  ! Read CPO file and write corresponding structures   
  open (unit = 20, file = equil_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (20)
     call open_read_file(19, equil_in_file )
     call read_cpo(equil(1), 'equilibrium' )
     call close_read_file
  else
     print *,"CPO file not found:",equil_in_file
     STOP
  end if
  
  ! CHEASE 
  call chease_cpo(equil, equil_new) 

  ! Collect outputs data (the quantity of interest is q)
  ! q profile size
  n_data = size(equil_new(1)%profiles_1d%q)
  
  ! Open the CSV output file
  call csv_out_file%open(out_file, n_cols=1, status_ok=outfile_status)

  ! Add headers
  call csv_out_file%add('q')
  call csv_out_file%next_row()
  
  ! Add data
  do i=1, n_data
    call csv_out_file%add(equil_new(1)%profiles_1d%q(i))
    call csv_out_file%next_row()
  end do

  ! Finished
  call csv_out_file%close(outfile_status)

  ! deallocations
  call deallocate_cpo(equil)
  call deallocate_cpo(equil_new)
 
end program chease_run
