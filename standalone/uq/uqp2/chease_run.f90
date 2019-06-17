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
use spl_module, only: splrep, splev

use csv_module

implicit none
  
  ! INPUTS (given by command arguments)
  character(len=128) :: cpo_dir  ! Path to the folder containing CPO files for ets
  character(len=128) :: in_fname ! NML file containing uncertain parameters values
  
  ! Spline degree and Control points number
  integer, parameter :: p = 3 
  integer, parameter :: n = 6

  character(len=128) :: equil_in_file  
  character(len=128) :: equil_out_file 
  
  ! CPO structures 
  type (type_equilibrium), pointer :: equil(:)     => NULL()
  type (type_equilibrium), pointer :: equil_new(:) => NULL()
  
  ! For UQP2: knots vector and conrol points
  real(8), dimension(:), allocatable :: knots, cp, rgrid
  real(8), dimension(:, :), allocatable :: pnew 

  ! Uncertain parameters (Control points)
  real(kind=8) :: C0, C2, C3, C4 

! Output file contraining values of interset (b_av, gm8, ...)
  character(len=128) :: out_file 
  type(csv_file)     :: csv_out_file 

  ! Other local variables  
  integer :: ios, i, n_data, n_outputs, m
  logical :: infile_status, outfile_status 

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
  namelist /eq_input_file/  C0, C2, C3, C4, &
                           & out_file  
 
  open(unit=20, file=trim(in_fname))
  read(20, eq_input_file)

  ! CPO files
  call get_command_argument(1, cpo_dir)
  equil_in_file   = trim(cpo_dir) // "/ets_equilibrium_in.cpo"
  
  ! Allocate CPO strutures  
  allocate(equil(1))
  allocate(equil_new(1))
  
  ! Read CPO file and write corresponding structures   
  open (unit = 20, file = equil_in_file, &
    status = 'old', form = 'formatted', &
    action = 'read', iostat = ios)
  
  if (ios == 0) then
    close (20)
    call open_read_file(20, equil_in_file )
    call read_cpo(equil(1), 'equilibrium' )

    ! knot vector and control points
    allocate(knots(n+p+1))
    do i =1, p+1
      knots(i) = 0.0
      knots(n+i) = 1.0
    enddo
    do i = 1, n-p-1
      knots(p+i+1) = 1.0*i/(n-p)
    enddo

    ! evaluation grid 
    m = 101
    allocate(rgrid(m))
    do i = 0, m-1
      rgrid(i+1) = 1.0*i/(m-1)
    enddo

    ! control points
    allocate(cp(n))
    cp(1) = C0
    cp(2) = C0
    cp(3) = C2
    cp(4) = C3
    cp(5) = C4
    cp(6) = equil(1)%profiles_1d%pressure(m)

    ! evaluate new p
    allocate(pnew(1, m))
    call splev(1, n, p, knots, cp, m, rgrid, pnew)
    equil(1)%profiles_1d%pressure(:) = pnew(1,:)
    
    deallocate(cp)
    deallocate(rgrid)
    deallocate(knots)
    deallocate(pnew)
    call close_read_file

  else
    print *,"CPO file not found:",equil_in_file
    STOP
  end if
  
  ! CHEASE 
  call chease_cpo(equil, equil_new) 
  
  ! Open the CSV output file
  call csv_out_file%open(out_file, n_cols=1, status_ok=outfile_status)

  ! Add headers
  call csv_out_file%add('b_av')
  !call csv_out_file%add('gm8')
  call csv_out_file%next_row()
  
  ! Add data
  n_data = size(equil_new(1)%profiles_1d%gm8)
  do i=1, n_data
    call csv_out_file%add(equil_new(1)%profiles_1d%b_av(i))
    !call csv_out_file%add(equil_new(1)%profiles_1d%gm8(i))
    call csv_out_file%next_row()
  end do

  ! Finished
  call csv_out_file%close(outfile_status)

  ! deallocations
  call deallocate_cpo(equil)
  call deallocate_cpo(equil_new)
 
end program chease_run
