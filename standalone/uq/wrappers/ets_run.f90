! -*- coding: UTF-8 -*- 
!> @brief  The code to run UQ script ets_test. 
!> Uncertainties in: Te and Ti boudaries (Edge).

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

use spl_module, only: splrep, splev
use csv_module

implicit none
  
  ! The input given by command argument: NML file containing uncertain values.
  character(len=128) :: in_fname 

  ! Path to the folder containing CPO files for ets. 
  ! This folder should be created and populated by the python code (easyVVUQ compaign).
  character(*), parameter :: cpo_dir = "../../common/"
  
  ! UQP method
  character(*), parameter :: uqp = "uqp1"
  
  ! Spline degree and Control points number
  integer, parameter :: p = 3 
  integer, parameter :: n = 6

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
  
  ! For UQP2: knots vector and conrol points
  real(8), dimension(:), allocatable :: knots, c


  ! Uncertain parameters
  real(kind=8) :: Te_boundary, Ti_boundary
  !real(kind=8) :: D1, D2, D3, D4 
  
  ! Output file contraining values of interset (te, ti, pressure ...)
  character(len=128) :: out_file 
  type(csv_file)     :: csv_out_file 

  ! Other local variables  
  integer :: ios, i, n_data, n_outputs 
  logical :: infile_status, outfile_status 

  ! ...
  if (command_argument_count() /=1) then
    write(*,*) "ERROR: exactly 1 input arguments are required"
  STOP
  end if
  
  ! NML file
  call get_command_argument(1, in_fname)

  inquire(file=trim(in_fname), exist=infile_status)
  if (.not. infile_status) then
    write(*,*) "ERROR: reference file '"//trim(in_fname)//"' does not exist"
    stop
  end if
 
  ! Read uncertain paramters (cf. inputs/boundaries.template) 
  namelist /boundaries_input_file/ Te_boundary, Ti_boundary, out_file  
 
 
  open(unit=20, file=trim(in_fname))
  read(20, boundaries_input_file)
  
  ! CPO files
  corep_in_file   = trim(cpo_dir) // "ets_coreprof_in.cpo"
  equil_in_file   = trim(cpo_dir) // "ets_equilibrium_in.cpo"
  cores_in_file   = trim(cpo_dir) // "ets_coresource_in.cpo"
  corei_in_file   = trim(cpo_dir) // "ets_coreimpur_in.cpo"
  coret_in_file   = trim(cpo_dir) // "ets_coretransp_in.cpo"
  toroidf_in_file = trim(cpo_dir) // "ets_toroidfield_in.cpo"
  
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
    ! Update the initial conditions (Te and Ti in the edge)
    corep(1)%te%boundary%value(1) = Te_boundary
    corep(1)%ti%boundary%value(1,1) = Ti_boundary
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
!     coret(1)%values(1)%te_transp%diff_eff(1)=D1
!     coret(1)%values(1)%te_transp%diff_eff(2)=D2
!     coret(1)%values(1)%te_transp%diff_eff(3)=D3
!     coret(1)%values(1)%te_transp%diff_eff(4)=D4
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
  
  select case(uqp)
    ! ====== UQP 1
    case('uqp1')
      ! To collect outputs data, the quantity of interest is Te
      n_data    = 100
      n_outputs = 2 
      ! Open the CSV output file
      call csv_out_file%open(out_file, n_cols=n_outputs, status_ok=outfile_status)

      ! Add headers
      call csv_out_file%add('Te')
      call csv_out_file%add('Ti')
      call csv_out_file%next_row()
      
      ! Add data
      do i=1, n_data
        call csv_out_file%add(corep_new(1)%te%value(i))
        call csv_out_file%add(corep_new(1)%ti%value(i, 1))
        call csv_out_file%next_row()
      end do
    
    ! ====== UQP 2
    case('uqp2')
      ! knotd vector and control points
      allocate(knots(n+p+1)) ! could be done inside splrep
      allocate(c(n))
      
      ! Approximation of p
      call splrep(equil_new(1)%profiles_1d%pressure, n, p, knots, c)
      
      ! the first derivative = 0 in rho=0
      c(2) = c(1) 
      
      ! Open the CSV output file
      call csv_out_file%open(out_file, n_cols=1, status_ok=outfile_status)
      
      ! Add headers
      call csv_out_file%add('c')
      call csv_out_file%next_row()
      
      ! Add data
      do i=2, n-1
        call csv_out_file%add(c(i))
        call csv_out_file%next_row()
      end do
      
      deallocate(knots)
      deallocate(c) 

    case default
      write(*,*) 'Error: Invalid UQP type!'
      STOP
   end select

  ! Finished
  call csv_out_file%close(outfile_status)

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
