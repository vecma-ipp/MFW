! -*- coding: UTF-8 -*- 
!> @brief  run UQ for the loop:  ETS + Update EQ + CHEASE + BOHMGB
!> Uncertainty in sources 

program gauss_src_run

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

use copy_structures, only: copy_cpo

use ets_standalone,         only: ets_cpo
use equilupdate_standalone, only: equilupdate2cpo
use chease_standalone,      only: chease_cpo
use bohmgb_standalone,      only: bohmgb_cpo
use sources_standalone, only: gaussian_source_cpo

use csv_module

implicit none
  
  ! INPUT: Path to the folder containing CPO files
  character(len=128) :: cpo_dir 
  ! INPUT: nml file containing uncertain parameters values
  character(len=128) :: in_fname   

  ! Uncertain parameters: 
  real(kind=8) :: S1, S2, S3
  real(8) :: params_in(3)
  
  ! Output file contraining values of interset (te, ti, pressure ...)
  character(len=128) :: out_file
  type(csv_file)     :: csv_out_file

  ! LOOP paramaters
  integer, parameter :: STEPS = 30 
  logical, parameter :: TIMETRACE = .FALSE.
  
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
  type (type_coreprof)   , pointer :: corep_in(:)   => NULL()
  type (type_equilibrium), pointer :: equil_in(:)   => NULL()
  type (type_coretransp) , pointer :: coret_in(:)   => NULL()
  type (type_coresource) , pointer :: cores_in(:)   => NULL()
  type (type_coreimpur)  , pointer :: corei_in(:)   => NULL()
  type (type_toroidfield), pointer :: toroidf_in(:) => NULL()

  type (type_coreprof)   , pointer :: corep_old(:) => NULL()
  type (type_coreprof)   , pointer :: corep_new(:) => NULL()
  type (type_coreprof)   , pointer :: corep_ets(:) => NULL()
  
  type (type_equilibrium), pointer :: equil_update(:)  => NULL()
  type (type_equilibrium), pointer :: equil_chease(:) => NULL()

  type(type_coretransp), pointer :: coret_bohmgb(:) => null()
  
  ! Other variables
  integer :: ios, it, i, n_outputs, n_data
  logical :: infile_status, outfile_status 
  character(4)  :: itstr
 
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
 
  ! Read uncertain paramters (cf. inputs/ic.template) 
  namelist /src_input_file/ S1, S2, S3, out_file  
  
  open(unit=9, file=trim(in_fname))
  read(9, src_input_file)
  
  ! Read the Path to CPO dir from the consol 
  call get_command_argument(1, cpo_dir)
  
  ! INPUT CPO files
  corep_in_file   = trim(cpo_dir) // "/ets_coreprof_in.cpo"
  equil_in_file   = trim(cpo_dir) // "/ets_equilibrium_in.cpo"
  cores_in_file   = trim(cpo_dir) // "/ets_coresource_in.cpo"
  corei_in_file   = trim(cpo_dir) // "/ets_coreimpur_in.cpo"
  coret_in_file   = trim(cpo_dir) //  "/ets_coretransp_in.cpo"
  toroidf_in_file = trim(cpo_dir) // "/ets_toroidfield_in.cpo"
  
  ! Allocate CPO strutures  
  allocate(corep_in(1))
  allocate(equil_in(1))
  allocate(coret_in(1))
  allocate(cores_in(1))
  allocate(corei_in(1))
  allocate(toroidf_in(1))

  ! Read CPO files and write corresponding structures   
  open (unit = 10, file = corep_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, corep_in_file)
     call read_cpo(corep_in(1), 'coreprof' )
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
     call read_cpo(equil_in(1), 'equilibrium' )
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
     call read_cpo(coret_in(1), 'coretransp')
     call close_read_file
  else
     print *,"CPO file not found:",coret_in_file
     STOP
  end if
  
!  open (unit = 13, file = cores_in_file, &
!       status = 'old', form = 'formatted', &
!       action = 'read', iostat = ios)
!  if (ios == 0) then
!     close (13)
!     call open_read_file(13, cores_in_file )
!     call read_cpo(cores_in(1), 'coresource' )
!     call close_read_file
!  else
!     print *,"CPO file not found:",cores_in_file
!     STOP
!  end if
  
  open (unit = 14, file = corei_in_file, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (14)
     call open_read_file(14, corei_in_file )
     call read_cpo(corei_in(1), 'coreimpur' )
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
     call read_cpo(toroidf_in(1), 'toroidfield' )
     call close_read_file
  else
     print *,"CPO file not found:",toroidf_in_file
     STOP
  end if
  
  ! To update code_parameters
  params_in(1) = S1 ! WTOT_el
  params_in(2) = S2 ! RHEAT_el
  params_in(3) = S3 ! FWHEAT_el

  ! Run gausian_sources
  call gaussian_source_cpo(corep_in, equil_in, params_in, cores_in)
  
!  if (.not.TIMETRACE) then
!    call open_write_file(19,'ets_coresource_out.cpo')
!    call write_cpo(cores_in(1),'coresource')
!    call close_write_file
!  end if
  
  ! Loop: ETS - CHEASE - BOHNGB
  allocate(corep_old(1))
  allocate(corep_ets(1))
  allocate(equil_chease(1))
  allocate(coret_bohmgb(1))

  call copy_cpo(corep_in(1), corep_ets(1))
  call copy_cpo(equil_in(1), equil_chease(1))
  call copy_cpo(coret_in(1), coret_bohmgb(1))

  do it=1, STEPS
    write(itstr,'(I4.4)') it
    print *,"**** iteration = "//itstr//" ****"

    ! ETS
    call copy_cpo(corep_ets(1), corep_old(1))
    call deallocate_cpo(corep_ets)
    nullify(corep_ets)
    call ets_cpo(corep_old, equil_chease, coret_bohmgb, cores_in, corei_in, corep_ets)

    if (TIMETRACE) then
      call open_write_file(20,'ets_coreprof_'//itstr//'.cpo')
      call write_cpo(corep_ets(1),'coreprof')
      call close_write_file
    end if

    ! EQUILUPDATE
    call deallocate_cpo(equil_update)
    nullify(equil_update)
    call equilupdate2cpo(corep_ets, toroidf_in, equil_chease, equil_update)

    if (TIMETRACE) then
      call open_write_file(21,'equilupdate_equilibrium_'//itstr//'.cpo')
      call write_cpo(equil_update(1),'equilibrium')
      call close_write_file
    end if

    ! CHEASE
    call deallocate_cpo(equil_chease)
    nullify(equil_chease)
    call chease_cpo(equil_update, equil_chease)

    if (TIMETRACE) then
      call open_write_file(22,'chease_equilibrium_'//itstr//'.cpo')
      call write_cpo(equil_chease(1),'equilibrium')
      call close_write_file
    end if

    ! BOHMGB
    call deallocate_cpo(coret_bohmgb)
    nullify(coret_bohmgb)
    call bohmgb_cpo(equil_chease, corep_ets, coret_bohmgb)

    if (TIMETRACE) then
      call open_write_file(23,'bohmgb_coretransp_'//itstr//'.cpo')
      call write_cpo(coret_bohmgb(1),'coretransp')
      call close_write_file
    end if

  end do
  
!  if (.not.TIMETRACE) then
!     call open_write_file(20,'ets_coreprof_'//itstr//'.cpo')
!     call write_cpo(corep_ets(1),'coreprof')
!     call close_write_file
!  end if
  
  ! UQ Analysis: collect outputs data, the quantity of interest is Te
  n_data    = 100 
  n_outputs = 2 
  ! Open the CSV output file
  call csv_out_file%open(out_file, n_cols=n_outputs, status_ok=outfile_status)

  ! Add headers
  call csv_out_file%add('te')
  call csv_out_file%add('ti')
  call csv_out_file%next_row()
  
  ! Add data
  do i=1, n_data
    call csv_out_file%add(corep_ets(1)%te%value(i))
    call csv_out_file%add(corep_ets(1)%ti%value(i, 1))
    call csv_out_file%next_row()
  end do

  ! Finished
  call csv_out_file%close(outfile_status)

  ! CPO deallocations
  call deallocate_cpo(corep_in)
  call deallocate_cpo(corep_old)
  call deallocate_cpo(corep_ets)
  call deallocate_cpo(coret_in)
  call deallocate_cpo(coret_bohmgb)
  call deallocate_cpo(equil_in)
  call deallocate_cpo(equil_update)
  call deallocate_cpo(equil_chease)
  call deallocate_cpo(cores_in)
  call deallocate_cpo(corei_in)
  call deallocate_cpo(toroidf_in)
 
end program gauss_src_run
