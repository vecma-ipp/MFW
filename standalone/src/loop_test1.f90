! -*- coding: UTF-8 -*-
!> Box for:  ETS + Update EQ + CHEASE + BOHMGB.

program loop_test1

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

implicit none

  ! CPO files
  character(len=*), parameter :: corep_file_in   = "ets_coreprof_in.cpo"
  character(len=*), parameter :: equil_file_in   = "ets_equilibrium_in.cpo"
  character(len=*), parameter :: cores_file_in   = "ets_coresource_in.cpo"
  character(len=*), parameter :: corei_file_in   = "ets_coreimpur_in.cpo"
  character(len=*), parameter :: coret_file_in   = "ets_coretransp_in.cpo"
  character(len=*), parameter :: toroidf_file_in = "ets_toroidfield_in.cpo"

  integer, parameter :: STEPS = 20
  logical, parameter :: TIMETRACE = .FALSE.

  type(type_coreprof), pointer :: corep_in(:), corep_old(:) => null(), corep_ets(:) => null()
  type(type_coretransp), pointer :: coret_in(:), coret_bohmgb(:) => null()
  type(type_coresource), pointer :: cores_in(:)
  type(type_coreimpur), pointer :: corei_in(:)
  type(type_equilibrium), pointer :: equil_in(:), equil_update(:) => null(), equil_chease(:) => null()
  type(type_toroidfield), pointer :: toroidf_in(:)
  integer :: ios, it
  character(4)  :: itstr

  !... Read initial CPOs
  allocate(corep_in(1))
  allocate(coret_in(1))
  allocate(cores_in(1))
  allocate(corei_in(1))
  allocate(equil_in(1))
  allocate(toroidf_in(1))

  open (unit = 10, file = corep_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (10)
     call open_read_file(10, corep_file_in)
     call read_cpo(corep_in(1), 'coreprof' )
     call close_read_file
  end if
  open (unit = 11, file = coret_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (11)
     call open_read_file(10, coret_file_in)
     call read_cpo(coret_in(1), 'coretransp' )
     call close_read_file
  end if
  open (unit = 12, file = cores_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (12)
     call open_read_file(12, cores_file_in)
     call read_cpo(cores_in(1), 'coresource' )
     call close_read_file
  end if
  open (unit = 13, file = corei_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (13)
     call open_read_file(13, corei_file_in)
     call read_cpo(corei_in(1), 'coreimpur' )
     call close_read_file
  end if
  open (unit = 14, file = equil_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (14)
     call open_read_file(14, equil_file_in)
     call read_cpo(equil_in(1), 'equilibrium' )
     call close_read_file
  end if
  open (unit = 15, file = toroidf_file_in, &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)
  if (ios == 0) then
     close (15)
     call open_read_file(15, toroidf_file_in)
     call read_cpo(toroidf_in(1), 'toroidfield' )
     call close_read_file
  end if


  ! Loop
  allocate(corep_old(1))
  allocate(corep_ets(1))
  allocate(equil_chease(1))
  allocate(coret_bohmgb(1))
  call copy_cpo(corep_in(1),corep_ets(1))
  call copy_cpo(equil_in(1),equil_chease(1))
  call copy_cpo(coret_in(1),coret_bohmgb(1))

  do it=1,STEPS

     write(itstr,'(I4.4)') it
     print *,"**** iteration = "//itstr//" ****"

     ! ETS
     call copy_cpo(corep_ets(1),corep_old(1))
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
        call open_write_file(21, 'equilupdate_equilibrium_'//itstr//'.cpo')
        call write_cpo(equil_update(1),'equilibrium')
        call close_write_file
     end if

     ! CHEASE
     call deallocate_cpo(equil_chease)
     nullify(equil_chease)
     call chease_cpo(equil_update, equil_chease)
     if (TIMETRACE) then
        call open_write_file(22, 'chease_equilibrium_'//itstr//'.cpo')
        call write_cpo(equil_chease(1),'equilibrium')
        call close_write_file
     end if

     ! BOHMGB
     call deallocate_cpo(coret_bohmgb)
     nullify(coret_bohmgb)
     call bohmgb_cpo(equil_chease, corep_ets, coret_bohmgb)
     if (TIMETRACE) then
        call open_write_file(23, 'bohmgb_coretransp_'//itstr//'.cpo')
        call write_cpo(coret_bohmgb(1),'coretransp')
        call close_write_file
     end if

  end do

  if (.not.TIMETRACE) then
     call open_write_file(20, 'ets_coreprof_out.cpo')
     call write_cpo(corep_ets(1),'coreprof')
     call close_write_file
  end if

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

end program loop_test1
