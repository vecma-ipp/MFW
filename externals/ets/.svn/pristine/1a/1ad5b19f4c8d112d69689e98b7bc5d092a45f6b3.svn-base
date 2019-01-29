! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Run helena based on a CPO stored in ascii format
!>
!> \author D. Coster
!>
!> \version "$Id: run_helena.f90 805 2010-08-28 12:21:07Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
program ascii_equilibrium_to_ual

  use itm_types
  USE EUITM_ROUTINES
  use euitm_schemas
  use xml_file_reader
  use read_structures
  use deallocate_structures
  implicit none

  type (type_equilibrium), pointer :: euitm_equilibrium(:)
  logical, save :: first = .true.
  integer, save :: ncall = 0
  character*256 :: arg
  character*32 :: filename, user, tokamak, ual_version
  integer(itm_i4) :: i, iargc
  integer         :: shot, run
  real(R8)        :: time
  integer         :: idx

  if(iargc().lt.6) then
     write(*,*) 'args should be: filename shot run user tokamak ual_version'
     stop 'error: missing arguments'
  endif
  call getarg(1,arg)
  filename=arg
  call getarg(2,arg)
  read(arg,*) shot
  call getarg(3,arg)
  read(arg,*) run
  call getarg(4,arg)
  user=arg
  call getarg(5,arg)
  tokamak=arg
  call getarg(6,arg)
  ual_version=arg

  write(*,*) 'Processing ',trim(filename), shot, run, trim(user), trim(tokamak), trim(ual_version)

  call open_read_file(1, filename)
  allocate(euitm_equilibrium(1))
  call read_cpo(euitm_equilibrium(1), 'equilibrium')
  call close_read_file

  time=0.0

  CALL EUITM_CREATE_ENV ('euitm', shot, run, 0, 0, idx, &
       user, tokamak, ual_version)
  euitm_equilibrium(1)%time = time
  write(*,*) 'euitm_put_non_timed: equilibrium', euitm_equilibrium(1)%time
  call euitm_put_non_timed (idx,"equilibrium",euitm_equilibrium(1))
  write(*,*) 'euitm_put_slice: equilibrium', euitm_equilibrium(1)%time
  call euitm_put_slice     (idx,"equilibrium",euitm_equilibrium(1))

  call deallocate_cpo(euitm_equilibrium)
  
end program ascii_equilibrium_to_ual
