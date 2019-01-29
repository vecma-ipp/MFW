! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Run helena based on a CPO stored in ascii format
!>
!> \author D. Coster
!>
!> \version "$Id: run_helena.f90 805 2010-08-28 12:21:07Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
program run_helena_cpo

  USE EUITM_ROUTINES
  use itm_types
  use euitm_schemas
  use xml_file_reader
  use read_structures
  use write_structures
  use deallocate_structures
  implicit none

  type (type_equilibrium), pointer :: euitm_equilibrium_in(:)
  type (type_equilibrium), pointer :: euitm_equilibrium_out(:)
  type (type_param), save  :: code_parameters
  logical, save :: first = .true.
  integer, save :: ncall = 0
  character*32 :: filename = 'EQ_######'
  integer(itm_i4) :: i, iargc
  integer, save   :: idx
  character*256   :: arg
  integer         :: shot, run_in, run_out
  real(R8)        :: time

  interface
     subroutine helena (euitm_equilibrium_in, euitm_equilibrium_out, in_path, code_parameters)
       use euitm_schemas
       IMPLICIT NONE
       type (type_equilibrium), pointer :: euitm_equilibrium_in(:)
       type (type_equilibrium), pointer :: euitm_equilibrium_out(:)
       character(len = 132), optional :: in_path
       type (type_param) :: code_parameters
     end SUBROUTINE Helena
  end interface
    
  if(iargc().lt.4) then
     write(*,*) 'shot, run_in, run_out and time missing'
     stop 'error: missing filename'
  endif


  call fill_param(code_parameters, 'XML/helena.xml', '', 'XML/helena.xsd')

  call getarg(1,arg)
  read(arg,*) shot
  call getarg(2,arg)
  read(arg,*) run_in
  call getarg(3,arg)
  read(arg,*) run_out
  call getarg(4,arg)
  read(arg,*) time

  write(*,*) 'Processing ',shot,run_in,run_out,time

  allocate(euitm_equilibrium_in(1))

#ifdef UAL
  CALL EUITM_OPEN ('euitm', shot, run_in, IDX)
  CALL EUITM_GET_SLICE (IDX, 'equilibrium', euitm_equilibrium_in(1), time, 1)
#else
  stop 'Error: no UAL'
#endif

  CALL HELENA(euitm_equilibrium_in, euitm_equilibrium_out, code_parameters=code_parameters)

#ifdef UAL
  CALL EUITM_CREATE ('euitm', shot, run_out, shot, run_in, idx)
  euitm_equilibrium_out(1)%time = time
  write(*,*) 'euitm_put_non_timed: equilibrium', euitm_equilibrium_out(1)%time
  call euitm_put_non_timed (idx,"equilibrium",euitm_equilibrium_out(1))
  write(*,*) 'euitm_put_slice: equilibrium', euitm_equilibrium_out(1)%time
  call euitm_put_slice     (idx,"equilibrium",euitm_equilibrium_out(1))
#else
  stop 'Error: no UAL'
#endif

  filename='EQ_######'
  write(filename(4:9),'(I6.6)') 1
  call open_write_file(1, trim(filename)//'.OUT2')
  call write_cpo(euitm_equilibrium_out(1), 'equilibrium')
  call close_write_file
  
  call deallocate_cpo(euitm_equilibrium_in)
  call deallocate_cpo(euitm_equilibrium_out)
  
end program run_helena_cpo
