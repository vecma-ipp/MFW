subroutine write_itm(equil_out,kitmopt,kitmshot,kitmrun,citmtree)
  !
  use globals
  use euITM_schemas                       ! module containing the equilibrium type definitions
  IMPLICIT NONE
  type(type_equilibrium),pointer      :: equil_out(:)
  character*120  :: citmtree
  integer        :: kitmopt, kitmshot, kitmrun
  !
  print *,'This routine should be linked to write_itm.f90 when CHEASE is compiled without libraries for ITM structure'
  print *,'It is not supposed to be called.'
  print *,'NITMOPT should always be -1 in such cases and routines load_itm and write_itm are not called'
  stop 'write_itm'

  return
end subroutine write_itm
