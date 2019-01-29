subroutine load_itm(equil_in,kitmopt,kitmshot,kitmrun,citmtree)
  !
  use globals
  use euITM_schemas                       ! module containing the equilibrium type definitions
  use euITM_routines
  IMPLICIT NONE
  type(type_equilibrium),pointer      :: equil_in(:) => null()
  character*120  :: citmtree
  integer        :: kitmopt, kitmshot, kitmrun
  integer        :: idx, i
  !
  character*11  :: signal_name ='equilibrium'
  !  character*13  :: signal_name ='equilibrium/3'
  character*5   :: treename
  !
  treename = trim(citmtree)
  !
  print *,'signal_name= ',signal_name
  print *,'treename= ',treename
  print *,'kitmshot,kitmrun= ',kitmshot,kitmrun
  call euitm_open(citmtree,kitmshot,kitmrun,idx)
  call euitm_get(idx,signal_name,equil_in)
  call euitm_close(idx)
  !
  if (associated(equil_in(1)%datainfo%comment)) then
     do i=1,size(equil_in(1)%datainfo%comment)
        write(6,'(A)') trim(equil_in(1)%datainfo%comment(i))
     end do
  end if

  ! print *,' equil_in(1)%global_param%volume= ',equil_in(1)%global_param%volume
  ! print *,' equil_in(1)%eqgeometry%geom_axis%r= ',equil_in(1)%eqgeometry%geom_axis%r
  ! print *,' equil_in(1)%eqgeometry%geom_axis%z= ',equil_in(1)%eqgeometry%geom_axis%z
  ! print *,' size(equil_in)= ',size(equil_in)
  print *,' equil_in(1)%time= ',equil_in(1)%time
  ! print *,' equil_in(1)%profiles_1d%pprime= ',equil_in(1)%profiles_1d%pprime

  return
end subroutine load_itm
