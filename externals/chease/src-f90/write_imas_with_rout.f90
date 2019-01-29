subroutine write_imas(equil_out,kitmopt,kitmshot,kitmrun,citmtree)
  !
  use globals
  use ids_schemas                       ! module containing the equilibrium type definitions
  use ids_routines
  IMPLICIT NONE
  type(ids_equilibrium)     :: equil_out
  character*120  :: citmtree
  integer        :: kitmopt, kitmshot, kitmrun
  integer        :: idx, irefrun, i
  !
  character(len=11)  :: signal_name ="Equilibrium"
  character(len=5)  :: citmtree2
  !
  if (kitmshot .eq. 22) then
    print *,' Do not write since shot number to write: ',kitmshot
    return
  end if
  print *,'signal_name= ',signal_name
  !
  !equil_out%datainfo%comment='CHEASE: Reconstruction of \TOP.equilibria.eq.NNN001 in euitm_imp1 tree'
  !equil_out%coord_sys%grid_type='CHEASE: Straight field line coordinates, Power of 2'
  !equil_out%coord_sys%type%dat='CHEASE: Straight field line coordinates'
  !equil_out%coord_sys%type%dat='CHEASE: Equal arc coordinates'
  !equil_out%coord_sys%type%dat='CHEASE: Theta-Psi coordinates'
  
  !  signal_name ='\\TOP.equilibria.eq.NNN006.'
  
  citmtree2 = trim(citmtree)
  ! At this stage, seems to need to do create_pulse in any case, so changed else part, but may be different in future
  if (kitmshot .lt. 0) then
    irefrun = 0 ! should always have refshot=shot and refrun=0?
    kitmshot = abs(kitmshot)
!    call imas_create(citmtree2,kitmshot,kitmrun,kitmshot,irefrun,idx)
    ! print *,'salut32, idx from create= ',idx,' tree= ',citmtree2
  else
    !     call euitm_open(citmtree2,kitmshot,kitmrun,idx)
    !     print *,'salut32, idx from open= ',idx
    irefrun = 0 ! should always have refshot=shot and refrun=0?
    print *,'salut32,  tree= ',citmtree2,' shot= ',kitmshot,' run= ',kitmrun
    call imas_create(citmtree2,kitmshot,kitmrun,kitmshot,irefrun,idx)
     print *,'salut32, idx from create= ',idx,' tree= ',citmtree2
  end if
  ! print *,' equil_out(1)%global_param%volume= ',equil_out(1)%global_param%volume
  ! print *,' equil_out(1)%eqgeometry%geom_axis%r= ',equil_out(1)%eqgeometry%geom_axis%r
  ! print *,' equil_out(1)%eqgeometry%geom_axis%z= ',equil_out(1)%eqgeometry%geom_axis%z
  ! print *,' equil_out(1)%eqgeometry%boundary%r(1:2)= ',equil_out(1)%eqgeometry%boundary%r(1:2)
  ! 
  
  call ids_put(idx,"Equilibrium",equil_out)
  
  call imas_close(idx)
  
  return
end subroutine write_imas
