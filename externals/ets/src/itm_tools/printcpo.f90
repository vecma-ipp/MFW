program printcom
!***********************************************************
! This module has been automatically generated from the
! ITM schemas.
! Author: Edmondo Giovannozzi (2011) ENEA                      
! The software is released under the ITM License.
!***********************************************************

  use itm_types
  use euitm_schemas
  use euitm_routines
  use euitm_printcpo

  implicit none

  integer(itm_i4) :: shot, run, idx, i
  character(len = 5)::treename
  logical :: pscreen
  character(len = 40) form

  type(type_amns) :: amns
  type(type_antennas), pointer :: antennas(:) => null()
  type(type_coredelta), pointer :: coredelta(:) => null()
  type(type_coreimpur), pointer :: coreimpur(:) => null()
  type(type_coreneutrals), pointer :: coreneutrals(:) => null()
  type(type_coreprof), pointer :: coreprof(:) => null()
  type(type_coresource), pointer :: coresource(:) => null()
  type(type_coretransp), pointer :: coretransp(:) => null()
  type(type_cxdiag), pointer :: cxdiag(:) => null()
  type(type_distribution), pointer :: distribution(:) => null()
  type(type_distsource), pointer :: distsource(:) => null()
  type(type_ecediag), pointer :: ecediag(:) => null()
  type(type_edge), pointer :: edge(:) => null()
  type(type_equilibrium), pointer :: equilibrium(:) => null()
  type(type_fusiondiag), pointer :: fusiondiag(:) => null()
  type(type_interfdiag), pointer :: interfdiag(:) => null()
  type(type_ironmodel), pointer :: ironmodel(:) => null()
  type(type_langmuirdiag), pointer :: langmuirdiag(:) => null()
  type(type_launchs), pointer :: launchs(:) => null()
  type(type_limiter) :: limiter
  type(type_magdiag), pointer :: magdiag(:) => null()
  type(type_mhd), pointer :: mhd(:) => null()
  type(type_msediag), pointer :: msediag(:) => null()
  type(type_nbi), pointer :: nbi(:) => null()
  type(type_neoclassic), pointer :: neoclassic(:) => null()
  type(type_orbit), pointer :: orbit(:) => null()
  type(type_pfsystems), pointer :: pfsystems(:) => null()
  type(type_polardiag), pointer :: polardiag(:) => null()
  type(type_reference), pointer :: reference(:) => null()
  type(type_sawteeth), pointer :: sawteeth(:) => null()
  type(type_scenario), pointer :: scenario(:) => null()
  type(type_summary) :: summary
  type(type_topinfo) :: topinfo
  type(type_toroidfield), pointer :: toroidfield(:) => null()
  type(type_tsdiag), pointer :: tsdiag(:) => null()
  type(type_turbulence), pointer :: turbulence(:) => null()
  type(type_vessel) :: vessel
  type(type_waves), pointer :: waves(:) => null()

! shot = 3 ! Your choice
! run = 2  ! Your choice
! treename = 'euitm'   ! Mandatory
! xmloutput is defined in euITM_printcpo (not a very good design: so, pay attention)
  call printcpoargs(shot, run, treename, pscreen, xmloutput)

  if (xmloutput) then 
    print '(A)', '<?xml version="1.0" encoding="utf-8"?>'
    print '(A)', '<!--'
  end if
  write(*, *) 'Open shot in MDS !'
  call euitm_open(treename, shot, run, idx)
  write(*, *) 'Value of the idx:', idx
  write(*, *) 'Reading the results :'

  call euitm_get(idx, 'amns', amns)
  call euitm_get(idx, 'antennas', antennas)
  call euitm_get(idx, 'coredelta', coredelta)
  call euitm_get(idx, 'coreimpur', coreimpur)
  call euitm_get(idx, 'coreneutrals', coreneutrals)
  call euitm_get(idx, 'coreprof', coreprof)
  call euitm_get(idx, 'coresource', coresource)
  call euitm_get(idx, 'coretransp', coretransp)
  call euitm_get(idx, 'cxdiag', cxdiag)
  call euitm_get(idx, 'distribution', distribution)
  call euitm_get(idx, 'distsource', distsource)
  call euitm_get(idx, 'ecediag', ecediag)
  call euitm_get(idx, 'edge', edge)
  call euitm_get(idx, 'equilibrium', equilibrium)
  call euitm_get(idx, 'fusiondiag', fusiondiag)
  call euitm_get(idx, 'interfdiag', interfdiag)
  call euitm_get(idx, 'ironmodel', ironmodel)
  call euitm_get(idx, 'langmuirdiag', langmuirdiag)
  call euitm_get(idx, 'launchs', launchs)
  call euitm_get(idx, 'limiter', limiter)
  call euitm_get(idx, 'magdiag', magdiag)
  call euitm_get(idx, 'mhd', mhd)
  call euitm_get(idx, 'msediag', msediag)
  call euitm_get(idx, 'nbi', nbi)
  call euitm_get(idx, 'neoclassic', neoclassic)
  call euitm_get(idx, 'orbit', orbit)
  call euitm_get(idx, 'pfsystems', pfsystems)
  call euitm_get(idx, 'polardiag', polardiag)
  call euitm_get(idx, 'reference', reference)
  call euitm_get(idx, 'sawteeth', sawteeth)
  call euitm_get(idx, 'scenario', scenario)
  call euitm_get(idx, 'summary', summary)
  call euitm_get(idx, 'topinfo', topinfo)
  call euitm_get(idx, 'toroidfield', toroidfield)
  call euitm_get(idx, 'tsdiag', tsdiag)
  call euitm_get(idx, 'turbulence', turbulence)
  call euitm_get(idx, 'vessel', vessel)
  call euitm_get(idx, 'waves', waves)

  print *
  if (xmloutput) then 
    print '(A)', '-->'
    print '(A)', '<top>'
  end if

  call printcpo(amns)
  call printcpo(antennas)
  call printcpo(coredelta)
  call printcpo(coreimpur)
  call printcpo(coreneutrals)
  call printcpo(coreprof)
  call printcpo(coresource)
  call printcpo(coretransp)
  call printcpo(cxdiag)
  call printcpo(distribution)
  call printcpo(distsource)
  call printcpo(ecediag)
  call printcpo(edge)
  call printcpo(equilibrium)
  call printcpo(fusiondiag)
  call printcpo(interfdiag)
  call printcpo(ironmodel)
  call printcpo(langmuirdiag)
  call printcpo(launchs)
  call printcpo(limiter)
  call printcpo(magdiag)
  call printcpo(mhd)
  call printcpo(msediag)
  call printcpo(nbi)
  call printcpo(neoclassic)
  call printcpo(orbit)
  call printcpo(pfsystems)
  call printcpo(polardiag)
  call printcpo(reference)
  call printcpo(sawteeth)
  call printcpo(scenario)
  call printcpo(summary)
  call printcpo(topinfo)
  call printcpo(toroidfield)
  call printcpo(tsdiag)
  call printcpo(turbulence)
  call printcpo(vessel)
  call printcpo(waves)
  call euitm_deallocate(amns)
  call euitm_deallocate(antennas)
  call euitm_deallocate(coredelta)
  call euitm_deallocate(coreimpur)
  call euitm_deallocate(coreneutrals)
  call euitm_deallocate(coreprof)
  call euitm_deallocate(coresource)
  call euitm_deallocate(coretransp)
  call euitm_deallocate(cxdiag)
  call euitm_deallocate(distribution)
  call euitm_deallocate(distsource)
  call euitm_deallocate(ecediag)
  call euitm_deallocate(edge)
  call euitm_deallocate(equilibrium)
  call euitm_deallocate(fusiondiag)
  call euitm_deallocate(interfdiag)
  call euitm_deallocate(ironmodel)
  call euitm_deallocate(langmuirdiag)
  call euitm_deallocate(launchs)
  call euitm_deallocate(limiter)
  call euitm_deallocate(magdiag)
  call euitm_deallocate(mhd)
  call euitm_deallocate(msediag)
  call euitm_deallocate(nbi)
  call euitm_deallocate(neoclassic)
  call euitm_deallocate(orbit)
  call euitm_deallocate(pfsystems)
  call euitm_deallocate(polardiag)
  call euitm_deallocate(reference)
  call euitm_deallocate(sawteeth)
  call euitm_deallocate(scenario)
  call euitm_deallocate(summary)
  call euitm_deallocate(topinfo)
  call euitm_deallocate(toroidfield)
  call euitm_deallocate(tsdiag)
  call euitm_deallocate(turbulence)
  call euitm_deallocate(vessel)
  call euitm_deallocate(waves)

  if (xmloutput) then 
    print '(A)', '</top>'
  end if

end program

