SUBROUTINE EqPostdate (eq, coreprof_in, coreprof)

!...  updates grid and geometry in coreprof from eq

  USE phys_constants
  USE l3interps
  USE euitm_schemas
  USE copy_structures

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:), coreprof_in(:)

  INTEGER(ITM_I4) :: nrho,npsi
  REAL(R8) :: rho_tor_max

!...  copy coreprof over

  ALLOCATE(coreprof(1))
  call copy_cpo(coreprof_in(1), coreprof(1))

!...  find radial grid sizes for profiles and boundary surface

  IF (.NOT.ASSOCIATED(coreprof(1)%rho_tor_norm)) THEN
     nrho=SIZE(coreprof(1)%rho_tor)
     ALLOCATE (coreprof(1)%rho_tor_norm(nrho))
     coreprof(1)%rho_tor_norm= &
          coreprof(1)%rho_tor/coreprof(1)%rho_tor(nrho)
  END IF

  npsi=SIZE(eq(1)%profiles_1d%rho_vol)
  nrho=SIZE(coreprof(1)%rho_tor_norm)
  IF (nrho < 1) THEN
     write (0,*) 'you need to fill coreprof rho tor norm'
     STOP
  END IF

!...  allocations

  IF (.NOT.ASSOCIATED(coreprof(1)%rho_tor)) &
       ALLOCATE(coreprof(1)%rho_tor(nrho))
  IF (.NOT.ASSOCIATED(coreprof(1)%psi%value)) &
       ALLOCATE(coreprof(1)%psi%value(nrho))
  IF (.NOT.ASSOCIATED(coreprof(1)%profiles1d%q%value)) &
       ALLOCATE(coreprof(1)%profiles1d%q%value(nrho))

!...  copy parameters 

  coreprof(1)%toroid_field%b0=eq(1)%global_param%toroid_field%b0
  coreprof(1)%toroid_field%r0=eq(1)%global_param%toroid_field%r0

!...  put in psi and q and rho tor

  CALL L3interp( eq(1)%profiles_1d%psi, eq(1)%profiles_1d%rho_vol, npsi, &
       coreprof(1)%psi%value, coreprof(1)%rho_tor_norm, nrho )

  CALL L3interp( eq(1)%profiles_1d%q, eq(1)%profiles_1d%rho_vol, npsi, &
       coreprof(1)%profiles1d%q%value, coreprof(1)%rho_tor_norm, nrho )

  CALL L3interp( eq(1)%profiles_1d%rho_tor, eq(1)%profiles_1d%rho_vol, npsi, &
       coreprof(1)%rho_tor, coreprof(1)%rho_tor_norm, nrho )

!...  done, times are already in

END SUBROUTINE EqPostdate
