SUBROUTINE EqUpdate (coreprof, eq_in, eq)

!...  strictly takes profiles from coreprof and boundary info from eq_in
!...  eq is refilled not copied
!...  only profiles used are allocated
!...  deallocation of eq_in done by wrapper

  USE phys_constants
  USE euitm_schemas
  USE copy_structures
  USE deallocate_structures

!...  fill 1D profiles in eq from those in coreprof
!...  geometric parameters remain as already set

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq_in(:), eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:)

  INTEGER(ITM_I4) :: nrho,neta, nion,ion
  INTEGER(ITM_I4) :: i,j,eflag = 0
  REAL(R8) :: a00,b00,r00

  REAL(R8), DIMENSION(:), POINTER :: rtor, ra, pressure, jphi, jpar
  REAL(R8), DIMENSION(:), POINTER :: rho, nne, nni, tte, tti, jtot, jtor
  REAL(R8), DIMENSION(:), POINTER :: q_prof,psi_prof, qq, psi

  LOGICAL :: first_flag = .true.
  LOGICAL :: write_diags = .true.

!...  find radial grid sizes for profiles and boundary surface

  IF (.NOT.ASSOCIATED(coreprof(1)%rho_tor_norm)) THEN
     nrho=SIZE(coreprof(1)%rho_tor)
     ALLOCATE (coreprof(1)%rho_tor_norm(nrho))
     coreprof(1)%rho_tor_norm= &
          coreprof(1)%rho_tor/coreprof(1)%rho_tor(nrho)
  END IF

  nrho=SIZE(coreprof(1)%rho_tor_norm)
  IF (nrho < 1) THEN
     write (0,*) 'you need to fill coreprof rho tor norm'
     STOP
  END IF
  nion=SIZE(coreprof(1)%ni%value, dim=2)
  neta=SIZE(eq_in(1)%eqgeometry%boundary)
  IF (neta < 1) THEN
     write (0,*) 'you need a boundary...  quitting...'
     STOP
  END IF
  neta=SIZE(eq_in(1)%eqgeometry%boundary(1)%r)
  IF (neta < 32) THEN
     write (0,*) 'you need a better boundary...  quitting...'
     STOP
  END IF

  IF (write_diags) WRITE (0,*) 'start with nr of boundary points = ',neta

  eflag = 0

  DO j=2,neta
     IF (eq_in(1)%eqgeometry%boundary(1)%r(j) == &
          eq_in(1)%eqgeometry%boundary(1)%r(j-1) .AND. &
          eq_in(1)%eqgeometry%boundary(1)%z(j) == &
          eq_in(1)%eqgeometry%boundary(1)%z(j-1)) THEN
        IF (eflag == 0) CYCLE
        IF (eflag == 1) EXIT
     END IF
     IF (eflag == 0) i=j-1
     eflag = 1
  END DO

  j=MIN(j,neta)

  IF (write_diags) WRITE (0,*) 'end boundary points = ',i,j

  IF (eq_in(1)%eqgeometry%boundary(1)%r(i) == &
       eq_in(1)%eqgeometry%boundary(1)%r(j) .AND. &
       eq_in(1)%eqgeometry%boundary(1)%z(i) == &
       eq_in(1)%eqgeometry%boundary(1)%z(j)) j=j-1

  IF (write_diags) WRITE (0,*) 'end boundary points = ',i,j

  neta=j-i+1

  IF (write_diags) WRITE (0,*) 'nr of boundary points = ',neta

!...  allocations

!  IF (first_flag) THEN
     allocate(eq(1))

     allocate(eq(1)%eqgeometry%boundary(1))
     allocate(eq(1)%eqgeometry%boundary(1)%r(neta))
     allocate(eq(1)%eqgeometry%boundary(1)%z(neta))

     ALLOCATE(eq(1)%profiles_1d%pressure(nrho))
     ALLOCATE(eq(1)%profiles_1d%jphi(nrho))
     ALLOCATE(eq(1)%profiles_1d%jparallel(nrho))
     ALLOCATE(eq(1)%profiles_1d%rho_tor(nrho))
     ALLOCATE(eq(1)%profiles_1d%rho_vol(nrho))
     ALLOCATE(eq(1)%profiles_1d%F_dia(nrho))

     eq(1)%profiles_1d%pressure=0._R8
     eq(1)%profiles_1d%jphi=0._R8
     eq(1)%profiles_1d%jparallel=0._R8
     eq(1)%profiles_1d%rho_tor=0._R8
     eq(1)%profiles_1d%rho_vol=0._R8

     first_flag = .false.
!  END IF

!...  find and copy parameters and boundary surface

  a00=(MAXVAL(eq_in(1)%eqgeometry%boundary(1)%r)-MINVAL(eq_in(1)%eqgeometry%boundary(1)%r))/2.0_R8
  r00=(MAXVAL(eq_in(1)%eqgeometry%boundary(1)%r)+MINVAL(eq_in(1)%eqgeometry%boundary(1)%r))/2.0_R8

  IF (itm_is_valid_real8(coreprof(1)%toroid_field%b0)) THEN
     b00=coreprof(1)%toroid_field%b0
  ELSE
     WRITE (0,*) 'ERROR: specify B0 in coreprof'
     STOP
  END IF

  eq(1)%eqgeometry%a_minor=a00
  eq(1)%global_param%toroid_field%b0=b00
  eq(1)%global_param%toroid_field%r0=r00

  eq(1)%eqgeometry%boundary(1)%r(1:neta) = &
       eq_in(1)%eqgeometry%boundary(1)%r(i:j)
  eq(1)%eqgeometry%boundary(1)%z(1:neta) = &
       eq_in(1)%eqgeometry%boundary(1)%z(i:j)

!...  associations

  rho => coreprof(1)%rho_tor_norm
  nne => coreprof(1)%ne%value
  tte => coreprof(1)%te%value
  jtor => coreprof(1)%profiles1d%jphi%value
  jtot => coreprof(1)%profiles1d%jtot%value

  rtor => eq(1)%profiles_1d%rho_tor
  ra => eq(1)%profiles_1d%rho_vol
  pressure => eq(1)%profiles_1d%pressure
  jphi => eq(1)%profiles_1d%jphi
  jpar => eq(1)%profiles_1d%jparallel

!...  put coreprof values onto equilibrium 
!...  get p (pressure), then the current (jphi)

  ra=rho
  rtor=coreprof(1)%rho_tor

  pressure=nne*kb*tte
  DO ion=1,nion
     nni => coreprof(1)%ni%value(:,ion)
     tti => coreprof(1)%ti%value(:,ion)
     pressure=pressure + nni*kb*tti
  END DO

  IF (.NOT.ASSOCIATED(jtor) .AND. .NOT.ASSOCIATED(jtot)) THEN
     WRITE (0,*) 'ERROR: you must fill one of jtot or jphi in coreprof!'
     STOP
  END IF

  IF (ASSOCIATED(jtor)) THEN
     jphi = jtor
  ELSE
     jphi = jtot
  END IF

  IF(ASSOCIATED(jtot)) THEN
     jpar = jtot
  ELSE
     jpar = jtor
  END IF

  eq(1)%profiles_1d%F_dia = b00*r00

  q_prof => coreprof(1)%profiles1d%q%value
  psi_prof => coreprof(1)%psi%value

  IF(ASSOCIATED(q_prof)) THEN
     ALLOCATE(eq(1)%profiles_1d%q(nrho))
     eq(1)%profiles_1d%q=q_prof
  END IF

  IF(ASSOCIATED(psi_prof)) THEN
     ALLOCATE(eq(1)%profiles_1d%psi(nrho))
     eq(1)%profiles_1d%psi=psi_prof
  END IF

!...  stamp time, ready for equilibrium

  eq(1)%time=coreprof(1)%time

END SUBROUTINE EqUpdate
