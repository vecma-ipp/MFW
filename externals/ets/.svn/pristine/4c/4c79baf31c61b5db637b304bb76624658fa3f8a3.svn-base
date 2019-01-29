PROGRAM Profile_Check

  USE ITM_Types
  USE ITM_Constants
  USE Euitm_schemas

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer ::  eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:)
  TYPE (type_coretransp), pointer :: coretransp(:)

  interface
     subroutine etaigb(eq, coreprof, coretransp)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq(:)
       type (type_coreprof), pointer :: coreprof(:)
       type (type_coretransp), pointer :: coretransp(:)
     end subroutine etaigb
  end interface

  INTEGER(ITM_I4) :: i
  INTEGER(ITM_I4) :: nrho_prof = 20, nion = 1, npsi_eq = 32, neta_eq = 64

  REAL(r8) :: pi=itm_pi
  REAL(r8) :: tpi=2.0_r8*itm_pi

  REAL(r8) :: mu_0 = itm_mu0
  REAL(r8) :: kb=itm_ev
  REAL(r8) :: ee=itm_qe
  REAL(r8) :: cc=1.0_r8, lcoul=14.0_r8

  REAL(r8) :: me=itm_me
  REAL(r8) :: md=(1875.612793_r8/0.510998910_r8)*itm_me

  REAL(R8), DIMENSION(:), POINTER :: eta,ra,psi,qq,jphi

!...  set up CPOs

  allocate(eq(1))
  allocate(coreprof(1))

!...  load profiles

  ALLOCATE(coreprof(1)%rho_tor_norm(nrho_prof))
  ALLOCATE(coreprof(1)%rho_tor(nrho_prof))
  ALLOCATE(coreprof(1)%psi%value(nrho_prof))
  ALLOCATE(coreprof(1)%drho_dt(nrho_prof))

  ALLOCATE(coreprof(1)%ne%value(nrho_prof))
  ALLOCATE(coreprof(1)%te%value(nrho_prof))
  ALLOCATE(coreprof(1)%ni%value(nrho_prof,nion))
  ALLOCATE(coreprof(1)%ti%value(nrho_prof,nion))

  ALLOCATE(coreprof(1)%profiles1d%q%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%zeff%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%wtor%value(nrho_prof,nion))
  ALLOCATE(coreprof(1)%profiles1d%pe%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%pi%value(nrho_prof,nion))
  ALLOCATE(coreprof(1)%profiles1d%pr_th%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%jtot%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%jni%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%joh%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%qoh%value(nrho_prof))
  ALLOCATE(coreprof(1)%profiles1d%sigmapar%value(nrho_prof))

!...  set the global parameters

  coreprof(1)%toroid_field%b0 = 2.0_R8
  coreprof(1)%toroid_field%r0 = 1.65_R8

  eq(1)%global_param%toroid_field%b0 = coreprof(1)%toroid_field%b0
  eq(1)%global_param%toroid_field%r0 = coreprof(1)%toroid_field%r0

  eq(1)%eqgeometry%geom_axis%r = coreprof(1)%toroid_field%r0
  eq(1)%eqgeometry%geom_axis%z = 0._R8
  eq(1)%eqgeometry%a_minor = 0.5_R8
  eq(1)%eqgeometry%elongation = 1.0_R8
  eq(1)%eqgeometry%tria_upper = 0._R8
  eq(1)%eqgeometry%tria_lower = 0._R8

  coreprof(1)%globalparam%vloop = 0._R8

!...  load equil

  ALLOCATE(eq(1)%coord_sys%grid%dim1(npsi_eq))
  eq(1)%coord_sys%grid%dim1 = &
       (/ ((1.0_R8/npsi_eq)*REAL(i-1),i=1,npsi_eq) /)

  ALLOCATE(eq(1)%profiles_1d%psi(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%rho_tor(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%rho_vol(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%pressure(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%jparallel(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%jphi(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%pprime(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%ffprime(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%F_dia(npsi_eq))
  ALLOCATE(eq(1)%profiles_1d%q(npsi_eq))

  eq(1)%profiles_1d%psi=0._R8
  eq(1)%profiles_1d%rho_tor=0._R8
  eq(1)%profiles_1d%rho_vol=0._R8
  eq(1)%profiles_1d%jphi=0._R8
  eq(1)%profiles_1d%jparallel=0._R8
  eq(1)%profiles_1d%pressure=0._R8
  eq(1)%profiles_1d%q=0._R8
  eq(1)%profiles_1d%pprime=0._R8
  eq(1)%profiles_1d%ffprime=0._R8
  eq(1)%profiles_1d%F_dia=0._R8

!...  set plasma boundary

  ALLOCATE(eq(1)%coord_sys%grid%dim2(neta_eq))
  ALLOCATE(eq(1)%eqgeometry%boundary(1))
  ALLOCATE(eq(1)%eqgeometry%boundary(1)%r(neta_eq))
  ALLOCATE(eq(1)%eqgeometry%boundary(1)%z(neta_eq))

  eta => eq(1)%coord_sys%grid%dim2

  eta=(/ ((2.0_R8*pi/neta_eq)*REAL(i-1),i=1,neta_eq) /)

  eq(1)%eqgeometry%boundary(1)%r = &
       eq(1)%eqgeometry%geom_axis%r + &
       eq(1)%eqgeometry%a_minor*COS(eta)
  eq(1)%eqgeometry%boundary(1)%z = &
       eq(1)%eqgeometry%geom_axis%z + &
       eq(1)%eqgeometry%a_minor*SIN(eta)

!...  set profiles

  coreprof(1)%rho_tor_norm=(/ ((1.0_R8/nrho_prof)*(i-0.5),i=1,nrho_prof) /)
  coreprof(1)%rho_tor=coreprof(1)%rho_tor_norm*eq(1)%eqgeometry%a_minor

  ra => coreprof(1)%rho_tor_norm
  psi => coreprof(1)%psi%value
  qq => coreprof(1)%profiles1d%q%value
  jphi => coreprof(1)%profiles1d%jtot%value

  coreprof(1)%ne%value=6.0e19_R8*EXP(-3.0_R8*ra*ra/3.3_R8)
  coreprof(1)%te%value=5.0e3_R8*EXP(-8.0_R8*ra*ra/3.3_R8)
  coreprof(1)%ni%value(:,1)=coreprof(1)%ne%value
  coreprof(1)%ti%value(:,1)=coreprof(1)%te%value
  coreprof(1)%profiles1d%zeff%value = 2.0_R8
  coreprof(1)%profiles1d%q%value = 1.5_R8+2.5_R8*ra*ra
  coreprof(1)%profiles1d%wtor%value=0.0_R8
  coreprof(1)%time=0.0_R8

  coreprof(1)%profiles1d%pe%value=coreprof(1)%ne%value*kb*coreprof(1)%te%value
  coreprof(1)%profiles1d%pi%value=coreprof(1)%ni%value*kb*coreprof(1)%ti%value
  coreprof(1)%profiles1d%pr_th%value=coreprof(1)%profiles1d%pe%value &
       + coreprof(1)%profiles1d%pi%value(:,1)

!... set current and psi from q

  jphi = (2._R8*coreprof(1)%toroid_field%b0/ &
       (mu_0*coreprof(1)%toroid_field%r0))*1.5_R8/(qq*qq)

  psi = (pi*coreprof(1)%toroid_field%b0* &
       eq(1)%eqgeometry%a_minor*eq(1)%eqgeometry%a_minor/ &
       (4.0_R8-1.5_R8)) * LOG(qq/1.5_R8)

  coreprof(1)%profiles1d%jni%value=0.0_R8
  coreprof(1)%profiles1d%joh%value=coreprof(1)%profiles1d%jtot%value

!...  call transport routine

  CALL Etaigb(eq, coreprof, coretransp)

!...  write profiles

  OPEN (10, file = 'prof.dat', form = 'formatted')
  WRITE(10,*) ' i   rho_tor       ne         Te         Ti       D        chi_e      chi_i'
  DO i=1,nrho_prof
     WRITE (10,100) i, &
!.     WRITE (10,*) &
          coreprof(1)%rho_tor(i), &
          coreprof(1)%ne%value(i), &
          coreprof(1)%te%value(i), &
          coreprof(1)%ti%value(i,1), &
          coretransp(1)%ne_transp%diff_eff(i,3), &
          coretransp(1)%te_transp%diff_eff(i), &
          coretransp(1)%ti_transp%diff_eff(i,1)

  END DO
  CLOSE (10)
100 FORMAT (i3,7g11.3)

!...  done

  STOP
END PROGRAM Profile_Check
