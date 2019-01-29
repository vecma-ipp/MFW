Module Auxiliaries

CONTAINS

SUBROUTINE prof_init(eq, coreprof)

  USE Phys_constants
  USE Euitm_schemas

  IMPLICIT NONE

  TYPE (type_equilibrium) :: eq
  TYPE (type_coreprof) :: coreprof

  INTEGER(ITM_I4) :: i
  INTEGER(ITM_I4) :: nrho_prof = 20, nion = 1, npsi_eq = 33, neta_eq = 64

  REAL(R8), DIMENSION(:), POINTER :: eta,ra,psi,qq,jphi,nne,tte

  REAL(R8) :: r_minor=0.5_R8, r_major=1.65_R8, mag_field=-2.5_R8, &
       q_axis = -1.5_R8, j_axis, psi_axis

!...  load profiles

  ALLOCATE(coreprof%composition%amn(nion))
  ALLOCATE(coreprof%composition%zion(nion))

  ALLOCATE(coreprof%rho_tor_norm(nrho_prof))
  ALLOCATE(coreprof%rho_tor(nrho_prof))
  ALLOCATE(coreprof%psi%value(nrho_prof))
  ALLOCATE(coreprof%drho_dt(nrho_prof))

  ALLOCATE(coreprof%ne%value(nrho_prof))
  ALLOCATE(coreprof%te%value(nrho_prof))
  ALLOCATE(coreprof%ni%value(nrho_prof,nion))
  ALLOCATE(coreprof%ti%value(nrho_prof,nion))

  ALLOCATE(coreprof%profiles1d%q%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%zeff%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%wtor%value(nrho_prof,nion))
  ALLOCATE(coreprof%profiles1d%pe%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%pi%value(nrho_prof,nion))
  ALLOCATE(coreprof%profiles1d%pr_th%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%jtot%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%jni%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%joh%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%qoh%value(nrho_prof))
  ALLOCATE(coreprof%profiles1d%sigmapar%value(nrho_prof))

!...  set the global parameters

  coreprof%toroid_field%b0 = mag_field
  coreprof%toroid_field%r0 = r_major

  coreprof%composition%amn = 1.0_R8
  coreprof%composition%zion = 1.0_R8

  eq%global_param%toroid_field%b0 = mag_field
  eq%global_param%toroid_field%r0 = r_major

  eq%global_param%mag_axis%q = q_axis

  eq%eqgeometry%geom_axis%r = r_major
  eq%eqgeometry%geom_axis%z = 0._R8
  eq%eqgeometry%a_minor = r_minor
  eq%eqgeometry%elongation = 1.0_R8
  eq%eqgeometry%tria_upper = 0._R8
  eq%eqgeometry%tria_lower = 0._R8

!...  load equil

  ALLOCATE(eq%codeparam%codename(1))
  eq%codeparam%codename(1) = "wrapper input for BDSEQ"

  ALLOCATE(eq%profiles_1d%psi(npsi_eq))
  ALLOCATE(eq%profiles_1d%phi(npsi_eq))
  ALLOCATE(eq%profiles_1d%rho_tor(npsi_eq))
  ALLOCATE(eq%profiles_1d%rho_vol(npsi_eq))
  ALLOCATE(eq%profiles_1d%pressure(npsi_eq))
  ALLOCATE(eq%profiles_1d%jparallel(npsi_eq))
  ALLOCATE(eq%profiles_1d%jphi(npsi_eq))
  ALLOCATE(eq%profiles_1d%pprime(npsi_eq))
  ALLOCATE(eq%profiles_1d%ffprime(npsi_eq))
  ALLOCATE(eq%profiles_1d%F_dia(npsi_eq))
  ALLOCATE(eq%profiles_1d%q(npsi_eq))

  eq%profiles_1d%phi=0._R8
  eq%profiles_1d%psi=0._R8
  eq%profiles_1d%rho_tor=0._R8
  eq%profiles_1d%rho_vol=0._R8
  eq%profiles_1d%jphi=0._R8
  eq%profiles_1d%jparallel=0._R8
  eq%profiles_1d%pressure=0._R8
  eq%profiles_1d%q=0._R8
  eq%profiles_1d%pprime=0._R8
  eq%profiles_1d%ffprime=0._R8
  eq%profiles_1d%F_dia=0._R8

!...  eq grid

  ALLOCATE(eq%coord_sys%grid_type(1))
  ALLOCATE(eq%coord_sys%grid%dim1(npsi_eq))
  ALLOCATE(eq%coord_sys%grid%dim2(neta_eq))
  eq%coord_sys%grid_type = "unit rho theta cyl grid for input"
  eq%coord_sys%grid%dim1 = &
       (/ ((1.0_R8/(npsi_eq-1))*REAL(i-1),i=1,npsi_eq) /)
  eq%coord_sys%grid%dim2 = &
       (/ ((tpi/neta_eq)*REAL(i-1),i=1,neta_eq) /)

  ra => eq%coord_sys%grid%dim1
  eta => eq%coord_sys%grid%dim2

!...  plasma boundary

  ALLOCATE(eq%eqgeometry%boundary(1))
  ALLOCATE(eq%eqgeometry%boundary(1)%r(neta_eq))
  ALLOCATE(eq%eqgeometry%boundary(1)%z(neta_eq))

  eq%eqgeometry%boundary(1)%r = &
       eq%eqgeometry%geom_axis%r + &
       eq%eqgeometry%a_minor*COS(eta)
  eq%eqgeometry%boundary(1)%z = &
       eq%eqgeometry%geom_axis%z + &
       eq%eqgeometry%a_minor*SIN(eta)

!...  eq profiles

  qq => eq%profiles_1d%q
  psi => eq%profiles_1d%psi
  jphi => eq%profiles_1d%jphi

  eq%profiles_1d%rho_tor=eq%eqgeometry%a_minor * ra
  eq%profiles_1d%rho_vol = ra

  j_axis = - 2.0_R8*mag_field/(mu_0*r_major*q_axis)
  psi_axis = (pi*r_minor*r_minor)*mag_field/q_axis

  qq=q_axis*EXP(ra*ra)
  jphi = j_axis*EXP(-ra*ra)*(1.0_R8-ra*ra)
  psi = psi_axis*(1.0_R8-EXP(-ra*ra))

  eq%profiles_1d%jparallel=eq%profiles_1d%jphi

  eq%profiles_1d%F_dia=r_major*mag_field

  eq%profiles_1d%phi=pi*mag_field*eq%profiles_1d%rho_tor*eq%profiles_1d%rho_tor

!...  coreprof grid and profiles

  coreprof%rho_tor_norm=(/ ((1.0_R8/nrho_prof)*(i-0.5),i=1,nrho_prof) /)
  coreprof%rho_tor=coreprof%rho_tor_norm * eq%eqgeometry%a_minor

  ra => coreprof%rho_tor_norm
  nne => coreprof%ne%value
  tte => coreprof%te%value

  coreprof%ne%value=5.0e19_R8*EXP(-8.0_R8*ra*ra/7.0_R8)
  coreprof%te%value=2.0e3_R8*EXP(-8.0_R8*ra*ra/3.0_R8)
  coreprof%ni%value(:,1)=coreprof%ne%value
  coreprof%ti%value(:,1)=coreprof%te%value
  coreprof%profiles1d%zeff%value = 2.0_R8
  coreprof%profiles1d%wtor%value=0.0_R8
  coreprof%time=0.0_R8

  coreprof%profiles1d%pe%value=coreprof%ne%value*kb*coreprof%te%value
  coreprof%profiles1d%pi%value=coreprof%ni%value*kb*coreprof%ti%value
  coreprof%profiles1d%pr_th%value=coreprof%profiles1d%pe%value &
       + coreprof%profiles1d%pi%value(:,1)

!...  put q and psi and J onto coreprof grid

  CALL L3interp( eq%profiles_1d%q, eq%profiles_1d%rho_tor, npsi_eq, &
       coreprof%profiles1d%q%value, coreprof%rho_tor, nrho_prof)
  CALL L3interp( eq%profiles_1d%psi, eq%profiles_1d%rho_tor, npsi_eq, &
       coreprof%psi%value, coreprof%rho_tor, nrho_prof)
  CALL L3interp( eq%profiles_1d%jphi, eq%profiles_1d%rho_tor, npsi_eq, &
       coreprof%profiles1d%jtot%value, coreprof%rho_tor, nrho_prof)

  coreprof%profiles1d%jni%value=0.0_R8
  coreprof%profiles1d%joh%value=coreprof%profiles1d%jtot%value

!...  put p onto equil grid and get derivs

  CALL L3interp( coreprof%profiles1d%pr_th%value, &
       coreprof%rho_tor, nrho_prof, &
       eq%profiles_1d%pressure, eq%profiles_1d%rho_tor, npsi_eq)

  CALL L3deriv( eq%profiles_1d%pressure, eq%profiles_1d%psi, npsi_eq, &
       eq%profiles_1d%pprime, eq%profiles_1d%psi, npsi_eq)

  eq%profiles_1d%ffprime = mu_0*r_major*(jphi/tpi - r_major* &
	eq%profiles_1d%pprime)

!...  set loop voltage

  coreprof%globalparam%vloop = 0._R8

!...  stamp the time

  coreprof%time=0.0_R8

  eq%time=0.0_R8

END SUBROUTINE prof_init
END Module Auxiliaries


PROGRAM BDSEQ_Wrapper

  !...  uses either simple input or a CPO file called IMP4Init to wrap BDSEQ
  !...  simple input if the file is absent

#ifdef MPI
  USE mpi
#endif

  USE ITM_Constants
  USE Euitm_Schemas
  USE Read_structures

  USE Auxiliaries

  IMPLICIT NONE

  INTEGER :: ios

  TYPE (type_equilibrium), pointer :: eq_in(:), eq(:)
  TYPE (type_coreprof), POINTER :: coreprof(:)
  TYPE (type_param) :: code_parameters

  interface
     subroutine bdseq(eq_in, eq, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq_in(:), eq(:)
       type (type_param) :: code_parameters
     end subroutine bdseq
  end interface

!...  this gets a copy of the code onto every core

#ifdef MPI
  INTEGER :: ierr
  CALL MPI_Init(ierr)
#endif

!...  set initial state

  ALLOCATE (eq_in(1))
  ALLOCATE (coreprof(1))

  OPEN (unit = 10, file = 'IMP4Init', &
       status = 'old', form = 'formatted', &
       action = 'read', iostat = ios)

  IF (ios == 0) THEN

     CLOSE (10)

     call open_read_file(10, 'IMP4Init' )
     call read_cpo(coreprof(1), 'IMP4InitCoreprof' )
     call read_cpo(eq_in(1), 'IMP4InitEquil' )
     call close_read_file

  ELSE

     call prof_init(eq_in(1),coreprof(1))

  END IF

!...  run BDSEQ

  CALL Get_Code_Parms(code_parameters, 'bdseq.xml', '', 'bdseq.xsd')
  CALL BDSEQ(eq_in, eq, code_parameters)

!...  write the results

#ifdef MPI
  CALL MPI_Finalize(ierr)
#endif

  STOP
END PROGRAM BDSEQ_Wrapper


