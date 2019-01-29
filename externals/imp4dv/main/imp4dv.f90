SUBROUTINE IMP4DV(eq, coreprof, coretransp_in, coretransp)

!...  note the ETS uses v_eff times grad rho squared as a convention
!...  so only gm3 = <|grad rho|^2> enters
!...  flux is the given quantity Gamma times V' for <Gamma dot grad V>
!...  in the CPO one finds D and Veff and Gamma
!...  the formula is <Gamma dot grad V> = V' gm3 (n Veff - D dn/drho)
!
!...  we need the equilibrium CPO for the gm3 and coreprof for the n's
!
!...  here we assume the incoming CPO has flux = <Gamma dot grad rho>
!...  so we do not have to deal with the V' to get D and Veff
!
!...  ambipolarity rules: all Ds are equal to De and fluxes determine the Vs

  USE IMP4DV_Coeff

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer ::  eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:)
  TYPE (type_coretransp), pointer :: coretransp_in(:), coretransp(:)

  INTEGER(ITM_I4) :: i,ion

!...  write out input cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'IMP4In' )
  call write_cpo(eq(1), 'equilibrium' )
  call write_cpo(coreprof(1), 'coreprof' )
  call write_cpo(coretransp_in(1), 'coretransp' )
  call close_write_file

  END IF

!...  grid sizes

  npsi = SIZE(eq(1)%profiles_1d%rho_tor)
  nrho_prof = SIZE(coreprof(1)%rho_tor_norm)
  nrho = SIZE(coretransp_in(1)%values(1)%rho_tor_norm)
  nion = SIZE(coretransp_in(1)%values(1)%ni_transp%flux, 2)

  IF (nrho_prof == 0 .OR. MAXVAL(coreprof(1)%rho_tor_norm) < 0.1) THEN
     WRITE (0,*) 'ERROR: please fill and set coreprof rho tor norm'
     STOP
  END IF

  IF (nrho == 0 .OR. MAXVAL(coretransp_in(1)%values(1)%rho_tor_norm) < 0.1) THEN
     WRITE (0,*) 'ERROR: please fill and set coretransp rho tor norm'
     STOP
  END IF

  IF (nion == 0 .OR. .NOT. ASSOCIATED(coretransp_in(1)%values(1)%ne_transp%flux)) THEN
     WRITE (0,*) 'ERROR: please fill and set fluxes'
     STOP
  END IF

!...  initialise output CPO

  allocate(coretransp(1))
  call copy_cpo(coretransp_in(1),coretransp(1))

!...  allocations

  rho_eq => eq(1)%profiles_1d%rho_tor
  rho => coretransp(1)%values(1)%rho_tor_norm
  rho_prof => coreprof(1)%rho_tor_norm

  ALLOCATE(gm3(nrho))

  ALLOCATE(nnix(nrho,0:nion))
  ALLOCATE(ttix(nrho,0:nion))
  ALLOCATE(rlnix(nrho,0:nion))
  ALLOCATE(rltix(nrho,0:nion))
  ALLOCATE(ffix(nrho,0:nion))
  ALLOCATE(ggix(nrho,0:nion))

!...  get geometry

  r00=eq(1)%global_param%toroid_field%r0

  rho_tor_max=MAXVAL(rho_eq)
  rho_eq=rho_eq/rho_tor_max

  IF (ASSOCIATED(eq(1)%profiles_1d%gm3)) THEN
     CALL L3interp( eq(1)%profiles_1d%gm3, rho_eq, npsi, &
          gm3, rho, nrho)
  ELSE
     gm3=1.0
  END IF

!...  get parameters

  CALL L3interp( coreprof(1)%ne%value(:), rho_prof, nrho_prof, &
       nnix(:,0), rho, nrho)
  CALL L3interp( coreprof(1)%te%value(:), rho_prof, nrho_prof, &
       ttix(:,0), rho, nrho)
  CALL L3deriv( coreprof(1)%ne%value(:), rho_prof, nrho_prof, &
       rlnix(:,0), rho, nrho)
  CALL L3deriv( coreprof(1)%te%value(:), rho_prof, nrho_prof, &
       rltix(:,0), rho, nrho)
  ffix(:,0)=coretransp(1)%values(1)%ne_transp%flux(:)
  ggix(:,0)=coretransp(1)%values(1)%te_transp%flux(:)

  DO ion=1,nion
     CALL L3interp( coreprof(1)%ni%value(:,ion), rho_prof, nrho_prof, &
          nnix(:,ion), rho, nrho)
     CALL L3interp( coreprof(1)%ti%value(:,ion), rho_prof, nrho_prof, &
          ttix(:,ion), rho, nrho)
     CALL L3deriv( coreprof(1)%ni%value(:,ion), rho_prof, nrho_prof, &
          rlnix(:,ion), rho, nrho)
     CALL L3deriv( coreprof(1)%ti%value(:,ion), rho_prof, nrho_prof, &
          rltix(:,ion), rho, nrho)
     ffix(:,ion)=coretransp(1)%values(1)%ni_transp%flux(:,ion)
     ggix(:,ion)=coretransp(1)%values(1)%ti_transp%flux(:,ion)
  END DO

!...  assume fluxes get Ds and Vs

  DO i=1,nrho
     DO ion=0,nion

        tti=ttix(i,ion)
        nni=nnix(i,ion)
        rlni=-rlnix(i,ion)/nni
        rlti=-rltix(i,ion)/tti
        ffi=ffix(i,ion)/(nni*gm3(i))
        ggi=ggix(i,ion)/(nni*kb*tti*gm3(i))

        llni=1./MAX(1./r00, ABS(rlni))
        llti=1./MAX(1./r00, ABS(rlti))

        diffi=ABS(ffi)*llni
        chii=ABS(ggi)*llti

        IF (ion == 0) THEN
           diffi = MAX(diffi, 0.2_R8*chii)
           diffe = diffi
        ELSE
           diffi = diffe
        END IF
        chii = MAX(chii, 0.2_R8*diffi)

        vconvi=ffi - diffi*rlni
        yconvi=ggi - chii*rlti

        IF (ion == 0) THEN
           coretransp(1)%values(1)%ne_transp%diff_eff(i,2) = diffi
           coretransp(1)%values(1)%te_transp%diff_eff(i) = chii
           coretransp(1)%values(1)%ne_transp%vconv_eff(i,2) = vconvi
           coretransp(1)%values(1)%te_transp%vconv_eff(i) = yconvi
        ELSE
           coretransp(1)%values(1)%ni_transp%diff_eff(i,ion,2) = diffi
           coretransp(1)%values(1)%ti_transp%diff_eff(i,ion) = chii
           coretransp(1)%values(1)%ni_transp%vconv_eff(i,ion,2) = vconvi
           coretransp(1)%values(1)%ti_transp%vconv_eff(i,ion) = yconvi
        END IF
     END DO
  END DO

!...  time

  coretransp(1)%time = coretransp_in(1)%time

!...  write out output cpos

  IF (write_cpos) THEN

  call open_write_file(12, 'IMP4Out' )
  call write_cpo(coretransp(1), 'Coretransp' )
  call close_write_file

  END IF

!...  clean up

  DEALLOCATE(gm3)

  DEALLOCATE(nnix)
  DEALLOCATE(ttix)
  DEALLOCATE(rlnix)
  DEALLOCATE(rltix)
  DEALLOCATE(ffix)
  DEALLOCATE(ggix)

END SUBROUTINE IMP4DV
