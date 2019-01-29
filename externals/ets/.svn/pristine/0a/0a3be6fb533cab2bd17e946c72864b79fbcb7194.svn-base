#ifdef DOIMPURE
SUBROUTINE Neowes(eq, coreprof, neoclassic, coreimpurity)
#else
SUBROUTINE Neowes(eq, coreprof, neoclassic)
#endif

!...  neoclassical model wrapper
!...  always use same grid as coreprof
!...  formulae from Wesson Chapter 4

  USE ITM_Constants
  USE Euitm_schemas
  USE copy_structures
  USE deallocate_structures

  IMPLICIT NONE

  TYPE (type_equilibrium), pointer :: eq(:)
  TYPE (type_coreprof), pointer :: coreprof(:)
#ifdef DOIMPURE
  TYPE (type_coreimpur), pointer :: coreimpurity(:)
#endif
  TYPE (type_neoclassic), pointer :: neoclassic(:)

  INTEGER(ITM_I4) :: ion=0,imp=0,icharge=0,iz=0,inucl=0
  INTEGER(ITM_I4) :: nrho_prof,nion_prof,nrho_eq
  INTEGER(ITM_I4) :: nrho_imp = 0
  INTEGER(ITM_I4), SAVE :: &
       nrho = 0, nion = 0, nimp = 0, ncharge = 0, nspecies = 0
  REAL(R8) :: a00,b00,r00,rho_tor_max
  REAL(R8) :: mass,charge
  INTEGER(ITM_I4), DIMENSION(:), POINTER :: ncharges
  REAL(R8), DIMENSION(:), POINTER :: &
       nne,tte,nni,tti,tavg,rlne,rlte,rlni,rlti,qq,zeff,zee, &
       rhoi,v_i,rnue,rnui,eps12,eps32,mneo
  REAL(R8), DIMENSION(:), POINTER :: &
       rho,diffe,diff,chi,vconv,flux,gradn,gradp,eer,sum1,sum2
  REAL(R8), DIMENSION(:,:), POINTER :: nneo,tneo,zneo

  REAL(R8) :: kb=itm_ev
  REAL(R8) :: ee=itm_qe
  REAL(R8) :: cc=1.0_R8, lcoul=14.0_R8
  REAL(R8) :: me=itm_me
  REAL(R8) :: md=itm_md

  character(len = 132), target :: codename(1) = 'NEOWES'
  character(len = 132), target :: codeversion(1) = 'CC_Cyprus12'

!...  find grid size and other stuff from coreprof

  nrho_prof=SIZE(coreprof(1)%rho_tor)
  nion_prof=SIZE(coreprof(1)%ni%value, dim=2)
  nrho_eq=SIZE(eq(1)%profiles_1d%rho_tor)
#ifdef DOIMPURE
  IF (ASSOCIATED(coreimpurity)) THEN
     IF (SIZE(coreimpurity) > 0) THEN
        nrho_imp=SIZE(coreimpurity(1)%rho_tor)
        IF (nrho_imp > 0) THEN
           nimp=SIZE(coreimpurity(1)%impurity)
!           DO imp=1,nimp
!              ncharge=SIZE(coreimpurity(1)%impurity(ion)%nz, dim=2)
!           END DO
        END IF
     END IF
  END IF
#endif

!...  dimensions and species

  nrho = nrho_prof
  nion = nion_prof

  iz = nion

#ifdef DOIMPURE
  IF (nimp > 0) THEN
     ALLOCATE(ncharges(nimp))

     DO imp=1,nimp
        ncharges(imp) = SIZE(coreimpurity(1)%impurity(imp)%nz, 2)
        DO icharge=1,ncharges(imp)
           iz=iz+1
        END DO
     END DO
  END IF
#endif

  nspecies = iz

!  WRITE (0,*) 'ions imps species',nion,nimp,iz

!...  allocations

  IF (.NOT. ASSOCIATED(neoclassic)) THEN
     ALLOCATE(neoclassic(1))
     ALLOCATE(neoclassic(1)%codeparam%codename(1))
     ALLOCATE(neoclassic(1)%codeparam%codeversion(1))
     neoclassic(1)%codeparam%codename = codename
     neoclassic(1)%codeparam%codeversion = codeversion

  ALLOCATE(neoclassic(1)%rho_tor_norm(nrho))
  ALLOCATE(neoclassic(1)%rho_tor(nrho))
  ALLOCATE(neoclassic(1)%sigma(nrho))
  ALLOCATE(neoclassic(1)%jboot(nrho))
  ALLOCATE(neoclassic(1)%er(nrho))
!  ALLOCATE(neoclassic(1)%fext(nrho,nion,nmoment))
!  ALLOCATE(neoclassic(1)%jext(nrho))
  ALLOCATE(neoclassic(1)%ne_neo%flux(nrho))
  ALLOCATE(neoclassic(1)%ni_neo%flux(nrho,nion))
  ALLOCATE(neoclassic(1)%te_neo%flux(nrho))
  ALLOCATE(neoclassic(1)%ti_neo%flux(nrho,nion))
  ALLOCATE(neoclassic(1)%ne_neo%diff_eff(nrho))
  ALLOCATE(neoclassic(1)%ni_neo%diff_eff(nrho,nion))
  ALLOCATE(neoclassic(1)%te_neo%diff_eff(nrho))
  ALLOCATE(neoclassic(1)%ti_neo%diff_eff(nrho,nion))
  ALLOCATE(neoclassic(1)%ne_neo%vconv_eff(nrho))
  ALLOCATE(neoclassic(1)%ni_neo%vconv_eff(nrho,nion))
  ALLOCATE(neoclassic(1)%te_neo%vconv_eff(nrho))
  ALLOCATE(neoclassic(1)%ti_neo%vconv_eff(nrho,nion))
  ALLOCATE(neoclassic(1)%ti_neo%exchange(nrho,nion))
  ALLOCATE(neoclassic(1)%ti_neo%qgi(nrho,nion))

  ALLOCATE(neoclassic(1)%vpol(nrho,nion))
  ALLOCATE(neoclassic(1)%mtor_neo%flux(nrho))
  ALLOCATE(neoclassic(1)%mtor_neo%diff_eff(nrho))
  ALLOCATE(neoclassic(1)%mtor_neo%vconv_eff(nrho))

  neoclassic(1)%ne_neo%flux=0._R8
  neoclassic(1)%ni_neo%flux=0._R8
  neoclassic(1)%te_neo%flux=0._R8
  neoclassic(1)%ti_neo%flux=0._R8
  neoclassic(1)%ne_neo%diff_eff=0._R8
  neoclassic(1)%ni_neo%diff_eff=0._R8
  neoclassic(1)%te_neo%diff_eff=0._R8
  neoclassic(1)%ti_neo%diff_eff=0._R8
  neoclassic(1)%ne_neo%vconv_eff=0._R8
  neoclassic(1)%ni_neo%vconv_eff=0._R8
  neoclassic(1)%te_neo%vconv_eff=0._R8
  neoclassic(1)%ti_neo%vconv_eff=0._R8
  neoclassic(1)%ti_neo%exchange=0._R8
  neoclassic(1)%ti_neo%qgi=0._R8

  neoclassic(1)%vpol=0._R8
  neoclassic(1)%mtor_neo%flux=0._R8
  neoclassic(1)%mtor_neo%diff_eff=0._R8
  neoclassic(1)%mtor_neo%vconv_eff=0._R8

#ifdef DOIMPURE
  IF (nimp > 0) THEN
     ALLOCATE(neoclassic(1)%nz_neo(nimp))
     ALLOCATE(neoclassic(1)%tz_neo(nimp))
     DO imp=1,nimp
        ncharge=ncharges(imp)
        ALLOCATE(neoclassic(1)%nz_neo(imp)%flux(nrho,ncharge))
        ALLOCATE(neoclassic(1)%nz_neo(imp)%diff_eff(nrho,ncharge))
        ALLOCATE(neoclassic(1)%nz_neo(imp)%vconv_eff(nrho,ncharge))
        neoclassic(1)%nz_neo(imp)%flux=0._R8
        neoclassic(1)%nz_neo(imp)%diff_eff=0._R8
        neoclassic(1)%nz_neo(imp)%vconv_eff=0._R8
        ALLOCATE(neoclassic(1)%tz_neo(imp)%flux(nrho,ncharge))
        ALLOCATE(neoclassic(1)%tz_neo(imp)%diff_eff(nrho,ncharge))
        ALLOCATE(neoclassic(1)%tz_neo(imp)%vconv_eff(nrho,ncharge))
        ALLOCATE(neoclassic(1)%tz_neo(imp)%exchange(nrho,ncharge))
        neoclassic(1)%tz_neo(imp)%flux=0._R8
        neoclassic(1)%tz_neo(imp)%diff_eff=0._R8
        neoclassic(1)%tz_neo(imp)%vconv_eff=0._R8
        neoclassic(1)%tz_neo(imp)%exchange=0._R8
     END DO
  END IF
#endif

!...  done initialisation

  END IF

!...  copy composition over

  call deallocate_cpo(neoclassic(1)%composition)
  call copy_cpo(coreprof(1)%composition,neoclassic(1)%composition)
  call deallocate_cpo(neoclassic(1)%compositions)
  call copy_cpo(coreprof(1)%compositions,neoclassic(1)%compositions)

!...  basic parameters

  a00=eq(1)%eqgeometry%a_minor
  b00=eq(1)%global_param%toroid_field%b0
  r00=eq(1)%global_param%toroid_field%r0

!...  use coreprof grid with equil boundary

  rho_tor_max=MAXVAL(eq(1)%profiles_1d%rho_tor)

!...  set neoclassical grid

  neoclassic(1)%rho_tor=coreprof(1)%rho_tor
  neoclassic(1)%rho_tor_norm=neoclassic(1)%rho_tor/rho_tor_max

!...  interpolate profiles

  rho => neoclassic(1)%rho_tor

  ALLOCATE(nne(nrho))
  ALLOCATE(tte(nrho))
  ALLOCATE(nni(nrho))
  ALLOCATE(tti(nrho))
  ALLOCATE(tavg(nrho))
  ALLOCATE(rlne(nrho))
  ALLOCATE(rlte(nrho))
  ALLOCATE(rlni(nrho))
  ALLOCATE(rlti(nrho))
  ALLOCATE(qq(nrho))
  ALLOCATE(zeff(nrho))
  ALLOCATE(zee(nrho))
  ALLOCATE(rhoi(nrho))
  ALLOCATE(v_i(nrho))
  ALLOCATE(rnue(nrho))
  ALLOCATE(rnui(nrho))
  ALLOCATE(eps12(nrho))
  ALLOCATE(eps32(nrho))

  ALLOCATE(nneo(nrho,0:nspecies))
  ALLOCATE(tneo(nrho,0:nspecies))
  ALLOCATE(zneo(nrho,0:nspecies))
  ALLOCATE(mneo(0:nspecies))

  CALL L3interp(coreprof(1)%profiles1d%zeff%value, &
       coreprof(1)%rho_tor,nrho_prof, zeff,rho,nrho)
  CALL L3interp(eq(1)%profiles_1d%q,eq(1)%profiles_1d%rho_tor, &
       nrho_eq, qq,rho,nrho)

  eps12=SQRT(rho/r00)

  CALL L3interp(coreprof(1)%ne%value,coreprof(1)%rho_tor, &
       nrho_prof, nneo(1,0),rho,nrho)
  CALL L3interp(coreprof(1)%te%value,coreprof(1)%rho_tor, &
       nrho_prof, tneo(1,0),rho,nrho)
  zneo(:,0)= - ee
  mneo(0)=itm_me

  DO ion=1,nion
     inucl = coreprof(1)%compositions%ions(ion)%nucindex
     mass = coreprof(1)%compositions%nuclei(inucl)%amn * itm_amu
     charge = ee*coreprof(1)%compositions%ions(ion)%zion
!     WRITE (0,*) 'ion mass charge',ion,mass,charge
     CALL L3interp(coreprof(1)%ni%value(1,ion),coreprof(1)%rho_tor, &
          nrho_prof, nneo(1,ion),rho,nrho)
     CALL L3interp(coreprof(1)%ti%value(1,ion),coreprof(1)%rho_tor, &
          nrho_prof, tneo(1,ion),rho,nrho)
!     zneo(:,ion)=ee*coreprof(1)%composition%zion(ion)
!     mneo(ion)=coreprof(1)%composition%amn(ion) * itm_amu
     zneo(:,ion)=charge
     mneo(ion)=mass
  END DO
  tavg=SUM(nneo(:,1:nion)*tneo(:,1:nion), dim=2)/SUM(nneo(:,1:nion), dim=2)

#ifdef DOIMPURE
  IF (nimp > 0) THEN
  iz = nion
  DO imp=1,nimp
     ncharge=ncharges(imp)
!     mass=coreimpurity(1)%desc_impur%amn(imp) * itm_amu
     inucl = coreimpurity(1)%compositions%impurities(imp)%nucindex
     mass = coreimpurity(1)%compositions%nuclei(inucl)%amn * itm_amu
!     WRITE (0,*) 'imp mass',imp,mass
     DO icharge=1,ncharge
        iz=iz+1
        CALL L3interp(coreimpurity(1)%impurity(imp)%nz(1,icharge), &
             coreimpurity(1)%rho_tor,nrho_imp, &
             nneo(1,iz),rho,nrho)
        CALL L3interp(coreimpurity(1)%impurity(imp)%z(1,icharge), &
             coreimpurity(1)%rho_tor,nrho_imp, &
             zneo(1,iz),rho,nrho)
        zneo(:,iz)=ee*zneo(:,iz)
        tneo(:,iz)=tavg
        mneo(iz)=mass
     END DO
  END DO
  END IF
#endif

!...  overall species loop

!  write (0,*) ' nion nimp nspecies = ',nion,nimp,nspecies
!  write (0,*) ' iz  imp  icharge        n         T         D'

  DO iz=0,nspecies

     ion = nion
#ifdef DOIMPURE
     DO imp=1,nimp
        DO icharge=1,ncharges(imp)
           ion=ion+1
           IF (iz == ion) EXIT
        END DO
        IF (iz == ion) EXIT
     END DO
     IF (iz <= nion) THEN
        imp=0
        icharge=0
     END IF
#endif

     nni=nneo(:,iz)
     tti=tneo(:,iz)

!     WRITE (0,*) 'species min n T',iz,MINVAL(nni),MINVAL(tti)

     CALL L3deriv(nni,rho,nrho,rlni,rho,nrho)
     CALL L3deriv(tti,rho,nrho,rlti,rho,nrho)
     rlni=rlni/nni
     rlti=rlti/tti

     mass = mneo(iz)
     zee = zneo(:,iz)
!     rnui=(lcoul/2.09e13_R8)*MAX(nneo(:,0), nni*zee*zee/(ee*ee))/(tti**1.5)
     IF (imp == 0) THEN
        rnui=(lcoul/2.09e13_R8)*nni*(zee*zee/(ee*ee))/(tti**1.5)
     ELSE
        rnui=(lcoul/2.09e13_R8)* &
             MAX( nni*(zee*zee/(ee*ee)), nne )/(tti**1.5)
     END IF

     IF (iz == 0) THEN
        nne=nni
        tte=tti
        rlne=rlni
        rlte=rlti
        rnue=(lcoul/3.44e11_R8)*zeff*nne/(tte**1.5)
        rnui=rnue
     END IF

!...  derived parameters

     rhoi=SQRT(cc*cc*mass*kb*tti/(zee*zee*b00*b00))
     v_i=SQRT(kb*tti/mass)

!...  electron-ion exchange set to classical
!...  do conductivity, bootstrap current
!...  do vpol and electric field at end, save grad p and grad n

     IF (iz == 0) THEN
        diff => neoclassic(1)%ne_neo%diff_eff
        gradn => neoclassic(1)%ne_neo%vconv_eff
        gradp => neoclassic(1)%ne_neo%flux
        chi => neoclassic(1)%te_neo%diff_eff
        flux => neoclassic(1)%te_neo%flux

        sum1 => neoclassic(1)%er
        sum2 => neoclassic(1)%te_neo%vconv_eff
        sum1 = 0.
        sum2 = 0.

        neoclassic(1)%sigma = ((1.-eps12)**2.)*nne*ee*ee/(0.51*rnue*itm_me)
        neoclassic(1)%jboot = nne*kb*tte*(2.44*rlne+0.69*rlte)
     ELSE IF (iz <= nion) THEN
        diff => neoclassic(1)%ni_neo%diff_eff(:,iz)
        gradn => neoclassic(1)%ni_neo%vconv_eff(:,iz)
        gradp => neoclassic(1)%ni_neo%flux(:,iz)
        chi => neoclassic(1)%ti_neo%diff_eff(:,iz)
        flux => neoclassic(1)%ti_neo%flux(:,iz)

        neoclassic(1)%ti_neo%exchange(:,iz) = &
             3.*(itm_me/mass)*nne*kb*rnue*(tti-tte)
        neoclassic(1)%jboot = neoclassic(1)%jboot &
             + nni*kb*tti*(2.44*rlne-0.42*rlti)
#ifdef DOIMPURE
     ELSE
        diff => neoclassic(1)%nz_neo(imp)%diff_eff(:,icharge)
        gradn => neoclassic(1)%nz_neo(imp)%vconv_eff(:,icharge)
        gradp => neoclassic(1)%nz_neo(imp)%flux(:,icharge)
        chi => neoclassic(1)%tz_neo(imp)%diff_eff(:,icharge)
        flux => neoclassic(1)%tz_neo(imp)%flux(:,icharge)
#endif
     END IF

     gradn = nni*rlni
     gradp = nni*kb*tti*(rlni+rlti)

!...  the baseline neoclassical diffusion coefficients
!...  Pfirsch Schlueter, plateau, banana
!...  this is greatly simplified, uses the figure from basic lectures
!...      here we allow addition of a cap on the eps factor
!...      as near the axis the banana orbits become potato orbits 

     eps32=MAX(rho,rhoi)/r00
     eps32=eps32**1.5
     diff=rhoi*rhoi*rnui*2._R8*qq*qq
     diff=MAX(diff, rhoi*rhoi*(v_i/ABS(qq*r00))*qq*qq)
     diff=MIN(diff, rhoi*rhoi*rnui*qq*qq/eps32)

!     write (0,"(5x,3i3,5x,3g12.3)") &
!          iz,imp,icharge,nni(nrho/2),tti(nrho/2),diff(nrho/2)

     chi=3.0*diff

     flux=nni*kb*tti*(-chi)*rlti

!... sums for ambipolarity

     sum1 = sum1 + zee*diff*gradp/(kb*tti)
     sum2 = sum2 + nni*zee*zee*diff/(kb*tti)

  END DO

!...  finish bootstrap current and electric field

  eer => neoclassic(1)%er 

  eer = sum1/sum2
  neoclassic(1)%jboot = - eps12*(qq*r00/((rho+1e-10)*b00))*neoclassic(1)%jboot
  

!...  charge correction for diffusion and fluxes in this model
!...  does it such that charge flux is zero, gives electric field
!...  D is in diff and gradn is in vconv and grad p is in flux
!...  correct all species

  diffe => neoclassic(1)%ne_neo%diff_eff

  diff => neoclassic(1)%ne_neo%diff_eff
  vconv => neoclassic(1)%ne_neo%vconv_eff
  flux => neoclassic(1)%ne_neo%flux
  gradn => neoclassic(1)%ne_neo%vconv_eff
  gradp => neoclassic(1)%ne_neo%flux

  zee = zneo(:,0)

  flux = -(diff/(kb*tte))*(gradp - nne*zee*eer)
  vconv = (flux + diff*gradn)/nne

  DO ion=1,nion
     diff => neoclassic(1)%ni_neo%diff_eff(:,ion)
     vconv => neoclassic(1)%ni_neo%vconv_eff(:,ion)
     flux => neoclassic(1)%ni_neo%flux(:,ion)
     gradn => neoclassic(1)%ni_neo%vconv_eff(:,ion)
     gradp => neoclassic(1)%ni_neo%flux(:,ion)

     zee=zneo(:,ion)
     nni=nneo(:,ion)
     tti=tneo(:,ion)

     neoclassic(1)%vpol(:,ion) = (gradp/(nni*zee) - eer)*cc/b00

     flux = -(diff/(kb*tti))*(gradp - nni*zee*eer)
     diff=diffe
     vconv = (flux + diff*gradn)/nni
  END DO

!...  also correct the impurity coefficients

#ifdef DOIMPURE
  iz=nion
  DO imp=1,nimp
     DO icharge=1,ncharges(imp)
        iz=iz+1

        diff => neoclassic(1)%nz_neo(imp)%diff_eff(:,icharge)
        vconv => neoclassic(1)%nz_neo(imp)%vconv_eff(:,icharge)
        flux => neoclassic(1)%nz_neo(imp)%flux(:,icharge)
        gradn => neoclassic(1)%nz_neo(imp)%vconv_eff(:,icharge)
        gradp => neoclassic(1)%nz_neo(imp)%flux(:,icharge)

        zee=zneo(:,iz)
        nni=nneo(:,iz)
        tti=tneo(:,iz)

        flux = -(diff/(kb*tti))*(gradp - nni*zee*eer)
!        diff=diffe
        vconv = (flux + diff*gradn)/nni
     END DO
  END DO
#endif

!...  stamp time

  neoclassic(1)%time=coreprof(1)%time

#ifdef DOIMPURE
  IF (nimp > 0) THEN
    if(associated(ncharges)) deallocate(ncharges)
 ENDIF
#endif
  if(associated(nne)) deallocate(nne)
  if(associated(tte)) deallocate(tte)
  if(associated(nni)) deallocate(nni)
  if(associated(tti)) deallocate(tti)
  if(associated(tavg)) deallocate(tavg)
  if(associated(rlne)) deallocate(rlne)
  if(associated(rlte)) deallocate(rlte)
  if(associated(rlni)) deallocate(rlni)
  if(associated(rlti)) deallocate(rlti)
  if(associated(qq)) deallocate(qq)
  if(associated(zeff)) deallocate(zeff)
  if(associated(zee)) deallocate(zee)
  if(associated(rhoi)) deallocate(rhoi)
  if(associated(v_i)) deallocate(v_i)
  if(associated(rnue)) deallocate(rnue)
  if(associated(rnui)) deallocate(rnui)
  if(associated(eps12)) deallocate(eps12)
  if(associated(eps32)) deallocate(eps32)
  if(associated(nneo)) deallocate(nneo)
  if(associated(tneo)) deallocate(tneo)
  if(associated(zneo)) deallocate(zneo)
  if(associated(mneo)) deallocate(mneo)

END SUBROUTINE Neowes
