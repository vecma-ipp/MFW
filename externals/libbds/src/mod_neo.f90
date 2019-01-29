MODULE Mod_Neo

  USE ITM_Types
  USE Euitm_schemas
  USE Copy_structures
!  use neoclassic_types

  IMPLICIT NONE

  PUBLIC :: neo_constructor, neo_destructor

  CONTAINS

!--------------------------------------------------------------------------
  SUBROUTINE Neo_Constructor(neoclassic, nrho0, nrho, nion, nimp, ncharges)
!--------------------------------------------------------------------------

    IMPLICIT NONE

    type (type_neoclassic) :: neoclassic
    INTEGER(ITM_I4), INTENT(IN) :: nrho0, nrho, nion, nimp, ncharges(:)
    INTEGER(ITM_I4) :: imp, nzimp

!...  allocate transport structure

    ALLOCATE(neoclassic%sigma(nrho0:nrho))
    ALLOCATE(neoclassic%jboot(nrho0:nrho))
    ALLOCATE(neoclassic%er(nrho0:nrho))
    ALLOCATE(neoclassic%vpol(nrho0:nrho,nion))
    ALLOCATE(neoclassic%vtor(nrho0:nrho,nion))
    ALLOCATE(neoclassic%mach(nrho0:nrho,nion))
    ALLOCATE(neoclassic%utheta_e(nrho0:nrho))
    ALLOCATE(neoclassic%utheta_i(nrho0:nrho,nion))
    ALLOCATE(neoclassic%viscosity_par(nrho0:nrho,nion))

    ALLOCATE(neoclassic%rho_tor_norm(nrho0:nrho))
    ALLOCATE(neoclassic%rho_tor(nrho0:nrho))

    ALLOCATE(neoclassic%ne_neo%flux(nrho0:nrho))
    ALLOCATE(neoclassic%ne_neo%diff_eff(nrho0:nrho))
    ALLOCATE(neoclassic%ne_neo%vconv_eff(nrho0:nrho))

    ALLOCATE(neoclassic%te_neo%flux(nrho0:nrho))
    ALLOCATE(neoclassic%te_neo%diff_eff(nrho0:nrho))
    ALLOCATE(neoclassic%te_neo%vconv_eff(nrho0:nrho))

    ALLOCATE(neoclassic%mtor_neo%flux(nrho0:nrho))
    ALLOCATE(neoclassic%mtor_neo%diff_eff(nrho0:nrho))
    ALLOCATE(neoclassic%mtor_neo%vconv_eff(nrho0:nrho))

    ALLOCATE(neoclassic%ni_neo%flux(nrho0:nrho,nion))
    ALLOCATE(neoclassic%ni_neo%diff_eff(nrho0:nrho,nion))
    ALLOCATE(neoclassic%ni_neo%vconv_eff(nrho0:nrho,nion))

    ALLOCATE(neoclassic%ti_neo%flux(nrho0:nrho,nion))
    ALLOCATE(neoclassic%ti_neo%diff_eff(nrho0:nrho,nion))
    ALLOCATE(neoclassic%ti_neo%vconv_eff(nrho0:nrho,nion))
    ALLOCATE(neoclassic%ti_neo%exchange(nrho0:nrho,nion))
    ALLOCATE(neoclassic%ti_neo%qgi(nrho0:nrho,nion))

    ALLOCATE(neoclassic%nz_neo(nimp))
    ALLOCATE(neoclassic%tz_neo(nimp))
    ALLOCATE(neoclassic%impurity(nimp))
    DO imp=1,nimp
       nzimp=ncharges(imp)
       ALLOCATE(neoclassic%nz_neo(imp)%flux(nrho0:nrho,nzimp))
       ALLOCATE(neoclassic%nz_neo(imp)%diff_eff(nrho0:nrho,nzimp))
       ALLOCATE(neoclassic%nz_neo(imp)%vconv_eff(nrho0:nrho,nzimp))

       ALLOCATE(neoclassic%tz_neo(imp)%flux(nrho0:nrho,nzimp))
       ALLOCATE(neoclassic%tz_neo(imp)%diff_eff(nrho0:nrho,nzimp))
       ALLOCATE(neoclassic%tz_neo(imp)%vconv_eff(nrho0:nrho,nzimp))
       ALLOCATE(neoclassic%tz_neo(imp)%exchange(nrho0:nrho,nzimp))

       ALLOCATE(neoclassic%impurity(imp)%utheta_z(nrho0:nrho,nzimp))
    END DO

    neoclassic%ne_neo%flux = 0._R8
    neoclassic%te_neo%flux = 0._R8
    neoclassic%ni_neo%flux = 0._R8
    neoclassic%ti_neo%flux = 0._R8
    neoclassic%ne_neo%diff_eff = 0._R8
    neoclassic%te_neo%diff_eff = 0._R8
    neoclassic%ni_neo%diff_eff = 0._R8
    neoclassic%ti_neo%diff_eff = 0._R8
    neoclassic%ne_neo%vconv_eff = 0._R8
    neoclassic%te_neo%vconv_eff = 0._R8
    neoclassic%ni_neo%vconv_eff = 0._R8
    neoclassic%ti_neo%vconv_eff = 0._R8
    neoclassic%vpol = 0._R8
    neoclassic%sigma = 0._R8
    neoclassic%jboot = 0._R8
    neoclassic%er = 0._R8

    DO imp = 1, nimp
       neoclassic%nz_neo(imp)%flux = 0._R8
       neoclassic%nz_neo(imp)%diff_eff = 0._R8 
       neoclassic%nz_neo(imp)%vconv_eff = 0._R8 

       neoclassic%tz_neo(imp)%flux = 0._R8
       neoclassic%tz_neo(imp)%diff_eff = 0._R8
       neoclassic%tz_neo(imp)%vconv_eff = 0._R8
       neoclassic%tz_neo(imp)%exchange = 0._R8

       neoclassic%impurity(imp)%utheta_z = 0._R8
    END DO

  END SUBROUTINE Neo_Constructor

!--------------------------------------------------------------------------
  SUBROUTINE Neo_Destructor(neoclassic)
!--------------------------------------------------------------------------

    IMPLICIT NONE

    TYPE (type_neoclassic) :: neoclassic
    INTEGER(ITM_I4) :: imp, nimp

    nimp=SIZE(neoclassic%impurity)

    DEALLOCATE(neoclassic%sigma)
    DEALLOCATE(neoclassic%jboot)
    DEALLOCATE(neoclassic%er)
    DEALLOCATE(neoclassic%vpol)
    DEALLOCATE(neoclassic%vtor)
    DEALLOCATE(neoclassic%mach)
    DEALLOCATE(neoclassic%utheta_e)
    DEALLOCATE(neoclassic%utheta_i)
    DEALLOCATE(neoclassic%viscosity_par)

    DEALLOCATE(neoclassic%rho_tor_norm)
    DEALLOCATE(neoclassic%rho_tor)

    DEALLOCATE(neoclassic%ne_neo%flux)
    DEALLOCATE(neoclassic%ne_neo%diff_eff)
    DEALLOCATE(neoclassic%ne_neo%vconv_eff)

    DEALLOCATE(neoclassic%te_neo%flux)
    DEALLOCATE(neoclassic%te_neo%diff_eff)
    DEALLOCATE(neoclassic%te_neo%vconv_eff)

    DEALLOCATE(neoclassic%ni_neo%flux)
    DEALLOCATE(neoclassic%ni_neo%diff_eff)
    DEALLOCATE(neoclassic%ni_neo%vconv_eff)

    DEALLOCATE(neoclassic%mtor_neo%flux)
    DEALLOCATE(neoclassic%mtor_neo%diff_eff)
    DEALLOCATE(neoclassic%mtor_neo%vconv_eff)

    DEALLOCATE(neoclassic%ti_neo%flux)
    DEALLOCATE(neoclassic%ti_neo%diff_eff)
    DEALLOCATE(neoclassic%ti_neo%vconv_eff)
    DEALLOCATE(neoclassic%ti_neo%exchange)
    DEALLOCATE(neoclassic%ti_neo%qgi)

    DO imp=1,nimp
       DEALLOCATE(neoclassic%nz_neo(imp)%flux)
       DEALLOCATE(neoclassic%nz_neo(imp)%diff_eff)
       DEALLOCATE(neoclassic%nz_neo(imp)%vconv_eff)

       DEALLOCATE(neoclassic%tz_neo(imp)%flux)
       DEALLOCATE(neoclassic%tz_neo(imp)%diff_eff)
       DEALLOCATE(neoclassic%tz_neo(imp)%vconv_eff)
       DEALLOCATE(neoclassic%tz_neo(imp)%exchange)

       DEALLOCATE(neoclassic%impurity(imp)%utheta_z)
    END DO
    DEALLOCATE(neoclassic%nz_neo)
    DEALLOCATE(neoclassic%tz_neo)
    DEALLOCATE(neoclassic%impurity)

  END SUBROUTINE neo_Destructor

END MODULE Mod_neo
