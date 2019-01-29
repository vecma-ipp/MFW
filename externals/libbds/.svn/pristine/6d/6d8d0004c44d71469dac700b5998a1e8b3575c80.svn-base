MODULE Mod_Turb

  USE ITM_Types
  USE Euitm_schemas
!  use coretransp_types

  IMPLICIT NONE

  PUBLIC :: turb_constructor, turb_destructor
!.  PUBLIC :: fill_turb

  CONTAINS

!--------------------------------------------------------------------------
  SUBROUTINE Turb_Constructor(coretransp, nrho0, nrho, nion)
!--------------------------------------------------------------------------

    IMPLICIT NONE

    TYPE (type_coretransp) :: coretransp
    INTEGER(ITM_I4), INTENT(IN) :: nrho0, nrho, nion

!...  allocate transport structure

     ALLOCATE(coretransp%values(1))

     ALLOCATE(coretransp%values(1)%sigma(nrho0:nrho))

     ALLOCATE(coretransp%values(1)%rho_tor(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%rho_tor_norm(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ne_transp%flux(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%te_transp%flux(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ni_transp%flux(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%ti_transp%flux(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%vtor_transp%flux(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%ne_transp%diff_eff(nrho0:nrho,3))
     ALLOCATE(coretransp%values(1)%te_transp%diff_eff(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ni_transp%diff_eff(nrho0:nrho,nion,3))
     ALLOCATE(coretransp%values(1)%ti_transp%diff_eff(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%vtor_transp%diff_eff(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%ne_transp%vconv_eff(nrho0:nrho,3))
     ALLOCATE(coretransp%values(1)%te_transp%vconv_eff(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ni_transp%vconv_eff(nrho0:nrho,nion,3))
     ALLOCATE(coretransp%values(1)%ti_transp%vconv_eff(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%vtor_transp%vconv_eff(nrho0:nrho,nion))

     coretransp%values(1)%ne_transp%flux = 0._R8
     coretransp%values(1)%te_transp%flux = 0._R8
     coretransp%values(1)%ni_transp%flux = 0._R8
     coretransp%values(1)%ti_transp%flux = 0._R8
     coretransp%values(1)%vtor_transp%flux = 0._R8
     coretransp%values(1)%ne_transp%diff_eff = 0._R8
     coretransp%values(1)%te_transp%diff_eff = 0._R8
     coretransp%values(1)%ni_transp%diff_eff = 0._R8
     coretransp%values(1)%ti_transp%diff_eff = 0._R8
     coretransp%values(1)%vtor_transp%diff_eff = 0._R8
     coretransp%values(1)%ne_transp%vconv_eff = 0._R8
     coretransp%values(1)%te_transp%vconv_eff = 0._R8
     coretransp%values(1)%ni_transp%vconv_eff = 0._R8
     coretransp%values(1)%ti_transp%vconv_eff = 0._R8
     coretransp%values(1)%vtor_transp%vconv_eff = 0._R8

  END SUBROUTINE Turb_Constructor

!--------------------------------------------------------------------------
  SUBROUTINE Turb_Destructor(coretransp)
!--------------------------------------------------------------------------

    IMPLICIT NONE

    TYPE (type_coretransp) :: coretransp

    INTEGER :: i,nvalues

!...  deallocate transport structure

     nvalues=SIZE(coretransp%values)

     DO i=1,nvalues
     DEALLOCATE(coretransp%values(i)%sigma)
     DEALLOCATE(coretransp%values(i)%rho_tor)
     DEALLOCATE(coretransp%values(i)%rho_tor_norm)
     DEALLOCATE(coretransp%values(i)%ne_transp%flux)
     DEALLOCATE(coretransp%values(i)%te_transp%flux)
     DEALLOCATE(coretransp%values(i)%ni_transp%flux)
     DEALLOCATE(coretransp%values(i)%ti_transp%flux)
     DEALLOCATE(coretransp%values(i)%vtor_transp%flux)
     DEALLOCATE(coretransp%values(i)%ne_transp%diff_eff)
     DEALLOCATE(coretransp%values(i)%te_transp%diff_eff)
     DEALLOCATE(coretransp%values(i)%ni_transp%diff_eff)
     DEALLOCATE(coretransp%values(i)%ti_transp%diff_eff)
     DEALLOCATE(coretransp%values(i)%vtor_transp%diff_eff)
     DEALLOCATE(coretransp%values(i)%ne_transp%vconv_eff)
     DEALLOCATE(coretransp%values(i)%te_transp%vconv_eff)
     DEALLOCATE(coretransp%values(i)%ni_transp%vconv_eff)
     DEALLOCATE(coretransp%values(i)%ti_transp%vconv_eff)
     DEALLOCATE(coretransp%values(i)%vtor_transp%vconv_eff)
     END DO

     DEALLOCATE(coretransp%values)

  END SUBROUTINE Turb_Destructor

END MODULE Mod_Turb
