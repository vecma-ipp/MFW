MODULE Mod_Turb

  USE ITM_Types
  USE Euitm_schemas

  IMPLICIT NONE

  PUBLIC :: turb_constructor, turb_destructor
!.  PUBLIC :: fill_turb

  CONTAINS

!--------------------------------------------------------------------------
  SUBROUTINE Turb_Constructor(coretransp, nrho0, nrho, nion)
!--------------------------------------------------------------------------

    IMPLICIT NONE

    type (type_coretransp) :: coretransp
    integer(ITM_I4) :: nrho0, nrho, nion

!...  allocate transport structure

     ALLOCATE(coretransp%values(1)%rho_tor(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%rho_tor_norm(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ne_transp%flux(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%te_transp%flux(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ni_transp%flux(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%ti_transp%flux(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%ne_transp%diff_eff(nrho0:nrho,3))
     ALLOCATE(coretransp%values(1)%te_transp%diff_eff(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ni_transp%diff_eff(nrho0:nrho,nion,3))
     ALLOCATE(coretransp%values(1)%ti_transp%diff_eff(nrho0:nrho,nion))
     ALLOCATE(coretransp%values(1)%ne_transp%vconv_eff(nrho0:nrho,3))
     ALLOCATE(coretransp%values(1)%te_transp%vconv_eff(nrho0:nrho))
     ALLOCATE(coretransp%values(1)%ni_transp%vconv_eff(nrho0:nrho,nion,3))
     ALLOCATE(coretransp%values(1)%ti_transp%vconv_eff(nrho0:nrho,nion))

     coretransp%values(1)%ne_transp%flux=0._R8
     coretransp%values(1)%te_transp%flux=0._R8
     coretransp%values(1)%ni_transp%flux=0._R8
     coretransp%values(1)%ti_transp%flux=0._R8
     coretransp%values(1)%ne_transp%diff_eff=0._R8
     coretransp%values(1)%te_transp%diff_eff=0._R8
     coretransp%values(1)%ni_transp%diff_eff=0._R8
     coretransp%values(1)%ti_transp%diff_eff=0._R8
     coretransp%values(1)%ne_transp%vconv_eff=0._R8
     coretransp%values(1)%te_transp%vconv_eff=0._R8
     coretransp%values(1)%ni_transp%vconv_eff=0._R8
     coretransp%values(1)%ti_transp%vconv_eff=0._R8

  END SUBROUTINE Turb_Constructor

!--------------------------------------------------------------------------
  SUBROUTINE Turb_Destructor(coretransp)
!--------------------------------------------------------------------------

    IMPLICIT NONE

    TYPE (type_coretransp) :: coretransp

!...  deallocate transport structure

     DEALLOCATE(coretransp%values(1)%rho_tor)
     DEALLOCATE(coretransp%values(1)%rho_tor_norm)
     DEALLOCATE(coretransp%values(1)%ne_transp%flux)
     DEALLOCATE(coretransp%values(1)%te_transp%flux)
     DEALLOCATE(coretransp%values(1)%ni_transp%flux)
     DEALLOCATE(coretransp%values(1)%ti_transp%flux)
     DEALLOCATE(coretransp%values(1)%ne_transp%diff_eff)
     DEALLOCATE(coretransp%values(1)%te_transp%diff_eff)
     DEALLOCATE(coretransp%values(1)%ni_transp%diff_eff)
     DEALLOCATE(coretransp%values(1)%ti_transp%diff_eff)
     DEALLOCATE(coretransp%values(1)%ne_transp%vconv_eff)
     DEALLOCATE(coretransp%values(1)%te_transp%vconv_eff)
     DEALLOCATE(coretransp%values(1)%ni_transp%vconv_eff)
     DEALLOCATE(coretransp%values(1)%ti_transp%vconv_eff)

  END SUBROUTINE Turb_Destructor

!-----------------------------------------------------------------------------
!  subroutine fill_turb(a, xaxis, nr, coretransp)
!-----------------------------------------------------------------------------
! This subroutine fills the coretransp CPO through calls
!     of the appropriate subroutines
! This subroutine shall be callable at any point inside the turb code
!     or module after the fluxes are known
!-----------------------------------------------------------------------------

!  end subroutine fill_turb

END MODULE Mod_Turb
