SUBROUTINE FC2K_EQINPUT(COREPROF_IN, TOROIDFIELD_IN, EQUILIBRIUM_IN, EQUILIBRIUM_OUT)
!--------------------------------------------------------
! This is FC2K wrapper for FILLCORETRANSP               !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE EQUILIBRIUM_INPUT
  
  IMPLICIT NONE

! +++ CPO derived types:
  TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
  TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
  TYPE (TYPE_TOROIDFIELD), POINTER  :: TOROIDFIELD_IN(:)   !toroidal field, major radius, total current
  TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input CPO with plasma profiles   


  CALL EQUIL_INPUT (COREPROF_IN, TOROIDFIELD_IN, EQUILIBRIUM_IN, EQUILIBRIUM_OUT)

  RETURN

END SUBROUTINE FC2K_EQINPUT
