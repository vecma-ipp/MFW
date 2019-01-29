! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module to cope with problems in fc2k
!>
!> Provides EQUIL_INPUT for FC2K with arguments:
!> in COREPROF: TYPE_COREPROF
!> in TOROIDFIELD: TYPE_TOROIDFIELD
!> in EQUILIBRIUM: TYPE_EQUILIBRIUM
!> out EQUILIBRIUM: TYPE_EQUILIBRIUM
!>
!> \author D.Kalupin
!>
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

     SUBROUTINE eqinput(COREPROF_IN, TOROIDFIELD_IN, EQUILIBRIUM_IN, EQUILIBRIUM_OUT)

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

     END SUBROUTINE eqinput
