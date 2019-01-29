! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

SUBROUTINE CALCULATE_RHOTOR(PLASMA_AX, MAG_AX, AMIN, ELONG_UP, ELONG_LOW, TRIA_UP, TRIA_LOW, RHO)

!--------------------------------------------------------
! This is FC2K wrapper for CALCULATE_RHO_TOR            !
!--------------------------------------------------------

  USE ITM_CONSTANTS
  USE EQUILIBRIUM_TOOLS
  USE ETS_PLASMA

  IMPLICIT NONE

  TYPE (DIAGNOSTIC)                :: DIAG 
  
  REAL (R8)                        :: GEO_AX(3)
  REAL (R8)                        :: MAG_AX(3)
  REAL (R8)                        :: PLASMA_AX(3)
  
  REAL (R8)                        :: IP
  REAL (R8)                        :: AMIN
  REAL (R8)                        :: ELONG_UP, ELONG_LOW
  REAL (R8)                        :: TRIA_UP,  TRIA_LOW

  REAL (R8)                        :: RHO


  CALL CALCULATE_RHO_TOR(PLASMA_AX, MAG_AX, AMIN, ELONG_UP, ELONG_LOW, TRIA_UP, TRIA_LOW, &
                         RHO, DIAG)


  RETURN

END SUBROUTINE CALCULATE_RHOTOR
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
