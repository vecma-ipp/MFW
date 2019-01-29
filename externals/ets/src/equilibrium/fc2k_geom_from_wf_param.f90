! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE GEOM_FROM_WF_PARAM(EQUILIBRIUM,   EQUILIBRIUM_OUT,  &
                              GEO_AX, MAG_AX, PLASMA_AX,       &
                              IP, AMIN,                        &
                              ELONG_UP, ELONG_LOW,             &
                              TRIA_UP, TRIA_LOW,               &
                              NPSI, NDIM1, NDIM2, NPOINTS) 

!--------------------------------------------------------
! This is FC2K wrapper for GEOMETRY_FROM_WF_PARAMETERS  !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE EQUILIBRIUM_TOOLS
  USE ETS_PLASMA
  
  IMPLICIT NONE
  
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)      
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OUT(:)  
  
  REAL (R8)                        :: GEO_AX(3)
  REAL (R8)                        :: MAG_AX(3)
  REAL (R8)                        :: PLASMA_AX(3)
  
  REAL (R8)                        :: IP
  REAL (R8)                        :: AMIN
  REAL (R8)                        :: ELONG_UP, ELONG_LOW
  REAL (R8)                        :: TRIA_UP,  TRIA_LOW
    
  INTEGER                          :: NPSI  
  INTEGER                          :: NDIM1   
  INTEGER                          :: NDIM2
  INTEGER                          :: NPOINTS

  CALL GEOMETRY_FROM_WF_PARAMETERS(EQUILIBRIUM,   EQUILIBRIUM_OUT,  &
                                   GEO_AX, MAG_AX, PLASMA_AX,       &
                                   IP, AMIN,                        &
                                   ELONG_UP, ELONG_LOW,             &
                                   TRIA_UP, TRIA_LOW,               &
                                   NPSI, NDIM1, NDIM2, NPOINTS)

  RETURN 


END SUBROUTINE GEOM_FROM_WF_PARAM
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
