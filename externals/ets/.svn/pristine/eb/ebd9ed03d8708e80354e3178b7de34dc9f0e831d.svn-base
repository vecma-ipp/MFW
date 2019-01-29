! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLCOREPROF (COREPROF_DB,  COREPROF_GRID,  COREPROF_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLCOREPROF                 !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_COREPROF), POINTER    :: COREPROF_GRID(:)   !input CPO with internal ETS parameters 
  TYPE (TYPE_COREPROF), POINTER    :: COREPROF_OUT(:)    !output CPO with profiles uploaded from the data base 
  TYPE (TYPE_COREPROF), POINTER    :: COREPROF_DB(:)     !time independent CPO slice 
    

  CALL FILLCOREPROF (COREPROF_DB,  COREPROF_GRID,  COREPROF_OUT, INTERPOL)


  RETURN 

END SUBROUTINE FC2K_FILLCOREPROF
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLCORETRANSP (CORETRANSP_DB, CORETRANSP_GRID, CORETRANSP_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLCORETRANSP               !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS
  
  IMPLICIT NONE

  INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_GRID(:) !input CPO with internal parameters 
  TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_OUT(:)  !output CPO with profiles uploaded from the data base 
  TYPE (TYPE_CORETRANSP), POINTER  :: CORETRANSP_DB(:)   !time independent CPO slice 


  CALL FILLCORETRANSP (CORETRANSP_DB, CORETRANSP_GRID, CORETRANSP_OUT, INTERPOL)


  RETURN 

END SUBROUTINE FC2K_FILLCORETRANSP
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLCORESOURCE (CORESOURCE_DB, CORESOURCE_GRID, CORESOURCE_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLCORESOURCE               !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_GRID(:) !input CPO with internal parameters 
  TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_OUT(:)  !output CPO with profiles uploaded from the data base 
  TYPE (TYPE_CORESOURCE), POINTER  :: CORESOURCE_DB(:)   !time independent CPO slice 


  CALL FILLCORESOURCE (CORESOURCE_DB, CORESOURCE_GRID, CORESOURCE_OUT, INTERPOL)


  RETURN 

END SUBROUTINE FC2K_FILLCORESOURCE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLCOREIMPUR (COREIMPUR_DB, COREIMPUR_GRID, COREIMPUR_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLCOREIMPUR                !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  INTEGER                          :: INTERPOL              !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_GRID(:)     !input CPO with internal parameters 
  TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_OUT(:)      !output CPO with sources uploaded from the data base 
  TYPE (TYPE_COREIMPUR), POINTER   :: COREIMPUR_DB(:)       !time independent CPO slice 


  CALL FILLCOREIMPUR (COREIMPUR_DB, COREIMPUR_GRID, COREIMPUR_OUT, INTERPOL)


  RETURN 

END SUBROUTINE FC2K_FILLCOREIMPUR
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLEQUILIBRIUM (EQUILIBRIUM_DB, EQUILIBRIUM_GRID, EQUILIBRIUM_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLEQUILIBRIUM               !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  INTEGER                          :: INTERPOL             !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_GRID(:)  !input CPO with internal parameters 
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OUT(:)   !output CPO with sources uploaded from the data base 
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_DB(:)    !time independent CPO slice 


  CALL FILLEQUILIBRIUM (EQUILIBRIUM_DB, EQUILIBRIUM_GRID, EQUILIBRIUM_OUT, INTERPOL)


  RETURN 


END SUBROUTINE FC2K_FILLEQUILIBRIUM
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLTOROIDFIELD (TOROIDFIELD, TOROIDFIELD_GRID, TOROIDFIELD_OUT)

!--------------------------------------------------------
! This is FC2K wrapper for FILLTOROIDFIELD              !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD_OUT(:)  
  TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD(:)  
  TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD_GRID(:)
    

  CALL FILLTOROIDFIELD (TOROIDFIELD, TOROIDFIELD_GRID, TOROIDFIELD_OUT)


  RETURN 

END SUBROUTINE FC2K_FILLTOROIDFIELD
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLCORENEUTRALS (CORENEUTRALS_DB,  CORENEUTRALS_GRID,  CORENEUTRALS_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLCORENEUTRALS             !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  INTEGER                            :: INTERPOL                       !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS_GRID(:)           !input CPO with internal ETS parameters 
  TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS_OUT(:)            !output CPO with profiles uploaded from the data base 
  TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS_DB(:)             !time independent CPO slice 


  CALL FILLCORENEUTRALS (CORENEUTRALS_DB,  CORENEUTRALS_GRID,  CORENEUTRALS_OUT, INTERPOL)


  RETURN 
 
END SUBROUTINE FC2K_FILLCORENEUTRALS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE FC2K_FILLNEOCLASSIC (NEOCLASSIC_DB, NEOCLASSIC_GRID, NEOCLASSIC_OUT, INTERPOL) 

!--------------------------------------------------------
! This is FC2K wrapper for FILLNEOCLASSIC               !
!--------------------------------------------------------

  USE ITM_TYPES
  USE EUITM_SCHEMAS
  USE FILL_CPOS

  IMPLICIT NONE

  INTEGER                          :: INTERPOL           !interpolation index "0"-based on RHO_TOR; all other - based on RHO_TOR_NORM  

  TYPE (TYPE_NEOCLASSIC), POINTER  :: NEOCLASSIC_GRID(:) !input CPO with internal parameters 
  TYPE (TYPE_NEOCLASSIC), POINTER  :: NEOCLASSIC_OUT(:)  !output CPO with profiles uploaded from the data base 
  TYPE (TYPE_NEOCLASSIC), POINTER  :: NEOCLASSIC_DB(:)   !time independent CPO slice 
  

  CALL FILLNEOCLASSIC (NEOCLASSIC_DB, NEOCLASSIC_GRID, NEOCLASSIC_OUT, INTERPOL)


  RETURN 

END SUBROUTINE FC2K_FILLNEOCLASSIC
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

