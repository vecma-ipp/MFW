! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module to cope with problems in fc2k 
!>
!> Provides itmets for FC2K with arguments:
!> in COREIMPUR_ITER:        TYPE_COREIMPUR
!> in EQUILIBRIUM_ITER:      TYPE_equilibrium
!> in COREPROF_ITER:         TYPE_COREPROF
!> in CORENEUTRALS_OLD:      TYPE_CORENEUTRALS
!> in CORENEUTRALS_ITER:     TYPE_CORENEUTRALS

!> in CONTROL_DOUBLE_IMP(4): REAL(R8)
!> 
!> out CORESOURCE_ITER:      TYPE_CORESOURCE
!> out CORENEUTRALS_new:     TYPE_CORENEUTRALS
!> 
!> 
!>
!> \author I.Ivanova
!>
!> \version "$Id: fc2k_workaround_neutrals.f90 458 2010-12-15 17:37:08Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ITMNEUTRALS (COREIMPUR_ITER,   EQUILIBRIUM_ITER,  COREPROF_ITER, &
                          CORENEUTRALS_OLD, CORENEUTRALS_ITER,                &
                          CORESOURCE_ITER,  CORENEUTRALS_NEW,                 &
		          CONTROL_INTEGER,  CONTROL_DOUBLE)									 


  USE EUITM_SCHEMAS 
  USE ITM_TYPES
  USE NEUTRALS

  
  IMPLICIT NONE
  
  
  TYPE (TYPE_COREIMPUR),   POINTER  :: COREIMPUR_ITER(:)
  TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_ITER(:)
  TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_ITER(:)
  TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE_ITER(:)
  TYPE (TYPE_CORENEUTRALS),POINTER  :: CORENEUTRALS_OLD(:),CORENEUTRALS_ITER(:),CORENEUTRALS_NEW(:)
  
 
  REAL (R8),             INTENT(IN) :: CONTROL_DOUBLE(2)   !real control parameters
  INTEGER,               INTENT(IN) :: CONTROL_INTEGER(1)  !real control parameters
    
    
  CALL NEUTRALS_ETS      (COREIMPUR_ITER,   EQUILIBRIUM_ITER,  COREPROF_ITER, &
                          CORENEUTRALS_OLD, CORENEUTRALS_ITER,                &
                          CORESOURCE_ITER,  CORENEUTRALS_NEW,                 &
		          CONTROL_INTEGER,  CONTROL_DOUBLE)
                                  
     
  
  RETURN
  
  END SUBROUTINE ITMNEUTRALS
  
