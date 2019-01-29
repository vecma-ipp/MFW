! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module to cope with problems in fc2k 
!>
!> Provides itmets for FC2K with arguments:
!> in EQUILIBRIUM_ITER:      TYPE_equilibrium
!> in COREPROF_ITER:         TYPE_COREPROF
!> in COREIPUR_OLD:          TYPE_COREIMPUR
!> in COREIMPUR_ITER:        TYPE_COREIMPUR
!> in CORESOURCE_ITER:       TYPE_CORESOURCE

!> in CONTROL_DOUBLE_IMP(4): REAL(R8)
!> 
!> out CORESOURCE_IMP:       TYPE_CORESOURCE
!> out COREIMPUR_new:        TYPE_COREIMPUR
!> 
!> 
!>
!> \author I.Ivanova
!>
!> \version "$Id: fc2k_workaround_impurity.f90 458 2010-12-15 17:37:08Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

  SUBROUTINE ITMIMPURITY (EQUILIBRIUM_ITER, COREPROF_ITER,  CORETRANSP_ITER,      &
                          COREIMPUR_OLD,    COREIMPUR_ITER, CORENEUTRALS_ITER,    &
                          CORESOURCE_ITER,  CORESOURCE_NEW, COREIMPUR_NEW,        &
                          CONTROL_INTEGER,  CONTROL_DOUBLE)
 

  USE EUITM_SCHEMAS 
  USE ITM_TYPES
  USE IMPURITY
  USE NEUTRALS
  

  
  IMPLICIT NONE
  
  TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_ITER(:)
  TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_ITER(:)
  TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP_ITER(:)
  TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE_NEW(:), CORESOURCE_ITER(:)
  TYPE (TYPE_COREIMPUR),   POINTER  :: COREIMPUR_OLD(:),COREIMPUR_ITER(:),COREIMPUR_NEW(:)
  TYPE (TYPE_CORENEUTRALS),POINTER  :: CORENEUTRALS_ITER(:)
 
  INTEGER,               INTENT(IN) :: CONTROL_INTEGER(2)    
  REAL (R8),             INTENT(IN) :: CONTROL_DOUBLE(5)   
   
          

  CALL IMPURITY_ETS  (EQUILIBRIUM_ITER, COREPROF_ITER,  CORETRANSP_ITER,      &
                      COREIMPUR_OLD,    COREIMPUR_ITER, CORENEUTRALS_ITER,    &
                      CORESOURCE_ITER,  CORESOURCE_NEW, COREIMPUR_NEW,        &
                      CONTROL_INTEGER,  CONTROL_DOUBLE)
     
  
  RETURN
  
  END SUBROUTINE ITMIMPURITY
  
