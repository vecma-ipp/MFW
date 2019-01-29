! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Convergence check
!> This routine checks the convergence of plasma profiles.
!> 
!> Note that the routine also copies COREPROF_OUT to COREPROF_ITER
!> after calculating the convergence criterium
!>
!> \author D.Kalupin
!>
!> \version "$Id: convergence_check.f90 358 2009-08-11 16:39:05Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE CHECKCONVERGENCE (COREPROF_ITER, COREPROF_NEW, CONV) 

!-------------------------------------------------------!
!     This routinechecks the convergence of plasma      !
!     profiles.                                         !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


! +++ Declaration of variables: 
    USE EUITM_SCHEMAS
    USE ITM_CONSTANTS
    USE CONVERGENCE_CHECK


    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_COREPROF), POINTER  :: COREPROF_ITER(:) !input/output CPO with internal ETS parameters profiles 
    TYPE (TYPE_COREPROF), POINTER  :: COREPROF_NEW(:)  !input/output CPO with internal ETS parameters profiles 


! +++ Internal ETS derived types:
    REAL (R8)                      :: CONTROL_DOUBLE(5)!real control parameters
    REAL (R8)                      :: CONV


    CALL CHECK_CONVERGENCE (COREPROF_ITER, COREPROF_NEW, CONTROL_DOUBLE) 

    CONV = CONTROL_DOUBLE(4)


    RETURN 

  END SUBROUTINE CHECKCONVERGENCE
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
