! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> The module declares types of variables used in analytical solution
!>
!> \author R.Stankiewicz, D.Kalupin (???)
!>
!> \version "$Id: type_analytics.f90 1290 2012-05-03 17:08:47Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

MODULE TYPE_ANALYTICS
! +++ The module declares types of variables used 
!     in analytical solution

  use itm_types

  IMPLICIT NONE

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  TYPE ANALYTICAL_SOLUTION

     INTEGER                 :: NRHO                       !number of radial points
     INTEGER                 :: NION                       !number of ion species
     INTEGER                 :: NSOL                       !number of numerical example

     REAL (R8), POINTER     :: SOLUTION(:,:)

  END TYPE ANALYTICAL_SOLUTION

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




CONTAINS



! + + + + + +    ALLOCATION/DEALLOCATION  + + + + + + + +     
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Allocate the analytical solution
!>
!> \author R.Stankiewicz, D.Kalupin (???)
!>
!> \version "$Id: type_analytics.f90 1290 2012-05-03 17:08:47Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ALLOCATE_ANALYTICAL_SOLUTION (NRHO, NION, NSOL, ASOLUTION, ifail)
!     Allocate the analytical solution

!     input/output
    INTEGER, INTENT (IN)       :: NRHO, NION, NSOL
    TYPE (ANALYTICAL_SOLUTION) :: ASOLUTION
    INTEGER, INTENT (INOUT)    :: ifail

!     Local variables
    INTEGER                    :: ISTAT    

    ALLOCATE (ASOLUTION%SOLUTION(NRHO,NION), STAT=ISTAT)

!     Error checking and reporting
    IF (ISTAT /= 0) THEN
       ifail = 1    ! Failure to allocate
       RETURN
    ELSE
       ifail = 0    ! Normal return
    END IF

!     Set dimensions
    ASOLUTION%NRHO             = NRHO
    ASOLUTION%NION             = NION
    ASOLUTION%NSOL             = NSOL

!     Zero out arrays

    ASOLUTION%SOLUTION(:,:)    = 0.0_R8

  END SUBROUTINE ALLOCATE_ANALYTICAL_SOLUTION
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Deallocate plasma profiles needed by the transport solver
!>
!> \author R.Stankiewicz, D.Kalupin (???)
!>
!> \version "$Id: type_analytics.f90 1290 2012-05-03 17:08:47Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE DEALLOCATE_ANALYTICAL_SOLUTION (ASOLUTION, ifail)
!     Deallocate plasma profiles needed by the transport solver

!     input/output
    TYPE (ANALYTICAL_SOLUTION) :: ASOLUTION
    INTEGER, INTENT (INOUT)    :: ifail

!     Local variables
    INTEGER                    :: ISTAT

    DEALLOCATE (ASOLUTION%SOLUTION, STAT=ISTAT)


!     Error checking and reporting
    IF (ISTAT /= 0) THEN
       ifail = 1    ! Failure to allocate
       RETURN
    ELSE
       ifail = 0    ! Normal return
    END IF

  END SUBROUTINE DEALLOCATE_ANALYTICAL_SOLUTION
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




END MODULE TYPE_ANALYTICS







