! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> types
!>
!> \author COS
!>
!> \version "$Id: cos_precision.f90 803 2010-08-28 12:07:30Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!*******************************************************************************
module cos_precision
  implicit none
  integer,parameter :: DP=kind(1.0D0)
  type tab_ptr
      real(DP),dimension(:),pointer  :: val
  end type tab_ptr
end module cos_precision
!*******************************************************************************
