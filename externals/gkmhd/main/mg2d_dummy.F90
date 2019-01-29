SUBROUTINE Mg2d

!...  dummy 2D solve

!...  not parallelised

  USE MPEs

  USE Coeff
  USE Vars

  IMPLICIT NONE

  INTEGER :: i

  DO i=1,nx
     fsfc(i)%uuc=fsfc(i)%uux
  END DO

END SUBROUTINE Mg2d
