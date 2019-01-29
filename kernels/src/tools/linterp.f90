! From ETS sources, @B.Scott
SUBROUTINE Linterp(y_in,x_in,nr_in,y_out,x_out,nr_out)

  USE itm_types

  IMPLICIT NONE

  INTEGER :: nr_in,nr_out
  REAL(R8), DIMENSION(nr_in), INTENT(IN) :: y_in,x_in
  REAL(R8), DIMENSION(nr_out), INTENT(IN) :: x_out
  REAL(R8), DIMENSION(nr_out), INTENT(OUT) :: y_out

  REAL(R8) :: x,ainterp

  INTEGER :: i,j

  j=1
  DO i=1,nr_out
     x=x_out(i)
     DO WHILE (x >= x_in(j) .AND. j < nr_in) 
        j=j+1
     END DO

     IF (j == 1) THEN
!...  extrapolate inside

        ainterp=(x-x_in(1))/(x_in(2)-x_in(1))
        y_out(i) = (1.-ainterp)*y_in(1) + ainterp*y_in(2)

     ELSE
!...  interpolate or extrapolate outside

        ainterp=(x-x_in(j-1))/(x_in(j)-x_in(j-1))
        y_out(i) = (1.-ainterp)*y_in(j-1) + ainterp*y_in(j)

     END IF
  END DO

END SUBROUTINE Linterp

!!$
!!$SUBROUTINE Lderiv(y,x,dydx,nx)
!!$
!!$  USE itm_types
!!$
!!$  IMPLICIT NONE
!!$
!!$  INTEGER :: nx
!!$  REAL(R8), DIMENSION(nx), INTENT(IN) :: x,y
!!$  REAL(R8), DIMENSION(nx), INTENT(OUT) :: dydx
!!$
!!$  INTEGER :: i,ip,im
!!$
!!$  DO i=1,nx
!!$     ip=MIN(i+1,nx)
!!$     im=MAX(i-1,1)
!!$     dydx(i)=(y(ip)-y(im))/(x(ip)-x(im))
!!$  END DO
!!$  dydx(1)=2.*dydx(2) - dydx(3)
!!$  dydx(nx)=2.*dydx(nx-1) - dydx(nx-2)
!!$
!!$END SUBROUTINE Lderiv
!!$
