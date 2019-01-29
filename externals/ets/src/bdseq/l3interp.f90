SUBROUTINE L3interp(y_in,x_in,nr_in,y_out,x_out,nr_out)

  USE ITM_Types

  IMPLICIT NONE

  INTEGER(ITM_I4) :: nr_in,nr_out
  REAL(R8), DIMENSION(nr_in), INTENT(IN) :: y_in,x_in
  REAL(R8), DIMENSION(nr_out), INTENT(IN) :: x_out
  REAL(R8), DIMENSION(nr_out), INTENT(OUT) :: y_out

  REAL(R8) :: x,aintm,aint0,aint1,aint2,xm,x0,x1,x2
  INTEGER(ITM_I4) :: j,jm,j0,j1,j2
  INTEGER :: jstart,jfirst,jlast,jstep

  IF (x_in(nr_in) > x_in(1)) THEN
     jstart=3
     jfirst=1
     jlast=nr_out
     jstep=1
  ELSE
     jstart=nr_out-2
     jfirst=nr_out
     jlast=1
     jstep=-1
  END IF

  j1=jstart
  DO j=jfirst,jlast,jstep
     x=x_out(j)
     DO WHILE (x >= x_in(j1) .AND. j1 < nr_in-1 .AND. j1 > 2) 
        j1=j1+jstep
     END DO

     j2=j1+jstep
     j0=j1-jstep
     jm=j1-2*jstep

!...  extrapolate inside or outside

     x2=x_in(j2)
     x1=x_in(j1)
     x0=x_in(j0)
     xm=x_in(jm)

     aintm=(x-x0)*(x-x1)*(x-x2)/((xm-x0)*(xm-x1)*(xm-x2))
     aint0=(x-xm)*(x-x1)*(x-x2)/((x0-xm)*(x0-x1)*(x0-x2))
     aint1=(x-xm)*(x-x0)*(x-x2)/((x1-xm)*(x1-x0)*(x1-x2))
     aint2=(x-xm)*(x-x0)*(x-x1)/((x2-xm)*(x2-x0)*(x2-x1))

     y_out(j)=aintm*y_in(jm)+aint0*y_in(j0) &
          +aint1*y_in(j1)+aint2*y_in(j2)

  END DO

END SUBROUTINE L3interp


SUBROUTINE L3deriv(y_in,x_in,nr_in,dydx_out,x_out,nr_out)

  USE ITM_Types

  IMPLICIT NONE

  INTEGER(ITM_I4) :: nr_in,nr_out
  REAL(R8), DIMENSION(nr_in), INTENT(IN) :: y_in,x_in
  REAL(R8), DIMENSION(nr_out), INTENT(IN) :: x_out
  REAL(R8), DIMENSION(nr_out), INTENT(OUT) :: dydx_out

  REAL(R8) :: x,aintm,aint0,aint1,aint2,xm,x0,x1,x2
  INTEGER(ITM_I4) :: j,jm,j0,j1,j2
  INTEGER(ITM_I4) :: jstart,jfirst,jlast,jstep

  IF (x_in(nr_in) > x_in(1)) THEN
     jstart=3
     jfirst=1
     jlast=nr_out
     jstep=1
  ELSE
     jstart=nr_out-2
     jfirst=nr_out
     jlast=1
     jstep=-1
  END IF

  j1=jstart
  DO j=jfirst,jlast,jstep
     x=x_out(j)
     DO WHILE (x >= x_in(j1) .AND. j1 < nr_in-1 .AND. j1 > 2) 
        j1=j1+jstep
     END DO

     j2=j1+jstep
     j0=j1-jstep
     jm=j1-2*jstep

!...  extrapolate inside or outside

     x2=x_in(j2)
     x1=x_in(j1)
     x0=x_in(j0)
     xm=x_in(jm)

     aintm=((x-x1)*(x-x2)+(x-x0)*(x-x2)+(x-x0)*(x-x1)) &
          /((xm-x0)*(xm-x1)*(xm-x2))
     aint0=((x-x1)*(x-x2)+(x-xm)*(x-x2)+(x-xm)*(x-x1)) &
          /((x0-xm)*(x0-x1)*(x0-x2))
     aint1=((x-x0)*(x-x2)+(x-xm)*(x-x2)+(x-xm)*(x-x0)) &
          /((x1-xm)*(x1-x0)*(x1-x2))
     aint2=((x-x0)*(x-x1)+(x-xm)*(x-x1)+(x-xm)*(x-x0)) &
          /((x2-xm)*(x2-x0)*(x2-x1))

     dydx_out(j)=aintm*y_in(jm)+aint0*y_in(j0) &
          +aint1*y_in(j1)+aint2*y_in(j2)

  END DO

END SUBROUTINE L3deriv


SUBROUTINE Lderiv(y,x,dydx,nx)

  USE ITM_Types

  IMPLICIT NONE

  INTEGER(ITM_I4) :: nx
  REAL(R8) :: dxp,dxm,dx0
  REAL(R8), DIMENSION(nx), INTENT(IN) :: x,y
  REAL(R8), DIMENSION(nx), INTENT(OUT) :: dydx

  INTEGER(ITM_I4) :: i,ip,im

  DO i=2,nx-1
     ip=MIN(i+1,nx)
     im=MAX(i-1,1)
     dxp=x(ip)-x(i)
     dxm=x(i)-x(im)
     dx0=x(ip)-x(im)
     dydx(i)=(dxm*dxm*(y(ip)-y(i))+dxp*dxp*(y(i)-y(im)))/(dxm*dxp*dx0)
  END DO
  dydx(1)=2.*dydx(2) - dydx(3)
  dydx(nx)=2.*dydx(nx-1) - dydx(nx-2)

END SUBROUTINE Lderiv

