module l3interps

contains

  SUBROUTINE L3interp(y_in,x_in,nr_in,y_out,x_out,nr_out)
!!$  Abramowitz & Stegun, Eqs 25.2.1 & 2 on p 878 for general form
!!$      equidistant is Eq 25.2.13 on p 879 for comparison and checking
!!$  4 pt general form coded by B Scott in 200x
!!$  added capability for decreasing abcissa in 2010
!!$  it works best if there is no extrapolation, ie, fixed end pts
!!$  a buffer region also works and is best for periodic cases
!!$      on which I usually use a double cycle even though it is overkill
!!$      because then you don't have to think about deformation or
!!$      resolution

    USE ITM_Types

    IMPLICIT NONE

    INTEGER(ITM_I4) :: nr_in,nr_out
    REAL(R8), DIMENSION(nr_in), INTENT(IN) :: y_in,x_in
    REAL(R8), DIMENSION(nr_out), INTENT(IN) :: x_out
    REAL(R8), DIMENSION(nr_out), INTENT(OUT) :: y_out

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

       aintm=(x-x0)*(x-x1)*(x-x2)/((xm-x0)*(xm-x1)*(xm-x2))
       aint0=(x-xm)*(x-x1)*(x-x2)/((x0-xm)*(x0-x1)*(x0-x2))
       aint1=(x-xm)*(x-x0)*(x-x2)/((x1-xm)*(x1-x0)*(x1-x2))
       aint2=(x-xm)*(x-x0)*(x-x1)/((x2-xm)*(x2-x0)*(x2-x1))

       y_out(j)=aintm*y_in(jm)+aint0*y_in(j0) &
            +aint1*y_in(j1)+aint2*y_in(j2)

    END DO

  END SUBROUTINE L3interp


SUBROUTINE L3deriv(y_in,x_in,nr_in,dydx_out,x_out,nr_out)
!!$  Abramowitz & Stegun, Eqs 25.2.1 & 2 on p 878 for general form
!!$      of interpolation, see L3interp above
!!$  coded by B Scott in 200x
!!$  added capability for decreasing abcissa in 2010
!!$  I did the derivative by analytically differentiating the form
!!$      in L3interp

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

end module l3interps
