!> prototype ITM_TOOLBOX
!>
!> This is the initial prototype of the ITM_TOOLBOX
!> At the moment it is merely a collection of routines
!> that are used by the ETS
!>
!> \author D. P. Coster
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

module itm_toolbox

  use itm_constants

  implicit none

  integer, parameter               :: BUFLEN = 256

contains

!> "generic" interpolation routine (only calls l3interp at the moment)
!>
!> fumction takes arrays x_in, y_in and x_out and returns y_out
!>
!> \author D. P. Coster
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

  function interpolate(x_in, y_in, x_out)

    implicit none

    REAL(R8), INTENT(IN) :: x_in(:), y_in(:), x_out(:)
    REAL(R8) :: interpolate(size(x_out)), y_out(size(x_out))

    call l3interp(y_in, x_in, size(x_in), y_out, x_out, size(x_out))
    interpolate = y_out

    return
  end function interpolate



#ifndef NO_LIBMATHEVAL

!> itm_toolbox:rho_with_accumulation
!>
!> given a character string describing a function of a normalized coordinate x giving
!> the relative weighting of the regions, and an array of x values (in the range [0:1]), 
!> it returns the value of a normalized rho with mesh accumulation in the regions of
!> interest
!>
!> \author D. P. Coster
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

  function rho_with_accumulation(function_string, x)

    implicit none

    real(R8)               :: x(:), rho_with_accumulation(1:size(x))
    character (len=BUFLEN) :: function_string
    real(R8), ALLOCATABLE  :: rho_1(:), rho_2(:), rho_3(:), rho_4(:), rho_5(:)
    real(R8)               :: dummy1
    integer                :: nrho, irho

    nrho=size(x)
    allocate(rho_1(0:10*nrho), rho_2(0:10*nrho), rho_3(0:10*nrho), rho_4(nrho), rho_5(nrho))
    rho_1 = (/ (1.0_R8/(10*NRHO) * (IRHO), IRHO=0,NRHO*10) /)
    rho_2 = profile (function_string, rho_1)
    rho_3(0)=0
    do irho=1,10*nrho
       call cubint(10*nrho+1, rho_1, rho_2, 1, irho+1, rho_3(irho), dummy1)
    enddo
    rho_3=rho_3/rho_3(10*nrho)
    rho_4 = (/ (1.0_R8/(NRHO-1) * (IRHO-1), IRHO=1,NRHO) /)
    call l3interp(rho_1, rho_3, 10*nrho+1, rho_5, rho_4, nrho)
    rho_with_accumulation = rho_5
    deallocate(rho_1, rho_2, rho_3, rho_4, rho_5)

  end function rho_with_accumulation



!> itm_toolbox:profile
!>
!> given a character string describing a function of a normalized coordinate x
!> and an array of x values (in the range [0:1]), it returns the value of the profile
!>
!> \author D. P. Coster
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

  function profile(function_string, x)

    implicit none

    real(R8)               :: x(:), profile(1:size(x))
    character (len=BUFLEN) :: function_string

    integer*8              :: evaluator_create, function_descriptor
    double precision       :: evaluator_evaluate_x
    external                  evaluator_destroy

    integer                :: i

    function_descriptor = evaluator_create (trim(function_string))
    if(function_descriptor == 0) then
       write(*,*) 'Invalid function ', trim(function_string)
       stop
    endif

    do i = 1, size(x)
       profile(i) = evaluator_evaluate_x (function_descriptor, x(i))
    enddo

    call evaluator_destroy(function_descriptor)

  end function profile

#endif


  subroutine cubint ( ntab, xtab, ftab, ia_in, ib_in, result, error )
    USE ITM_TYPES, only : R8

    ! obtained from
    !     http://people.sc.fsu.edu/~jburkardt/f_src/intlib/intlib.html

    !*****************************************************************************80
    !
    !! CUBINT approximates an integral using cubic interpolation of data.
    !
    !  Discussion:
    !
    !    The integral to be approximated is
    ! 
    !      Integral ( XTAB(IB) <= X <= XTAB(IA) ) F(X) DX
    !
    !    The routine estimates the error in integration.
    !
    !  Modified:
    !
    !    10 February 2006
    !
    !  Reference:
    !
    !    Philip Davis, Philip Rabinowitz,
    !    Methods of Numerical Integration,
    !    Second Edition,
    !    Dover, 2007,
    !    ISBN: 0486453391,
    !    LC: QA299.3.D28.
    !
    !    Philip Gill, GF Miller,
    !    An algorithm for the integration of unequally spaced data,
    !    The Computer Journal, 
    !    Number 15, Number 1, 1972, pages 80-83.
    !
    !  Parameters:
    !
    !    Input, integer NTAB, the number of tabulated points.
    !    NTAB must be at least 4.
    !
    !    Input, real (R8) XTAB(NTAB), contains the points at which the
    !    function was tabulated.  XTAB should contain distinct
    !    values, given in ascending order.
    !
    !    Input, real (R8) FTAB(NTAB), contains the tabulated function
    !    values, FTAB(I) = F(XTAB(I)).
    !
    !    Input, integer IA, the entry of XTAB at which integration
    !    is to begin.  IA must be no less than 1 and no greater
    !    than NTAB.
    !
    !    Input, integer IB, the entry of XTAB at which integration
    !    is to end.  IB must be no less than 1 and no greater than
    !    NTAB.
    !
    !    Output, real (R8) RESULT, the approximate value of the
    !    integral from XTAB(IA) to XTAB(IB) of the function.
    !
    !    Output, real (R8) ERROR, an estimate of the error in
    !    integration.
    !
    implicit none

    integer ntab

    real (R8) c
    real (R8) d1
    real (R8) d2
    real (R8) d3
    real (R8) error
    real (R8) ftab(ntab)
    real (R8) h1
    real (R8) h2
    real (R8) h3
    real (R8) h4
    integer i
    integer ia, ia_in
    integer ib, ib_in
    integer ind
    integer it
    integer j
    integer k
    real (R8) r1
    real (R8) r2
    real (R8) r3
    real (R8) r4
    real (R8) result
    real (R8) s
    real (R8) term
    real (R8) xtab(ntab)

    ia = ia_in
    ib = ib_in
    result = 0.0_R8
    error = 0.0_R8

    if ( ia == ib ) then
       return
    end if

    if ( ntab < 4 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'CUBINT - Fatal error!'
       write ( *, '(a,i8)' ) '  NTAB must be at least 4, but input NTAB = ', ntab
       stop
    end if

    if ( ia < 1 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'CUBINT - Fatal error!'
       write ( *, '(a,i8)' ) '  IA must be at least 1, but input IA = ', ia
       stop
    end if

    if ( ntab < ia ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'CUBINT - Fatal error!'
       write ( *, '(a,i8)' ) '  IA must be <= NTAB, but input IA = ', ia
       stop
    end if

    if ( ib < 1 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'CUBINT - Fatal error!'
       write ( *, '(a,i8)' ) '  IB must be at least 1, but input IB = ', ib
       stop
    end if

    if ( ntab < ib ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'CUBINT - Fatal error!'
       write ( *, '(a,i8)' ) '  IB must be <= NTAB, but input IB = ', ib
       stop
    end if
    !
    !  Temporarily switch IA and IB, and store minus sign in IND
    !  so that, while integration is carried out from low X's
    !  to high ones, the sense of the integral is preserved.
    !
    if ( ib < ia ) then
       ind = -1
       it = ib
       ib = ia
       ia = it
    else
       ind = 1
    end if

    s = 0.0_R8
    c = 0.0_R8
    r4 = 0.0_R8
    j = ntab-2
    if ( ia < ntab-1 .or. ntab == 4 ) then
       j = max ( 3, ia )
    end if

    k = 4
    if ( 2 < ib .or. ntab == 4 ) then
       k = min ( ntab, ib + 2 ) - 1
    end if

    do i = j, k

       if ( i <= j ) then

          h2 = xtab(j-1) - xtab(j-2)
          d3 = ( ftab(j-1) - ftab(j-2) ) / h2
          h3 = xtab(j) - xtab(j-1)
          d1 = ( ftab(j) - ftab(j-1) ) / h3
          h1 = h2 + h3
          d2 = ( d1 - d3 ) / h1
          h4 = xtab(j+1) - xtab(j)
          r1 = ( ftab(j+1) - ftab(j) ) / h4
          r2 = ( r1 - d1 ) / ( h4 + h3 )
          h1 = h1 + h4
          r3 = (r2-d2) / h1

          if ( ia <= 1 ) then
             result = h2 * ( ftab(1) + h2 * ( 0.5_R8 * d3 - h2 &
                  * ( d2 / 6.0_R8 -(h2+h3+h3)*r3/12.0_R8)))
             s = -h2**3 * (h2*(3.0_R8*h2+5.0_R8*h4)+10.0_R8*h3*h1) / 60.0_R8
          end if

       else

          h4 = xtab(i+1) - xtab(i)
          r1 = ( ftab(i+1) - ftab(i) ) / h4
          r4 = h4 + h3
          r2 = ( r1 - d1 ) / r4
          r4 = r4 + h2
          r3 = ( r2 - d2 ) / r4
          r4 = ( r3 - d3 ) / ( r4 + h1 )

       end if

       if ( ia < i .and. i <= ib ) then

          term = h3 * ( ( ftab(i) + ftab(i-1) ) * 0.5_R8 &
               -h3 * h3 * ( d2 + r2 + ( h2 - h4 ) * r3 ) / 12.0_R8 )
          result = result + term
          c = h3**3 * ( 2.0_R8 * h3 * h3 &
               + 5.0_R8 * ( h3 * ( h4 + h2 ) + 2.0_R8 * h2 * h4 ) ) / 120.0_R8
          error = error + (c+s)*r4

          if ( i /= j ) then
             s = c
          else
             s = s + c + c
          end if

       else

          error = error + r4 * s

       end if

       if ( k <= i ) then

          if ( ntab <= ib ) then
             term = h4 * ( ftab(ntab) - h4 * ( 0.5 * r1 &
                  + h4 * ( r2 / 6.0_R8 + ( h3 + h3 + h4 ) * r3 / 12.0_R8 )))
             result = result + term
             error = error - h4**3 * r4 * &
                  ( h4 * ( 3.0_R8 * h4 + 5.0_R8 * h2 ) &
                  + 10.0_R8 * h3 * ( h2 + h3 + h4 ) ) / 60.0_R8
          end if

          if ( ntab-1 <= ib ) then
             error = error + s * r4
          end if

       else

          h1 = h2
          h2 = h3
          h3 = h4
          d1 = r1
          d2 = r2
          d3 = r3
       end if

    end do
    !
    !  Restore original values of IA and IB, reverse signs
    !  of RESULT and ERROR, to account for integration
    !  that proceeded from high X to low X.
    !
    if ( ind /= 1 ) then
       it = ib
       ib = ia
       ia = it
       result = -result
       error = -error
    end if

    return
  end subroutine cubint

  subroutine avint ( ntab, xtab, ytab, a, b, result )
    USE ITM_TYPES, only : R8

    ! obtained from
    !     http://people.sc.fsu.edu/~jburkardt/f_src/intlib/intlib.html

    !*****************************************************************************80
    !
    !! AVINT estimates the integral of unevenly spaced data.
    !
    !  Discussion:
    !
    !    The data is given as NTAB pairs of values 
    !    ( XTAB(1:NTAB), YTAB(1:NTAB) ).
    !
    !    The quadrature method uses overlapping parabolas and smoothing.
    !
    !  Modified:
    !
    !    10 February 2006
    !
    !  Reference:
    !
    !    Philip Davis, Philip Rabinowitz,
    !    Methods of Numerical Integration,
    !    Second Edition,
    !    Dover, 2007,
    !    ISBN: 0486453391,
    !    LC: QA299.3.D28.
    !
    !    Paul Hennion,
    !    Algorithm 77:
    !    Interpolation, Differentiation and Integration,
    !    Communications of the ACM,
    !    Volume 5, page 96, 1962.
    !
    !  Parameters:
    !
    !    Input, integer NTAB, the number of entries in XTAB and
    !    YTAB.  NTAB must be at least 2.
    !
    !    Input, real (R8) XTAB(NTAB), the abscissas at which the
    !    function values are given.  The XTAB's must be distinct
    !    and in ascending order.
    !
    !    Input, real (R8) YTAB(NTAB), the function values,
    !    YTAB(I) = F(XTAB(I)).
    !
    !    Input, real (R8) A, the lower limit of integration.  A should
    !    be, but need not be, near one endpoint of the interval
    !    (X(1), X(NTAB)).
    !
    !    Input, real (R8) B, the upper limit of integration.  B should
    !    be, but need not be, near one endpoint of the interval
    !    (X(1), X(NTAB)).
    !
    !    Output, real (R8) RESULT, the approximate value of the integral.
    !
    implicit none

    integer ntab

    real (R8) a
    real (R8) b
    real (R8) ba
    real (R8) bb
    real (R8) bc
    real (R8) ca
    real (R8) cb
    real (R8) cc
    real (R8) fa
    real (R8) fb
    integer i
    integer inlft
    integer inrt
    integer istart
    integer istop
    real (R8) result
    real (R8) slope
    real (R8) syl
    real (R8) syl2
    real (R8) syl3
    real (R8) syu
    real (R8) syu2
    real (R8) syu3
    real (R8) term1
    real (R8) term2
    real (R8) term3
    real (R8) total
    real (R8) x1
    real (R8) x12
    real (R8) x13
    real (R8) x2
    real (R8) x23
    real (R8) x3
    real (R8) xtab(ntab)
    real (R8) ytab(ntab)

    result = 0.0_R8

    if ( a == b ) then
       return
    end if

    if ( b < a ) then
    end if

    if ( ntab < 2 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'AVINT - Fatal error!'
       write ( *, '(a,i8)' ) '  NTAB is less than 3.  NTAB = ', ntab
       stop
    end if

    do i = 2, ntab

       if ( xtab(i) <= xtab(i-1) ) then
          write ( *, '(a)' ) ' '
          write ( *, '(a)' ) 'AVINT - Fatal error!'
          write ( *, '(a)' ) '  XTAB(I) is not greater than XTAB(I-1).'
          write ( *, '(a,i8)' ) '  Here, I = ', I
          write ( *, '(a,g14.6)' ) '  XTAB(I-1) = ', xtab(i-1)
          write ( *, '(a,g14.6)' ) '  XTAB(I) =   ', xtab(i)
          stop
       end if

    end do
    !
    !  Special case for NTAB = 2.
    !
    if ( ntab == 2 ) then
       slope = ( ytab(2) - ytab(1) ) / ( xtab(2) - xtab(1) )
       fa = ytab(1) + slope * ( a - xtab(1) )
       fb = ytab(2) + slope * ( b - xtab(2) )
       result = 0.5_R8 * ( fa + fb ) * ( b - a )
       return
    end if

    if ( xtab(ntab-2) < a .or. b < xtab(3) ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'AVINT - Fatal error!'
       write ( *, '(a)' ) '  There were less than 3 function values'
       write ( *, '(a)' ) '  between the limits of integration.'
       stop
    end if

    i = 1
    do

       if ( a <= xtab(i) ) then
          exit
       end if

       i = i + 1

    end do

    inlft = i

    i = ntab

    do

       if ( xtab(i) <= b ) then
          exit
       end if

       i = i - 1

    end do

    inrt = i

    if ( inrt - inlft < 2 ) then
       write ( *, '(a)' ) ' '
       write ( *, '(a)' ) 'AVINT - Fatal error!'
       write ( *, '(a)' ) '  There were less than 3 function values'
       write ( *, '(a)' ) '  between the limits of integration.'
       stop
    end if

    if ( inlft == 1 ) then
       istart = 2
    else
       istart = inlft
    end if

    if ( inrt == ntab ) then
       istop = ntab - 1
    else
       istop = inrt
    end if

    total = 0.0_R8

    syl = a
    syl2 = syl * syl
    syl3 = syl2 * syl

    do i = istart, istop

       x1 = xtab(i-1)
       x2 = xtab(i)
       x3 = xtab(i+1)

       x12 = x1 - x2
       x13 = x1 - x3
       x23 = x2 - x3

       term1 =   ( ytab(i-1) ) / ( x12 * x13 )
       term2 = - ( ytab(i)   ) / ( x12 * x23 )
       term3 =   ( ytab(i+1) ) / ( x13 * x23 )

       ba = term1 + term2 + term3
       bb = - ( x2 + x3 ) * term1 - ( x1 + x3 ) * term2 - ( x1 + x2 ) * term3
       bc = x2 * x3 * term1 + x1 * x3 * term2 + x1 * x2 * term3

       if ( i == istart ) then
          ca = ba
          cb = bb
          cc = bc
       else
          ca = 0.5_R8 * ( ba + ca )
          cb = 0.5_R8 * ( bb + cb )
          cc = 0.5_R8 * ( bc + cc )
       end if

       syu = x2
       syu2 = syu * syu
       syu3 = syu2 * syu

       total = total + ca * ( syu3 - syl3 ) / 3.0_R8 &
            + cb * ( syu2 - syl2 ) / 2.0_R8 &
            + cc * ( syu  - syl )
       ca = ba
       cb = bb
       cc = bc

       syl  = syu
       syl2 = syu2
       syl3 = syu3

    end do

    syu = b
    syu2 = syu * syu
    syu3 = syu2 * syu

    result = total + ca * ( syu3 - syl3 ) / 3.0_R8 &
         + cb * ( syu2 - syl2 ) / 2.0_R8 &
         + cc * ( syu  - syl  )

    return
  end subroutine avint

!> 3rd order interpolation
!>
!> \author B. Scott
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

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


!> derivative based on a 3rd order interpolation
!>
!> \author B. Scott
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

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


!> 1st order derivative
!>
!> \author B. Scott
!>
!> \version "$Id: itm_toolbox.F90 635 2013-08-06 11:23:05Z olivh $"

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

end module itm_toolbox
