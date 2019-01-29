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
end
