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
end
