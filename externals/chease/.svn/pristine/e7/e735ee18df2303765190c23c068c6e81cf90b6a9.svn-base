!*DECK CRAY06
!*CALL PROCESS
!
! ------------------------------------------------------------
!
SUBROUTINE DAXPY (N, SA, SX, INCX, SY, INCY)
  !***BEGIN PROLOGUE  DAXPY
  !***PURPOSE  Compute a constant times a vector plus a vector.
  !***LIBRARY   SLATEC (BLAS)
  !***CATEGORY  D1A7
  !***TYPE      SINGLE PRECISION (DAXPY-S, DAXPY-D, CAXPY-C)
  !***KEYWORDS  BLAS, LINEAR ALGEBRA, TRIAD, VECTOR
  !***AUTHOR  Lawson, C. L., (JPL)
  !           Hanson, R. J., (SNLA)
  !           Kincaid, D. R., (U. of Texas)
  !           Krogh, F. T., (JPL)
  !***DESCRIPTION
  !
  !                B L A S  Subprogram
  !    Description of Parameters
  !
  !     --Input--
  !        N  number of elements in input vector(s)
  !       SA  single precision scalar multiplier
  !       SX  single precision vector with N elements
  !     INCX  storage spacing between elements of SX
  !       SY  single precision vector with N elements
  !     INCY  storage spacing between elements of SY
  !
  !     --Output--
  !       SY  single precision result (unchanged if N .LE. 0)
  !
  !     Overwrite single precision SY with single precision SA*SX +SY.
  !     For I = 0 to N-1, replace  SY(LY+I*INCY) with SA*SX(LX+I*INCX) +
  !       SY(LY+I*INCY),
  !     where LX = 1 if INCX .GE. 0, else LX = 1+(1-N)*INCX, and LY is
  !     defined in a similar way using INCY.
  !
  !***REFERENCES  C. L. Lawson, R. J. Hanson, D. R. Kincaid and F. T.
  !                 Krogh, Basic linear algebra subprograms for Fortran
  !                 usage, Algorithm No. 539, Transactions on Mathematical
  !                 Software 5, 3 (September 1979), pp. 308-323.
  !***ROUTINES CALLED  (NONE)
  !***REVISION HISTORY  (YYMMDD)
  !   791001  DATE WRITTEN
  !   890831  Modified array declarations.  (WRB)
  !   890831  REVISION DATE from Version 3.2
  !   891214  Prologue converted to Version 4.0 format.  (BAB)
  !   920310  Corrected definition of LX in DESCRIPTION.  (WRB)
  !   920501  Reformatted the REFERENCES section.  (WRB)
  !***END PROLOGUE  DAXPY
  USE prec_const
  IMPLICIT NONE
  INTEGER          ::     NS
  INTEGER          ::     MP1
  INTEGER          ::     M
  REAL(RKIND)      ::     SX
  REAL(RKIND)      ::     SY
  INTEGER          ::     I
  INTEGER          ::     IY
  INTEGER          ::     IX
  INTEGER          ::     INCY
  INTEGER          ::     INCX
  REAL(RKIND)      ::     SA
  INTEGER          ::     N
  DIMENSION SX(*), SY(*)
  !%OS      REAL SX(*), SY(*), SA
  !***FIRST EXECUTABLE STATEMENT  DAXPY
  IF (N.LE.0 .OR. SA.EQ.0.0E0_RKIND) RETURN
  IF (INCX .EQ. INCY) IF (INCX-1) 5,20,60
  !
  !     Code for unequal or nonpositive increments.
  !
5 IX = 1
  IY = 1
  IF (INCX .LT. 0) IX = (-N+1)*INCX + 1
  IF (INCY .LT. 0) IY = (-N+1)*INCY + 1
  DO I = 1,N
     SY(IY) = SY(IY) + SA*SX(IX)
     IX = IX + INCX
     IY = IY + INCY
  END DO
  RETURN
  !
  !     Code for both increments equal to 1.
  !
  !     Clean-up loop so remaining vector length is a multiple of 4.
  !
20 M = MOD(N,4)
  IF (M .EQ. 0) GO TO 40
  DO I = 1,M
     SY(I) = SY(I) + SA*SX(I)
  END DO
  IF (N .LT. 4) RETURN
40 MP1 = M + 1
  DO I = MP1,N,4
     SY(I) = SY(I) + SA*SX(I)
     SY(I+1) = SY(I+1) + SA*SX(I+1)
     SY(I+2) = SY(I+2) + SA*SX(I+2)
     SY(I+3) = SY(I+3) + SA*SX(I+3)
  END DO
  RETURN
  !
  !     Code for equal, positive, non-unit increments.
  !
60 NS = N*INCX
  DO I = 1,NS,INCX
     SY(I) = SA*SX(I) + SY(I)
  END DO
  RETURN
END SUBROUTINE DAXPY
