!*DECK CRAY07
!*CALL PROCESS
!..................................................................
!     The following blas routines are included here so that the
!     code can be compiled on the HP with default double precision
!     (using autodblpad option), but with calls to the standard 
!     single precision subroutine names.
!..................................................................
!**** SUBROUTINE  DCOPY  (N,SX,INCX,SY,INCY)
!   IMSL ROUTINE NAME   - VBLA=DCOPY
!
!-----------------------------------------------------------------------
!
!   COMPUTER            - CRAY/SINGLE
!
!   LATEST REVISION     - JANUARY 1, 1978
!
!   PURPOSE             - COPY A VECTOR X TO A VECTOR Y, BOTH
!                           SINGLE PRECISION
!
!   USAGE               - CALL DCOPY (N,SX,INCX,SY,INCY)
!
!   ARGUMENTS    N      - LENGTH OF VECTORS X AND Y. (INPUT)
!                SX     - REAL VECTOR OF LENGTH MAX(N*IABS(INCX),1).
!                           (INPUT)
!                INCX   - DISPLACEMENT BETWEEN ELEMENTS OF SX. (INPUT)
!                           X(I) IS DEFINED TO BE..
!                           SX(1+(I-1)*INCX) IF INCX.GE.0 OR
!                           SX(1+(I-N)*INCX) IF INCX.LT.0.
!                SY     - REAL VECTOR OF LENGTH MAX(N*IABS(INCY),1).
!                           (OUTPUT)
!                           DCOPY COPIES X(I) TO Y(I) FOR I=1,...,N.
!                           X(I) AND Y(I) REFER TO SPECIFIC ELEMENTS
!                           OF SX AND SY, RESPECTIVELY. SEE INCX AND
!                           INCY ARGUMENT DESCRIPTIONS.
!                INCY   - DISPLACEMENT BETWEEN ELEMENTS OF SY. (INPUT)
!                           Y(I) IS DEFINED TO BE..
!                           SY(1+(I-1)*INCY) IF INCY.GE.0 OR
!                           SY(1+(I-N)*INCY) IF INCY.LT.0.
!
!   PRECISION/HARDWARE  - SINGLE/ALL
!
!   REQD. IMSL ROUTINES - NONE REQUIRED
!
!   NOTATION            - INFORMATION ON SPECIAL NOTATION AND
!                           CONVENTIONS IS AVAILABLE IN THE MANUAL
!                           INTRODUCTION OR THROUGH IMSL ROUTINE UHELP
!
!   COPYRIGHT           - 1978 BY IMSL, INC. ALL RIGHTS RESERVED.
!
!   WARRANTY            - IMSL WARRANTS ONLY THAT IMSL TESTING HAS BEEN
!                           APPLIED TO THIS CODE. NO OTHER WARRANTY,
!                           EXPRESSED OR IMPLIED, IS APPLICABLE.
!
!-----------------------------------------------------------------------
!
SUBROUTINE  DCOPY  (N,SX,INCX,SY,INCY)
  !
  !                                  SPECIFICATIONS FOR ARGUMENTS
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     SX
  REAL(RKIND)      ::     SY
  INTEGER            N,INCX,INCY
  DIMENSION          SX(*),SY(*)
  !                                  SPECIFICATIONS FOR LOCAL VARIABLES
  INTEGER            I,IY,M,MP1,NS,IX
  !                                  FIRST EXECUTABLE STATEMENT
  IF (N.LE.0) RETURN
  IF (INCX.EQ.INCY) IF (INCX-1) 5,15,35
5 CONTINUE 
  !                                  CODE FOR UNEQUAL OR NONPOSITIVE
  !                                    INCREMENTS.
  IX = 1
  IY = 1
  IF (INCX.LT.0) IX = (-N+1)*INCX+1
  IF (INCY.LT.0) IY = (-N+1)*INCY+1
  DO I=1,N
     SY(IY) = SX(IX)
     IX = IX+INCX
     IY = IY+INCY
  END DO
  RETURN
  !                                  CODE FOR BOTH INCREMENTS EQUAL TO 1
  !                                    CLEAN-UP LOOP SO REMAINING VECTOR
  !                                    LENGTH IS A MULTIPLE OF 7.
15 M = N-(N/7)*7
  IF (M.EQ.0) GO TO 25
  DO I=1,M
     SY(I) = SX(I)
  END DO
  IF (N.LT.7) RETURN
25 MP1 = M+1
  DO I=MP1,N,7
     SY(I) = SX(I)
     SY(I+1) = SX(I+1)
     SY(I+2) = SX(I+2)
     SY(I+3) = SX(I+3)
     SY(I+4) = SX(I+4)
     SY(I+5) = SX(I+5)
     SY(I+6) = SX(I+6)
  END DO
  RETURN
  !                                  CODE FOR EQUAL, POSITIVE, NONUNIT
  !                                    INCREMENTS.
35 CONTINUE 
  NS = N*INCX
  DO I=1,NS,INCX
     SY(I) = SX(I)
  END DO
  RETURN
END SUBROUTINE DCOPY
