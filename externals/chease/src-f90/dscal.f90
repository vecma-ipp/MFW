!*DECK CRAY09
!*CALL PROCESS

!**** SUBROUTINE DSCAL  (N,SA,SX,INCX)
!   IMSL ROUTINE NAME   - VBLA=SSCAL
!
!-----------------------------------------------------------------------
!
!   COMPUTER            - CRAY/SINGLE
!
!   LATEST REVISION     - JANUARY 1, 1978
!
!   PURPOSE             - COMPUTE A SINGLE PRECISION CONSTANT
!                           TIMES A SINGLE PRECISION VECTOR
!
!   USAGE               - CALL SSCAL (N,SA,SX,INCX)
!
!   ARGUMENTS    N      - LENGTH OF VECTOR X. (INPUT)
!                SA     - REAL SCALAR. (INPUT)
!                SX     - REAL VECTOR OF LENGTH N*INCX. (INPUT/OUTPUT)
!                           SSCAL REPLACES X(I) WITH SA*X(I) FOR
!                           I=1,...,N.
!                           X(I) REFERS TO A SPECIFIC ELEMENT OF SX.
!                           SEE INCX ARGUMENT DESCRIPTION.
!                INCX   - DISPLACEMENT BETWEEN ELEMENTS OF SX. (INPUT)
!                           X(I) IS DEFINED TO BE SX(1+(I-1)*INCX).
!                           INCX MUST BE GREATER THAN ZERO.
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
SUBROUTINE DSCAL  (N,SA,SX,INCX)
  !
  !                                  SPECIFICATIONS FOR ARGUMENTS
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     SA
  REAL(RKIND)      ::     SX
  INTEGER            INCX,N
  DIMENSION          SX(*)
  !%OS      REAL          SA,SX(*)
  !                                  SPECIFICATIONS FOR LOCAL VARIABLES
  INTEGER            I,M,MP1,NS
  !                                  FIRST EXECUTABLE STATEMENT
  IF (N.LE.0) RETURN
  IF( N.LE.0 .OR. INCX.LE.0 )RETURN
  IF (INCX.EQ.1) GO TO 10
  !                                  CODE FOR INCREMENTS NOT EQUAL TO 1.
  NS = N*INCX
  DO I=1,NS,INCX
     SX(I) = SA*SX(I)
  END DO
  RETURN
  !                                  CODE FOR INCREMENTS EQUAL TO 1.
  !                                    CLEAN-UP LOOP SO REMAINING VECTOR
  !                                    LENGTH IS A MULTIPLE OF 5.
10 M = N-(N/5)*5
  IF (M.EQ.0) GO TO 20
  DO I=1,M
     SX(I) = SA*SX(I)
  END DO
  IF (N.LT.5) RETURN
20 MP1 = M+1
  DO I=MP1,N,5
     SX(I) = SA*SX(I)
     SX(I+1) = SA*SX(I+1)
     SX(I+2) = SA*SX(I+2)
     SX(I+3) = SA*SX(I+3)
     SX(I+4) = SA*SX(I+4)
  END DO
  RETURN
END SUBROUTINE DSCAL
