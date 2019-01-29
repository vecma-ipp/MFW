!*DECK MAT11
!*CALL PROCESS
SUBROUTINE CCOPY(N,CX,NX,CY,NY)
  !        -------------------------------
  !
  ! COPIES COMPLEX ARRAY CX INTO COMPLEX ARRAY CY.
  ! CX IS INCREMENTED BY NX AND CY BY NY.
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J
  INTEGER          ::     NM1
  INTEGER          ::     NY
  INTEGER          ::     NX
  INTEGER          ::     N
  COMPLEX(ckind)    CX, CY
  DIMENSION &
       &   CX(N*NX),  CY(N*NY)
  !
  IF (N .LE. 0) RETURN
  !
  CY(1) = CX(1)
  !
  IF (N .EQ. 1) RETURN
  !
  NM1 = N - 1
  !
  DO J=1,NM1
     CY(J*NY+1) = CX(J*NX+1)
  END DO
  !
  RETURN
END SUBROUTINE CCOPY
