!*DECK MAT9
!*CALL PROCESS
SUBROUTINE ACOPY(N,X,NX,Y,NY)
  !        -----------------------------
  !
  ! COPIES REAL ARRAY X INTO REAL ARRAY Y WITH INTERMEDIATE
  ! STORAGE IN ZX. X IS INCREMENTED BY NX AND Y BY NY.
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     NY
  INTEGER          ::     NX
  REAL(RKIND),allocatable ::     ZX(:)
  INTEGER          ::     J
  INTEGER          ::     NM1
  REAL(RKIND)      ::     X(*)
  REAL(RKIND)      ::     Y(*)
  INTEGER          ::     N
  !
  IF (N .LE. 0) RETURN
  !
  Y(1) = X(1)
  !
  IF (N .EQ. 1) RETURN
  !
  NM1 = N - 1
  !
  ALLOCATE(ZX(NM1))
  DO J=1,NM1
     ZX(J) = X(J*NX+1) 
  END DO
  !
  DO J=1,NM1
     Y(J*NY+1) = ZX(J)
  END DO
  deallocate(zx)
  !
  RETURN
END SUBROUTINE ACOPY
