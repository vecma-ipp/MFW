!*DECK CRAY03
!*CALL PROCESS
FUNCTION ISAMIN(N,PV,NX)
  !        ------------------------
  !
  !  FIND ELEMENT WITH MINIMUM ABSOLUTE VALUE IN REAL ARRAY PV.
  !  PV IS INCREMENTED BY NX
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PV
  INTEGER          ::     I
  INTEGER          ::     J
  INTEGER          ::     NM1
  INTEGER          ::     ISM
  INTEGER          ::     ISAMIN
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION PV(N*NX)
  !
  ISAMIN = 0
  IF (N .LE. 0) RETURN
  !
  ISM = 1
  !
  IF (N .EQ. 1) GOTO 2
  !
  NM1 = N - 1
  !
  DO J=1,NM1
     I = J * NX + 1
     IF (ABS(PV(I)) .LT. ABS(PV(ISM))) ISM = I         
  END DO
2 CONTINUE   
  !
  ISAMIN = ISM
  !
  RETURN
END FUNCTION ISAMIN
