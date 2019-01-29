!*DECK CRAY02
!*CALL PROCESS
FUNCTION ISMIN(N,PV,NX)
  !        -----------------------
  !
  !  FIND ELEMENT WITH MAXIMUM VALUE IN REAL ARRAY PV.
  !  PV IS INCREMENTED BY NX
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PV
  INTEGER          ::     I
  INTEGER          ::     J
  INTEGER          ::     NM1
  INTEGER          ::     ISM
  INTEGER          ::     ISMIN
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION PV(N*NX)
  !
  ISMIN = 0
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
     IF (PV(I) .LT. PV(ISM)) ISM = I         
  END DO
2 CONTINUE   
  !
  ISMIN = ISM
  !
  RETURN
END FUNCTION ISMIN
