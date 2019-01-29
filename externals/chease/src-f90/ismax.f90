!*DECK CRAY01
!*CALL PROCESS
FUNCTION ISMAX(N,PV,NX)
  !        -----------------------
  !
  !  FIND ELEMENT WITH MINIMUM VALUE IN REAL ARRAY PV
  !  PV IS INCREMENTED BY NX
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PV
  INTEGER          ::     I
  INTEGER          ::     J
  INTEGER          ::     NM1
  INTEGER          ::     ISM
  INTEGER          ::     ISMAX
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION PV(N*NX)
  !
  ISMAX = 0
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
     IF (PV(I) .GT. PV(ISM)) ISM = I         
  END DO
2 CONTINUE   
  !
  ISMAX = ISM
  !
  RETURN
END FUNCTION ISMAX
