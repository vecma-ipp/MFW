!*DECK CRAY05
!*CALL PROCESS
FUNCTION ISRCHFGE(N,PV,NX,TARGET)
  !        ---------------------------------
  !
  !  FIND FIRST ELEMENT IN REAL ARRAY PV WHICH IS GREATER OR EQUAL
  !  THAN TARGET. PV IS INCREMENTED BY NX
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     TARGET
  REAL(RKIND)      ::     PV
  INTEGER          ::     J
  INTEGER          ::     I
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION &
       &   PV(N*NX)
  !
  I = 1
  IF (NX .LT. 0) STOP
  !
  DO J=1,N
     IF (PV(I) .GE. TARGET) GOTO 2
     I = I + NX
  END DO
2 ISRCHFGE = I
  !
  RETURN
END FUNCTION ISRCHFGE
FUNCTION ISRCHFGT(N,PV,NX,TARGET)
  !        ---------------------------------
  !
  !  FIND FIRST ELEMENT IN REAL ARRAY PV WHICH IS GREATER OR EQUAL
  !  THAN TARGET. PV IS INCREMENTED BY NX
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     TARGET
  REAL(RKIND)      ::     PV
  INTEGER          ::     J
  INTEGER          ::     I
  INTEGER          ::     ISRCHFGT
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION &
       &   PV(N*NX)
  !
  I = 1
  IF (NX .LT. 0) STOP
  !
  DO J=1,N
     IF (PV(I) .GT. TARGET) GOTO 2
     I = I + NX
  END DO
2 ISRCHFGT = I
  !
  RETURN
END FUNCTION ISRCHFGT
