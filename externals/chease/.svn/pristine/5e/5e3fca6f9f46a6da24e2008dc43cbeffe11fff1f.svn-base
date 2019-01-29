!*DECK CRAY10
!*CALL PROCESS
REAL*8 FUNCTION SSUM(N,PV,NX)
  !        ----------------------
  !
  !  SUMS ALL ELEMENTS OF REAL ARRAY PV
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J
  INTEGER          ::     NM1
  REAL(RKIND)      ::     PV
  INTEGER          ::     I
  REAL(RKIND)      ::     SS
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION &
       &   PV(N*NX)
  !
  SS = 0._RKIND
  !
  IF (N .LE. 0) GOTO 2
  !
  I = 1
  IF (NX .LT. 0) STOP
  SS = PV(I)
  !
  IF (N .EQ. 1) GOTO 2
  !
  NM1 = N - 1
  !
  DO J=1,NM1
     I = I + NX
     SS = SS + PV(I)
  END DO
2 CONTINUE 
  !   
  SSUM = SS
  !
  RETURN
END FUNCTION SSUM
