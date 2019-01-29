!*DECK CRAY11
!*CALL PROCESS
FUNCTION ISSUM(N,IV,NX)
  !        -----------------------
  !
  !  SUMS ALL ELEMENTS OF INTEGER ARRAY IC
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J
  INTEGER          ::     NM1
  INTEGER          ::     IV
  INTEGER          ::     I
  INTEGER          ::     IS
  INTEGER          ::     ISSUM
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION &
       &   IV(N*NX)
  !
  IS = 0
  !
  IF (N .LE. 0) GOTO 2
  !
  I = 1
  IF (NX .LT. 0) STOP
  IS = IV(I)
  !
  IF (N .EQ. 1) GOTO 2
  !
  NM1 = N - 1
  !
  DO J=1,NM1
     I = I + NX
     IS = IS + IV(I)
  END DO
2 CONTINUE 
  !
  ISSUM = IS
  !
  RETURN
END FUNCTION ISSUM
