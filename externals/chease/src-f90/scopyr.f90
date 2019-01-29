!*DECK U49
!*CALL PROCESS
SUBROUTINE SCOPYR(RF, N, X, NX, Y, NY) 
  !        --------------------------------------
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !
  ! Y = Y + RF * (X - Y)
  ! X IS INCREMENTED BY NX
  ! Y IS INCREMENTED BY NY
  !
  !
  USE prec_const
  IMPLICIT NONE
  INTEGER          ::     J
  INTEGER          ::     NM1
  REAL(RKIND)      ::     X
  REAL(RKIND)      ::     RF
  REAL(RKIND)      ::     Y
  INTEGER          ::     NY
  INTEGER          ::     NX
  INTEGER          ::     N
  DIMENSION &
       &   X(N*NX), Y(N*NY)
  !
  IF (N .LE. 0) RETURN
  !
  Y(1) = Y(1) + RF*(X(1)-Y(1))
  !
  IF (N .EQ. 1) RETURN
  !
  IF (NX.LT.0 .OR. NY.LT.0) THEN
     PRINT*,'NEGATIVE INCREMENTS FORBIDDEN'
     STOP
  ENDIF
  !
  NM1 = N - 1
  !
  DO J=1,NM1 
     Y(J*NY+1) = Y(J*NY+1) + RF*(X(J*NX+1)-Y(J*NY+1))
  END DO
  !
  RETURN
END SUBROUTINE SCOPYR
