!*DECK MAT7
!*CALL PROCESS
SUBROUTINE VZERO(PV,N)
  !        ----------------------
  !
  !  SET REAL ARRAY PV TO ZERO.
  !
  USE prec_const
  IMPLICIT NONE
  REAL(RKIND)      ::     PV
  INTEGER          ::     J
  INTEGER          ::     N
  DIMENSION &
       &   PV(N)
  !
  DO J=1,N
     PV(J) = 0._RKIND
  END DO
  !
  RETURN
END SUBROUTINE VZERO
