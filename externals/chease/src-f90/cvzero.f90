!*DECK MAT8
!*CALL PROCESS
SUBROUTINE CVZERO(CV,N)
  !        -------------------------
  !
  ! SET COMPLEX ARRAY CV TO 0.
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J
  INTEGER          ::     N
  COMPLEX(ckind)    CV
  DIMENSION &
       &   CV(N)
  !
  DO J=1,N
     CV(J) = (0._RKIND,0._RKIND)
  END DO
  !
  RETURN
END SUBROUTINE CVZERO
