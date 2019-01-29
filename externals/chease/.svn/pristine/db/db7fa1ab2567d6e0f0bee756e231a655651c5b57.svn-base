!*DECK U2
!*CALL PROCESS
SUBROUTINE BLINES(K)
  !        --------------------
  !
  !  INSERT K BLANK LINES ON OUTPUT CHANEL UNIT 6
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     K
  INTEGER          ::     J
  DO J=1,K
     WRITE (6,9900)
  END DO
  !
  RETURN
9900 FORMAT(' ')
END SUBROUTINE BLINES
