!*DECK U31
!*CALL PROCESS
SUBROUTINE IARRAY(KNAME,KA,KDIM)
  !        ---------- ------
  !
  ! U.31   PRINT NAME AND VALUES OF INTEGER ARRAY
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J
  INTEGER          ::     KA
  INTEGER          ::     KDIM
  CHARACTER*(*) KNAME
  DIMENSION KA(KDIM)
  !
  CALL BLINES(1)
  WRITE (6,9900) KNAME
  WRITE (6,9901) (KA(J),J=1,KDIM)
  !
  RETURN
9900 FORMAT(1X,A)
9901 FORMAT((1X,8(I13)))
END SUBROUTINE IARRAY
