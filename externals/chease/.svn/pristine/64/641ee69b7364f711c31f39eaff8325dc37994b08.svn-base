!*DECK U35
!*CALL PROCESS
SUBROUTINE OARRAY(NCHAN,KNAME,PA,KDIM)
  !        ---------- ------
  !
  ! U.35   SAME AS RARRAY BUT TO FILE NUMBER NCHAN
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     J
  REAL(RKIND)      ::     PA
  INTEGER          ::     NCHAN
  INTEGER          ::     KDIM
  CHARACTER*(*) KNAME
  DIMENSION &
       &   PA(KDIM)
  !
  WRITE (NCHAN,9900) KNAME
  WRITE (NCHAN,9901) (PA(J),J=1,KDIM)
  !
  RETURN
9900 FORMAT(1X,A)
9901 FORMAT((1X,10(1PE13.4)))
END SUBROUTINE OARRAY
