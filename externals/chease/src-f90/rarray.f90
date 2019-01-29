!*DECK U30 
!*CALL PROCESS
SUBROUTINE RARRAY(KNAME,PA,KDIM)
  !        ---------- ------
  !
  ! U.30   PRINT NAME AND VALUES OF REAL ARRAY
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !
  USE globals
  IMPLICIT NONE
  INTEGER, INTENT(IN)       :: KDIM
  REAL(RKIND), INTENT(IN)   :: PA(KDIM)
  CHARACTER*(*), INTENT(IN) :: KNAME
  INTEGER                   :: J
!!$  DIMENSION &
!!$       &   PA(KDIM)
  !
  CALL BLINES(1)
  WRITE (6,9900) KNAME
  WRITE (6,9901) (PA(J),J=1,KDIM)
  !
  RETURN
9900 FORMAT(1X,A)
9901 FORMAT((1X,8(1PE13.4)))
END SUBROUTINE RARRAY
