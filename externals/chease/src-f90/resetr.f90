!*DECK U40
!*CALL PROCESS
SUBROUTINE RESETR(PA,KDIM,PVALUE)
  !        #################################
  !
  ! U.40   RESET REAL ARRAY TO SPECIFIED VALUE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)   	::  	PVALUE 	! <resetr.f90>
  REAL(RKIND)   	::  	PA 	! <resetr.f90>
  INTEGER       	::  	J1 	! <resetr.f90>
  INTEGER       	::  	KDIM 	! <resetr.f90>
  DIMENSION &
       &   PA(KDIM)
  !
  DO J1=1,KDIM
     !
     PA(J1) = PVALUE
     !
  END DO
  !
  RETURN
END SUBROUTINE RESETR
