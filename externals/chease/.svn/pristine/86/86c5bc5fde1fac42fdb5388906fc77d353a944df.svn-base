!*DECK U41
!*CALL PROCESS
SUBROUTINE RESETI(KA,KDIM,KVALUE)
  !        #################################
  !
  ! U.41   RESET INTEGER ARRAY TO SPECIFIED VALUE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  USE globals
  IMPLICIT NONE
  INTEGER       	::  	KVALUE 	! <reseti.f90>
  INTEGER       	::  	KA 	! <reseti.f90>
  INTEGER       	::  	J1 	! <reseti.f90>
  INTEGER       	::  	KDIM 	! <reseti.f90>
  DIMENSION &
       &   KA(KDIM)
  !
  DO J1=1,KDIM
     !
     KA(J1) = KVALUE
     !
  END DO
  !
  RETURN
END SUBROUTINE RESETI
