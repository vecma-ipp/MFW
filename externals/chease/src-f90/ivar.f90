!*DECK U23
!*CALL PROCESS
SUBROUTINE IVAR(KNAME,KVALUE)
  !        #############################
  !**********************************************************************
  !                                                                     *
  ! U.23   PRINT NAME AND VALUE OF INTEGER VARIABLE                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  INTEGER          ::     KVALUE
  CHARACTER*(*) KNAME
  !
  WRITE (6,9900) KNAME, KVALUE
  !
  RETURN
9900 FORMAT(/,1X,A,' =',I12)
END SUBROUTINE IVAR
