!*DECK U20
!*CALL PROCESS
SUBROUTINE RVAR(KNAME,PVALUE)
  !        -----------------------------
  !**********************************************************************
  !                                                                     *
  ! U.20   PRINT NAME AND VALUE OF REAL VARIABLE                        *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  REAL(RKIND)      ::     PVALUE
  CHARACTER*(*) KNAME
  !
  WRITE (6,9900) KNAME, PVALUE
  !
  RETURN
9900 FORMAT(/,1X,A,' =',1PE17.8)
END SUBROUTINE RVAR
