!*DECK C2SD03
!*CALL PROCESS
SUBROUTINE LIMITA
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SD03 IMPOSE BOUNDARY CONDITIONS IN A. (SEE EQ. (30) IN PAPER)     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J2
  INTEGER          ::     IBAND2
  INTEGER          ::     IBAND1
  INTEGER          ::     IDIAG
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZC
  DIMENSION &
       &   ZC(N4NT,3)
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  CALL CENTER(ZC)
  CALL IDENTA(3,ZC)
  !
  DO J1=1,4*NT-3
     !
     IDIAG  = J1
     IBAND1 = 1
     IBAND2 = IDIAG + NBAND - 1
     !
     CALL AWAY(IDIAG,IBAND1,IBAND2)
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 5. PLASMA OUTSIDE BOUNDARY CONDITION : PSI = ZERO                   *
  !                                                                     *
  !**********************************************************************
  !
  DO J2=1,NT
     !
     IDIAG  = 4 * (NS * NT + J2 - 1) + 1
     IBAND1 = IDIAG - NBAND + 1
     IBAND2 = IDIAG + NBAND - 1
     !
     CALL AWAY(IDIAG,IBAND1,IBAND2)
     !
     IDIAG  = 4 * (NS * NT + J2 - 1) + 3
     IBAND1 = IDIAG - NBAND + 1
     IBAND2 = IDIAG + NBAND - 1
     !
     CALL AWAY(IDIAG,IBAND1,IBAND2)
     !
  END DO
  !
  RETURN
END SUBROUTINE LIMITA
