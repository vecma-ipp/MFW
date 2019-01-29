!*DECK C2SD08
!*CALL PROCESS
SUBROUTINE CENTER(PC)
  !        #####################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SD08 EVALUATE THE COFFICIENTS REQUIRED TO IMPOSE BOUNDARY         *
  !        CONDITIONS (SEE EQ. (30) IN THE PUBLICATION)                 *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     I
  REAL(RKIND)      ::     ZSINT
  REAL(RKIND)      ::     ZCOST
  INTEGER          ::     J1
  REAL(RKIND)      ::     PC
  DIMENSION &
       &   PC(N4NT,3)
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  CALL VZERO(PC,12*NPT)
  !
  DO J1=1,NT
     !
     ZCOST = COS(CT(J1))
     ZSINT = SIN(CT(J1))
     !
     I = 4 * (NUPDWN(J1) - 1)
     !
     PC(I+1,1) = 1
     PC(I+2,2) = RHOS(J1) * ZCOST
     PC(I+2,3) = RHOS(J1) * ZSINT
     PC(I+4,2) = DRSDT(J1) * ZCOST - RHOS(J1) * ZSINT
     PC(I+4,3) = DRSDT(J1) * ZSINT + RHOS(J1) * ZCOST
     !
  END DO
  !
  RETURN
END SUBROUTINE CENTER
