!*DECK C2SF04
!*CALL PROCESS
SUBROUTINE PRNORM(PC,KN)
  !        ########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SF04 SCALE EQUILIBRIUM. (SEE EQ. (32) IN PUBLICATION)             *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  INTEGER          ::     KN
  REAL(RKIND)      ::     PC
  CALL DSCAL(N4NSNT,PC,CPSI,1)
  CALL DSCAL(N4NSNT,PC,CPSICL,1)
  CALL DSCAL(KN,PC,PSIISO,1)
  CALL DSCAL(KN,PC,TMF,1)
  CALL DSCAL(KN,PC,TTP,1)
  CALL DSCAL(KN,PC**2,CPR,1)
  CALL DSCAL(KN,PC,CPPR,1)
  !
  SPSIM = PC * SPSIM
  T0    = PC * T0
  !
  RETURN
END SUBROUTINE PRNORM
