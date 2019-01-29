!*DECK C2SF02
!*CALL PROCESS
SUBROUTINE RSCALE(PR,KN)
  !        ########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SF02 SCALE EQUILIBRIUM SO THAT PR = 1. (SEE EQ. (34) IN           *
  !        PUBLICATION)                                                 *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  INTEGER          ::     KN
  REAL(RKIND)      ::     PR
  RMAG   = RMAG / PR
  RZMAG  = RZMAG / PR
  R0     = R0 / PR
  RZ0    = RZ0 / PR
  RC     = RC / PR
  RZ0C   = RZ0C / PR
  R0W    = R0W / PR
  RZ0W   = RZ0W / PR
  SPSIM  = SPSIM / PR
  CPSRF  = CPSRF / PR
  !
  !     ADAPT R0EXP SO THAT R0 STAYS SAME VALUE IN PHYSICAL UNIT
  !     AND B0EXP SO THAT R0EXP*B0EXP=CST (SO TOT. CURRENT STAYS SAME
  !     AS R SCALE AT CONSTANT CURRENT IS DONE HERE)
  !
  R0EXP = R0EXP * PR
  B0EXP = B0EXP / PR
  !
  BPS( 1) = BPS( 1) / PR
  BPS( 2) = BPS( 2) / PR
  BPS( 3) = BPS( 3) / PR
  BPS( 6) = BPS( 6) / PR
  BPS(12) = BPS(12) / PR
  !
  CALL DSCAL(NBPSOUT,RC1P/PR,RRBPSOU,1)
  CALL DSCAL(NBPSOUT,RC1P/PR,RZBPSOU,1)
  !
  IF (NSURF .EQ. 6) THEN
     !
     CALL DSCAL(NBPS,RC1P/PR,RRBPS,1)
     CALL DSCAL(NBPS,RC1P/PR,RZBPS,1)
     CALL BNDSPL
     !
  ENDIF
  !
  IF (NSURF .EQ. 7) THEN
     CALL DSCAL(NFOURPB,RC1P/PR,BPSCOS,1)
     CALL DSCAL(NFOURPB,RC1P/PR,BPSSIN,1)
     ALZERO=ALZERO/PR
  ENDIF
  !
  CALL DSCAL(N4NSNT,RC1P/PR,CPSI,1)
  CALL DSCAL(N4NSNT,RC1P/PR,CPSICL,1)
  CALL DSCAL(KN,RC1P/PR,PSIISO,1)
  CALL DSCAL(KN,PR,TTP,1)
  CALL DSCAL(KN,PR**2,CPR,1)
  CALL DSCAL(KN,PR**2*PR,CPPR,1)
  !
  RETURN
END SUBROUTINE RSCALE
