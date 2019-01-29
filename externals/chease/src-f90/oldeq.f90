!*DECK C2S10
!*CALL PROCESS
SUBROUTINE OLDEQ
  !        ################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2S10 : READ CONVERGED SOLUTION OF GRAD-SHAFRANOV IN AUXILLARY      *
  !         ARRAYS                                                      *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  CALL DCOPY(NS1,CSIGO,1,CSIG,1)
  CALL DCOPY(NT1,CTO,1,CT,1)
  CALL DCOPY(NISO,CID0O,1,CID0,1)
  CALL DCOPY(NISO,CID2O,1,CID2,1)
  CALL DCOPY(NISO,CIDRTORO,1,CIDRTOR,1)
  CALL DCOPY(NISO,CPPRO,1,CPPR,1)
  CALL DCOPY(NISO,D2CID0O,1,D2CID0,1)
  CALL DCOPY(NISO,D2CID2O,1,D2CID2,1)
  CALL DCOPY(NISO,D2CIDRTORO,1,D2CIDRTOR,1)
  CALL DCOPY(NISO,D2CPPRO,1,D2CPPR,1)
  CALL DCOPY(NISO,TTPO,1,TTP,1)
  CALL DCOPY(NISO,TMFO,1,TMF,1)
  CALL DCOPY(NISO,D2TMFO,1,D2TMF,1)
  CALL DCOPY(NISO,CSIPRO,1,CSIPR,1)
  CALL DCOPY(N4NSNT,CPSIO,1,CPSICL,1)
  CALL DCOPY(14,BPSO,1,BPS,1)
  !
  SPSIM  = SPSIMO
  R0     = R0O
  RZ0    = RZ0O
  R0W    = R0WO
  RZ0W   = RZ0WO
  RMAG   = RMAGO
  RZMAG  = RZMAGO
  NT     = NTO
  NS     = NSO
  NISO   = NISOO
  !
  RETURN
END SUBROUTINE OLDEQ
