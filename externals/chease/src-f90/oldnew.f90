!*DECK C2S09
!*CALL PROCESS
SUBROUTINE OLDNEW
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2S09 : STORE CONVERGED SOLUTION OF GRAD-SHAFRANOV INTO AUXILLARY   *
  !         ARRAYS                                                      *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  CALL DCOPY(NS1,CSIG,1,CSIGO,1)
  CALL DCOPY(NT1,CT,1,CTO,1)
  CALL DCOPY(NISO,CID0,1,CID0O,1)
  CALL DCOPY(NISO,CID2,1,CID2O,1)
  CALL DCOPY(NISO,CIDRTOR,1,CIDRTORO,1)
  CALL DCOPY(NISO,CPPR,1,CPPRO,1)
  CALL DCOPY(NISO,D2CID0,1,D2CID0O,1)
  CALL DCOPY(NISO,D2CID2,1,D2CID2O,1)
  CALL DCOPY(NISO,D2CIDRTOR,1,D2CIDRTORO,1)
  CALL DCOPY(NISO,D2CPPR,1,D2CPPRO,1)
  CALL DCOPY(NISO,TTP,1,TTPO,1)
  CALL DCOPY(NISO,TMF,1,TMFO,1)
  CALL DCOPY(NISO,D2TMF,1,D2TMFO,1)
  CALL DCOPY(NISO,CSIPR,1,CSIPRO,1)
  CALL DCOPY(N4NSNT,CPSICL,1,CPSIO,1)
  CALL DCOPY(14,BPS,1,BPSO,1)
  !
  SPSIMO  = SPSIM
  R0O     = R0
  RZ0O    = RZ0
  R0WO    = R0W
  RZ0WO   = RZ0W
  RMAGO   = RMAG
  RZMAGO  = RZMAG
  NTO     = NT
  NSO     = NS
  NISOO   = NISO
  !
  RETURN
END SUBROUTINE OLDNEW
