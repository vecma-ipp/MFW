!*DECK C2SP09
!*CALL PROCESS
SUBROUTINE RESPPR
  !        #################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP09  INITIALIZE ARRAYS FOR BALLOONING OPTIMIZATION              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)   	::  	XPRCNT 	! <resppr.f90>
  NSRCH  = 3
  XPRCNT = 1._RKIND
  !
  CALL RESETR(XLAMB,NPPR+1,RC1P)
  CALL VZERO(XPPRMN,NPPR+1)
  CALL VZERO(XPPRMX,NPPR+1)
  CALL VZERO(XPPRDF,NPPR+1)
  CALL VZERO(XP0,NPPR+1)
  CALL VZERO(XP1,NPPR+1)
  CALL VZERO(XP2,NPPR+1)
  CALL VZERO(XP3,NPPR+1)
  CALL VZERO(XP4,NPPR+1)
  !
  CALL RESETI(NP0,NPPR+1,-10)
  CALL RESETI(NP1,NPPR+1,0)
  CALL RESETI(NP2,NPPR+1,0)
  CALL RESETI(NP3,NPPR+1,0)
  CALL RESETI(NP4,NPPR+1,0)
  !
  RETURN
END SUBROUTINE RESPPR
