!*DECK C2SX05
!*CALL PROCESS
SUBROUTINE RZBOUND
  !        ##################
  !                                        AUTHORS:
  !                                        O.SAUTER,  CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SX05  COMPUTE (R,Z) OF PLASMA BOUNDARY ON NBPSOUT POINTS          *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZTETBPS
  INTEGER          ::     I
  REAL(RKIND)      ::     ZDTHETA
  DIMENSION &
       &      ZTETBPS(NPBPS), ZBND(NPBPS)
  !
  !-----------------------------------------------------------------------
  !
  !L       1. SET THETA MESH
  !
  ZDTHETA = 2._RKIND * CPI / REAL(NBPSOUT-1,RKIND)
  DO I=1,NBPSOUT
     ZTETBPS(I) = REAL(I-1,RKIND) * ZDTHETA
  END DO
  !
  !L       2. COMPUTE R,Z
  !
  CALL BOUND(NBPSOUT,ZTETBPS,ZBND)
  DO I=1,NBPSOUT
     RRBPSOU(I) = R0 + ZBND(I) * COS(ZTETBPS(I))
     RZBPSOU(I) = RZ0+ ZBND(I) * SIN(ZTETBPS(I))
  END DO
  !
  RETURN
END SUBROUTINE RZBOUND
