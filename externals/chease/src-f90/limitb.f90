!*DECK C2SD04
!*CALL PROCESS
SUBROUTINE LIMITB
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SD04 IMPOSE BOUNDARY CONDITIONS IN B. (SEE EQ. (30) IN PAPER)     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J2
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZC
  DIMENSION &
       &   ZC(N4NT,3)
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  !**********************************************************************
  !                                                                     *
  ! 1. PSI IDENTIFICATION ON GEOMETRIC CENTER OF MESH :                 *
  !                                                                     *
  !      ADD COEFFICIENTS OF EQUATIONS NO. 4*(J-1)+1 ,J=1,NT-1 TO       *
  !      COEFFICENTS OF EQUATION NO. 4*(NT-1)+1                         *
  !                                                                     *
  !**********************************************************************
  !
  CALL CENTER(ZC)
  CALL IDENTB(3,ZC)
  !
  DO J1=1,4*NT-3
     !
     B(J1) = 0
     !
  END DO
  !
  DO J2=1,NT
     !
     B(4*(NS*NT+J2-1)+1) = 0
     B(4*(NS*NT+J2-1)+3) = 0
     !
  END DO
  !
  RETURN
END SUBROUTINE LIMITB
