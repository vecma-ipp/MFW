!*DECK C2SD06
!*CALL PROCESS
SUBROUTINE IDENTB(KVAR,PC)
  !        ##########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SD06 PERFORM ROW OPERATIONS REQUIRED TO IMPOSE BOUNDARY           *
  !        CONDITIONS IN B                                              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J3
  REAL(RKIND)      ::     PC
  INTEGER          ::     JBROW
  INTEGER          ::     KVAR
  INTEGER          ::     J1
  INTEGER          ::     J2
  REAL(RKIND)      ::     PB
  DIMENSION &
       &   PB(3),   PC(N4NT,3)
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  CALL VZERO(PB,3)
  !
  DO J2=1,4*NT
     !
     DO J1=1,KVAR
        !
        JBROW = J2
        !
        PB(J1) = PB(J1) + PC(J2,J1) * B(JBROW)
        !
     END DO
  END DO
  !
  DO J3=1,KVAR
     !
     JBROW = 4 * NT - KVAR + J3
     !
     B(JBROW) = PB(J3)
     !
  END DO
  !
  RETURN
END SUBROUTINE IDENTB
