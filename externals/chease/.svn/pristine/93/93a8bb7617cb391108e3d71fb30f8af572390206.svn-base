!*DECK C2SD05
!*CALL PROCESS
SUBROUTINE IDENTA(KVAR,PC)
  !        ##########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SD05 PERFORM ROW AND COLUMN OPERATIONS REQUIRED TO IMPOSE         *
  !        BOUNDARY CONDITIONS IN A                                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J15
  INTEGER          ::     J16
  INTEGER          ::     J13
  INTEGER          ::     J14
  INTEGER          ::     JR
  INTEGER          ::     JPAAR
  INTEGER          ::     J11
  INTEGER          ::     J12
  INTEGER          ::     J9
  INTEGER          ::     J10
  INTEGER          ::     J7
  INTEGER          ::     J8
  INTEGER          ::     JROW
  INTEGER          ::     JCOL
  INTEGER          ::     J5
  INTEGER          ::     J6
  REAL(RKIND)      ::     PC
  INTEGER          ::     KVAR
  INTEGER          ::     J3
  INTEGER          ::     J4
  INTEGER          ::     JAACOL
  INTEGER          ::     JAROW
  INTEGER          ::     JACOL
  INTEGER          ::     J1
  INTEGER          ::     IBAND2
  INTEGER          ::     IBAND1
  INTEGER          ::     IDIAG
  INTEGER          ::     J2
  REAL(RKIND)      ::     PA
  REAL(RKIND)      ::     PAA
  DIMENSION &
       &   PA(N4NT,3),  PAA(N4NT,N4NT),  PC(N4NT,3)
  !
  INCLUDE 'BNDIND.inc'
  !
  !----*-----*-----*-----*-----*-----*-----*-----*-----*-----*-----*----
  !
  CALL VZERO(PAA,N16NT*NPT)
  CALL VZERO(PA,12*NPT)
  !
  DO J2=1,4*NT
     !
     IDIAG  = J2
     IBAND1 = 4 * NT + 1
     IBAND2 = MIN(IDIAG+NBAND-1, 8*NT)
     !
     DO J1=IBAND1,IBAND2
        !
        JACOL  = INDCOL(J1,IDIAG)
        JAROW  = INDROW(J1,IDIAG)
        JAACOL = J1 - 4 * NT
        !
        PAA(JAACOL,J2) = A(JACOL,JAROW)
        !
     END DO
  END DO
  !
  DO J4=1,4*NT
     !
     DO J3=1,KVAR
        !
        CALL DAXPY(4*NT,PC(J4,J3),PAA(1,J4),1,PA(1,J3),1)
        !
     END DO
  END DO
  !
  DO J6=1,KVAR
     !
     DO J5=1,4*NT
        !
        JCOL = 4 * NT + J5
        JROW = 4 * NT - KVAR + J6
        !
        JACOL  = INDCOL(JCOL,JROW)
        JAROW  = INDROW(JCOL,JROW)
        !
        A(JACOL,JAROW) = PA(J5,J6)
        !
     END DO
  END DO
  !
  CALL VZERO(PAA,N16NT*NPT)
  CALL VZERO(PA,12*NPT)
  !
  DO J8=1,4*NT
     !
     DO J7=1,4*NT
        !
        JCOL = J7
        JROW = J8
        !
        JACOL = INDCOL(JCOL,JROW)
        JAROW = INDROW(JCOL,JROW)
        !
        PAA(J7,J8) = A(JACOL,JAROW)
        !
     END DO
  END DO
  !
  DO J10=1,4*NT
     !
     DO J9=1,KVAR
        !
        CALL DAXPY(4*NT,PC(J10,J9),PAA(1,J10),1,PA(1,J9),1)
        !
     END DO
  END DO
  !
  DO J12=1,4*NT
     !
     DO J11=1,KVAR
        !
        JPAAR = 4 * NT - KVAR + J11
        !
        PAA(J12,JPAAR) = PA(J12,J11)
        !
     END DO
  END DO
  !
  CALL VZERO(PA,12*NPT)
  !
  JR = 4 * NT - KVAR + 1
  !
  DO J14=1,4*NT
     !
     DO J13=1,KVAR
        !
        CALL DAXPY(KVAR,PC(J14,J13),PAA(J14,JR),N4NT,PA(J13,1),N4NT)
        !
     END DO
  END DO
  !
  DO J16=1,KVAR
     !
     DO J15=1,KVAR
        !
        JCOL = 4 * NT - KVAR + J15
        JROW = 4 * NT - KVAR + J16
        !
        JACOL  = INDCOL(JCOL,JROW)
        JAROW  = INDROW(JCOL,JROW)
        !
        A(JACOL,JAROW) = PA(J15,J16)
        !
     END DO
  END DO
  !
  RETURN
END SUBROUTINE IDENTA
