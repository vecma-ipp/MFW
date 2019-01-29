!*DECK C2SP08
!*CALL PROCESS
SUBROUTINE BLTEST
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP08  LEAD EVALUATION OF SURFACE QUANTITIES, BALLOONING STABILITY*
  !          AND LOCAL INTERCHANGE CRITERIA AND GLOBAL EQUILIBRIUM      *
  !          QUANTITIES DURING BALLOONING OPTIMIZATION                  *
  !          LIMIT P' ACCORDING TO EQ. (41) IN PUBLICATION              *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  INTEGER          ::     J4
  INTEGER          ::     J3
  INTEGER          ::     J2
  INTEGER          ::     JEND
  INTEGER          ::     J5
  INTEGER          ::     J1
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     IP
  IP = ISRCHFGE(NPPR+1,PSIISO,1,CPSICL(1))
  !
  IF (IP.LT.1)      IP = 1
  IF (IP.GT.NPPR+1) IP = NPPR+1
  !
  DO J1=IP,NPPR+1
     !
     CALL SURFACE(J1,SIGPSI(1,J1),TETPSI(1,J1),WGTPSI(1,J1), &
          &                PCSM(J1))
     CALL CHIPSI(NPPR+1,J1)
     !
  END DO
  !
  !%OS         IF (NPPR+1-IP+1 .LE. NPPSBAL) THEN
  !%OS           CALL BALOON(IP,NPPR+1,PCSM)
  !%OS         ELSE
  DO J5=IP,NPPR+1,NPPSBAL
     JEND = MIN(J5+NPPSBAL-1,NPPR+1)
     CALL BALOON(J5,JEND,PCSM)
  ENDDO
  !%OS         ENDIF
  !
  IF (IP .GT. 1) THEN
     !
     DO J2=1,IP-1
        !
        CPR(J2)   = CPR(IP)
        CPPR(J2)  = 0._RKIND
        NCBAL(J2) = - 1
        SMERCI(J2) = 0.25_RKIND
        !
     END DO
     !
  ENDIF
  !
  CP0   = FCCCC0(CPR(IP),CPR(IP+1),CPR(IP+2),CPR(IP+3), &
       &                  PCSM(IP),PCSM(IP+1),PCSM(IP+2),PCSM(IP+3),RC0P)
  DPDP0 = FCCCC0(CPPR(IP),CPPR(IP+1),CPPR(IP+2),CPPR(IP+3), &
       &                  PCSM(IP),PCSM(IP+1),PCSM(IP+2),PCSM(IP+3),RC0P)
  !
  CALL GLOQUA(PCSM,NPPR+1,2)
  CALL RESETI(N2BAL,NPPR+1,0)
  !
  DO J3=IP,NPPR+1
     !
     IF (ABS(CPPR(J3)) .GT. ABS(CFBAL * CDQ(J3)) .OR. &
          &       ABS(CPPR(J3)) .GT. 5._RKIND) THEN
        !
        NCBAL(J3) = 1 
        N2BAL(J3) = 1
        !
     ENDIF
     !
  END DO
  !
  DO J4=NPPR-2,NPPR+1
     !
     IF (ABS(CPPR(J4)) .GT. ABS(CPPR(J4-1))) THEN
        !
        CPPR(J4) = CPPR(J4-1)
        !
        PRINT*,'*****WARNING***** : CPPR(',J4,') = CPPR(',J4-1,')'
        PRINT*,' IMPOSED. NPPR = ',NPPR
        !
        NCBAL(J4) = 1 
        N2BAL(J4) = 1
        !
     ENDIF
     !
  END DO
  !
  RETURN
END SUBROUTINE BLTEST
