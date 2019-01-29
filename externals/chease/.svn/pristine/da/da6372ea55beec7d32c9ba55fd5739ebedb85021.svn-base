!*DECK C2SP11
!*CALL PROCESS
SUBROUTINE PPBSTR(KPPR1,KFIN)
  !        ##################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP10  MODIFY P' WHEN BOOTSTRAP CURRENT DENSITY IS PRESCRIBED     *
  !          ACCORDING TO EQ. (42) IN THE PUBLICATION.                  *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     KPPR1, KFIN
  !
  INTEGER          ::     IP, J1, J2, J3
  REAL(RKIND)      ::     ZBSFR(KPPR1), ZDIFF(KPPR1), ZWORK(KPPR1)
  REAL(RKIND)      ::     ZEPS, ZERR, ZMCOR, ZNRPRM, ZRPRM
  ! FUNCTION
  INTEGER          ::     ISRCHFGE
  !
  PARAMETER (ZMCOR = 0.50_RKIND, ZEPS = 1.E-3_RKIND)
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (KPPR1 .NE. NPPR+1) THEN
    PRINT *,'PROBLEM IN PPBSTR, KPPR1=',KPPR1,' .NE. NPPR+1=',NPPR+1
    STOP
  END IF
  !
  IP = ISRCHFGE(NPPR+1,PSIISO,1,CPSICL(1))
  !
  IF (IP.LT.1)      IP = 1
  IF (IP.GT.NPPR+1) IP = NPPR+1
  !
  DO J1=IP,NPPR+1
     CALL SURFACE(J1,SIGPSI(1,J1),TETPSI(1,J1),WGTPSI(1,J1), &
          &                PCSM(J1))
     CALL CHIPSI(NPPR+1,J1)
  END DO
  !
  IF (IP .GT. 1) THEN
     DO J2=1,IP-1
        CPR(J2)   = CPR(IP)
        CPPR(J2)  = 0._RKIND
     END DO
  ENDIF
  !
  CP0   = FCCCC0(CPR(IP),CPR(IP+1),CPR(IP+2),CPR(IP+3), &
       &                  PCSM(IP),PCSM(IP+1),PCSM(IP+2),PCSM(IP+3),RC0P)
  DPDP0 = FCCCC0(CPPR(IP),CPPR(IP+1),CPPR(IP+2),CPPR(IP+3), &
       &                  PCSM(IP),PCSM(IP+1),PCSM(IP+2),PCSM(IP+3),RC0P)
  !
  CALL GLOQUA(PCSM,NPPR+1,2)
  CALL DCOPY(NPPR+1,RPRM,1,XP0,1)
  !
  IF (NBSTRP .EQ. 2) THEN
     CALL BSFUNC(NPPR+1,PSIISO,ZBSFR)
  ENDIF
  !
  ZERR    = 0._RKIND
  !
  DO J3=1,NPPR+1
     ZRPRM  = RPRM(J3)
     IF (NBSTRP .EQ. 1) THEN
        ZNRPRM = RPRM(J3) * RJPAR(J3)*BSFRAC / (RJBSOS(J3,2)+1.E-6_RKIND)
     ELSE IF (NBSTRP .EQ. 2) THEN
        ZNRPRM = RPRM(J3) * RJPAR(J3) * ZBSFR(J3) / &
             &               (RJBSOS(J3,2)+1.E-6_RKIND)
     ENDIF
     !
     IF (ZNRPRM .LT. ZRPRM - ZMCOR) ZNRPRM = ZRPRM - ZMCOR
     IF (ZNRPRM .GT. ZRPRM + ZMCOR) ZNRPRM = ZRPRM + ZMCOR
     IF (ZNRPRM .LT. - CFBAL)       ZNRPRM = - CFBAL
     !
     RPRM(J3)  = ZNRPRM
     IF (RPRM(J3) .GT. RC0P) RPRM(J3) = 0._RKIND
     ZDIFF(J3) = ABS(RPRM(J3) - XP0(J3))
     IF (ZDIFF(J3) .GT. ZERR) ZERR = ZDIFF(J3)
     !
  END DO
  !
  WRITE(*,*) 
  !
  IF (ZERR .LE. ZEPS) THEN
     WRITE(6,110) 'EPSILON > ERROR'
     WRITE(6,*) ZEPS,ZERR
     KFIN = 3
  ELSE
     WRITE(*,*) 'ERROR > EPSILON ',ZERR,ZEPS
  ENDIF

  CALL SPLINE(NPPR+1,PCSM,RPRM,D2RPRM,ZWORK)
  !
110 FORMAT (A,/)
  !
  RETURN
END SUBROUTINE PPBSTR
