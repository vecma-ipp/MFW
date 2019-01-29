!*DECK C2ST03
!*CALL PROCESS
SUBROUTINE POLYFUN(KSOUR,PCOEF,KN,PPSI,PY,KOPT,KOPTX)
  !        ###################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2ST03  COMPUTE FUNCTION PY OR ITS NTH DERIVATIVE AS POLYNOMIAL    *
  !          OF DEGREE KSOUR IN S,S**2,ETC. DEPENDING ON KOPTX     *
  !          KOPT = 1: COMPUTE PY                                       *
  !          KOPT = 2: COMPUTE D(PY)/DPSI                               *
  !          KOPT = 3: TO BE ADDED WHEN NEEDED                          *
  !                                                                     *
  !          KOPTX = 1: FUNCTION OF S**2                                *
  !          KOPTX = 2: FUNCTION OF PSI/PSIMIN=1.-S**2                  *
  !          KOPTX = 3: TO BE ADDED WHEN NEEDED                         *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZDYDPSI
  REAL(RKIND)      ::     PPSI
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     KOPTX
  INTEGER          ::     J1
  INTEGER          ::     J2
  REAL(RKIND)      ::     PCOEF
  REAL(RKIND)      ::     PY
  INTEGER          ::     J2END
  INTEGER          ::     KOPT
  INTEGER          ::     KN
  INTEGER          ::     KSOUR
  DIMENSION &
       &   PCOEF(KSOUR),            PY(KN), PPSI(KN)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (KOPT .EQ. 1) THEN
     J2END = 1
     CALL RESETR(PY,KN,PCOEF(KSOUR))
  ELSE IF (KOPT .EQ. 2) THEN
     J2END = 2
     CALL RESETR(PY,KN,REAL(KSOUR-1)*PCOEF(KSOUR))
  ENDIF
  DO J2 = KSOUR-1,J2END,-1
     !
     DO J1 = 1, KN
        !
        IF (KOPTX .EQ. 1) THEN
           ZS1 = 1._RKIND - PPSI(J1) / SPSIM
           ZDYDPSI = - 1._RKIND/SPSIM
        ELSE IF (KOPTX .EQ. 2) THEN
           ZS1 = PPSI(J1) / SPSIM
           ZDYDPSI =  1._RKIND/SPSIM
        ENDIF
        !             
        IF (KOPT .EQ. 1) THEN
           PY(J1) = PY(J1) * ZS1 + PCOEF(J2)
        ELSE IF (KOPT .EQ. 2) THEN
           IF (J2 .EQ. KSOUR-1) PY(J1) = PY(J1) * ZDYDPSI
           PY(J1) = PY(J1) * ZS1 + REAL(J2-1,RKIND)*PCOEF(J2)*ZDYDPSI
        ENDIF
        !     
     END DO
  END DO
  !
  !
  RETURN
END SUBROUTINE POLYFUN
