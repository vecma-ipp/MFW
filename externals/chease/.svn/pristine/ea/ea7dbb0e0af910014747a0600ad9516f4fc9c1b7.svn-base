!*DECK C2SM16
!*CALL PROCESS
SUBROUTINE STCHPS(KP,N,PCHI,PTMAP,PD2TM,PSMAP,PD2SM, &
     &                     PSIG,PTET,KT0)
  !        ####################################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM16 EVALUATE (SIGMA,THETA) COODINATES OF (S,CHI) NODES REQUIRED  *
  !        BY NOVA-W AND PEST                                           *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PD2SM
  REAL(RKIND)      ::     PSMAP
  REAL(RKIND)      ::     PSIG
  REAL(RKIND)      ::     PD2TM
  REAL(RKIND)      ::     PTMAP
  REAL(RKIND)      ::     PTET
  REAL(RKIND)      ::     ZD
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  REAL(RKIND)      ::     ZA
  REAL(RKIND)      ::     ZH
  INTEGER          ::     ICHI
  INTEGER          ::     J2
  REAL(RKIND)      ::     PCHI
  INTEGER          ::     KP
  INTEGER          ::     KT0
  INTEGER          ::     JT
  INTEGER          ::     IC
  INTEGER          ::     JG
  INTEGER          ::     N
  DIMENSION &
       &   KT0(N), &
       &   PCHI(N),      PD2SM(NTP2), PD2TM(NTP2), PSIG(N), &
       &   PSMAP(NTP2),  PTET(N),     PTMAP(NTP2)
  !
  DO  JG=1,N
     IC = 1
     DO  JT = 1,NT2
        IF (IC.EQ.0) GOTO 1
        KT0(JG) = JT-1
        IF (CHIN(JT,KP).GE.PCHI(JG)) IC = 0
     END DO
1    CONTINUE 
  END DO
  !
  DO J2=1,N
     !
     ICHI = KT0(J2)
     !
     IF (ICHI .LT. 1)   ICHI = 1
     IF (ICHI .GT. NT1) ICHI = NT1
     !
     ZH = CHIN(ICHI+1,KP) - CHIN(ICHI,KP)
     ZA = (CHIN(ICHI+1,KP) - PCHI(J2)) / ZH
     ZB = (PCHI(J2) - CHIN(ICHI,KP)) / ZH
     ZC = (ZA + 1) * (ZA - 1) * ZH * &
          &        (CHIN(ICHI+1,KP) - PCHI(J2)) / 6._RKIND
     ZD = (ZB + 1) * (ZB - 1) * ZH * &
          &        (PCHI(J2) - CHIN(ICHI,KP)) / 6._RKIND
     ! 
     PTET(J2) = ZA*PTMAP(ICHI) + ZB*PTMAP(ICHI+1) + &
          &              ZC*PD2TM(ICHI) + ZD*PD2TM(ICHI+1)
     !
     IF (KP .EQ. 2*NPSI) THEN
        !
        PSIG(J2) = 1._RKIND
        !
     ELSE
        !
        PSIG(J2) = ZA*PSMAP(ICHI) + ZB*PSMAP(ICHI+1) + &
             &                ZC*PD2SM(ICHI) + ZD*PD2SM(ICHI+1)
        !
     ENDIF
     !
  END DO
  !
  RETURN
END SUBROUTINE STCHPS
