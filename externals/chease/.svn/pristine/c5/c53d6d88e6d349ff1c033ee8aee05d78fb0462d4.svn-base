!*DECK C2SA05
!*CALL PROCESS
SUBROUTINE QPLACS
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SA05 : DENSIFY STABILITY MESH AT A PREDEFINED SET OF Q-VALUES     *
  !          GIVEN IN QPLACS (SEE SECTION 6.4.5 IN PUBLICATION)         *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     J
  INTEGER          ::     J12
  INTEGER          ::     J13
  INTEGER          ::     JPLACE
  INTEGER          ::     J11
  INTEGER          ::     J10
  INTEGER          ::     J9
  INTEGER          ::     J8
  INTEGER          ::     J7
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZCSCAL
  INTEGER          ::     J4
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZCSHFT
  REAL(RKIND)      ::     TICS
  REAL(RKIND)      ::     QICS
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     ICS
  REAL(RKIND)      ::     ZQ
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J2
  INTEGER          ::     J1
  DIMENSION &
       &   ZQ(NPPSI1),   ZS(NPPSI1)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO J1=1,NPSI
     !
     CSM(J1) = .5_RKIND * (CS(J1+1) + CS(J1))
     !
  END DO
  !
  CSM(NPSI1) = 1._RKIND
  !
  CALL PREMAP(3)
  !
  IF (NRFP .EQ. 0) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! TOKAMAK EQUILIBRIUM                                                 *
     !                                                                     *
     !**********************************************************************
     !
     IF (NCSCAL .EQ. 1) THEN
        !
        DO J2=1,NPSI1
           !
           ZS(J2) = SQRT(1._RKIND - PSIISO(J2) / SPSIM)
           ZQ(J2) = .5_RKIND * TMF(J2) * CIDQ(J2) / CPI
           !
        END DO
        !
        ICS = ISRCHFGE(NPSI1,ZS,1,CSSPEC) - 1
        !
        IF (ICS .LT. 2)         ICS = 2
        IF (ICS .GE. NPSI1 - 1) ICS = NPSI1 - 2
        !
        QICS = FCCCC0(ZQ(ICS-1),ZQ(ICS),ZQ(ICS+1),ZQ(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS),ZS(ICS+1),ZS(ICS+2),CSSPEC)
        TICS = FCCCC0(TMF(ICS-1),TMF(ICS),TMF(ICS+1),TMF(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS), ZS(ICS+1),ZS(ICS+2),CSSPEC)
        !
        ZCSHFT = TICS**2 * ((QSPEC / QICS)**2 - 1._RKIND)
        !
        DO J3=1,NPSI1
           !
           ZQ(J3) = ZQ(J3) * SQRT(1._RKIND + ZCSHFT / TMF(J3)**2)
           !
        END DO
        !
     ELSE IF (NCSCAL .EQ. 2) THEN
        !
        T0 = FCCCC0(TMF(1),TMF(2),TMF(3),TMF(4), &
             &                  PSIISO(1),PSIISO(2),PSIISO(3),PSIISO(4),SPSIM)
        !
        DO J4=1,NPSI1
           !
           ZQ(J4) = .5_RKIND * TMF(J4) * CIDQ(J4) / CPI
           !
        END DO
        !
        IF (NSURF .EQ. 1) THEN
           !
           ZCSCAL = 1._RKIND
           !
        ELSE
           !
           ZCSCAL = CURRT / CUROLD
           !
        ENDIF
        !
        IF (NTMF0 .EQ. 0) THEN
           !
           ZCSHFT = 1._RKIND - (ZCSCAL * TMF(NPSI1))**2
           !
        ELSE IF (NTMF0 .EQ. 1) THEN
           !
           ZCSHFT = 1._RKIND - (ZCSCAL * T0)**2
           !
        ENDIF
        !
        DO J5=1,NPSI1
           ! 
           ZQ(J5) = ZQ(J5) * SQRT(1._RKIND + ZCSHFT / (ZCSCAL*TMF(J5))**2)
           !
        END DO
        !
     ELSE IF (NCSCAL .EQ. 3) THEN
        !
        ZS(1) = CSM(1)**2 * ABS(SPSIM) * CIDR(1)
        ZQ(1) = .5_RKIND * TMF(1) * CIDQ(1) / CPI
        !
        DO J7=2,NPSI1
           !
           ZS(J7) = ZS(J7-1) + ABS(SPSIM) * (CSM(J7) - CSM(J7-1)) * &
                &               (CSM(J7) * CIDR(J7) + CSM(J7-1) * CIDR(J7-1))
           ZQ(J7) = .5_RKIND * TMF(J7) * CIDQ(J7) / CPI
           !
        END DO
        !
        DO J8=1,NPSI1
           !
           ZS(J8) = SQRT(ZS(J8) / ZS(NPSI1))
           !
        END DO
        !
        ICS = ISRCHFGE(NPSI1,ZS,1,CSSPEC) - 1
        !
        IF (ICS .LT. 2)      ICS = 2
        IF (ICS .GE. NPSI1 - 1) ICS = NPSI1 - 2
        !
        QICS = FCCCC0(ZQ(ICS-1),ZQ(ICS),ZQ(ICS+1),ZQ(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS),ZS(ICS+1),ZS(ICS+2),CSSPEC)
        TICS = FCCCC0(TMF(ICS-1),TMF(ICS),TMF(ICS+1),TMF(ICS+2), &
             &                    ZS(ICS-1),ZS(ICS), ZS(ICS+1),ZS(ICS+2),CSSPEC)
        !
        ZCSHFT = TICS**2 * ((QSPEC / QICS)**2 - 1._RKIND)
        !
        DO J9=1,NPSI1
           !
           ZQ(J9) = ZQ(J9) * SQRT(1._RKIND + ZCSHFT / TMF(J9)**2)
           !
        END DO
        !
     ELSE IF (NCSCAL .EQ. 4) THEN
        !
        DO J10=1,NPSI1
           !
           ZQ(J10) = .5_RKIND * TMF(J10) * CIDQ(J10) / CPI
           !
        END DO
        !
     ENDIF
     !
     !**********************************************************************
     !                                                                     *
     ! REVERSED FIELD PINCH EQUILIBRIUM                                    *
     !                                                                     *
     !**********************************************************************
     !
  ELSE IF (NRFP .EQ. 1) THEN
     !
     DO J11=1,NPSI1
        !
        ZQ(J11) = .5_RKIND * TMF(J11) * CIDQ(J11) / CPI
        !
     END DO
     !
  ENDIF
  !
  JPLACE = 0
  !
  DO J13=1,NPOIDQ
     !
     DO J12=1,NPSI
        !
        IF ((QPLACE(J13)-ZQ(J12))*(QPLACE(J13)-ZQ(J12+1)) .LE. 0._RKIND) THEN
           !
           JPLACE = JPLACE + 1
           !
           APLACE(JPLACE) = CSM(J12) + (CSM(J12+1) - CSM(J12)) * &
                &                       (QPLACE(J13)-ZQ(J12)) / (ZQ(J12+1)-ZQ(J12))
           AWIDTH(JPLACE) = QWIDTH(J13)
           !
        ENDIF
        !
     END DO
  END DO
  !
  NPOIDA = JPLACE
  !
  WRITE(6,'(/,A20,/,A20,I2,A,/,(1P10E12.4))') ' AFTER Q PACKING:' &
       &     ,'APLACE(1:',NPOIDA,')',(APLACE(J),J=1,NPOIDA)
  WRITE(6,'(A20,I2,A,/,(1P10E12.4))') &
       &     'AWIDTH(1:',NPOIDA,')',(AWIDTH(J),J=1,NPOIDA)
  !
  RETURN
END SUBROUTINE QPLACS
