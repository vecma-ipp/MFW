!*DECK C2SI01
!*CALL PROCESS
SUBROUTINE PRFUNC(KN,PP,PT)
  !        ###########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SI01  EVALUATE FUNCTIONAL FORM OF TT', I* OR I_PARALLEL          *
  !          NFUNC = 1 -----> POLYNOMIAL (NIPR; AT(1:NSOUR))            *
  !          NFUNC = 2 -----> POL. 3 SECT. (NIPR; AT,AT2,AT3(1:7); AT4(1:3))
  !          NFUNC = 3 -----> PRINCETON PROFILE DEFINITION (AT(1:8))    *
  !          NFUNC = 4 -----> EXPERIMENTAL DATA (RFUNC(1:NPPF+1))       *
  !          NFUNC = 5 -----> COMPLICATED, UNCOMMENTED                  *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     KN
  REAL(RKIND)      ::     PP(KN), PT(KN)
  !
  INTEGER          ::     J1, J2, J3, J4, J5, J6, J7, J9, J31
  REAL(RKIND)      ::     PT0(KN), ZS(KN), ZDUMAR(KN)
  REAL(RKIND)      ::     ZTT
  REAL(RKIND)      ::     ZALF1
  REAL(RKIND)      ::     ZALF
  REAL(RKIND)      ::     ZB3
  REAL(RKIND)      ::     ZB2
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZB0
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     ZA0
  REAL(RKIND)      ::     PTEMP3
  REAL(RKIND)      ::     PTEMP2
  REAL(RKIND)      ::     PTEMP1
  REAL(RKIND)      ::     ZARG4
  REAL(RKIND)      ::     ZARG3
  REAL(RKIND)      ::     ZARG2
  REAL(RKIND)      ::     ZARG1
  REAL(RKIND)      ::     ZS1
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(PT,KN)
  !
  IF (NRFP .EQ. 1) GOTO 30
  !
  IF (NFUNC .EQ. 1) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! FUNC GIVEN AS POLYNOMIAL IN PSI/PSIMIN OF DEGREE NSOUR              *
     !                                                                     *
     !**********************************************************************
     !
     CALL RESETR(PT,KN,AT(NSOUR))
     !
     DO J2 = NSOUR-1,1,-1
        !
        DO J1 = 1, KN
           !
           IF (NIPR .EQ. 1) THEN
              ZS1 = PP(J1) / SPSIM
           ELSE IF (NIPR .EQ. 2) THEN
              ZS1 = SQRT(PP(J1) / SPSIM)
           ELSE IF (NIPR .EQ. 3) THEN
              ZS1 = (PP(J1) / SPSIM)**(1._RKIND/3._RKIND)
           ELSE IF (NIPR .EQ. 4) THEN
              ZS1 = (PP(J1) / SPSIM)**(1._RKIND/4._RKIND)
           ENDIF
           !             
           PT(J1) = PT(J1) * ZS1 + AT(J2)
           !
        END DO
     END DO
     !
  ELSE IF (NFUNC .EQ. 2) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  FUNC IS GIVEN AS A SUM OF NIPR PROFILES                            *
     !                                                                     *
     !  FOR S=0     TO S=AT(1)  FUNC(PSI) = (ZA0+ZA1*S+ZA2*S**2)           *
     !  FOR S=AT(1) TO S=AT(2)  FUNC(PSI) = (ZB0+ZB1*S+ZB2*S**2+ZB3*S**3)  *
     !  FOR S=AT(2) TO S=1      FUNC(PSI) = (ZC0+ZC1*S)                    *
     !                                                                     *
     !         FOR FIRST PROFILE  S = S1 = (1-PSI/PSIM)                    *
     !             SECOND PROFILE S = S2 = SQRT(S1)                        *
     !             THIRD PROFILE  S = S3 = S1**0.25                        *
     !                                                                     *
     !  NIPR = 4 : A FOURTH PROFILE IS ADDED                               *
     !                                                                     *
     !  FUNC(PSI) = AT4(3) * EXP(-((S1- AT4(1)) / AT4(2))**2)              *
     !                                                                     *
     !  WITH AT4(1)  : CENTRE IN S1                                        *
     !  WITH AT4(2)  : WIDTH IN S1                                         *
     !  WITH AT4(3)  : HEIGHT                                              *
     !                                                                     *
     !**********************************************************************
     !                                                                     *
     !  FIRST PROFILE :                                                    *
     !                                                                     *
     !**********************************************************************
     !
     DO J3=1,KN
        !
        ZS(J3) = 1._RKIND - PP(J3) / SPSIM
        !
     END DO
     !
     CALL ATCOEF(KN,ZS,AT,PT,1)
     !
     IF (NIPR .EQ. 1) RETURN
     !
     !**********************************************************************
     !                                                                     *
     !  SECOND PROFILE :                                                   *
     !                                                                     *
     !**********************************************************************
     !
     DO J4=1,KN
        !
        ZS1 = 1._RKIND - PP(J4) / SPSIM
        !
        IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
        !
        ZS(J4) = SQRT(ZS1)
        !
     END DO
     !
     CALL ATCOEF(KN,ZS,AT2,PT,1)
     !
     IF (NIPR .EQ. 2) RETURN
     !
     !**********************************************************************
     !                                                                     *
     !  THIRD  PROFILE :                                                   *
     !                                                                     *
     !**********************************************************************
     !
     DO J5=1,KN
        !
        ZS1 = 1._RKIND - PP(J5) / SPSIM
        !
        IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
        !
        ZS(J5) = (ZS1)**0.25_RKIND
        !
     END DO
     !
     CALL ATCOEF(KN,ZS,AT3,PT,1)
     !
     IF (NIPR .EQ. 3) RETURN
     !
     !**********************************************************************
     !                                                                     *
     !  FOURTH PROFILE:                                                    *
     !                                                                     *
     !**********************************************************************
     !
     IF (AT4(2) .LE. 0._RKIND) RETURN
     !
     DO J6=1,KN
        !
        ZS(J6) = 1._RKIND - PP(J6) / SPSIM
        !
        IF (ZS(J6) .LT. 0._RKIND) ZS(J6) = 0._RKIND
        !
     END DO
     !
     CALL ATCOEF(KN,ZS,AT4,PT,2)
     !
  ELSE IF (NFUNC .EQ. 3) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! "PRINCETON" PROFILE WITH TWO EXPONENTS                              *
     !  see Manickam et al., Phys.Plasmas 1 p.1601, May 1994               *
     !                                                                     *
     !  FUNC = AT(1) * (1 - S**AT(3))**AT(2) +                             *
     !         AT(1)*AT(4)*AT(6)**2*S*(1-S**AT(8))**AT(7)/                 *
     !                               ((S-AT(5))**2+AT(6)**2)               *
     !                                                                     *
     !         WHERE S = 1 - PSI / SPSIM                                   *
     !                                                                     *
     !**********************************************************************
     !
     DO J7=1,KN
        !
        ZS1 = 1._RKIND - PP(J7) / SPSIM
        !
        !            zs1 = at(13)*zs1                   !Please leave this. Needed for certain profiles (HL)
        !
        IF (ZS1 .LT. EPSMCH) ZS1 = EPSMCH
        ZARG1 = 1._RKIND - ZS1**at(3)
        if (ZARG1.lt.EPSMCH) ZARG1 = EPSMCH
        ZARG2 = 1._RKIND - ZS1**at(8)
        if (ZARG2.lt.EPSMCH) ZARG2 = EPSMCH
        ZARG3 = ZS1 - at(5)
        !        ZARG4 = 1._RKIND - (ZS1/AT(12))**at(11)  !Please leave this. Needed for certain profiles (HL)
        !        if (ZARG4.lt.EPSMCH) ZARG4 = EPSMCH      !Please leave this. Needed for certain profiles (HL)
        !
        PTEMP1 = AT(1) * ZARG1**AT(2)
        PTEMP2 = AT(6)**2*ZS1 * ZARG2**AT(7)
        PTEMP3 = ZARG3 * ZARG3 + AT(6) * AT(6)
        PTEMP2 = AT(4) * AT(1) * PTEMP2 / PTEMP3
        !
        !            PT(J7) = PTEMP1 + PTEMP2+at(14)     !Please leave this. Needed for certain profiles (HL)
        PT(J7) = PTEMP1 + PTEMP2
        !
     END DO
     !
  ELSE IF (NFUNC .EQ. 4) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  INTERPOLATE FUNC WITH CUBIC SPLINES ON RFUN VALUES (IF FUNC IS     *
     !  GIVEN BY A SET OF POINTS)                                          *
     !                                                                     *
     !**********************************************************************
     !
     IF (NFUNRHO.EQ.0 .OR. NRHOMESH.EQ.0) THEN
        CALL PPSPLN(KN,PP,NPPF,FCSM,RFUN,D2RFUN,PT,ZDUMAR(1),0)
     ELSE IF (NFUNRHO.EQ.1 .AND. NRHOMESH.EQ.1) THEN
        !        GET VALUE OF RHOTOR FOR GIVEN S ARRAY
        DO J6=1,KN
           ZS(J6) = 1._RKIND - PP(J6) / SPSIM
           IF (ZS(J6) .LE. 0._RKIND) THEN
              ZS(J6) = 0._RKIND
           ELSE
              ZS(J6) = SQRT(ZS(J6))
           END IF
        END DO
        IF (CSIPRI(NISO).LT.0.9) THEN
           PRINT *,'NISO, CSIPRI(NISO)= ',NISO, CSIPRI(NISO)
        ENDIF
        !OS               if (niso .gt. 101) print *,' zs, pp, 1-pp/spsim, kn=',kn,' zp(2)= ',zs(2)
        !OS               if (niso .gt. 101) write(*,'(1p3e12.4)') (ZS(j7),pp(j7),1._RKIND-pp(j7)/SPSIM,j7=1,kn)
        !OS               if (niso .gt. 101) print *,' CSIPRI,CIDRTOR,D2CIDRTOR, niso=',niso
        !OS               if (niso .gt. 101) write(*,'(1p3e12.4)') (CSIPRI(j7),CIDRTOR(j7),D2CIDRTOR(j7),j7=1,NISO)
        CALL PPSPLN2(KN,ZS,NISO-1,CSIPRI,CIDRTOR,D2CIDRTOR,PT0,ZDUMAR,ZDUMAR)
        !        GET FUNCTION FOR GIVEN RHOTOR
        CALL PPSPLN2(KN,PT0,NPPF,FCSM,RFUN,D2RFUN,PT,ZDUMAR,ZDUMAR)
        !OS               if (niso .gt. 101) print *,' fcsm, rfun, d2rfun, NPPF= ',NPPF+1
        !OS               if (niso .gt. 101) then
        !OS                  do j7=1,NPPF+1
        !OS                     write(*,'(1p3e12.4)') fcsm(j7),RFUN(j7),D2RFUN(j7)
        !OS                  end do
        !OS                  print *,' zs, pt0, pt, kn=',kn
        !OS                  write(*,'(1p3e12.4)') (ZS(j7),PT0(j7),PT(j7),j7=1,kn)
        !OS!                  stop
        !OS               endif
!!$               PRINT *,'NPPF,  FCSM(1:NPPF)',NPPF,FCSM(1:NPPF)
!!$               PRINT *,'NPPF, RFUN(1:NPPF)',NPPF,RFUN(1:NPPF)
!!$               PRINT *,'KN, PT0(1:KN)',KN,PT0(1:KN)
!!$               PRINT *,'KN, PT(1:KN)',KN,PT(1:KN)
     ELSE
        PRINT *,'NFUNRHO= ',NFUNRHO,' AND NRHOMESH= ',NRHOMESH,' NOT YET CODED IN'
        STOP 'PPRIME'

     ENDIF
     !
  ELSE IF (NFUNC .EQ. 5) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  2 CUBICS                                                           *
     !                                                                     *
     !**********************************************************************
     !
     ZA0 = 2._RKIND*(1._RKIND-AT(2))/AT(1)+AT(3)
     ZA1 =  -3._RKIND*(1._RKIND-AT(2))/AT(1)-AT(3)
     !
     ZB0 = AT(3)-2._RKIND*AT(2)/(AT(1)-1._RKIND)
     ZB1 = -AT(3)*(AT(1)+2._RKIND)+3._RKIND*AT(2)*(AT(1)+1._RKIND)/(AT(1)-1._RKIND)
     ZB2 = AT(3)*(2._RKIND*AT(1)+1._RKIND)-6._RKIND*AT(1)*AT(2)/(AT(1)-1._RKIND)
     ZB3 = -AT(1)*AT(3)+AT(2)*(3._RKIND*AT(1)-1._RKIND)/(AT(1)-1._RKIND)
     !
     DO J9=1,KN
        !
        ZS1 = 1._RKIND - PP(J9) / SPSIM
        !
        IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
        !
        IF (NIPR .EQ. 2) THEN
           ZS1 = SQRT(ZS1)
        ELSE IF (NIPR .EQ. 3) THEN
           ZS1 = ZS1**0.25_RKIND
        ELSE IF (NIPR .EQ. 4) THEN
           ZS1 = ZS1**2
        ENDIF
        !
        IF (ZS1 .LE. AT(1)) THEN
           PT(J9) = 1._RKIND + ZS1**2 * (ZA1 + ZA0 * ZS1 / AT(1))/AT(1)
        ELSE 
           PT(J9) = (ZB3+ZS1*(ZB2+ZS1*(ZB1+ZS1*ZB0)))/(AT(1)-1._RKIND)**2
        ENDIF
        !
     END DO
     !
  ENDIF
  !
  RETURN
  !
30 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  ! REVERSED FIELD PINCH EQUILIBRIUM :                                  *
  !                                                                     *
  ! COMPUTE TTPRIME(PSI) FOR THE RFP ASSUMING THE PROFILES              *
  ! DETERMINED BY MU(0)=AT(1) , ALFA=AT(2) , Bz(1)=AT(3)                *
  !                                                                     *
  !**********************************************************************
  !
  ZALF  = AT(2) 
  ZALF1 = ZALF + 1._RKIND
  !
  DO J31=1,KN
     !
     ZS1 = 1._RKIND - PP(J31) / SPSIM
     !
     IF (ZS1 .LT. 0._RKIND) ZS1 = 0._RKIND
     !
     ZTT      = AT(3) + AT(1) * (- ZS1 + (1._RKIND/ZALF1) * ZS1**ZALF1)
     PT(J31) = ZTT * AT(1) * (1._RKIND - ZS1**ZALF) / SPSIM
     !
  END DO
  !
  RETURN
END SUBROUTINE PRFUNC
