!*DECK C2SP01
!*CALL PROCESS
SUBROUTINE PPRIME(KN,PP,PT)
  !        ###########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP01  COMPUTE P-PRIME(PSI)                                       *
  !          NPPFUN = 1  -----> POLYNOMIAL (AP(1:NSOUR))                *
  !          NPPFUN = 2  -----> POL. IN 3 SECTIONS (AP AND/OR AP2(1:7)) *
  !          NPPFUN = 3  -----> PRINCETON DEFINITION (AP OR AP2(1:6))   *
  !          NPPFUN = 4  -----> EXPERIMENTAL DATA (RPPF(1:NPPF))        *
  !          NPPFUN = 5  -----> GAUSSIAN (AP(1:3))                      *
  !          NPPFUN = 6  -----> 6 SECTIONS (AP AND/OR AP2(1:13))        *
  !          NPPFUN = 7  -----> VIA PRESSURE (AP OR AP2(1:3))           *
  !          NPPFUN = 8  -----> VIA PRESSURE FROM EXPERIMENTAL DATA (RPPF(1:NPPF)) *
  !                                                                     *
  !**********************************************************************
  !
  !     THE PROFILES CAN BE RESCALED WITH CPRESS. IN THIS CASE, THE
  !     PARAMETERS SHOULD BE MODIFIED IN AUXVAL AND CPRESS SET BACK TO 1.0
  !     AS ONE MODIFIES THE INPUT PARAMETERS. CPRESSO KEEPS INPUT CPRESS
  !
  !-----------------------------------------------------------------------
  USE globals
  USE interpol
  USE interpos_module
  IMPLICIT NONE
  !
  INTEGER          ::     KN
  REAL(RKIND)      ::     PP(KN), PT(KN)
  !
  REAL(RKIND)      ::     ZPT0(KN), ZS(KN), ZDUMAR(KN), ZTEMP(nppsi1)
  INTEGER          ::     J10
  INTEGER          ::     J8
  INTEGER          ::     J7
  REAL(RKIND)      ::     Z0P9
  REAL(RKIND)      ::     ZEXP
  REAL(RKIND)      ::     Z0P1
  INTEGER          ::     J6
  REAL(RKIND)      ::     ZYP2
  REAL(RKIND)      ::     ZYP1
  REAL(RKIND)      ::     ZY2
  REAL(RKIND)      ::     ZY1
  REAL(RKIND)      ::     ZYSH
  REAL(RKIND)      ::     ZARG2
  REAL(RKIND)      ::     ZARG1
  REAL(RKIND)      ::     ZYS1
  REAL(RKIND)      ::     ZYS0
  REAL(RKIND)      ::     ZPT2
  REAL(RKIND)      ::     ZPT1
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZEPS
  INTEGER          ::     J4
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J1
  INTEGER          ::     J2, NPPF1
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(PT,KN)
  !
  IF (NBLOPT .GE. 2 .OR. NBSOPT .EQ. 2) GOTO 11
  !
  IF (NPPFUN .EQ. 1) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! P-PRIME(PSI) DEFINED AS POLYNOMIAL IN PSI/PSIMIN OF DEGREE NSOUR    *
     !                                                                     *
     !**********************************************************************
     !
     CALL RESETR(PT,KN,AP(NSOUR))
     !
     DO J2=NSOUR-1,1,-1
        !
        DO J1=1, KN
           !
           IF (NPP.EQ.1) THEN
              ZS1    = 1._rkind-PP(J1) / SPSIM
           ELSE IF (NPP.EQ.2) THEN
              ZS1    = SQRT(1._rkind-PP(J1) / SPSIM)
           ELSE IF (NPP.EQ.3) THEN
              ! THIS IS ACTUALLY USUAL STANDARD FOR POLYNOMIAL
              ZS1    = PP(J1) / SPSIM
           ENDIF
           PT(J1) = PT(J1) * ZS1 + AP(J2)
           !
        END DO
     END DO
     !
  ELSE IF (NPPFUN .EQ. 2) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  NPP =1 OR 2                                                        *
     !                                                                     *
     !   PPRIME IS GIVEN AS A SUM OF NPP PROFILES                          *
     !                                                                     *
     !  FOR S=0     TO S=AP(1)  PPRIME(PSI)=-(ZA0+ZA1*S)                   *
     !  FOR S=AP(1) TO S=AP(2)  PPRIME(PSI)=-(ZB0+ZB1*S+ZB2*S**2+ZB3*S**3) *
     !  FOR S=AP(2) TO S=1      PPRIME(PSI)=-(ZC0+ZC1*S+ZC2*S**2)          *
     !                                                                     *
     !         FOR FIRST PROFILE  S = S1 = (1-PSI/PSIM)                    *
     !             SECOND PROFILE S = S2 = SQRT(S1)                        *
     !                                                                     *
     !**********************************************************************
     !
     !**********************************************************************
     !                                                                     *
     !  FIRST PROFILE                                                      *
     !                                                                     *
     !**********************************************************************
     !
     DO J3=1,KN
        !
        ZS(J3) = 1._RKIND - PP(J3) / SPSIM
        !
        IF (ZS(J3) .LT. RC0P) ZS(J3) = 0._RKIND
        !
     END DO
     !
     CALL APCOEF(KN,ZS,AP,PT)
     !
     IF (NPP .EQ. 1) RETURN
     !
     !**********************************************************************
     !                                                                     *
     !  SECOND PROFILE                                                     *
     !                                                                     *
     !**********************************************************************
     !
     DO J4=1,KN
        !
        ZS1 = 1._RKIND - PP(J4) / SPSIM
        !
        IF (ZS1 .LT. RC0P) ZS1 = 0._RKIND
        !
        ZS(J4) = SQRT(ZS1)
        !
     END DO
     !
     CALL APCOEF(KN,ZS,AP2,PT)
     !
  ELSE IF (NPPFUN .EQ. 3) THEN
     !
     !**********************************************************************
     !                                                                     *
     !     USE EITHER A OR AP2 DEPENDING ON NPP=1 OR 2                     *
     !                                                                     *
     !  P-PRIME(PSI) = AP(1)*(1-S**AP(3))**AP(2) + AP(4)*S**AP(5)          *
     !  IF AP(6) .GT. 0. THEN:                                             *
     !  P-PRIME(PSI) = AP(1)*(1-S**AP(3))**AP(2) +                         *
     !               + AP(4)*S**AP(5)*(1-EXP((S-1)/AP(6)))                 *
     !                                                                     *
     !  WHERE  S = 1 - PSI / SPSIM                                         *
     !                                                                     *
     !**********************************************************************
     !
     ZEPS = 1.E-12_RKIND
     !
     DO J5=1,KN
        !
        ZS1 = 1._RKIND - PP(J5) / SPSIM
        !
        IF (ZS1 .LT. ZEPS) ZS1 = ZEPS
        !
        ZPT1 = AP(1) * (1._RKIND - ZS1**AP(3))**AP(2)
        ZPT2 = AP(4) * ZS1**AP(5)
        IF (AP(6).GT.RC0P) ZPT2 = ZPT2 * (1._RKIND - EXP((ZS1-1._RKIND)/AP(6)))
        PT(J5) = ZPT1 + ZPT2
        !
     END DO
     !
     IF (NPP .EQ. 1) RETURN

     DO J5=1,KN
        !
        ZS1 = 1._RKIND - PP(J5) / SPSIM
        !
        IF (ZS1 .LT. ZEPS) ZS1 = ZEPS
        !
        ZPT1 = AP2(1) * (1 - ZS1**AP2(3))**AP2(2)
        ZPT2 = AP2(4) * ZS1**AP2(5)
        IF (AP2(6).GT.RC0P) ZPT2 = ZPT2 * (1._RKIND -EXP((ZS1-1._RKIND)/AP2(6)))
        PT(J5) = ZPT1 + ZPT2
        !
     ENDDO
     !
  ELSE IF (NPPFUN .EQ. 4) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  INTERPOLATE P' WITH CUBIC SPLINES ON RPPF VALUES (IF P-PRIME IS    *
     !  GIVEN BY A SET OF POINTS)                                          *
     !                                                                     *
     !**********************************************************************
     !
     IF (NFUNRHO.EQ.0 .OR. NRHOMESH.EQ.0) THEN
        CALL PPSPLN(KN,PP,NPPF,FCSM,RPPF,D2RPPF,PT,ZDUMAR,0)
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
        CALL PPSPLN2(KN,ZS,NISO-1,CSIPRI,CIDRTOR,D2CIDRTOR,ZPT0,ZDUMAR,ZDUMAR)
        !        GET FUNCTION FOR GIVEN RHOTOR
        CALL PPSPLN2(KN,ZPT0,NPPF,FCSM,RPPF,D2RPPF,PT,ZDUMAR,ZDUMAR)
!!$               PRINT *,'NPPF+1,  FCSM(1:NPPF+1)',NPPF+1,FCSM(1:NPPF+1)
!!$               PRINT *,'NPPF+1, RPPF(1:NPPF+1)',NPPF+1,RPPF(1:NPPF+1)
!!$               PRINT *,'KN, ZPT0(1:KN)',KN,ZPT0(1:KN)
!!$               PRINT *,'KN, PT(1:KN)',KN,PT(1:KN)
     ELSE
        PRINT *,'NFUNRHO= ',NFUNRHO,' AND NRHOMESH= ',NRHOMESH,' NOT YET CODED IN'
        STOP 'PPRIME'
     ENDIF
     !
  ELSE IF (NPPFUN .EQ. 5) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  DEFINE P' WITH A GAUSSIAN CENTERED AT AP(1), OF HEIGHT AP(2) AND   *
     !  OF WIDTH AP(3)                                                     *
     !                                                                     *
     !**********************************************************************
     !
     ZYS0  = 0._RKIND
     ZYS1  = 1._RKIND
     ZARG1 = (AP(1) / AP(3))**2
     ZARG2 = ((1._RKIND - AP(1)) / AP(3))**2
     !
     IF (ZARG1 .LT. 100._RKIND) THEN
        ZYS0 = AP(2) * EXP(-ZARG1)
     ENDIF
     IF (ZARG2 .LT. 100._RKIND) THEN
        ZYS1 = AP(2) * EXP(-ZARG2)
     ENDIF
     !
     ZYSH = MAX(ZYS0,ZYS1)
     !
     ZARG1 = (0.1_RKIND - AP(1)) / AP(3)
     ZARG2 = (0.9_RKIND - AP(1)) / AP(3)
     ZY1   = ZYSH
     ZY2   = ZYSH
     !
     IF (ZARG1**2 .LT. 100._RKIND) THEN
        ZY1   = - AP(2) * EXP(-ZARG1**2) + ZYSH
     ENDIF
     IF (ZARG2**2 .LT. 100._RKIND) THEN
        ZY2   = - AP(2) * EXP(-ZARG2**2) + ZYSH
     ENDIF
     !
     ZYP1  = - 2._RKIND * ZARG1 * (ZY1 - ZYSH) / AP(3)
     ZYP2  = - 2._RKIND * ZARG2 * (ZY2 - ZYSH) / AP(3)
     !
     DO J6=1,KN
        !
        ZS1 = 1._RKIND - PP(J6) / SPSIM
        !
        IF (ZS1 .LT. RC0P) ZS1 = 0._RKIND
        !
        ZS1 = SQRT(ZS1)
        !
        IF (ZS1 .LT. 0.1_RKIND) THEN
           !
           Z0P1 = 0.1_RKIND
           PT(J6) = FCDCD0(RC0P,RC0P,RC0P,Z0P1,ZY1,ZYP1,ZS1)
           !
        ELSE IF (ZS1 .GE. 0.1_RKIND .AND. ZS1 .LE. 0.9_RKIND) THEN
           !
           ZEXP = ((ZS1 - AP(1)) / AP(3))**2
           !
           IF (ZEXP .LT. 100._RKIND) THEN
              !            
              PT(J6) = - AP(2) * EXP(-ZEXP) + ZYSH
              !
           ENDIF
           !
        ELSE IF (ZS1 .GT. 0.9_RKIND) THEN
           !
           Z0P9 = 0.9_RKIND
           PT(J6) = FCDCD0(Z0P9,ZY2,ZYP2,RC1P,RC0P,RC0P,ZS1)
           !
        ENDIF
        !
        IF (PT(J6) .GT. RC0P) PT(J6) = 0._RKIND
        !
     END DO
     !
  ELSE IF (NPPFUN .EQ. 6) THEN
     !
     !**********************************************************************
     !                                                                     *
     !  NPP =1 OR 2                                                        *
     !                                                                     *
     !   PPRIME IS GIVEN AS A SUM OF NPP PROFILES                          *
     !                                                                     *
     !  FOR S=0     TO S=AP(1)  PPRIME(PSI)=-(ZA0+ZA1*S)                   *
     !  FOR S=AP(1) TO S=AP(2)  PPRIME(PSI)=-(ZB0+ZB1*S+ZB2*S**2+ZB3*S**3) *
     !  FOR S=AP(2) TO S=AP(3)  PPRIME(PSI)=-(ZC0+ZC1*S+ZC2*S**2)          *
     !  FOR S=AP(3) TO S=AP(4)  PPRIME(PSI)=-(ZB0+ZB1*S+ZB2*S**2+ZB3*S**3) *
     !  FOR S=AP(4) TO S=AP(5)  PPRIME(PSI)=-(ZA0+ZA1*S)                   *
     !  FOR S=AP(5) TO S=1      PPRIME(PSI)=-(ZC0+ZC1*S+ZC2*S**2)          *
     !                                                                     *
     !     WITH  (AP OR AP2)                                               *
     !          F0=AP(6)                                                   *
     !                      P1=AP(7)                                       *
     !          F2=AP(8)  ; P2=AP(9)                                       *
     !          F3=AP(10)                                                  *
     !          F4=AP(11) ; P4=AP(12)                                      *
     !          F6=AP(13)                                                  *
     !                                                                     *
     !         FOR FIRST PROFILE  S = S1 = (1-PSI/PSIM)                    *
     !             SECOND PROFILE S = S2 = SQRT(S1)                        *
     !                                                                     *
     !**********************************************************************
     !
     !**********************************************************************
     !                                                                     *
     !  FIRST PROFILE                                                      *
     !                                                                     *
     !**********************************************************************
     !
     DO J7=1,KN
        !
        ZS(J7) = 1._RKIND - PP(J7) / SPSIM
        !
        IF (ZS(J7) .LT. RC0P) ZS(J7) = 0._RKIND
        !
     END DO
     !
     CALL APCOEF2(KN,ZS,AP,PT)
     !
     IF (NPP .EQ. 1) RETURN
     !
     !**********************************************************************
     !                                                                     *
     !  SECOND PROFILE                                                     *
     !                                                                     *
     !**********************************************************************
     !
     DO J8=1,KN
        !
        ZS1 = 1._RKIND - PP(J8) / SPSIM
        !
        IF (ZS1 .LT. RC0P) ZS1 = 0._RKIND
        !
        ZS(J8) = SQRT(ZS1)
        !
     END DO
     !
     CALL APCOEF2(KN,ZS,AP2,PT)
     !
  ELSE IF (NPPFUN .EQ. 7) THEN
     !
     !**********************************************************************
     !                                                                     *
     !                                                                     *
     !  P(PSI) = SPSIM * AP(1) * (1 - S**AP(3))**AP(2)                     *
     !  AND P-PRIME(PSI) = D(P(PSI))/D(PSI)                                *
     !                                                                     *
     !  WHERE  S = 1 - PSI / SPSIM                                         *
     !                                                                     *
     !**********************************************************************
     !
     DO J10=1,KN
        !
        ZS1 = 1._RKIND - PP(J10) / SPSIM
        !
        IF (ZS1 .LT. RC0P) ZS1 = 0._RKIND
        !
        IF (AP(2).EQ.RC1P.AND.AP(3).EQ.RC1P) THEN
           PT(J10) = AP(1)
        ELSE IF (AP(2).EQ.RC1P.AND.AP(3).NE.RC1P) THEN
           PT(J10) = AP(1)*AP(3)**ZS1**(AP(3)-1._RKIND)
        ELSE IF (AP(2).NE.RC1P.AND.AP(3).EQ.RC1P) THEN
           PT(J10) = AP(1)*AP(2)*(1._RKIND-ZS1)**(AP(2)-1._RKIND)
        ELSE
           PT(J10) = AP(1)*AP(2)*AP(3)* &
                &                   (1._RKIND-ZS1**AP(3))**(AP(2)-1._RKIND)*ZS1**(AP(3)-1._RKIND)
        ENDIF
        !
     END DO
     !
     IF (NPP .EQ. 1) RETURN
     !
     DO J10=1,KN
        !
        ZS1 = 1._RKIND - PP(J10) / SPSIM
        !
        IF (ZS1 .LT. RC0P) ZS1 = 0._RKIND
        !
        IF (AP2(2).EQ.RC1P.AND.AP2(3).EQ.RC1P) THEN
           PT(J10) = AP2(1)
        ELSE IF (AP2(2).EQ.RC1P.AND.AP2(3).NE.RC1P) THEN
           PT(J10) = AP2(1)*AP2(3)**ZS1**(AP2(3)-1._RKIND)
        ELSE IF (AP2(2).NE.RC1P.AND.AP2(3).EQ.RC1P) THEN
           PT(J10) = AP2(1)*AP2(2)*(1._RKIND-ZS1)**(AP2(2)-1._RKIND)
        ELSE
           PT(J10) = AP2(1)*AP2(2)*AP2(3)* &
                &                   (1._RKIND-ZS1**AP2(3))**(AP2(2)-1._RKIND)*ZS1**(AP2(3)-1._RKIND)
        ENDIF
        !
     ENDDO
     !
  ELSE IF (NPPFUN .EQ. 8) THEN
     !
     !**********************************************************************
     !  P'=dp/dpsiN (-1/psi0), interpolate dp/dpsiN given from p(sqrt(psiN)) *
     !  INTERPOLATE P' WITH CUBIC SPLINES ON RPPF VALUES (IF P-PRIME IS    *
     !  GIVEN BY A SET OF POINTS)                                          *
     !                                                                     *
     !**********************************************************************
     !
     IF (NFUNRHO.EQ.0 .OR. NRHOMESH.EQ.0) THEN
        CALL PPSPLN(KN,PP,NPPF,FCSM,RPPF,D2RPPF,PT,ZDUMAR,0)
        PT =  - PT  / SPSIM
     ELSE IF (NFUNRHO.EQ.1 .AND. NRHOMESH.EQ.1) THEN
       !  P'=dp/drhotor drhotor/dpsi, get dp/drhotor from input on rhotor(psi)
       !  and use drhotor/dpsi from equilibrium
       !        GET VALUE OF RHOTOR FOR GIVEN S ARRAY
        DO J6=1,KN
           ZS(J6) = 1._RKIND - PP(J6) / SPSIM
           IF (ZS(J6) .LE. 0._RKIND) THEN
              ZS(J6) = 0._RKIND
           ELSE
              ZS(J6) = SQRT(ZS(J6))
           END IF
        END DO
        ! Get p_input values on CSIPRI mesh via CIDRTOR
        NPPF1 = NPPF+1
        if (TENSPROF .NE. 0) THEN
          call interpos(FCSM(1:NPPF1),RPPF(1:NPPF1),nin=NPPF1,nout=NISO,xout=CIDRTOR(1:NISO),tension=TENSPROF,YOUT=ZTEMP, &
               & nbc=(/2 , 2 /),ybc=(/ RPPF(1), RPPF(NPPF1) /) )
        else
          call interpos(FCSM(1:NPPF1),RPPF(1:NPPF1),nin=NPPF1,nout=NISO,xout=CIDRTOR(1:NISO),tension=TENSPROF,YOUT=ZTEMP, &
               & nbc=(/0 , 0 /),ybc=(/ 0._rkind, 0._rkind /) )
        end if
        ! Compute dp/dpsi_norm on ZS, use finite tension to be able to fix edge values
        call interpos(CSIPR(1:NISO)**2,ZTEMP(1:NISO),nin=NISO,nout=KN,xout=ZS(1:KN)**2,tension=-0.1_rkind,YOUTP=PT, &
             & nbc=(/2 , 2 /),ybc=(/ ZTEMP(1), ZTEMP(NISO) /) )
        PT = - PT / SPSIM
!!$        PRINT *,'NPPF+1,  FCSM(1:NPPF+1)',NPPF+1,FCSM(1:NPPF+1)
!!$        PRINT *,'NPPF+1, RPPF(1:NPPF+1)',NPPF+1,RPPF(1:NPPF+1)
!!$        PRINT *,'KN, PT(1:KN)',KN,PT(1:KN)
     ELSE
        PRINT *,'NFUNRHO= ',NFUNRHO,' AND NRHOMESH= ',NRHOMESH,' NOT YET CODED IN'
        STOP 'PPRIME'
     ENDIF
     !
  ENDIF
  !
  RETURN
  !
  !**********************************************************************
  !                                                                     *
  !  INTERPOLATE P' WITH CUBIC SPLINES ON RPRM VALUES (FOR BALLOONING   *
  !  OPTIMIZATION ONLY).                                                *
  !                                                                     *
  !**********************************************************************
  !
11 CONTINUE 
  !
  CALL PPSPLN(KN,PP,NPPR,PCSM,RPRM,D2RPRM,PT,ZDUMAR,0)
  !
  RETURN
END SUBROUTINE PPRIME
