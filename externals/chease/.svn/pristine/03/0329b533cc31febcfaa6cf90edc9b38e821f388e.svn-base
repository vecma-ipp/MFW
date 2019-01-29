!!*DECK C2SB01
!*CALL PROCESS
SUBROUTINE GUESS(KGUESS)
  !        ########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SB01 INITIALIZE PICARD ITERATION:                                 *
  !        KGUESS = 1:  PSI(I,J) = -.1 * (1 - CSIG(I,J)**2)             *
  !        KGUESS = 2:  INTEPOLATE SOLUTION ON PREVIOUS DISCRETIZATION  *
  !                     MESH                                            *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZJDB0
  INTEGER          ::     J14
  INTEGER          ::     J12
  INTEGER          ::     J10
  INTEGER          ::     J11
  INTEGER          ::     ISIPR
  INTEGER          ::     ISIPRI
  INTEGER          ::     J9
  INTEGER          ::     J7
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZPCEL
  INTEGER          ::     I
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J4
  INTEGER          ::     IT0
  INTEGER          ::     JT
  INTEGER          ::     IS0
  INTEGER          ::     IS0I
  REAL(RKIND)      ::     ZSIGMA
  INTEGER          ::     JG
  INTEGER          ::     JS
  INTEGER          ::     IC
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZT
  REAL(RKIND)      ::     ZS
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZWORK
  INTEGER          ::     J1
  INTEGER          ::     J6
  INTEGER          ::     KGUESS
  DIMENSION &
       &   IC(NPT+NPPSI1), IS0(NPT+NPPSI1), IS0I(NPPSI1), IT0(NPT+NPPSI1), &
       &   ZBND(NPT),  ZPCEL(NPT,16),  ZF(NPT,16), &
       &   ZS(NPT),    ZT(NPT),        ZSIGMA(NPT), &
       &   ZT1(NPT),   ZT2(NPT),       ZS1(NPT),      ZS2(NPT), ZWORK(NPISOEFF)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  IF (KGUESS .EQ. 2) THEN
     !
     BPS( 1) = R0O
     BPS(12) = RZ0O
     !
     IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
     IF (NSURF .EQ. 6) CALL BNDSPL
     !
     DO J6=1,NS1
        !
        DO J1=1,NT
           !
           ZR = CSIG(J6) * RHOS(J1) * COS(CT(J1)) + R0
           ZZ = CSIG(J6) * RHOS(J1) * SIN(CT(J1)) + RZ0
           !
           ZS(J1) = SQRT((ZR - R0O)**2 + (ZZ - RZ0O)**2)
           !
           IF (ZR .EQ. R0O) THEN
              ZT(J1) = CTO(J1)
           ELSE
              ZT(J1) = ATAN2(ZZ - RZ0O,ZR - R0O)
           ENDIF
           !
           IF (ZT(J1) .LT. CTO(1)) ZT(J1) = ZT(J1) + 2._RKIND * CPI
           !
        END DO
        !
        CALL BOUND(NT,ZT,ZBND)
        !
        CALL RESETI(IC,NT,1)
        DO JS = 1,NSO+1
           DO JG=1,NT
              IF (IC(JG).EQ.1) THEN
                 ZSIGMA(JG) = ZS(JG) / ZBND(JG)
                 IS0(JG) = JS-1
                 IF (ZSIGMA(JG).LE.CSIGO(JS)) IC(JG)  = 0
              ENDIF
           ENDDO
        ENDDO
        CALL RESETI(IC,NT,1)
        DO JT = 1,NTO+1
           DO JG=1,NT
              IF (IC(JG).EQ.1) THEN
                 IT0(JG) = JT-1
                 IF (ZT(JG).LE.CTO(JT)) IC(JG)  = 0
              ENDIF
           ENDDO
        ENDDO
        !
        DO J4=1,NT
           !
           IF (IS0(J4).LT.  1) IS0(J4) = 1
           IF (IS0(J4).GE.NSO) IS0(J4) = NSO
           IF (IT0(J4).LT.  1) IT0(J4) = 1
           IF (IT0(J4).GE.NTO) IT0(J4) = NTO
           !
           ZS1(J4) = CSIGO(IS0(J4))
           ZS2(J4) = CSIGO(IS0(J4)+1)
           ZT1(J4) = CTO(IT0(J4))
           ZT2(J4) = CTO(IT0(J4)+1)
           !
           I = (IS0(J4) - 1) * NTO + IT0(J4)
           !
           ZPCEL(J4, 1) = CPSIO(4*I-3)
           ZPCEL(J4, 2) = CPSIO(4*I-2)
           ZPCEL(J4, 3) = CPSIO(4*I-1)
           ZPCEL(J4, 4) = CPSIO(4*I  )
           ZPCEL(J4, 5) = CPSIO(4*(I+NTO)-3)
           ZPCEL(J4, 6) = CPSIO(4*(I+NTO)-2)
           ZPCEL(J4, 7) = CPSIO(4*(I+NTO)-1)
           ZPCEL(J4, 8) = CPSIO(4*(I+NTO)  )
           !
           IF (IT0(J4) .NE. NTO) THEN
              !
              ZPCEL(J4, 9) = CPSIO(4*I+1)
              ZPCEL(J4,10) = CPSIO(4*I+2)
              ZPCEL(J4,11) = CPSIO(4*I+3)
              ZPCEL(J4,12) = CPSIO(4*I+4)
              ZPCEL(J4,13) = CPSIO(4*(I+NTO)+1)
              ZPCEL(J4,14) = CPSIO(4*(I+NTO)+2)
              ZPCEL(J4,15) = CPSIO(4*(I+NTO)+3)
              ZPCEL(J4,16) = CPSIO(4*(I+NTO)+4)
              !
           ELSE
              !
              ZPCEL(J4, 9) = CPSIO(4*(I-NTO)+1)
              ZPCEL(J4,10) = CPSIO(4*(I-NTO)+2)
              ZPCEL(J4,11) = CPSIO(4*(I-NTO)+3)
              ZPCEL(J4,12) = CPSIO(4*(I-NTO)+4)
              ZPCEL(J4,13) = CPSIO(4*I+1)
              ZPCEL(J4,14) = CPSIO(4*I+2)
              ZPCEL(J4,15) = CPSIO(4*I+3)
              ZPCEL(J4,16) = CPSIO(4*I+4)
              !
           ENDIF
           !
        END DO
        !
        CALL BASIS1(NT,NPT,ZS1,ZS2,ZT1,ZT2,ZSIGMA,ZT,ZF)
        !
        DO J5=1,NT
           !
           I = 4 * ((J6 - 1) * NT + J5) - 3
           !
           CPSICL(I) = ZF(J5, 1) * ZPCEL(J5, 1) + &
                &               ZF(J5, 2) * ZPCEL(J5, 2) + &
                &               ZF(J5, 3) * ZPCEL(J5, 3) + &
                &               ZF(J5, 4) * ZPCEL(J5, 4) + &
                &               ZF(J5, 5) * ZPCEL(J5, 5) + &
                &               ZF(J5, 6) * ZPCEL(J5, 6) + &
                &               ZF(J5, 7) * ZPCEL(J5, 7) + &
                &               ZF(J5, 8) * ZPCEL(J5, 8) + &
                &               ZF(J5, 9) * ZPCEL(J5, 9) + &
                &               ZF(J5,10) * ZPCEL(J5,10) + &
                &               ZF(J5,11) * ZPCEL(J5,11) + &
                &               ZF(J5,12) * ZPCEL(J5,12) + &
                &               ZF(J5,13) * ZPCEL(J5,13) + &
                &               ZF(J5,14) * ZPCEL(J5,14) + &
                &               ZF(J5,15) * ZPCEL(J5,15) + &
                &               ZF(J5,16) * ZPCEL(J5,16)
           !
        END DO
     END DO
     !
     ! SMOOTH THE NEW SOLUTION WITH BICUBIC SPLINES AND COMPUTE
     ! DERIVATIVES ON THE (SIGMA; THETA) GRID
     !
     IF (NSMOOTH.EQ.1) CALL SMOOTH
     !
     DO J7=1,NT
        !
        I = 4 * (J7 - 1)
        !
        CPSICL(I+2) = 0._RKIND
        CPSICL(I+3) = 0._RKIND
        CPSICL(I+4) = 0._RKIND
        !
     END DO
     !
     BPS( 1) = R0
     BPS(12) = RZ0
     !
     IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
     IF (NSURF .EQ. 6) CALL BNDSPL
     !
     SPSIM = CPSICL(1)
     !
     IF (NSTTP .GE. 2) THEN
        !
        CALL RESETI(IC,NISO,1)
        DO JS = 1,NISOO
           DO JG=1,NISO
              IF (IC(JG).EQ.1) THEN
                 IS0(JG) = JS-1
                 IF (CSIPR(JG).LE.CSIPRO(JS)) IC(JG)  = 0
              ENDIF
           ENDDO
        ENDDO
        !  RECONSTRUCT CSIPRI ALA CS FROM CSIPRO ALA CSM
        ZWORK(1)=0._RKIND
        DO JS=1,NISOO-2
           ZWORK(JS+1)=2*CSIPRO(JS)-ZWORK(JS)
        END DO
        ZWORK(NISOO)=1._RKIND
        CALL RESETI(IC,NISO,1)
        DO JS = 1,NISOO
           DO JG=1,NISO
              IF (IC(JG).EQ.1) THEN
                 IS0I(JG) = JS-1
                 IF (CSIPRI(JG).LE.ZWORK(JS)) IC(JG)  = 0
              ENDIF
           ENDDO
        ENDDO
        !
        DO J9=1,NISO
           !
           ISIPR = IS0(J9)
           ISIPRI = IS0I(J9)
           !
           IF (ISIPR .GT. NISOO - 2) ISIPR = NISOO - 2
           IF (ISIPR .LT. 2)         ISIPR = 2
           IF (ISIPRI .GT. NISOO - 2) ISIPRI = NISOO - 2
           IF (ISIPRI .LT. 2)         ISIPRI = 2
           !
           CID0(J9)   = FCCCC0(CID0O(ISIPR-1),CID0O(ISIPR), &
                &                             CID0O(ISIPR+1),CID0O(ISIPR+2), &
                &                             CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                &                             CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                &                             CSIPR(J9))
           D2CID0(J9) = FCCCC0(D2CID0O(ISIPR-1),D2CID0O(ISIPR), &
                &                             D2CID0O(ISIPR+1),D2CID0O(ISIPR+2), &
                &                             CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                &                             CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                &                             CSIPR(J9))
           CID2(J9)   = FCCCC0(CID2O(ISIPR-1),CID2O(ISIPR), &
                &                             CID2O(ISIPR+1),CID2O(ISIPR+2), &
                &                             CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                &                             CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                &                             CSIPR(J9))
           D2CID2(J9) = FCCCC0(D2CID2O(ISIPR-1),D2CID2O(ISIPR), &
                &                             D2CID2O(ISIPR+1),D2CID2O(ISIPR+2), &
                &                             CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                &                             CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                &                             CSIPR(J9))
           TTP(J9)    = FCCCC0(TTPO(ISIPR-1),TTPO(ISIPR), &
                &                             TTPO(ISIPR+1),TTPO(ISIPR+2), &
                &                             CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                &                             CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                &                             CSIPR(J9))
           CPPR(J9)   = FCCCC0(CPPRO(ISIPR-1),CPPRO(ISIPR), &
                &                             CPPRO(ISIPR+1),CPPRO(ISIPR+2), &
                &                             CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                &                             CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                &                             CSIPR(J9))
           CIDRTOR(J9)   = FCCCC0(CIDRTORO(ISIPRI-1),CIDRTORO(ISIPRI), &
                &                             CIDRTORO(ISIPRI+1),CIDRTORO(ISIPRI+2), &
                &                             ZWORK(ISIPRI-1),ZWORK(ISIPRI), &
                &                             ZWORK(ISIPRI+1),ZWORK(ISIPRI+2), &
                &                             CSIPRI(J9))
           D2CIDRTOR(J9) = FCCCC0(D2CIDRTORO(ISIPRI-1),D2CIDRTORO(ISIPRI), &
                &                             D2CIDRTORO(ISIPRI+1),D2CIDRTORO(ISIPRI+2), &
                &                             ZWORK(ISIPRI-1),ZWORK(ISIPRI), &
                &                             ZWORK(ISIPRI+1),ZWORK(ISIPRI+2), &
                &                             CSIPRI(J9))
           !
           IF ((NSTTP .EQ. 3) .OR. (NSTTP .EQ. 4)) THEN
              !
              TMF(J9)    = FCCCC0(TMFO(ISIPR-1),TMFO(ISIPR), &
                   &                                TMFO(ISIPR+1),TMFO(ISIPR+2), &
                   &                                CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                   &                                CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                   &                                CSIPR(J9))
              D2TMF(J9)  = FCCCC0(D2TMFO(ISIPR-1),D2TMFO(ISIPR), &
                   &                                D2TMFO(ISIPR+1),D2TMFO(ISIPR+2), &
                   &                                CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                   &                                CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                   &                                CSIPR(J9))
              !
           ENDIF
           !
           IF (NPROFZ .EQ. 1) THEN
              !
              D2CPPR(J9) = FCCCC0(D2CPPRO(ISIPR-1),D2CPPRO(ISIPR), &
                   &                                D2CPPRO(ISIPR+1),D2CPPRO(ISIPR+2), &
                   &                                CSIPRO(ISIPR-1),CSIPRO(ISIPR), &
                   &                                CSIPRO(ISIPR+1),CSIPRO(ISIPR+2), &
                   &                                CSIPR(J9))
              !
           ENDIF
           !
        END DO
        IF (CIDRTOR(1) .LT. 0._RKIND) CIDRTOR(1) = 0._RKIND
        !
     ENDIF
     !
  ELSE IF (KGUESS .EQ. 1) THEN
     !
     SPSIM = - 0.1_RKIND
     RMAG  = R0
     RZMAG = RZ0
     !
     DO J11=1,NS1
        !
        DO J10=1,NT
           !
           I = (J11 - 1) * NT + J10
           !
           CPSICL(4*I-3) = SPSIM * (1 - CSIG(J11)**2)
           CPSICL(4*I-2) = - 2._RKIND * SPSIM * CSIG(J11)
           CPSICL(4*I-1) = 0._RKIND
           CPSICL(4*I  ) = 0._RKIND
           !
        END DO
     END DO
     !
     IF (NSTTP .GE. 2) THEN
        !
        CALL VZERO(CID0,NPISOEFF)
        CALL VZERO(CIDQ,NPISOEFF)
        CALL VZERO(CIDR,NPISOEFF)
        CALL VZERO(CID2,NPISOEFF)
        CALL VZERO(SIGPSI,NPMGS*NTP1*NPISOEFF)
        CALL VZERO(TETPSI,NPMGS*NTP1*NPISOEFF)
        CALL VZERO(WGTPSI,NPMGS*NTP1*NPISOEFF)
        CALL RESETR(TMF,NPISOEFF,RC1P)
        !
        DO J12=1,NISO
           PSIISO(J12)   = SPSIM * (1._RKIND - CSIPR(J12)**2)
           TETMAP(1,J12) = 0._RKIND
           SIGMAP(1,J12) = CSIPR(J12)
        END DO
        !
        CALL ISOFIND(1,NISO,SIGPSI,TETPSI,WGTPSI,SPSIM,RC0P)
        !
        DO J14=1,NISO
           CALL CINT(J14,SIGPSI(1,J14),TETPSI(1,J14),WGTPSI(1,J14))
        END DO
        !
        !        COMPUTE RHO_TOR MESH IF NEEDED TO GET PROFILES
        IF (NRHOMESH .GE. 0) THEN
           !        COMPUTE RHOTOR ON CSIPRI MESH, ASSUME T=1 CST
           CIDRTOR(1)=0._RKIND
           DO J1=2,NISO
              CIDRTOR(J1) = CIDRTOR(J1-1) + CIDQ(J1-1)*SPSIM*(CSIPRI(J1-1)**2-CSIPRI(J1)**2)
           END DO
           CIDRTOR(1:NISO) = sqrt(CIDRTOR(1:NISO)/CIDRTOR(NISO))
!!$                  PRINT *,'CSIPRI, CIDRTOR, NISO= ',NISO
!!$                  WRITE(*,'1P2E15.5') (CSIPRI(J1), CIDRTOR(J1), J1=1,NISO)
           CALL SPLINE(NISO,CSIPRI,CIDRTOR,D2CIDRTOR,ZWORK)
        ENDIF
        !
        IF ((NSTTP .EQ. 3) .OR. (NSTTP .EQ. 4)) THEN
           !
           IF (NCSCAL .EQ. 1 .OR. NCSCAL .EQ. 3) THEN
              !
              CALL PRFUNC(1,SPSIM,ZJDB0)
              !
              SCALE  = (2._RKIND / (QSPEC * ZJDB0))**SCEXP
              SCALAC = SCALE * SCALAC
              !
           ELSE IF (NCSCAL .EQ. 2) THEN
             !
             IF (NVERBOSE .GE. 0) THEN
               PRINT*,'NOREPT : NSTTP=',NSTTP,'; NCSCAL=2; NOPT=',NOPT,';'
               PRINT*,'NBLOPT=',NBLOPT,'; CPRESS=',CPRESS,';'
               PRINT*,'THIS OPTION IS NOT POSSIBLE'
             END IF
             !                     STOP
             !
           ENDIF
           !
           IF (NFUNC .EQ. 1) THEN
              !
              CALL DSCAL(NSOUR,SCALE,AT,1)
              !
           ELSE IF (NFUNC .EQ. 2) THEN
              !
              CALL DSCAL(5,SCALE,AT(3),1)
              CALL DSCAL(5,SCALE,AT2(3),1)
              CALL DSCAL(5,SCALE,AT3(3),1)
              !
              AT4(3) = SCALE * AT4(3)
              !
           ELSE IF (NFUNC .EQ. 3) THEN
              !
              AT(1) = SCALE * AT(1)
              !
           ELSE IF (NFUNC .EQ. 4) THEN
              !
              CALL DSCAL(NPPF+1,SCALE,RFUN,1)
              !
           ENDIF
           !
           CALL RVAR('SCALE            ',SCALE)
           CALL RVAR('ACCUMULATED SCALE',SCALAC)
           !
        ENDIF
        !
        CALL ISOFUN(NISO)
        !
     ENDIF
  ENDIF
  !
  RETURN
END SUBROUTINE GUESS
