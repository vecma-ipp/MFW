!
SUBROUTINE ISOFUN(KN)
  !        ##########
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SJ02  COMPUTES T, T-TPRIME, PRESSURE AND P-PRIME PROFILES AT     *
  !          CONSTANT FLUX SURFACES                                     *
  !                                                                     *
  !**********************************************************************
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J301
  INTEGER          ::     J9
  INTEGER          ::     J8P1
  INTEGER          ::     J8
  INTEGER          ::     J7P1
  INTEGER          ::     J7
  INTEGER          ::     J6
  INTEGER          ::     J5
  INTEGER          ::     J4
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZDTEDP
  REAL(RKIND)      ::     ZDNEDP
  INTEGER          ::     J
  INTEGER          ::     I
  REAL(RKIND)      ::     ZFAC
  INTEGER          ::     J2P1
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZCPR
  INTEGER          ::     J101
  INTEGER          ::     J100
  REAL(RKIND)      ::     ZTEMP
  REAL(RKIND)      ::     ZWORK
  REAL(RKIND)      ::     ZD2PPR
  INTEGER          ::     IN
  REAL(RKIND)      ::     ZPISO
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZCPPR
  INTEGER          ::     KN
  DIMENSION &
       &   ZDNEDP(KN+1), ZDTEDP(KN+1), &
       &   ZCPR(KN+1),   ZCPPR(KN+1), &
       &   ZD2PPR(KN+1), ZPISO(KN+1), &
       &   ZS(KN+1),     ZWORK(KN+1), &
       &   ztemp(KN+1)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  IF (NSURF .EQ. 1) GOTO 300
  !
  IF (NPROFZ .EQ. 0) THEN
     !
     CALL PPRIME(KN,PSIISO,ZCPPR)
     !
     !**********************************************************************
     !                                                                     *
     !  INTEGRATE P'   BY CUBIC SPLINE QUADRATURE (SEE FORSYTHE G.E.,      *
     !  MALCOLM M.A., MOLER C.B., COMPUTER METHODS FOR MATHEMATICAL        *
     !  COMPUTATIONS (ENGLEWOOD CLIFFS, N.J., PRENTICE HALL), $5.2,        *
     !  P. 89, (1977)                                                      *
     !                                                                     *
     !**********************************************************************
     !
     DO J1=1,KN
        !
        ZS(J1)    = SQRT(1 - PSIISO(J1) / SPSIM)
        ZPISO(J1) = PSIISO(J1)
        !
     END DO
     !
     IN = KN
     !
     IF (ZS(IN) .NE. 1._RKIND) THEN
        !
        IN        = KN + 1
        ZS(IN)    = 1._RKIND
        ZPISO(IN) = 0._RKIND
        !
        CALL PPRIME(1,ZPISO(IN),ZCPPR(IN))
        !
     ENDIF
     !
     !%OS            CALL SPLINE(IN,ZS,ZCPPR,ZD2PPR,ZWORK)
     CALL SPLINE(IN,ZPISO,ZCPPR,ZD2PPR,ZWORK)
     ztemp(1) = zd2ppr(1)
     ztemp(in) = zd2ppr(in)
     do j100 = 2,in-1
        ztemp(j100) = zd2ppr(j100)
        if (abs(zd2ppr(j100)).lt.1.e3_RKIND) goto 100
        if (((zd2ppr(j100-1) .ge. 0._RKIND) .and. (zd2ppr(j100) .ge. 0._RKIND)) &
             &             .and. (zd2ppr(j100+1) .ge. 0._RKIND)) goto 100
        if (((zd2ppr(j100-1) .le. 0._RKIND) .and. (zd2ppr(j100) .le. 0._RKIND)) &
             &             .and. (zd2ppr(j100+1) .le. 0._RKIND)) goto 100
        IF (NVERBOSE .GE. 1) write(*,*) ' warning inaccurate integration, i =',j100
100     CONTINUE
     END DO
!!$               print *,'j  zpiso   zs  PSIISO  csmtor  ; niso= ',niso
!!$               write(*,*) (j100,zpiso(j100),zs(j100),PSIISO(j100),csmtor(j100),j100=1,in)
     if (ztemp(in-1)*ztemp(in).le.0._RKIND) ztemp(in) = 0._RKIND
     if (ztemp(1)*ztemp(2).le.0._RKIND) ztemp(1) = 0._RKIND
     do j101 = 1,in
        zd2ppr(j101) = ztemp(j101)
     END do
     !
     ZCPR(IN) = PREDGE
     !
     DO J2=IN-1,1,-1
        !
        J2P1 = J2 + 1
        !
        ZCPR(J2) = ZCPR(J2P1) - (ZPISO(J2P1) - ZPISO(J2)) * &
             &                            (.5_RKIND*(ZCPPR(J2) + ZCPPR(J2P1)) - &
             &                             (ZD2PPR(J2) + ZD2PPR(J2P1)) * &
             &                             (ZPISO(J2P1) - ZPISO(J2))**2/24._RKIND)
        !
     END DO
     !
     !%OS  CHECK IF JUMP IN ZCPR (AS ZD2PPR CAN BE WILD)
     ZFAC = 3._RKIND
     DO I=11,IN-1
        IF (ABS(ZCPR(I+1)-ZCPR(I)) .GT. ABS( ZFAC * &
             &           0.5_RKIND*(ZCPPR(I)+ZCPPR(I+1))*(ZPISO(I+1)-ZPISO(I)))) THEN
           PRINT *,' JUMP IN PRESSURE AT I= ',I
           PRINT *,'  I     ZPISO     ZCPPR     ZD2PPR     ZCPR'
           WRITE(6,'(I3,1P4E14.5)') &
                &            (J,ZPISO(J),ZCPPR(J),ZD2PPR(J),ZCPR(J),J=1,IN)
           ! STOP 'ISOFUN CHECK'
        END IF
     END DO
     !%OS
     !
     CALL DCOPY(KN,ZCPR,1,CPR,1)
     CALL DCOPY(KN,ZCPPR,1,CPPR,1)
     !%OS            CALL DCOPY(KN,ZD2PPR,1,D2CPPR,1)
     CALL SPLINE(IN,ZS,ZCPPR,D2CPPR,ZWORK)
     CALL SPLINE(KN,ZS,CPR,D2CPR,ZWORK)
     !
  ELSE IF (NPROFZ .EQ. 1) THEN
     !
     CALL POLYNM(KN,AP,DENSTY,1)
     CALL POLYNM(KN,AT,TEMPER,1)
     CALL POLYNM(KN,AP,ZDNEDP,2)
     CALL POLYNM(KN,AT,ZDTEDP,2)
     CALL DSCAL(KN,SCALNE,DENSTY,1)
     CALL DSCAL(KN,SCALNE,ZDNEDP,1)
     !
     DO J3=1,KN
        !
        CPR(J3)  = DENSTY(J3) * TEMPER(J3)
        CPPR(J3) = ZDNEDP(J3) * TEMPER(J3) + &
             &                 DENSTY(J3) * ZDTEDP(J3)
        !
        IF (CPPR(J3) .GT. 0._RKIND) CPPR(J3) = 0._RKIND
        !
     END DO
     !
     CALL SPLINE(KN,ZS,CPPR,D2CPPR,ZWORK)
     CALL SPLINE(KN,ZS,CPR,D2CPR,ZWORK)
     !
  ENDIF
  !
  IF (NSTTP .EQ. 1) THEN
     !
     CALL PRFUNC(KN,PSIISO,TTP)
     !
  ELSE IF (NSTTP .EQ. 2) THEN
     !
     IF (NPROFZ .EQ. 0) THEN
        !
        CALL PRFUNC(KN,PSIISO,CIPR)
        !
     ELSE IF (NPROFZ .EQ. 1) THEN
        !
        DO J4=1,KN
           !
           CIPR(J4) = TEMPER(J4) * SQRT(TEMPER(J4))
           !
           IF (CIPR(J4) .LT. 0._RKIND) CIPR(J4) = 0._RKIND
           !
        END DO
        ! 
     ENDIF
     !
     DO J5=1,KN
        !
        TTP(J5) = - CIPR(J5) * CID0(J5) - &
             &                  CPPR(J5) * CID2(J5)
        ZS(J5)  = SQRT(1 - PSIISO(J5) / SPSIM)
        !
     END DO
     !
  ELSE IF (NSTTP .EQ. 3) THEN
     !
     CALL PRFUNC(KN,PSIISO,CIPR)
     !
     DO J6=1,KN
        !
        TTP(J6) = - (CIPR(J6) + CPPR(J6) * CID0(J6)) / &
             &                  (1._RKIND + CID2(J6) / TMF(J6)**2)
        ZS(J6)  = SQRT(1 - PSIISO(J6) / SPSIM)
        !
     END DO
     !
  ELSE IF (NSTTP .EQ. 4) THEN
     !
     CALL PRFUNC(KN,PSIISO,CIPR)
     !
     DO J6=1,KN
        !
        TTP(J6) = - (CIPR(J6)/TMF(J6) + CPPR(J6)) * CID0(J6) / &
             &                  (1._RKIND + CID2(J6) / TMF(J6)**2)
        ZS(J6)  = SQRT(1 - PSIISO(J6) / SPSIM)
        !
     END DO
     !
  ENDIF
  !
  CALL SPLINE(KN,ZS,CID0,D2CID0,ZWORK)
  CALL SPLINE(KN,ZS,CID2,D2CID2,ZWORK)
  !
  !**********************************************************************
  !                                                                     *
  !  INTEGRATE T T' BY CUBIC SPLINE QUADRATURE (SEE FORSYTHE G.E.,      *
  !  MALCOLM M.A., MOLER C.B., COMPUTER METHODS FOR MATHEMATICAL        *
  !  COMPUTATIONS (ENGLEWOOD CLIFFS, N.J., PRENTICE HALL), $5.2,        *
  !  P. 89, (1977)                                                      *
  !                                                                     *
  !**********************************************************************
  !
  !%OS         CALL SPLINE(KN,ZS,TTP,D2TTP,ZWORK)
  CALL SPLINE(KN,PSIISO,TTP,D2TTP,ZWORK)
  !
  IF (NTMF0.EQ.0) THEN
     !
     TMF(KN) = 0.5_RKIND
     !
     DO J7=KN-1,1,-1
        !
        J7P1 = J7 + 1
        !
        TMF(J7) = TMF(J7P1) - (PSIISO(J7P1) - PSIISO(J7)) * &
             &                            (.5_RKIND*(TTP(J7) + TTP(J7P1)) - &
             &                            (D2TTP(J7) + D2TTP(J7P1)) * &
             &                            (PSIISO(J7P1) - PSIISO(J7))**2 / 24._RKIND)
        !
     END DO
     !
  ELSE
     !
     TMF(1) = 0.5_RKIND
     !
     DO J8=1,KN-1
        !
        J8P1 = J8 + 1
        !
        TMF(J8P1) = TMF(J8) + (PSIISO(J8P1) - PSIISO(J8)) * &
             &                            (.5_RKIND*(TTP(J8) + TTP(J8P1)) - &
             &                             (D2TTP(J8) + D2TTP(J8P1)) * &
             &                             (PSIISO(J8P1) - PSIISO(J8))**2 / 24._RKIND)
        !
     END DO
     !
  ENDIF
  !
  DO J9=1,KN
     !
     IF (TMF(J9) .LT. 0) THEN
        !
        PRINT*,'TMF(',J9,')**2 NEGATIVE. READ MESSAGE PRECEEDING', &
             &             'DO LOOP 8 IN SUBROUTINE ISOFUN'
        STOP
        !
     ENDIF
     !
     TMF(J9) = SQRT(2._RKIND * TMF(J9))
     !
  END DO
  !
  CALL SPLINE(KN,ZS,TTP,D2TTP,ZWORK)
  CALL SPLINE(KN,ZS,TMF,D2TMF,ZWORK)
  !
  RETURN
  !
300 CONTINUE 
  !
  !**********************************************************************
  !                                                                     *
  !  PROFILES FOR SOLOVEV CASE                                          *
  !                                                                     *
  !**********************************************************************
  !
  DO J301=1,KN
     !
     CPR(J301)  = PSIISO(J301) * CPP
     CPPR(J301) = CPP
     TMF(J301)  = 1._RKIND
     TTP(J301)  = 0._RKIND
     !
  END DO
  !
  RETURN
END SUBROUTINE ISOFUN
