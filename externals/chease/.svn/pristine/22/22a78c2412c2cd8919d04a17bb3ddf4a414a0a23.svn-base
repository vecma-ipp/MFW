!*DECK C2SE05
!*CALL PROCESS
SUBROUTINE SMOOTH
  !        #################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SE05  SMMOTH BICUBIC HERMITE SOLUTION WITH BICUBIC SPLINES        *
  !         (SEE APPENDIX C OF PUBLICATION)                             *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J11
  INTEGER          ::     IS
  INTEGER          ::     J9
  INTEGER          ::     IT
  INTEGER          ::     J10
  REAL(RKIND)      ::     ZDUM2
  REAL(RKIND)      ::     ZDUM1
  INTEGER          ::     IP2
  INTEGER          ::     IP1
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZERD2P
  REAL(RKIND)      ::     ZERDPT
  REAL(RKIND)      ::     ZC2
  REAL(RKIND)      ::     ZB2
  REAL(RKIND)      ::     ZA2
  REAL(RKIND)      ::     YD2P2
  REAL(RKIND)      ::     Y2
  REAL(RKIND)      ::     X2
  INTEGER          ::     J6
  INTEGER          ::     J7
  REAL(RKIND)      ::     ZDUM
  INTEGER          ::     IP
  INTEGER          ::     J4
  INTEGER          ::     J5
  REAL(RKIND)      ::     ZERDPS
  REAL(RKIND)      ::     ZWORK
  REAL(RKIND)      ::     ZB1
  REAL(RKIND)      ::     ZA1
  REAL(RKIND)      ::     YD2P1
  REAL(RKIND)      ::     YPN
  REAL(RKIND)      ::     YP1
  INTEGER          ::     J3
  REAL(RKIND)      ::     Y1
  REAL(RKIND)      ::     X1
  INTEGER          ::     J1
  INTEGER          ::     J2
  DIMENSION X1(NPT,NSP1),    Y1(NPT,NSP1),    YD2P1(NPT,NSP1), &
       &             X2(2*NSP1,NPT),  Y2(2*NSP1,NPT),  YD2P2(2*NSP1,NPT), &
       &             ZA1(NPT,NSP1),   ZB1(NPT,NSP1),   ZWORK(NPT+2*NSP1), &
       &             ZA2(2*NSP1,NPT), ZB2(2*NSP1,NPT), ZC2(2*NSP1,NPT), &
       &             YP1(NPT),        YPN(NPT)
  !
  DO J2=1,NS1
     DO J1=1,NT
        !
        X1(J1,J2) = CSIG(J2)
        Y1(J1,J2) = CPSICL(4*((J2-1)*NT+J1)-3)
        !
     END DO
  END DO
  !
  DO J3=1,NT
     !
     YP1(J3) = CPSICL(4*J3-2)
     YPN(J3) = CPSICL(4*(NS*NT+J3)-2)
     !
  END DO
  !
  CALL MSPLINE(X1,Y1,NS1,NPT,NT,YP1,YPN,YD2P1,ZA1,ZB1,ZWORK)
  !
  ZERDPS = 0._RKIND
  !
  DO J5=2,NS
     DO J4=1,NT
        !
        IP = 4 * ((J5 - 1) * NT + J4) - 2
        !
        ZDUM      = CPSICL(IP)
        CPSICL(IP) = &
             &            (Y1(J4,J5) - Y1(J4,J5-1)) / &
             &            (X1(J4,J5) - X1(J4,J5-1)) + &
             &            YD2P1(J4,J5-1) * (X1(J4,J5) - X1(J4,J5-1)) / 6._RKIND + &
             &            YD2P1(J4,J5) * (X1(J4,J5) - X1(J4,J5-1)) / 3._RKIND
        !
        ZERDPS = ZERDPS + (ZDUM - CPSICL(IP))**2
        !
     END DO
  END DO
  !
  ZERDPS = SQRT(ZERDPS) / REAL(NS1 * NT,RKIND)
  !
  DO J7=1,NT
     DO J6=1,NS1
        !
        X2(J6,J7)     = CT(J7)
        X2(NS1+J6,J7) = CT(J7)
        Y2(J6,J7)     = CPSICL(4*((J6-1)*NT+J7)-3)
        Y2(NS1+J6,J7) = CPSICL(4*((J6-1)*NT+J7)-2)
        !
     END DO
  END DO
  !
  CALL MSPLCY(X2,Y2,NT,2*NSP1,2*NS1,RC2PI,YD2P2, &
       &               ZA2,ZB2,ZC2,ZWORK)
  !
  ZERDPT = 0._RKIND
  ZERD2P = 0._RKIND
  !
  DO J8=2,NS1
     !
     IP1   = 4 * (J8 - 1) * NT + 3
     IP2   = 4 * (J8 - 1) * NT + 4
     ZDUM1 = CPSICL(IP1)
     ZDUM2 = CPSICL(IP2)
     !
     CPSICL(IP1) = &
          &         (Y2(J8,2) - Y2(J8,1)) / (X2(J8,2) - X2(J8,1)) - &
          &         YD2P2(J8,1) * (X2(J8,2) - X2(J8,1)) / 3._RKIND - &
          &         YD2P2(J8,2) * (X2(J8,2) - X2(J8,1)) / 6._RKIND
     !
     CPSICL(IP2) = &
          &         (Y2(NS1+J8,2) - Y2(NS1+J8,1)) / &
          &         (X2(NS1+J8,2) - X2(NS1+J8,1)) - &
          &         YD2P2(NS1+J8,1) * (X2(NS1+J8,2) - X2(NS1+J8,1)) / 3._RKIND - &
          &         YD2P2(NS1+J8,2) * (X2(NS1+J8,2) - X2(NS1+J8,1)) / 6._RKIND
     !
     ZERDPT = ZERDPT + (ZDUM1 - CPSICL(IP1))**2
     ZERD2P = ZERD2P + (ZDUM2 - CPSICL(IP2))**2
     !
  END DO
  !
  DO J10=2,NT
     !
     IT = J10 - 1
     !
     DO J9=2,NS1
        !
        IS    = NS1 + J9
        IP1   = 4 * ((J9 - 1) * NT + J10) - 1
        IP2   = 4 * ((J9 - 1) * NT + J10)
        ZDUM1 = CPSICL(IP1)
        ZDUM2 = CPSICL(IP2)
        !
        CPSICL(IP1) = &
             &            (Y2(J9,J10) - Y2(J9,IT)) / (X2(J9,J10) - X2(J9,IT)) + &
             &            YD2P2(J9,IT) * (X2(J9,J10) - X2(J9,IT)) / 6._RKIND + &
             &            YD2P2(J9,J10) * (X2(J9,J10) - X2(J9,IT)) / 3._RKIND
        !
        CPSICL(IP2) = &
             &            (Y2(IS,J10) - Y2(IS,IT)) / (X2(IS,J10) - X2(IS,IT)) + &
             &            YD2P2(IS,IT) * (X2(IS,J10) - X2(IS,IT)) / 6._RKIND + &
             &            YD2P2(IS,J10) * (X2(IS,J10) - X2(IS,IT)) / 3._RKIND
        !
        ZERDPT = ZERDPT + (ZDUM1 - CPSICL(IP1))**2
        ZERD2P = ZERD2P + (ZDUM2 - CPSICL(IP2))**2
        !
     END DO
  END DO
  !
  ZERDPT = SQRT(ZERDPT) / REAL(NS1 * NT,RKIND)
  ZERD2P = SQRT(ZERD2P) / REAL(NS1 * NT,RKIND)
  !
  DO J11=1,NS1*NT
     !
     CPSI(4*NUPDWN(J11)-3) = CPSICL(4*J11-3)
     CPSI(4*NUPDWN(J11)-2) = CPSICL(4*J11-2)
     CPSI(4*NUPDWN(J11)-1) = CPSICL(4*J11-1)
     CPSI(4*NUPDWN(J11)-1) = CPSICL(4*J11)
     !
  END DO
  !
END SUBROUTINE SMOOTH
