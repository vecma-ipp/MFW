!
SUBROUTINE PSVOL
  !        ################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SA03 : DENSIFY STABILITY S-MESH SO THAT THE STABILITY MESH IS     *
  !          EQUIDISTANT IN RHO (SEE SECTION 6.4.5 IN PUBLICATION       *
  !          AND TABLE 1 FOR THE DEFINITION OF RHO)                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  INTEGER          ::     J11
  INTEGER          ::     IS
  REAL(RKIND)      ::     ZWS
  INTEGER          ::     J10
  REAL(RKIND)      ::     ZS
  REAL(RKIND)      ::     ZW
  INTEGER          ::     J9
  INTEGER          ::     J7
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZDVDS
  INTEGER          ::     J6
  INTEGER          ::     J5
  INTEGER          ::     J4
  INTEGER          ::     I
  INTEGER          ::     J3
  INTEGER          ::     J2
  INTEGER          ::     ISRCHFGE
  INTEGER          ::     IP
  REAL(RKIND)      ::     ZPAR
  REAL(RKIND)      ::     ZPISO
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZU2
  REAL(RKIND)      ::     ZU1
  REAL(RKIND)      ::     ZVOL
  DIMENSION &
       &   ZDVDS(NPPSI1), ZPAR(NPPSI1), ZPISO(NPPSI1),   ZS(NPPSI1), &
       &   ZU1(NPPSI1),   ZU2(NPPSI1),  ZVOL(NPPSI1),    ZW(NPPSI1)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(CID0,NPISOEFF)
  CALL VZERO(CIDR,NPISOEFF)
  CALL VZERO(CIDQ,NPISOEFF)
  CALL VZERO(CID2,NPISOEFF)
  CALL VZERO(ZVOL,NPPSI1)
  CALL VZERO(ZU1,NPPSI1)
  CALL VZERO(ZU2,NPPSI1)
  !
  DO J1=1,NPSI1
     !
     PSIISO(J1) = SPSIM * (1._RKIND - CSM(J1) * CSM(J1))
     ZPISO(J1)  = SPSIM * (1._RKIND - CS(J1) * CS(J1))
     !
  END DO
  !
  CALL RMRAD(NPSI1,SPSIM,RC0P,RC0P,ZPAR,SIGMAP,TETMAP,NTP2)
  !
  IP = ISRCHFGE(NPSI1,PSIISO,1,CPSICL(1))
  !
  IF (IP.LT.1)     IP = 1
  IF (IP.GT.NPSI1) IP = NPSI1
  !
  CALL ISOFIND(IP,NPSI1,SIGPSI,TETPSI,WGTPSI,SPSIM,RC0P)
  !
  DO J2=IP,NPSI1
     !
     CALL CINT(J2,SIGPSI(1,J2),TETPSI(1,J2),WGTPSI(1,J2))
     !
  END DO
  !
  IF (IP .GT. 1) THEN
     !
     DO J3=1,IP-1
        !
        I = J3
        !
        IF (J3 .EQ. 1) I = 2
        !
        CIDR(J3) = FCCCC0(CIDR(I-1),CIDR(I),CIDR(I+1),CIDR(I+2), &
             &                        PSIISO(I-1),PSIISO(I),PSIISO(I+1), &
             &                        PSIISO(I+2),PSIISO(J3))
        !
     END DO
     !
  ENDIF
  !
  ZVOL(1) = 0._RKIND
  !
  DO J4=2,NPSI1
     !
     ZVOL(J4) = ZVOL(J4-1) + CIDR(J4-1) * (ZPISO(J4)-ZPISO(J4-1))
     !
  END DO
  !
  DO J5=1,NPSI1
     !
     ZVOL(J5) = SQRT(ZVOL(J5))
     !
  END DO
  !
  DO J6=1,NPSI
     !
     ZDVDS(J6) = (ZVOL(J6+1) - ZVOL(J6)) / (CS(J6+1) - CS(J6))
     !
  END DO
  !
  ZU1(1) = 0._RKIND
  ZU2(1) = 0._RKIND
  !
  DO J8=2,NPSI1
     !
     ZU1(J8) = ZVOL(J8)
     !
     IF (NPOIDA .EQ. 0 .OR. NMESHA .EQ. 0) THEN
        !
        ZU2(J8) = ZVOL(J8)
        !
     ELSE
        !
        ZU2(J8) = ZU2(J8-1)
        !
        DO J7=1,NPOIDA
           !
           ZU2(J8) = ZU2(J8) + ZDVDS(J8-1) * AWIDTH(J7) / &
                &                (AWIDTH(J7)**2 + (APLACE(J7)-CSM(J8-1))**2) * &
                &                (CS(J8) - CS(J8-1))
           !
        END DO
        !
     ENDIF
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3. NORMALIZE IT TO ONE                                              *
  !                                                                     *
  !**********************************************************************
  !
  DO J9=1,NPSI1
     !
     ZW(J9) = SOLPDA * ZU1(J9) / ZU1(NPSI1) + &
          &            (1._RKIND - SOLPDA) * ZU2(J9) / ZU2(NPSI1)
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 4. FIND MESH POSITIONS                                              *
  !                                                                     *
  !**********************************************************************
  !
  ZS(1)     = 0._RKIND
  ZS(NPSI1) = 1._RKIND
  !
  DO J10=2,NPSI
     !
     ZWS = REAL(J10 - 1,RKIND) / REAL(NPSI,RKIND)
     !
     IS = ISRCHFGE(NPSI1,ZW,1,ZWS)
     !
     IF (IS.LT.2)     IS = 2
     IF (IS.GT.NPSI1) IS = NPSI1
     !
     ZS(J10) = CS(IS-1) + (CS(IS) - CS(IS-1)) * (ZWS - ZW(IS-1)) / &
          &                        (ZW(IS) -  ZW(IS-1))
     !
  END DO
  !
  CALL DCOPY(NPSI1,ZS,1,CS,1)
  !
  DO J11=1,NPSI
     !
     CSM(J11) = .5_RKIND * (CS(J11+1) + CS(J11))
     !
  END DO
  !
  CSM(NPSI1) = 1._RKIND
  !
  RETURN
END SUBROUTINE PSVOL
