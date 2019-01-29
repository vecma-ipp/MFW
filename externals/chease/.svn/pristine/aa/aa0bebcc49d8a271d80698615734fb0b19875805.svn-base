!*DECK C2SA04
!*CALL PROCESS
SUBROUTINE TETARE(PT,KN)
  !        ########################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SA04 : DENSIFY EQUILIBRIUM THETA-MESH AUTOMATICALLY               *
  !          NDIFT= 1: CONSTANT POLOIDAL FLUX BETWEEN 2 SUCESSIVE ANGLES*
  !          NDIFT= 2: CONSTANT ARC LENGHT AT PLASMA SURFACE FOR EVERY  *
  !                    THETA INTERVAL                                   *
  !          (SEE SECTION 6.4.5 IN PUBLICATION)                         *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZT
  INTEGER          ::     J9
  INTEGER          ::     I
  REAL(RKIND)      ::     ZF
  REAL(RKIND)      ::     ZDP
  INTEGER          ::     KN
  REAL(RKIND)      ::     PT
  REAL(RKIND)      ::     ZW
  INTEGER          ::     J7
  INTEGER          ::     J5
  INTEGER          ::     J6
  REAL(RKIND)      ::     ZDRSDT
  INTEGER          ::     J4
  INTEGER          ::     J2
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZBND2
  REAL(RKIND)      ::     ZBND1
  REAL(RKIND)      ::     ZBND
  REAL(RKIND)      ::     ZTET2
  REAL(RKIND)      ::     ZTET1
  REAL(RKIND)      ::     ZTET
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZDT
  REAL(RKIND)      ::     ZEPS
  REAL(RKIND)      ::     ZU2
  REAL(RKIND)      ::     ZU1
  INTEGER          ::     IM
  PARAMETER (IM = 301)
  !
  DIMENSION &
       &   PT(*), &
       &   ZBND(IM),   ZBND1(IM),   ZBND2(IM),   ZDRSDT(IM),   ZTET(IM), &
       &   ZTET1(IM),  ZTET2(IM),   ZU1(IM),     ZU2(IM),      ZW(IM)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  CALL VZERO(ZU1,IM)
  CALL VZERO(ZU2,IM)
  !
  ZEPS = 1.E-4_RKIND
  ZDT  = 2._RKIND*CPI / REAL(IM - 1,RKIND)
  !
  DO J1=1,IM
     !
     ZTET(J1)  = (J1 - 1) * ZDT
     ZTET1(J1) = ZTET(J1) - ZEPS
     ZTET2(J1) = ZTET(J1) + ZEPS
     !
  END DO
  !
  CALL BOUND(IM,ZTET,ZBND)
  CALL BOUND(IM,ZTET1,ZBND1)
  CALL BOUND(IM,ZTET2,ZBND2)
  !
  ZU1(1) = 0._RKIND
  ZU2(1) = 0._RKIND
  !
  IF (NDIFT .EQ. 1) THEN
     !
     DO J3=2,IM
        !
        ZU1(J3) = ZU1(J3-1) + .25_RKIND * (ZTET(J3) - ZTET(J3-1)) * &
             &             (ZBND(J3)**2 + ZBND(J3-1)**2)
        !
        IF (NPOIDD .EQ. 0 .OR. NMESHD .EQ. 0) THEN
           !
           ZU2(J3) = ZU1(J3)
           !
        ELSE
           !
           ZU2(J3) = ZU2(J3-1)
           !
           DO J2=1,NPOIDD
              !
              ZU2(J3)= ZU2(J3) + .5_RKIND * (ZTET(J3) - ZTET(J3-1)) * &
                   &               (ZBND(J3-1)**2 * DWIDTH(J2) / &
                   &               (DWIDTH(J2)**2+SIN((DPLACE(J2)-ZTET(J3-1))/2._RKIND)**2)+ &
                   &               ZBND(J3)**2 * DWIDTH(J2) / &
                   &               (DWIDTH(J2)**2+SIN((DPLACE(J2)-ZTET(J3))/2._RKIND)**2))
              !
           END DO
           !
        ENDIF
        !
     END DO
     !
  ELSE IF (NDIFT .EQ. 2) THEN
     !
     DO J4=1,IM
        !
        ZDRSDT(J4) = .5_RKIND *(ZBND2(J4) - ZBND1(J4)) / ZEPS
        !
     END DO
     !
     DO J6=2,IM
        !
        ZU1(J6) = ZU1(J6-1) + .5_RKIND * (ZTET(J6) - ZTET(J6-1)) * &
             &             (SQRT(ZBND(J6)**2 + ZDRSDT(J6)**2) + &
             &              SQRT(ZBND(J6-1)**2 + ZDRSDT(J6-1)**2))
        !
        IF (NPOIDD .EQ. 0 .OR. NMESHD .EQ. 0) THEN
           !
           ZU2(J6) = ZU1(J6)
           !
        ELSE
           !
           ZU2(J6) = ZU2(J6-1)
           !
           DO J5=1,NPOIDD
              !
              ZU2(J6)= ZU2(J6) + .5_RKIND * (ZTET(J6) - ZTET(J6-1)) * &
                   &               (ZBND(J6-1) * DWIDTH(J5) / &
                   &               (DWIDTH(J5)**2+SIN((DPLACE(J5)-ZTET(J6-1))/2._RKIND)**2)+ &
                   &               ZBND(J6) * DWIDTH(J5) / &
                   &               (DWIDTH(J5)**2+SIN((DPLACE(J5)-ZTET(J6))/2._RKIND)**2))
              !
           END DO
           !
        ENDIF
        !
     END DO
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  ! 3. NORMALIZE IT TO ONE                                              *
  !                                                                     *
  !**********************************************************************
  !
  DO J7=1,IM
     !
     ZW(J7) = 2._RKIND * CPI * (SOLPDD * ZU1(J7) / ZU1(IM) + &
          &                        (1._RKIND - SOLPDD) * ZU2(J7) / ZU2(IM))
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 4. FIND MESH POSITIONS                                              *
  !                                                                     *
  !**********************************************************************
  !
  PT(1)   = 0._RKIND
  PT(KN+1) = 2._RKIND*CPI
  !
  ZDP = 2._RKIND*CPI / REAL(KN,RKIND)
  ZF  = ZDP
  I   = 1
  !
  DO J9=2,IM
     !
8    CONTINUE 
     !
     IF (ZW(J9) .LE. ZF) GOTO 9
     !
     I     = I + 1
     ZT    = (J9 - 2) * ZDT
     PT(I) = ZT + (ZF - ZW(J9-1)) * ZDT / (ZW(J9) - ZW(J9-1))
     ZF    = ZF + ZDP
     !
     GOTO 8
     !
9    CONTINUE 
  END DO
  !
  RETURN
END SUBROUTINE TETARE
