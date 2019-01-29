!*DECK C2SD02
!*CALL PROCESS
SUBROUTINE SETUPB
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SD02 CONSTRUCT VECTOR B. THIS VECTOR IS OBTAINED FROM THE RIGHT   *
  !        HAND SIDE OF EQ. (27) IN THE PUBLICATION                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     IBROW16
  INTEGER          ::     IBROW15
  INTEGER          ::     IBROW14
  INTEGER          ::     IBROW13
  INTEGER          ::     IBROW12
  INTEGER          ::     IBROW11
  INTEGER          ::     IBROW10
  INTEGER          ::     IBROW9
  INTEGER          ::     IBROW8
  INTEGER          ::     IBROW7
  INTEGER          ::     IBROW6
  INTEGER          ::     IBROW5
  INTEGER          ::     IBROW4
  INTEGER          ::     IBROW3
  INTEGER          ::     IBROW2
  INTEGER          ::     IBROW1
  INTEGER          ::     I4
  INTEGER          ::     I3
  INTEGER          ::     I2
  INTEGER          ::     I1
  INTEGER          ::     J11
  REAL(RKIND)      ::     SSUM
  REAL(RKIND)      ::     ZCUR1
  REAL(RKIND)      ::     ZINTJ
  REAL(RKIND)      ::     ZW
  INTEGER          ::     J7
  REAL(RKIND)      ::     ZJIPHI
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZPSI
  INTEGER          ::     J5
  INTEGER          ::     J10
  REAL(RKIND)      ::     ZPCEL
  INTEGER          ::     I
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZXB
  INTEGER          ::     J1
  INTEGER          ::     J2
  INTEGER          ::     J14
  REAL(RKIND)      ::     ZCUR
  DIMENSION &
       &   ZJIPHI(NPT),  ZPCEL(NPT,16),    ZPSI(NPT,NPGAUS),   ZR(NPT), &
       &   ZXB(NPT,16),  ZCUR1(NPT)
  !
  !----*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !**********************************************************************
  !                                                                     *
  ! 1. INITIALIZATION                                                   *
  !                                                                     *
  !**********************************************************************
  !
  CALL VZERO(B,N4NSNT)
  !
  ZCUR = 0._RKIND
  !
  !**********************************************************************
  !                                                                     *
  ! 2. SCAN OVER ALL CELLS                                              *
  !                                                                     *
  !**********************************************************************
  !
  DO J14=1,NS
     !
     !**********************************************************************
     !                                                                     *
     ! 2.1. INITIALIZATION OF LOCAL ARRAYS                                 *
     !                                                                     *
     !**********************************************************************
     !
     DO J2=1,16
        !
        DO J1=1,NT
           !
           ZXB(J1,J2) = 0._RKIND
           !
        END DO
     END DO
     !
     !**********************************************************************
     !                                                                     *
     ! 2.2. COMPUTE ALL QUANTITIES TO DEFINE PSI ON CELL                   *
     !                                                                     *
     !**********************************************************************
     !
     DO J4=1,NT
        !
        I = (J14 - 1) * NT + J4
        !
        ZPCEL(J4,1) = CPSICL(4*I-3)
        ZPCEL(J4,2) = CPSICL(4*I-2)
        ZPCEL(J4,3) = CPSICL(4*I-1)
        ZPCEL(J4,4) = CPSICL(4*I  )
        ZPCEL(J4,5) = CPSICL(4*(I+NT)-3)
        ZPCEL(J4,6) = CPSICL(4*(I+NT)-2)
        ZPCEL(J4,7) = CPSICL(4*(I+NT)-1)
        ZPCEL(J4,8) = CPSICL(4*(I+NT)  )
        !
        IF (J4 .EQ. NT) THEN
           !
           ZPCEL(J4, 9) = CPSICL(4*(I-NT)+1)
           ZPCEL(J4,10) = CPSICL(4*(I-NT)+2)
           ZPCEL(J4,11) = CPSICL(4*(I-NT)+3)
           ZPCEL(J4,12) = CPSICL(4*(I-NT)+4)
           ZPCEL(J4,13) = CPSICL(4*I+1)
           ZPCEL(J4,14) = CPSICL(4*I+2)
           ZPCEL(J4,15) = CPSICL(4*I+3)
           ZPCEL(J4,16) = CPSICL(4*I+4)
           !
        ELSE
           !
           ZPCEL(J4, 9) = CPSICL(4*I+1)
           ZPCEL(J4,10) = CPSICL(4*I+2)
           ZPCEL(J4,11) = CPSICL(4*I+3)
           ZPCEL(J4,12) = CPSICL(4*I+4)
           ZPCEL(J4,13) = CPSICL(4*(I+NT)+1)
           ZPCEL(J4,14) = CPSICL(4*(I+NT)+2)
           ZPCEL(J4,15) = CPSICL(4*(I+NT)+3)
           ZPCEL(J4,16) = CPSICL(4*(I+NT)+4)
           !
        ENDIF
        !
     END DO
     !
     !**********************************************************************
     !                                                                     *
     ! 2.3 COMPUTE VERTICAL POSITIONS IN A                                 *
     !                                                                     *
     !**********************************************************************
     !
     !**********************************************************************
     !                                                                     *
     ! 2.4 COMPUTE PSI, R AND SOURCE TERM ON INTEGRATION POINTS            *
     !                                                                     *
     !**********************************************************************
     !
     DO J10=1,NWGAUS
        !
        DO J5=1,NT
           !
           ZPSI(J5,J10) = ZPCEL(J5, 1) * FB(J5,J10, 1,J14) + &
                &                  ZPCEL(J5, 2) * FB(J5,J10, 2,J14) + &
                &                  ZPCEL(J5, 3) * FB(J5,J10, 3,J14) + &
                &                  ZPCEL(J5, 4) * FB(J5,J10, 4,J14) + &
                &                  ZPCEL(J5, 5) * FB(J5,J10, 5,J14) + &
                &                  ZPCEL(J5, 6) * FB(J5,J10, 6,J14) + &
                &                  ZPCEL(J5, 7) * FB(J5,J10, 7,J14) + &
                &                  ZPCEL(J5, 8) * FB(J5,J10, 8,J14) + &
                &                  ZPCEL(J5, 9) * FB(J5,J10, 9,J14) + &
                &                  ZPCEL(J5,10) * FB(J5,J10,10,J14) + &
                &                  ZPCEL(J5,11) * FB(J5,J10,11,J14) + &
                &                  ZPCEL(J5,12) * FB(J5,J10,12,J14) + &
                &                  ZPCEL(J5,13) * FB(J5,J10,13,J14) + &
                &                  ZPCEL(J5,14) * FB(J5,J10,14,J14) + &
                &                  ZPCEL(J5,15) * FB(J5,J10,15,J14) + &
                &                  ZPCEL(J5,16) * FB(J5,J10,16,J14) 
           !
           ZR(J5) = RSINT(J14,J10)*YRST(J5,J10)*COS(RTINT(J5,J10)) + R0
           !
        END DO
        !
        CALL CURENT(NT,ZPSI(1,J10),ZR,ZJIPHI)
        !
        DO J7=1,NT
           !
           !**********************************************************************
           !                                                                     *
           ! 2.5. INTEGRATION OF CURRENT                                         *
           !                                                                     *
           !**********************************************************************
           !
           ZW     = CW(J10) * (CSIG(J14+1)-CSIG(J14)) * (CT(J7+1)-CT(J7))
           ZINTJ  = - ZJIPHI(J7) * YRST(J7,J10)**2 * RSINT(J14,J10)
           !
           ZCUR1(J7) = - ZW * ZINTJ
           !
           !**********************************************************************
           !                                                                     *
           ! 2.7. COMPUTE VECTOR CONTRIBUTIONS                                   *
           !                                                                     *
           !**********************************************************************
           !
           ZXB(J7, 1) = ZXB(J7, 1) - ZCUR1(J7) * FB(J7,J10, 1,J14)
           ZXB(J7, 2) = ZXB(J7, 2) - ZCUR1(J7) * FB(J7,J10, 2,J14)
           ZXB(J7, 3) = ZXB(J7, 3) - ZCUR1(J7) * FB(J7,J10, 3,J14)
           ZXB(J7, 4) = ZXB(J7, 4) - ZCUR1(J7) * FB(J7,J10, 4,J14)
           ZXB(J7, 5) = ZXB(J7, 5) - ZCUR1(J7) * FB(J7,J10, 5,J14)
           ZXB(J7, 6) = ZXB(J7, 6) - ZCUR1(J7) * FB(J7,J10, 6,J14)
           ZXB(J7, 7) = ZXB(J7, 7) - ZCUR1(J7) * FB(J7,J10, 7,J14)
           ZXB(J7, 8) = ZXB(J7, 8) - ZCUR1(J7) * FB(J7,J10, 8,J14)
           ZXB(J7, 9) = ZXB(J7, 9) - ZCUR1(J7) * FB(J7,J10, 9,J14)
           ZXB(J7,10) = ZXB(J7,10) - ZCUR1(J7) * FB(J7,J10,10,J14)
           ZXB(J7,11) = ZXB(J7,11) - ZCUR1(J7) * FB(J7,J10,11,J14)
           ZXB(J7,12) = ZXB(J7,12) - ZCUR1(J7) * FB(J7,J10,12,J14)
           ZXB(J7,13) = ZXB(J7,13) - ZCUR1(J7) * FB(J7,J10,13,J14)
           ZXB(J7,14) = ZXB(J7,14) - ZCUR1(J7) * FB(J7,J10,14,J14)
           ZXB(J7,15) = ZXB(J7,15) - ZCUR1(J7) * FB(J7,J10,15,J14)
           ZXB(J7,16) = ZXB(J7,16) - ZCUR1(J7) * FB(J7,J10,16,J14)
           !
        END DO
        ZCUR = ZCUR + SSUM(NT,ZCUR1,1)
     END DO
     !
     !**********************************************************************
     !                                                                     *
     ! 2.8. ADD TO VECTOR B                                                *
     !                                                                     *
     !**********************************************************************
     !
     !DIR$ IVDEP
     DO J11=1,NT
        !
        I = (J14 - 1) * NT + J11
        !
        I1 = 4 * (NUPDWN(I)      - 1)
        I2 = 4 * (NUPDWN(I + NT) - 1)
        !
        IF (J11 .EQ. NT) THEN
           !
           I3 = 4 * (NUPDWN(I-NT+1) - 1)
           I4 = 4 * (NUPDWN(I+1)    - 1)
           !
        ELSE
           !
           I3 = 4 * (NUPDWN(I+1)    - 1)
           I4 = 4 * (NUPDWN(I+NT+1) - 1)
           !
        ENDIF
        !
        IBROW1  = I1 + 1
        IBROW2  = I1 + 2
        IBROW3  = I1 + 3
        IBROW4  = I1 + 4
        IBROW5  = I2 + 1
        IBROW6  = I2 + 2
        IBROW7  = I2 + 3
        IBROW8  = I2 + 4
        IBROW9  = I3 + 1
        IBROW10 = I3 + 2
        IBROW11 = I3 + 3
        IBROW12 = I3 + 4
        IBROW13 = I4 + 1
        IBROW14 = I4 + 2
        IBROW15 = I4 + 3
        IBROW16 = I4 + 4
        !
        B(IBROW1 ) = B(IBROW1 ) + ZXB(J11, 1)
        B(IBROW2 ) = B(IBROW2 ) + ZXB(J11, 2)
        B(IBROW3 ) = B(IBROW3 ) + ZXB(J11, 3)
        B(IBROW4 ) = B(IBROW4 ) + ZXB(J11, 4)
        B(IBROW5 ) = B(IBROW5 ) + ZXB(J11, 5)
        B(IBROW6 ) = B(IBROW6 ) + ZXB(J11, 6)
        B(IBROW7 ) = B(IBROW7 ) + ZXB(J11, 7)
        B(IBROW8 ) = B(IBROW8 ) + ZXB(J11, 8)
        B(IBROW9 ) = B(IBROW9 ) + ZXB(J11, 9)
        B(IBROW10) = B(IBROW10) + ZXB(J11,10)
        B(IBROW11) = B(IBROW11) + ZXB(J11,11)
        B(IBROW12) = B(IBROW12) + ZXB(J11,12)
        B(IBROW13) = B(IBROW13) + ZXB(J11,13)
        B(IBROW14) = B(IBROW14) + ZXB(J11,14)
        B(IBROW15) = B(IBROW15) + ZXB(J11,15)
        B(IBROW16) = B(IBROW16) + ZXB(J11,16)
        !
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3. INTRODUCE LIMIT CONDITIONS                                       *
  !                                                                     *
  !**********************************************************************
  !
  CALL LIMITB
  !
  !**********************************************************************
  !                                                                     *
  ! 4. TOTAL TOROIDAL CURRENT                                           *
  !                                                                     *
  !**********************************************************************
  !
  CUROLD = ZCUR
  !
  RETURN
END SUBROUTINE SETUPB
