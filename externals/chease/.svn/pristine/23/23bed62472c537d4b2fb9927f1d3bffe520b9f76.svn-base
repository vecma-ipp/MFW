!*DECK C2SD01
!*CALL PROCESS
SUBROUTINE SETUPA
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SD01 CONSTRUCT MATRIX A. THIS MATRIX IS OBTAINED FROM THE LEFT    *
  !        HAND SIDE OF EQ. (27) IN THE PUBLICATION                     *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J17
  INTEGER          ::     J18
  INTEGER          ::     J19
  INTEGER          ::     J20
  INTEGER          ::     J21
  INTEGER          ::     IXACOL
  INTEGER          ::     IXAROW
  INTEGER          ::     IACOL
  INTEGER          ::     IAROW
  INTEGER          ::     J13
  INTEGER          ::     IMAX
  INTEGER          ::     J14
  INTEGER          ::     J15
  INTEGER          ::     J16
  REAL(RKIND)      ::     ZCOEF
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZW
  INTEGER          ::     J9
  INTEGER          ::     J11
  REAL(RKIND)      ::     ZV
  REAL(RKIND)      ::     ZFRAC
  INTEGER          ::     J8
  REAL(RKIND)      ::     ZDBDT
  REAL(RKIND)      ::     ZDBDS
  REAL(RKIND)      ::     ZT
  REAL(RKIND)      ::     ZS
  INTEGER          ::     J4
  INTEGER          ::     J12
  REAL(RKIND)      ::     ZS2
  REAL(RKIND)      ::     ZS1
  INTEGER          ::     J3
  INTEGER          ::     I
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZXA
  INTEGER          ::     J22
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  INTEGER          ::     J1
  DIMENSION &
       &     ZDBDS(NPT,16), ZDBDT(NPT,16), ZS(NPT),       ZS1(NPT), &
       &     ZS2(NPT),      ZT(NPT),       ZT1(NPT),      ZT2(NPT), &
       &     ZV(NPT,16),    ZXA(NPT,16,16)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  CALL VZERO(A,NPBAND*NP4NST)
  !
  DO J1=1,NT
     !
     ZT1(J1) = CT(J1)
     ZT2(J1) = CT(J1+1)
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 1. SCAN OVER ALL INTERVALS                                          *
  !                                                                     *
  !**********************************************************************
  !
  DO J22=1,NS
     !
     !**********************************************************************
     !                                                                     *
     ! 1.1. INITIALIZATION OF LOCAL ARRAYS                                 *
     !                                                                     *
     !**********************************************************************
     !
     CALL VZERO(ZXA,256*NPT)
     !
     !**********************************************************************
     !                                                                     *
     ! 1.2 COMPUTE VERTICAL POSITIONS IN A                                 *
     !                                                                     *
     !**********************************************************************
     !
     DO J2=1,NT-1
        !
        I = (J22 - 1) * NT + J2
        !
        NPLAC(J2,1) = NUPDWN(I)
        NPLAC(J2,2) = NUPDWN(I+NT)
        NPLAC(J2,3) = NUPDWN(I+1)
        NPLAC(J2,4) = NUPDWN(I+NT+1)
        !
     END DO
     !
     I = J22 * NT
     !
     NPLAC(NT,1) = NUPDWN(I)
     NPLAC(NT,2) = NUPDWN(I+NT)
     NPLAC(NT,3) = NUPDWN(I+1-NT)
     NPLAC(NT,4) = NUPDWN(I+1)
     !
     DO J3=1,NT
        !
        ZS1(J3) = CSIG(J22)
        ZS2(J3) = CSIG(J22+1)
        !
     END DO
     !
     !**********************************************************************
     !                                                                     *
     ! 1.3. COMPUTE INTEGRANTS OF VARITIONNAL PROBLEM                      *
     !                                                                     *
     !**********************************************************************
     !
     DO J12=1,NWGAUS
        !
        DO J4=1,NT
           !
           ZS(J4) = RSINT(J22,J12)
           ZT(J4) = RTINT(J4,J12)
           !
        END DO
        !
        CALL BASIS2(NT,NPT,ZS1,ZS2,ZT1,ZT2,ZS,ZT,ZDBDS,ZDBDT)
        !
        DO J8=1,NT
           !
           ZFRAC = YDRSDT(J8,J12) / YRST(J8,J12)
           !
           ZV(J8, 1) = ZDBDT(J8, 1)/RSINT(J22,J12) - ZDBDS(J8, 1)*ZFRAC
           ZV(J8, 2) = ZDBDT(J8, 2)/RSINT(J22,J12) - ZDBDS(J8, 2)*ZFRAC
           ZV(J8, 3) = ZDBDT(J8, 3)/RSINT(J22,J12) - ZDBDS(J8, 3)*ZFRAC
           ZV(J8, 4) = ZDBDT(J8, 4)/RSINT(J22,J12) - ZDBDS(J8, 4)*ZFRAC
           ZV(J8, 5) = ZDBDT(J8, 5)/RSINT(J22,J12) - ZDBDS(J8, 5)*ZFRAC
           ZV(J8, 6) = ZDBDT(J8, 6)/RSINT(J22,J12) - ZDBDS(J8, 6)*ZFRAC
           ZV(J8, 7) = ZDBDT(J8, 7)/RSINT(J22,J12) - ZDBDS(J8, 7)*ZFRAC
           ZV(J8, 8) = ZDBDT(J8, 8)/RSINT(J22,J12) - ZDBDS(J8, 8)*ZFRAC
           ZV(J8, 9) = ZDBDT(J8, 9)/RSINT(J22,J12) - ZDBDS(J8, 9)*ZFRAC
           ZV(J8,10) = ZDBDT(J8,10)/RSINT(J22,J12) - ZDBDS(J8,10)*ZFRAC
           ZV(J8,11) = ZDBDT(J8,11)/RSINT(J22,J12) - ZDBDS(J8,11)*ZFRAC
           ZV(J8,12) = ZDBDT(J8,12)/RSINT(J22,J12) - ZDBDS(J8,12)*ZFRAC
           ZV(J8,13) = ZDBDT(J8,13)/RSINT(J22,J12) - ZDBDS(J8,13)*ZFRAC
           ZV(J8,14) = ZDBDT(J8,14)/RSINT(J22,J12) - ZDBDS(J8,14)*ZFRAC
           ZV(J8,15) = ZDBDT(J8,15)/RSINT(J22,J12) - ZDBDS(J8,15)*ZFRAC
           ZV(J8,16) = ZDBDT(J8,16)/RSINT(J22,J12) - ZDBDS(J8,16)*ZFRAC
           !
        END DO
        !
        !**********************************************************************
        !                                                                     *
        ! 1.4. PERFORMS DIADIC PRODUCT AND COMPUTE MATRIX CONTRIBUTION        *
        !                                                                     *
        !**********************************************************************
        !
        DO J11=1,16
           !
           DO J9=1,NT
              !
              ZW    = CW(J12) * (CSIG(J22+1) - CSIG(J22)) * &
                   &                  (CT(J9+1) - CT(J9))
              ZR    = RSINT(J22,J12)*YRST(J9,J12)*COS(RTINT(J9,J12)) + R0
              ZCOEF = ZW * RSINT(J22,J12) / ZR
              !
              ZXA(J9, 1,J11) = ZXA(J9, 1,J11) + ZCOEF * &
                   &      (ZV(J9, 1)*ZV(J9,J11) + ZDBDS(J9, 1)*ZDBDS(J9,J11)) 
              ZXA(J9, 2,J11) = ZXA(J9, 2,J11) + ZCOEF * &
                   &      (ZV(J9, 2)*ZV(J9,J11) + ZDBDS(J9, 2)*ZDBDS(J9,J11)) 
              ZXA(J9, 3,J11) = ZXA(J9, 3,J11) + ZCOEF * &
                   &      (ZV(J9, 3)*ZV(J9,J11) + ZDBDS(J9, 3)*ZDBDS(J9,J11)) 
              ZXA(J9, 4,J11) = ZXA(J9, 4,J11) + ZCOEF * &
                   &      (ZV(J9, 4)*ZV(J9,J11) + ZDBDS(J9, 4)*ZDBDS(J9,J11)) 
              ZXA(J9, 5,J11) = ZXA(J9, 5,J11) + ZCOEF * &
                   &      (ZV(J9, 5)*ZV(J9,J11) + ZDBDS(J9, 5)*ZDBDS(J9,J11)) 
              ZXA(J9, 6,J11) = ZXA(J9, 6,J11) + ZCOEF * &
                   &      (ZV(J9, 6)*ZV(J9,J11) + ZDBDS(J9, 6)*ZDBDS(J9,J11)) 
              ZXA(J9, 7,J11) = ZXA(J9, 7,J11) + ZCOEF * &
                   &      (ZV(J9, 7)*ZV(J9,J11) + ZDBDS(J9, 7)*ZDBDS(J9,J11)) 
              ZXA(J9, 8,J11) = ZXA(J9, 8,J11) + ZCOEF * &
                   &      (ZV(J9, 8)*ZV(J9,J11) + ZDBDS(J9, 8)*ZDBDS(J9,J11)) 
              ZXA(J9, 9,J11) = ZXA(J9, 9,J11) + ZCOEF * &
                   &      (ZV(J9, 9)*ZV(J9,J11) + ZDBDS(J9, 9)*ZDBDS(J9,J11)) 
              ZXA(J9,10,J11) = ZXA(J9,10,J11) + ZCOEF * &
                   &      (ZV(J9,10)*ZV(J9,J11) + ZDBDS(J9,10)*ZDBDS(J9,J11)) 
              ZXA(J9,11,J11) = ZXA(J9,11,J11) + ZCOEF * &
                   &      (ZV(J9,11)*ZV(J9,J11) + ZDBDS(J9,11)*ZDBDS(J9,J11)) 
              ZXA(J9,12,J11) = ZXA(J9,12,J11) + ZCOEF * &
                   &      (ZV(J9,12)*ZV(J9,J11) + ZDBDS(J9,12)*ZDBDS(J9,J11)) 
              ZXA(J9,13,J11) = ZXA(J9,13,J11) + ZCOEF * &
                   &      (ZV(J9,13)*ZV(J9,J11) + ZDBDS(J9,13)*ZDBDS(J9,J11)) 
              ZXA(J9,14,J11) = ZXA(J9,14,J11) + ZCOEF * &
                   &      (ZV(J9,14)*ZV(J9,J11) + ZDBDS(J9,14)*ZDBDS(J9,J11)) 
              ZXA(J9,15,J11) = ZXA(J9,15,J11) + ZCOEF * &
                   &      (ZV(J9,15)*ZV(J9,J11) + ZDBDS(J9,15)*ZDBDS(J9,J11)) 
              ZXA(J9,16,J11) = ZXA(J9,16,J11) + ZCOEF * &
                   &      (ZV(J9,16)*ZV(J9,J11) + ZDBDS(J9,16)*ZDBDS(J9,J11)) 
              !
           END DO
        END DO
     END DO
     !
     !**********************************************************************
     !                                                                     *
     ! 1.5. ADD TO MATRIX A                                                *
     !                                                                     *
     !**********************************************************************
     !                                                                     *
     ! 1.5.1. ADD DIAGONAL BLOCS OF ZXA TO A :                             *
     !                                                                     *
     !                   XA    =====>                       A              *
     !                   ==                                 =              *
     !                                                                     *
     ! 4*(J16-1)+1 --> * * * *     4*(NPLAC(J16)-1)+1 --> * * * *          *
     !                 ! * * *                            * * *            *
     !                 !   * * =====>                     * *              *
     !                 !     *                            *                *
     !                 !                                                   *
     !                -!--------> IXACOL                 -!-----!--> IACOL *
     !                 4*(J16-1)+1                        1     4          *
     !                                                                     *
     !**********************************************************************
     !
     !**********************************************************************
     !                                                                     *
     !    INDEXATION:                                                      *
     !    -----------                                                      *
     !      J16 ===> VERTICAL POSITION OF BLOC                             *
     !      J14 ===> ROW IN BLOC                                           *
     !      J13 ===> COLUMN IN BLOC                                        *
     !                                                                     *
     !**********************************************************************
     !
     !
     DO J16=1,4
        !
        DO J15=1,NT
           !
           DO J14=1,4
              !
              IMAX = 5 - J14
              !
              DO J13=1,IMAX
                 !
                 IAROW  = 4 * (NPLAC(J15,J16) - 1) + J14
                 IACOL  = J13
                 IXAROW = 4 * (J16 - 1) + J14
                 IXACOL = IXAROW + J13 - 1
                 !
                 A(IACOL,IAROW) = A(IACOL,IAROW) + ZXA(J15,IXACOL,IXAROW)
                 !
              END DO
           END DO
        END DO
     END DO
     !
     !**********************************************************************
     !                                                                     *
     !  1.5.2. ADD OUT OF DIAGONAL BLOCS OF ZXA TO A :                     *
     !                                                                     *
     !          ZXA            =====>              A                       *
     !          ===                                =                       *
     !                                                                     *
     ! 4*(J21-1)+1 --> * * * *      4*(NPLAC(J21)-1)+1 --> * * * *         *
     !                 * * * *                           * * * *           *
     !                 * * * * =====>                  * * * *             *
     !                 * * * *                       * * * *               *
     !                 !                                   !               *
     !               --!-------> IXACOL             -------!--------> IACOL*
     !                 4*(J21-1)+1                  4*(KPLAC(J20,J21)-1)+1 *
     !                                                                     *
     !**********************************************************************
     !
     !**********************************************************************
     !                                                                     *
     !    INDEXATION:                                                      *
     !    -----------                                                      *
     !      J21 ===> VERTICAL POSITION OF BLOC                             *
     !      J20 ===> HORIZONTAL POSITION OF BLOC                           *
     !      J18 ===> ROW IN BLOC                                           *
     !      J17 ===> COLUMN IN BLOC                                        *
     !                                                                     *
     !**********************************************************************
     !
     DO J21=1,4
        !
        DO J20=1,4
           !
           DO J19=1,NT
              !
              !**********************************************************************
              !                                                                     *
              ! TESTS IF BLOC MUST BE ADDED TO A                                    *
              !                                                                     *
              !**********************************************************************
              !
              IF (MPLA(J19,J20,J21) .GT. 1) THEN
                 !
                 DO J18=1,4
                    !
                    DO J17=1,4
                       !
                       IAROW  = 4 * (NPLAC(J19,J21) - 1) + J18
                       IACOL  = 4 * (MPLA(J19,J20,J21)-1) + J17 - J18 + 1
                       IXAROW = 4 * (J21 - 1) + J18
                       IXACOL = 4 * (J20 - 1) + J17
                       !
                       A(IACOL,IAROW) = A(IACOL,IAROW) + ZXA(J19,IXACOL,IXAROW)
                       !
                    END DO
                 END DO
                 !
              ENDIF
              !
           END DO
        END DO
     END DO
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 2. INTRODUCE LIMIT CONDITIONS                                       *
  !                                                                     *
  !**********************************************************************
  !
  CALL LIMITA
  !
  RETURN
END SUBROUTINE SETUPA
