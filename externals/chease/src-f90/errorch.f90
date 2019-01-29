!*DECK C2SE03
!*CALL PROCESS
SUBROUTINE ERRORCH(PSIK,PSIK1)
  !        ############################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SE03 COMPUTE ERROR (SEE EQ. (28) IN PUBLICATION)                  *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     SSUM
  REAL(RKIND)      ::     ZCSURF
  REAL(RKIND)      ::     ZPSIK1
  REAL(RKIND)      ::     ZPSIK
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZRES
  INTEGER          ::     J4
  REAL(RKIND)      ::     PSIK1
  REAL(RKIND)      ::     ZPK1
  REAL(RKIND)      ::     PSIK
  REAL(RKIND)      ::     ZPK
  INTEGER          ::     IS
  INTEGER          ::     J7
  INTEGER          ::     IT
  INTEGER          ::     J1
  DIMENSION &
       &   IS(NPT),       IT(NPT), &
       &   PSIK(*),       PSIK1(*),          ZPK(NPT,16), &
       &   ZPK1(NPT,16),  ZRES(NPT)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !**********************************************************************
  !                                                                     *
  ! 1. INITIALIZATION                                                   *
  !                                                                     *
  !**********************************************************************
  !
  RESIDU = 0._RKIND
  !
  DO J1=1,NT
     !
     IT(J1) = J1
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 2. SCAN OVER ALL CELLS                                              *
  !                                                                     *
  !**********************************************************************
  !
  DO J7=1,NS
     !
     CALL RESETI(IS,NT,J7)
     !
     !**********************************************************************
     !                                                                     *
     ! 2.1. COMPUTE ALL QUANTITIES TO DEFINE PSI FOR PICARD ITERATION K AND*
     !      K + 1                                                          *
     !                                                                     *
     !**********************************************************************
     !
     CALL PSICEL(IS,IT,NT,NPT,ZPK,PSIK)
     CALL PSICEL(IS,IT,NT,NPT,ZPK1,PSIK1)
     !
     !**********************************************************************
     !                                                                     *
     ! 2.2 COMPUTE PSI, R AND SOURCE TERM ON INTEGRATION POINTS            *
     !                                                                     *
     !**********************************************************************
     !
     DO J4=1,NWGAUS
        !
        CALL VZERO(ZRES,NT)
        !
        DO J2=1,NT
           !
           ZPSIK  = ZPK(J2, 1) * FB(J2,J4, 1,J7) + &
                &            ZPK(J2, 2) * FB(J2,J4, 2,J7) + &
                &            ZPK(J2, 3) * FB(J2,J4, 3,J7) + &
                &            ZPK(J2, 4) * FB(J2,J4, 4,J7) + &
                &            ZPK(J2, 5) * FB(J2,J4, 5,J7) + &
                &            ZPK(J2, 6) * FB(J2,J4, 6,J7) + &
                &            ZPK(J2, 7) * FB(J2,J4, 7,J7) + &
                &            ZPK(J2, 8) * FB(J2,J4, 8,J7) + &
                &            ZPK(J2, 9) * FB(J2,J4, 9,J7) + &
                &            ZPK(J2,10) * FB(J2,J4,10,J7) + &
                &            ZPK(J2,11) * FB(J2,J4,11,J7) + &
                &            ZPK(J2,12) * FB(J2,J4,12,J7) + &
                &            ZPK(J2,13) * FB(J2,J4,13,J7) + &
                &            ZPK(J2,14) * FB(J2,J4,14,J7) + &
                &            ZPK(J2,15) * FB(J2,J4,15,J7) + &
                &            ZPK(J2,16) * FB(J2,J4,16,J7) 
           !
           ZPSIK1 = ZPK1(J2, 1) * FB(J2,J4, 1,J7) + &
                &            ZPK1(J2, 2) * FB(J2,J4, 2,J7) + &
                &            ZPK1(J2, 3) * FB(J2,J4, 3,J7) + &
                &            ZPK1(J2, 4) * FB(J2,J4, 4,J7) + &
                &            ZPK1(J2, 5) * FB(J2,J4, 5,J7) + &
                &            ZPK1(J2, 6) * FB(J2,J4, 6,J7) + &
                &            ZPK1(J2, 7) * FB(J2,J4, 7,J7) + &
                &            ZPK1(J2, 8) * FB(J2,J4, 8,J7) + &
                &            ZPK1(J2, 9) * FB(J2,J4, 9,J7) + &
                &            ZPK1(J2,10) * FB(J2,J4,10,J7) + &
                &            ZPK1(J2,11) * FB(J2,J4,11,J7) + &
                &            ZPK1(J2,12) * FB(J2,J4,12,J7) + &
                &            ZPK1(J2,13) * FB(J2,J4,13,J7) + &
                &            ZPK1(J2,14) * FB(J2,J4,14,J7) + &
                &            ZPK1(J2,15) * FB(J2,J4,15,J7) + &
                &            ZPK1(J2,16) * FB(J2,J4,16,J7) 
           !
           ZCSURF   = CW(J4) * (CSIG(J7+1)-CSIG(J7)) * (CT(J2+1)-CT(J2))
           ZRES(J2) = ZCSURF * YRST(J2,J4)**2 * RSINT(J7,J4) * &
                &              (ZPSIK1 - ZPSIK)**2
           !
        END DO
        ! 
        RESIDU = RESIDU + SSUM(NT,ZRES,1)
        !
     END DO
  END DO
  !
  RESIDU = SQRT(RESIDU)
  !
  RETURN
END SUBROUTINE ERRORCH
