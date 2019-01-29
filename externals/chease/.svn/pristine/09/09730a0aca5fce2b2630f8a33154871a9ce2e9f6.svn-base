!*DECK C2SM14
!*CALL PROCESS
SUBROUTINE VLION
  !        ################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SM14 COMPUTE AUXILIARY QUANTITIES AT PLASMA SURFACE FOR LION      *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZBND2
  REAL(RKIND)      ::     ZBND1
  REAL(RKIND)      ::     ZT2
  REAL(RKIND)      ::     ZT1
  INTEGER          ::     J2
  INTEGER          ::     J4
  INTEGER          ::     J1
  REAL(RKIND)      ::     ZEPS
  REAL(RKIND)      ::     ZISQR6
  DIMENSION &
       &   ZT1(NPCHI),     ZT2(NPCHI), &
       &   ZBND1(NPCHI),   ZBND2(NPCHI)
  !
  ZISQR6 = 1._RKIND / SQRT(6._RKIND)
  ZEPS   = 1.E-4_RKIND
  !
  !         TETVAC(1) = .5 * (TETVAC(1) - TETVAC(2))
  !         TETVAC(2) = - TETVAC(1)
  !         RHOVAC(1) = .5 * (RHOVAC(1) + RHOVAC(2))
  !         RHOVAC(2) = RHOVAC(1)
  !         TETVACM(1)        = 0.
  !         TETVACM(NCHI1)    = 2. * CPI
  !         TETVACM(NCHI/2+1) = CPI
  !
  !         DO J20=3,NCHI/2+1
  !
  !         TETVAC(J20) = .5 * (TETVAC(J20) - TETVAC(NCHI-J20+3) + 2.*CPI) 
  !         RHOVAC(J20) = .5 * (RHOVAC(J20) + RHOVAC(NCHI-J20+3))
  !         TETVAC(NCHI-J20+3) = 2. * CPI - TETVAC(J20)
  !         RHOVAC(NCHI-J20+3) = RHOVAC(J20)
  !
  !  20     CONTINUE
  !
  !         DO J21=2,NCHI/2
  !
  !         TETVACM(J21) = .5*(TETVACM(J21) - TETVACM(NCHI-J21+2) + 2.*CPI) 
  !         RHOVACM(J21) = .5*(RHOVACM(J21) + RHOVACM(NCHI-J21+2))
  !
  !  21     CONTINUE
  !
  DO J1=1,NCHI
     !
     TETVACI(J1,2) = .5_RKIND * (TETVAC(J1) + TETVAC(J1+1))
     TETVACI(J1,1) = ZISQR6 * (TETVAC(J1) - TETVAC(J1+1)) + &
          &                   TETVACI(J1,2)
     TETVACI(J1,3) = ZISQR6 * (TETVAC(J1+1) - TETVAC(J1)) + &
          &                   TETVACI(J1,2)
     !
  END DO
  !
  BPS( 1) = RMAG
  BPS(12) = RZMAG
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  CALL BOUND(NCHI,TETVACI(1,1),RHOVACI(1,1))
  CALL BOUND(NCHI,TETVACI(1,2),RHOVACI(1,2))
  CALL BOUND(NCHI,TETVACI(1,3),RHOVACI(1,3))
  !
  DO J4=1,3
     !
     DO J2=1,NCHI
        !
        ZT1(J2) = TETVACI(J2,J4) - ZEPS
        ZT2(J2) = TETVACI(J2,J4) + ZEPS
        !
     END DO
     !
     CALL BOUND(NCHI,ZT1,ZBND1)
     CALL BOUND(NCHI,ZT2,ZBND2)
     !
     DO J3=1,NCHI
        !
        DRHOPI(J3,J4) = .5_RKIND * (ZBND2(J3) - ZBND1(J3)) / ZEPS
        !                
     END DO
  END DO
  !
  BPS( 1) = R0
  BPS(12) = RZ0
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  RETURN
END SUBROUTINE VLION
