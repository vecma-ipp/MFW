!*DECK C2SE01
!*CALL PROCESS
SUBROUTINE SOLVIT
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2SE01 LEAD GAUSS ELIMINATION FOR THE COMPUTATION OF PSI = A**(-1)*B*
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     J3
  INTEGER          ::     I
  INTEGER          ::     J2
  REAL(RKIND)      ::     ZC
  REAL(RKIND)      ::     ZB
  INTEGER          ::     J1
  DIMENSION &
       &   ZB(3),  ZC(N4NT,3)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !**********************************************************************
  !                                                                     *
  ! 1. GAUSS ELIMINATION : SOLVE A * X = B                              *
  !                                                                     *
  !**********************************************************************
  !
  CALL DIRECT
  CALL DCOPY(N4NSNT,B,1,CPSILI,1)
  !
  !**********************************************************************
  !                                                                     *
  ! 2. IDENTIFICATIONS AT MESH CENTER                                   *
  !                                                                     *
  !**********************************************************************
  !
  DO J1=1,3
     !
     ZB(J1)       = B(4*NT-3+J1)
     B(4*NT-3+J1) = 0._RKIND
     !
  END DO
  !
  CALL CENTER(ZC)
  !
  DO J2=1,NT
     !
     I = 4 * (NUPDWN(J2) - 1)
     !
     B(I+1) = ZB(1)
     B(I+2) = ZC(I+2,2) * ZB(2) + ZC(I+2,3) * ZB(3)
     B(I+4) = ZC(I+4,2) * ZB(2) + ZC(I+4,3) * ZB(3)
     !
  END DO
  !
  !**********************************************************************
  !                                                                     *
  ! 3. NEW PSI - SOLUTION                                               *
  !                                                                     *
  !**********************************************************************
  !
  CALL DCOPY(N4NSNT,B,1,CPSI,1)
  !
  !**********************************************************************
  !                                                                     *
  ! 4. NEW PSI - SOLUTION IN INVERSE CLOCKWISE NUMEROTATION             *
  !                                                                     *
  !**********************************************************************
  !
  DO J3=1,NSTMAX
     !
     CPSICL(4*(J3-1)+1) = B(4*(NUPDWN(J3)-1)+1)
     CPSICL(4*(J3-1)+2) = B(4*(NUPDWN(J3)-1)+2)
     CPSICL(4*(J3-1)+3) = B(4*(NUPDWN(J3)-1)+3)
     CPSICL(4*(J3-1)+4) = B(4*(NUPDWN(J3)-1)+4)
     !
  END DO
  !
  RETURN
END SUBROUTINE SOLVIT
