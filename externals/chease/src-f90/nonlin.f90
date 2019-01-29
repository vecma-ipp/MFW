!*DECK C2S07
!*CALL PROCESS
SUBROUTINE NONLIN
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2S07 LEAD ITERATION OVER THE NONLINEARITY                          *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  INTEGER          ::     J1
  DO J1=1,NINSCA
     !
     ! SET UP RIGHT HAND SIDE B 
     !
     CALL SETUPB
     !
     ! SAVE OLD SOLUTION IN CPSIO;
     ! SOLVE SYSTEM L * D * LT * X = B
     !
     CALL DCOPY(N4NSNT,CPSICL,1,CPSIO,1)
     CALL SOLVIT
     call scopyr(relax,n4nsnt,cpsio,1,cpsicl,1)
     !
     ! FIND PSIMIN AND MAGNETIC AXIS
     !
     CALL MAGAXE
     !
     ! PRINT OUT :
     !          - SPSIM, RMAG AND RZMAG FOR SOLOVEV CASE
     !          - SPSIM, RMAG ,RZMAG, RESIDU AND EPSLON
     !            FOR OTHER CASES
     !
     IF (NVERBOSE .GE. 2) CALL OUTPUT(4)
     !
     IF (NSMOOTH .EQ. 1) CALL SMOOTH
     !
     CALL ERRORCH(CPSIO,CPSICL)
     !
     IF (NSURF .EQ. 1) GOTO 2
     !
     IF (NVERBOSE .GE. 2) WRITE(6,1301) RESIDU,CEPS
1301 FORMAT(73X,'RESIDU = ',E13.6,3X,'EPSLON = ',E13.6,/)
     !
     ! CONVERGENCE TEST
     !
     CALL CONVER(1,NCON)
     !
     IF (NCON .EQ. 1) GOTO 2
     !
  END DO
  !
  ! ITERATION OVER NON-LINEARITY HAS NOT CONVERGED
  !
  WRITE (6,1450)
1450 FORMAT(/,73X,'  *****   ITERATION OVER NON LINEARITY DID NOT CONVERGED')
  !
2 CONTINUE 
  !
  RETURN
END SUBROUTINE NONLIN
