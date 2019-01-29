!*DECK C2SY05
!*CALL PROCESS
SUBROUTINE PSICEL(KS,KT,KN,KPN,PCEL,PSICL)
  !        ##########################################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C2SY05 EXTRACT PSI AND ITS DERIVATIVES AT THE 4 NODES OF A CELL     *
  !        FROM THE CPSICL ARRAY, WHICH CONTAINS THE COMPLETE BICUBIC   *
  !        HERMITE SOLUTION                                             *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     PSICL
  REAL(RKIND)      ::     PCEL
  INTEGER          ::     I4KMNT
  INTEGER          ::     I4KPNT
  INTEGER          ::     I4K
  INTEGER          ::     K
  INTEGER          ::     KT
  INTEGER          ::     JT
  INTEGER          ::     KS
  INTEGER          ::     JS
  INTEGER          ::     KN
  INTEGER          ::     J1
  INTEGER          ::     KPN
  DIMENSION &
       &   KS(KPN),         KT(KPN), &
       &   PCEL(KPN,16),    PSICL(*)
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  DO J1=1,KN
     !
     JS = KS(J1)
     JT = KT(J1)
     K  = (JS - 1) * NT + JT
     I4K = 4*K
     I4KPNT = 4*(K+NT)
     I4KMNT = 4*(K-NT)
     !
     PCEL(J1,1) = PSICL(I4K-3)
     PCEL(J1,2) = PSICL(I4K-2)
     PCEL(J1,3) = PSICL(I4K-1)
     PCEL(J1,4) = PSICL(I4K  )
     PCEL(J1,5) = PSICL(I4KPNT-3)
     PCEL(J1,6) = PSICL(I4KPNT-2)
     PCEL(J1,7) = PSICL(I4KPNT-1)
     PCEL(J1,8) = PSICL(I4KPNT  )
     !
     IF (JT .EQ. NT) THEN
        !
        PCEL(J1, 9) = PSICL(I4KMNT+1)
        PCEL(J1,10) = PSICL(I4KMNT+2)
        PCEL(J1,11) = PSICL(I4KMNT+3)
        PCEL(J1,12) = PSICL(I4KMNT+4)
        PCEL(J1,13) = PSICL(I4K+1)
        PCEL(J1,14) = PSICL(I4K+2)
        PCEL(J1,15) = PSICL(I4K+3)
        PCEL(J1,16) = PSICL(I4K+4)
        !
     ELSE
        !
        PCEL(J1, 9) = PSICL(I4K+1)
        PCEL(J1,10) = PSICL(I4K+2)
        PCEL(J1,11) = PSICL(I4K+3)
        PCEL(J1,12) = PSICL(I4K+4)
        PCEL(J1,13) = PSICL(I4KPNT+1)
        PCEL(J1,14) = PSICL(I4KPNT+2)
        PCEL(J1,15) = PSICL(I4KPNT+3)
        PCEL(J1,16) = PSICL(I4KPNT+4)
        !
     ENDIF
     !
  END DO
  !
  RETURN
END SUBROUTINE PSICEL
