!*DECK C2S04
!*CALL PROCESS
SUBROUTINE EQDIM(K)
  !        ###################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C2S04 CONTROL SIZE OF SMALL AND FINAL EQUILIBRIUM                   *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     K
  IF (K .EQ. 0) THEN
     !
     IF (NSURF .NE. 1) THEN
        !
        NDIM(1) = NS
        NDIM(2) = NT
        NDIM(3) = NISO
        !
        NS      = MIN(24,NDIM(1))
        NT      = MIN(24,NDIM(2))
        NISO    = MIN(100,NDIM(3))
        !
        CEPS    = MAX(EPNON0,EPSLON)
        !
     ENDIF
     !
     NS1    = NS + 1
     NT1    = NT + 1
     NT2    = NT + 2
     NPSI1  = NPSI + 1
     NCHI1  = NCHI + 1
     NV1    = NV + 1
     NBAND  = 4 * NT + 12
     NSTMAX = NS1 * NT
     N4NSNT = 4 * NSTMAX
     NWGAUS = NSGAUS * NTGAUS
     !
  ELSE IF (K .EQ. 2) THEN
     !
     NS     = NDIM(1)
     NT     = NDIM(2)
     NISO   = NDIM(3)
     CEPS   = EPSLON
     NS1    = NS + 1
     NT1    = NT + 1
     NT2    = NT + 2
     NBAND  = 4 * NT + 12
     NSTMAX = NS1 * NT
     N4NSNT = 4 * NSTMAX
     NWGAUS = NSGAUS * NTGAUS
     !
  ENDIF
  !
  RETURN
END SUBROUTINE EQDIM
