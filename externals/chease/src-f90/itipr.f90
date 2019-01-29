!
SUBROUTINE ITIPR(NISOFORDIM)
  !        ################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  ! LEAD ITERATION OVER CURRENT PROFILE
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER :: NISOFORDIM
  !
  INTEGER          ::     J2, J3
  REAL(RKIND)      ::     ZNDM, ZNM
  REAL(RKIND)      ::     ZCPPR(NISOFORDIM)
  REAL(RKIND)      ::     ZTTP(NISOFORDIM)
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  DO J3=1,NINMAP
     !
     CALL NONLIN
     !
     IF (NSTTP .EQ. 1) GOTO 4
     !
     CALL DCOPY(NISO,TTP,1,ZTTP,1)
     CALL DCOPY(NISO,CPPR,1,ZCPPR,1)
     !
     CALL PREMAP(1)
     !
     ZNM  = 0._RKIND
     ZNDM = 0._RKIND
     !
     DO J2=1,NISO
        ZNM  = ZNM + (CPPR(J2) - ZCPPR(J2))**2 + &
             &                (TTP(J2) - ZTTP(J2))**2
        ZNDM = ZNDM + TTP(J2)**2 + CPPR(J2)**2
     END DO
     !
     RESMAP = SQRT(ZNM/ZNDM)
     !
     ! IF NSTTP = 3/4 : RESCALE AT'S AT EVERY ITERATION OVER CURRENT 
     ! PROFILE IF NO BALLOONING OPTIMIZATION IS USED OR THE OPTIMIZED 
     ! BALLOONING PRESSURE PROFILE IS RESCALED BY CPRESS. IF BALLOONING
     ! OPTIMIZATION IS USED, THIS OPERATION IS DONE ONLY AFTER EVERY
     ! STEP OVER THE PRESSURE PROFILE, SINCE IT HAS BEEN OBSERVED THAT 
     ! THE CONVERGENCE IS FASTER IN THAT WAY.
     !
     IF (((NSTTP.EQ.3) .OR. (NSTTP.EQ.4)) .AND. &
          &       (NBLOPT .EQ. 0 .OR. (NOPT.EQ.1 .AND. &
          &       (NBLOPT.NE.0 .AND. CPRESS.NE.1._RKIND)))) THEN
        !
        CALL NOREPT(NISO,0)
     ENDIF
     !
     IF (NVERBOSE .GE. 2) WRITE(6,1302) RESMAP,CIDR(NISO),CIDQ(NISO), &
          &     CID2(NISO)*CIDQ(NISO),CUROLD
1302 FORMAT(/,3X,'RESIDU OF ITERATION OVER MAPPING = ',1PE20.10, &
          &     '  C_1= ',E17.10,'  C_2= ',E17.10,'  C_3= ',E17.10, &
          &     '  IOLD= ',E17.10/)
  !
     CALL CONVER(2,NCON)
     !
     IF (NCON .EQ. 2) GOTO 4
     !
  END DO
  !
  !  ITERATION OVER CURRENT PROFILE HAS NOT CONVERGED
  !  (WILL STOP AFTER HAVING SAVED THE EQUIL. IN STEPON)
  !
  NCON = -2
  !
  WRITE(6,1500)
1500 FORMAT(/,73X,'  *****   ITERATION OVER CURRENT PROFILE NOT ', &
          &          'CONVERGED')
  !
4 CONTINUE 
  !
  RETURN
END SUBROUTINE ITIPR
