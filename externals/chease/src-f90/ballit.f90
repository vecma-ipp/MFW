!*DECK C2S02
!*CALL PROCESS
SUBROUTINE BALLIT
  !        #################
  !
  !**********************************************************************
  !                                                                     *
  ! C2S02 LEAD ITERATION TO SOLVE GRAD-SHAFRANOV
  !       ALSO ITERATE OVER PRESSURE PROFILE FOR BALLOONING             *
  !       OPTIMIZATION OR SPECIFICATION OF BOOTSTRAP CURRENT            *
  !       (SEE SECTION 4, 5.5 AND 6.4.3 IN PUBLICATION)                 *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  !  IDIM = 0 : FIRST ITERATION OVER EQUILIBRIUM. ARRAY DIMENSIONS
  !             ARE SMALL AND SPECIFIED IN SUBROUTINE EQDIM.
  !  IDIM = 1 : ITERATION OVER BALLOONING CALCULATION. ARRAY
  !             DIMENSIONS ARE STILL THE SAME AS FOR IDIM = 0.
  !  IDIM = 2 : IF NO BALLOONING OPTIMIZATION : SMALL CASE HAS
  !             CONVERGED. ARRAY DIMENSIONS ARE SET TO NAMELIST
  !             VALUES.
  !             IF BALLOONING OPTIMIZATION : ITERATION OVER P-PRIME
  !             HAS CONVERGED. ARRAY DIMENSIONS ARE SET TO NAMELIST
  !             VALUES
  !
  !  IGUESS = 1 : FIRST TIME EQUILIBRIUM IS COMPUTED : GUESS FOR 
  !               THE SOLUTION USED TO COMPUTE THE CURRENT DENSITY 
  !               IS A PARABOLOID.
  !  IGUESS = 2 : NEXT TIMES EQUILIBRIUM IS COMPUTED : GUESS FOR 
  !               THE SOLUTION USED TO COMPUTE THE CURRENT 
  !               DENSITY IS INTERPOLATED ON SOLUTION OF THE
  !               PREVIOUS EQUILIBRIUM
  !
  !  IFIN = -1 : FIRST ITERATION OVER P-PRIME
  !          0 : ERROR ON P-PRIME LARGER THAN EPSILON
  !          1 : ERROR ON P-PRIME SMALLER THAN EPSILON BUT NOT STABLE 
  !              EVERYWHERE   
  !          2 : ERROR ON P-PRIME SMALLER THAN EPSILON AND STABLE 
  !              EVERYWHERE  OR SMALLER THAN EPSILON / 10.0
  !          3 : CONVERGED                                              
  !
  REAL(RKIND)      ::     ZPSITEST
  INTEGER          ::     IDIM2LOOP
  INTEGER          ::     IDIMLOOP
  INTEGER          ::     IFIN
  INTEGER          ::     IDIM
  INTEGER          ::     IGUESS
  IGUESS = 1
  IDIM   = 0
  IF (NOPT.GE.-2 .AND. NOPT.LE.-1) THEN
     IGUESS = 2
     IF ((NBSOPT.EQ.0 .AND. NBLOPT.EQ.0) .OR. NOPT.EQ.1) THEN 
        IDIM = 2
     ELSE
        IDIM = 1
     ENDIF
     IF (NOPT .EQ. -1) IDIM = 0
     NOPT = 0
     R0  = RMAG
     RZ0 = RZMAG
     BPS( 1) = RMAG
     BPS(12) = RZMAG
     !
     IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
     IF (NSURF .EQ. 6) CALL BNDSPL
  ENDIF
  !
  IF (NBLOPT .EQ. 1) THEN
     !
     CALL RESPPR
     ! 
     IFIN = -1
     !
  ENDIF
  !
  IF (NBSOPT .EQ. 1) THEN
     ! 
     IFIN = -1
     !
  ENDIF
  !
  IDIMLOOP = 0
  !     NUMBER OF LOOP WITH THE BIG MESH (IDIM=2)
  IDIM2LOOP = 0
  !
1 CONTINUE
  !
  IDIMLOOP = IDIMLOOP + 1
  IF (IDIMLOOP .GE. NINSCA .AND. NBLOPT.NE.0) IFIN=3
  !
  NCON = 0
  !
  CALL EQDIM(IDIM)
  IF (IDIM .EQ. 2) IDIM2LOOP = IDIM2LOOP + 1
  !
  IF (NSURF .EQ. 1) IDIM = 2
  !
  CALL MESH(4)
  CALL INITIA
  IF (NVERBOSE .GE. 0) CALL OUTPUT(3)
  CALL MESH(1)
  CALL MESH(2)
  !
  IF (NSURF  .NE. 1) CALL GUESS(IGUESS)
  IF (NBLOPT .NE. 0 .OR. NBSOPT .NE. 0) CALL MESH(6)
  !
  IGUESS = 2
  !
  IF (NVERBOSE .GE. 1) CALL OUTPUT(5)
  CALL MATRIX
  IF (NVERBOSE .GE. 1) CALL OUTPUT(6)
  CALL ITIPR(NISO)
  CALL OLDNEW
  IF (NVERBOSE .GE. 1) CALL RUNTIM
  !
  IF (IDIM .EQ. 0) THEN
     !
     IF ((NBSOPT.EQ.0 .AND. NBLOPT.EQ.0) .OR. NOPT.EQ.1) THEN 
        !
        IDIM = 2
        !
     ELSE
        !
        IDIM = 1
        !
     ENDIF
     !
     R0  = RMAG
     RZ0 = RZMAG
     BPS( 1) = RMAG
     BPS(12) = RZMAG
     !
     IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
     IF (NSURF .EQ. 6) CALL BNDSPL
     !
     GOTO 1
     !
  ELSE IF (IDIM .EQ. 1) THEN
     !
     CALL PREMAP(2)
     !
     IF (NBLOPT .NE. 0) THEN
        !
        IF (NBLOPT .EQ. 1) CALL DCOPY(NPPR+1,CPPR,1,RPRM,1)
        !
        CALL NOREPT(NPPR+1,1)
        CALL BLTEST
        !
        ! IF IFIN = 3, BALLOONING STABILITY IS TESTED, BUT THE P-PRIME
        ! PROFILE IS NOT CHANGED.
        !
        IF (IFIN .EQ. 3) THEN
          !
          IF (NVERBOSE .GE. 1) CALL OUTPUT(10)
          !
          IDIM = 2
          !
        ELSE
          !
          CALL PPRM(NPPR+1,IFIN)
          IF (NVERBOSE .GE. 1) CALL OUTPUT(10)
          !
        ENDIF
        !
        NBLOPT = 2
        !
     ENDIF
     !
     IF (NBSOPT .NE. 0) THEN
        !
        CALL DCOPY(NPPR+1,CPPR,1,RPRM,1)
        CALL NOREPT(NPPR+1,1)
        CALL PPBSTR(NPPR+1,IFIN)
        !
        IF (IFIN .EQ. 3) THEN
          !
          IF (NVERBOSE .GE. 1) CALL OUTPUT(10)
          !
          IDIM = 2
          !
        ELSE
          !
          IF (NVERBOSE .GE. 1) CALL OUTPUT(10)
          !
        ENDIF
        !
        NBSOPT = 2
        !
     ENDIF
     !
     CALL OLDEQ
     !
     R0  = RMAG
     RZ0 = RZMAG
     BPS( 1) = RMAG
     BPS(12) = RZMAG
     !
     IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
     IF (NSURF .EQ. 6) CALL BNDSPL
     !
     GOTO 1
     !
     !   ELSE IF (IDIM.EQ.2 .AND. (IDIMLOOP.LE.3  .OR. &
  ELSE IF (IDIM.EQ.2 .AND. (IDIMLOOP.LE.2  .OR. &
       &        ((NSTTP.EQ.3 .OR. NSTTP.EQ.4) .AND. IDIM2LOOP.LE.3)) ) THEN
     !
     !     THERE MIGHT BE A PROBLEM IF PSIISO(1) WILL BE .LT. CPSICL(1)
     !     IN THIS CASE, PROFILE CALLS ISOFIND(K1>1) AND TETPSI(.,1), FOR E.G.,
     !     IS NOT COMPUTED IN ISOFIND. THIS HAPPENS IF R0 IS TOO FAR FROM RMAG.
     !     THEN SPSIM IS TOO MUCH LOWER THAN CPSICL(1) AND IT
     !     LEAVES ROOM FOR PSIISO(1) TO BE IN BETWEEN SPSIM AND CPSICL(1).
     !     PSIISO(1) IS TYPICALLY SPSIM*(1.-CSM(1)**2), WHICH FOR AN
     !     EQUIDISTANT MESH IS: SPSIM*(1.-(1/2/NPSI)**2).
     !
     !%OS            ZMACON = SQRT((RMAG-R0)**2+(RZMAG-RZ0)**2)
     !%OS            IF (ZMACON .GT. 0.3/ASPCT/FLOAT(NPSI)) THEN
     !
     ZPSITEST = SPSIM*(1._RKIND - 1._RKIND/(2._RKIND*NPSI)**2)
     IF (NVERBOSE .GE. 2) WRITE(6,'(/," IN BALLIT : ZPSITEST= ",1PE15.8, &
          &        "  CPSICL(1)= ",E15.8,/)') ZPSITEST, CPSICL(1)
     !
     !     EXTRA ITERATION ON BIG MESH
     !%OS            IF (ZPSITEST.LT.CPSICL(1) .OR.
     !%OS     +        (NIDEAL.EQ.6 .AND. IDIM2LOOP.LE.1)) THEN
     IF (ZPSITEST.LT.CPSICL(1) ) THEN
        R0  = RMAG
        RZ0 = RZMAG
        BPS( 1) = RMAG
        BPS(12) = RZMAG
        !
        IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
        IF (NSURF .EQ. 6) CALL BNDSPL
        !
        GOTO 1
     ENDIF
  ENDIF
  !
  IF (IDIM.EQ.2 .AND. IDIMLOOP.GT.2) THEN
     ZPSITEST = SPSIM*(1._RKIND - 1._RKIND/(2._RKIND*NPSI)**2)
     IF (NVERBOSE .GE. 2) WRITE(6,'(/," IN BALLIT : ZPSITEST= ",1PE15.8, &
          &        "  CPSICL(1)= ",E15.8,/)') ZPSITEST, CPSICL(1)
  ENDIF
  !
  CALL CHECK
  CALL DCOPY(NISO,TMF,1,TMFO,1)
  !
  RETURN
END SUBROUTINE BALLIT
