!*DECK C1S05 
!*CALL PROCESS
SUBROUTINE AUXVAL
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C1S05 SET UP AUXILLIARY VALUES                                      *
  !       THIS ROUTINE SETS UP ALL QUANTITIES FOR THE CONSTRUCTION OF   *
  !       THE PLASMA BOUNDARY (SEE SECTION 6.4.1 IN PUBLICATION)        *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE GLOBALS
  IMPLICIT NONE
  !
  REAL(RKIND)      ::     ZRM
  REAL(RKIND)      ::     ZRX
  INTEGER          ::     ISMIN
  INTEGER          ::     IMN
  INTEGER          ::     ISMAX
  INTEGER          ::     IMX
  INTEGER          ::     J4
  REAL(RKIND)      ::     ZR
  REAL(RKIND)      ::     ZT
  INTEGER          ::     J3
  REAL(RKIND)      ::     ZWORK
  DIMENSION &
       &   ZR(12*NPT),ZT(12*NPT),ZWORK(NPBPS)
  !-----------------------------------------------------------------------
  !
  ! Various flags for diag etc
  !
  IF (NVERBOSE .GE. 2) THEN
    NDIAGOP = 1
  ELSEIF (NVERBOSE .LE. 0) THEN
    NDIAGOP = -1
  END IF
  IF (MFLGDIAG1 .EQ. 0) NDIAGOP = -1
  !
  !        SET SOME PARAMETERS FOR EQDSK AND NOT EUITM INPUT
  !
  IF (NSURF.EQ.6 .AND. NEQDSK>=1 .AND. NITMOPT.NE.22 .AND. mod(NITMOPT,10).NE.1) THEN 
     NPPFUN = 4
     NFUNC  = 4
     NSTTP  = 1
  ENDIF
  ! in case do not write on ITM database
  IF (NITMOPT .LE. 9) NPROF2D = 0
  !
  !  READ EXPERIMENTAL EQUILIBRIUM DATA'S 
  !     IF NOPT .NE. 1
  !
  IF ((NFUNC .EQ. 4 .OR. NPPFUN .EQ. 4 .OR. NPPFUN .EQ. 8 .OR. NSURF .EQ. 6 .OR. &
       &         NSURF .EQ. 7)  .AND.  NOPT .NE. 1) &
       &   CALL IODISK(3)
  !
  ! At this stage expected IP, B0 signs should be + or -1. A value of "-9" is used to keep related value from input equilibrium
  ! in above call to iodisk(3)
  IF (SIGNIPXP < -1) SIGNIPXP = 1.0_rkind
  IF (SIGNB0XP < -1) SIGNB0XP = 1.0_rkind
  !
  IF (NOPT .NE. 1) THEN
     !  NOTE: CAN GIVE SPECIFIC SIGN TO B0EXP OR R0EXP, BUT THIS IS TO ENFORCE NAMELIST VALUE INSTEAD
     !        OF NIN OR EQDSK VALUES. IT IS NOT RELATED TO EFFECTIVE SIGN OF IP AND/OR B0 IN EXPERIMENT
     !        WHICH ARE IRRELEVANT FOR CHEASE. 
     !        THEY CAN BE GIVEN IN THE NAMELIST THROUGH SIGNB0XP AND SIGNIPXP FOR OUTPUTS 
     R0EXP = ABS(R0EXP)
     B0EXP = ABS(B0EXP)
  ENDIF
  !
  ! FLAGS RELATED TO INPUT X-ARRAY BEING FUNCTION OF RHO_TOR INSTEAD OF RHO_PSI
  ! TO BE CHECKED AFTER HAVING READ EXP. EQUILIBRIUM
  !
  IF (NFUNRHO .GE. 1) THEN
     IF (NVERBOSE .GE. 0) write(0,*) 'EXP. PROFILES MAY BE NOT GIVEN IN TERMS OF RHO PSI'
     IF ( ((NPPFUN .NE. 4) .AND. NPPFUN.NE.8) .OR. (NFUNC .NE. 4) ) THEN
       IF (NVERBOSE .GE. 0) write(0,*) ' NFUNRHO>=1 ONLY WITH NPPFUN=4 or 8 AND NFUNC=4'
       STOP 'auxval'
     ENDIF
     IF (NSTTP .EQ. 1) THEN
       IF (NVERBOSE .GE. 0) PRINT *,' NFUNRHO>=1 ONLY WITH NSTTP>=2 SO FAR'
       STOP
     ENDIF
     IF (NEQDSK>=1 .AND. NITMOPT.LT.10) THEN
       IF (NVERBOSE .GE. 0) PRINT *,'EQDSK PROFILES SHOULD BE IN TERMS OF RHO_PSI'
       STOP
     ENDIF
  ENDIF
  !
  ! SET FLAGS TO EVALUATE PSI IN PSIBOX ON (1:NRBOX,1:NZBOX) GRID
  !
  IF (NEQDXTPO.LT.0 .AND. NEQDXTPO.GT.-100) THEN
    NEQDZMG = 0
    NEQDXTPO = ABS(NEQDXTPO)
  ELSE IF (NEQDXTPO .LE. -100 .AND. NEQDXTPO.GT.-999) THEN
    NEQDZMG = 2
    NEQDXTPO = ABS(NEQDXTPO+100)
  ELSE IF (NEQDXTPO .LE. -1000) THEN
    ! zboxmid=0 and shift to zaxis=0
    NEQDZMG = 3
    NEQDXTPO = ABS(NEQDXTPO+1000)
  ENDIF
  !
  !     READ TE, NE, ZEFF AND/OR TI FROM EXPTNZ FILE
  !
  IF (NBSEXPQ .GT. 0) CALL BSEXPEQ(1)
  !
  !-----------------------------------------------------------------------
  !
  !     MODIFY INPUTS TO SCALE PPRIME PROFILE WITH CPRESS FOR NEW EQUIL.
  !
  IF (CPRESS.NE.1.0_RKIND .AND. NOPT.NE.1) THEN
     !
     IF (NPPFUN .EQ. 1) THEN
        CALL DSCAL(NSOUR,CPRESS,AP,1)
     ELSE IF (NPPFUN .EQ. 2) THEN
        CALL DSCAL(5,CPRESS,AP(3),1)
        CALL DSCAL(5,CPRESS,AP2(3),1)
     ELSE IF (NPPFUN .EQ. 3) THEN
        AP(1)  = CPRESS * AP(1)
        AP(4)  = CPRESS * AP(4)
        AP2(1) = CPRESS * AP2(1)
        AP2(4) = CPRESS * AP2(4)
     ELSE IF (NPPFUN .EQ. 4 .OR. NPPFUN .EQ. 8) THEN
        CALL DSCAL(NPPF+1,CPRESS,RPPF,1)
     ELSE IF (NPPFUN .EQ. 5) THEN  
        AP(2) = CPRESS * AP(2)
     ELSE IF (NPPFUN .EQ. 6) THEN  
        CALL DSCAL(8,CPRESS,AP(6),1)
        CALL DSCAL(8,CPRESS,AP2(6),1)
     ELSE IF (NPPFUN .EQ. 7) THEN  
        AP(1)  = CPRESS * AP(1)
        AP2(1) = CPRESS * AP2(1)
     ELSE
       IF (NVERBOSE .GE. 0) write(0,*) ' ERROR IN RESCALING PPRIME PARAMETERS IN AUXVAL'
       IF (NVERBOSE .GE. 0) write(0,*) ' NPPFUN= ',NPPFUN,' NOT YET DEFINED'
       STOP
     ENDIF
     !
     IF (NVERBOSE .GE. 1) WRITE(*,'(/,20X,"PPRIME PARAMETERS ADAPTED AS CPRESS= ", &
       &       F12.4,/)') CPRESS
     CPRESSO = CPRESS
     !
  ENDIF
  !
  CPRESS = 1.0_RKIND
  !
  !-----------------------------------------------------------------------
  !
  !     MODIFY INPUTS TO SCALE FUNC PROFILE WITH CFNRESS FOR NEW EQUIL.
  !
  IF (CFNRESS.NE.1.0_RKIND .AND. NOPT.NE.1 .AND. NRFP.NE.1) THEN
     !
     IF (NFUNC .EQ. 1) THEN
        CALL DSCAL(NSOUR,CFNRESS,AT,1)
     ELSE IF (NFUNC .EQ. 2) THEN
        CALL DSCAL(5,CFNRESS,AT(3),1)
        CALL DSCAL(5,CFNRESS,AT2(3),1)
        CALL DSCAL(5,CFNRESS,AT3(3),1)
        AT4(3)  = CFNRESS * AT4(3)
     ELSE IF (NFUNC .EQ. 3) THEN
        AT(1)  = CFNRESS * AT(1)
     ELSE IF (NFUNC .EQ. 4) THEN
        CALL DSCAL(NPPF+1,CFNRESS,RFUN,1)
     ELSE IF (NFUNC .EQ. 5) THEN  
       IF (NVERBOSE .GE. 0) write(0,*) ' OPTION NFUNC=5 AND CFNRESS NOT YET DEFINED'
       IF (NVERBOSE .GE. 0) write(0,*) ' FUNCTION NOT DEFINED WELL ENOUGH IN PRFUNC'
       STOP 'auxval'
     ELSE
       IF (NVERBOSE .GE. 0) write(0,*) ' ERROR IN RESCALING FUNC PARAMETERS IN AUXVAL'
       IF (NVERBOSE .GE. 0) WRITE(0,*) ' NFUNC= ',NFUNC,' NOT YET DEFINED'
       STOP 'AUXVAL'
     ENDIF
     !
     IF (NVERBOSE .GE. 1) WRITE(*,'(/,20X,"FUNC PARAMETERS ADAPTED AS CFNRESS= ", &
       &       F7.4,/)') CFNRESS
     CFNRESSO = CFNRESS
     !
  ENDIF
  !
  CFNRESS = 1.0_RKIND
  !
  !-----------------------------------------------------------------------
  !
  ! packing
  !
  ! If NMESHPOL .ne. 0, overrides ndift and change NEMSHD=2 and NMESHE=2
  if (nmeshpol .gt. 0) then
    NMESHD = 2
    NMESHE = 2
    SOLPDD = SOLPDPOL
    SOLPDE = SOLPDPOL
    NDIFT = 0
  end if
  !
  !-----------------------------------------------------------------------
  !
  !     REST OF SUBROUTINE: DETERMINE VARIABLES RELATED TO PLASMA BOUNDARY
  !
  !
  !     IF NOPT.EQ.1 THEN ALL NEEDED VARIABLES SHOULD READ FROM NIN => RETURN
  !
  IF (NOPT .EQ. 1) RETURN
  !
  !  FIT EXPERIMENTAL PROFILES WITH CUBIC SPLINES. COMPUTE 2ND DERIVATIVE ARRAYS
  !
  IF (NPPFUN .EQ. 4 .OR. NPPFUN .EQ. 8) CALL SPLINE(NPPF+1,FCSM,RPPF,D2RPPF,ZWORK)
  IF (NFUNC  .EQ. 4) CALL SPLINE(NPPF+1,FCSM,RFUN,D2RFUN,ZWORK)
  !
  IF (NSURF .EQ. 1) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 1. SOLOVEV                                                          *
     !                                                                     *
     !**********************************************************************
     !
     SPSI0 = .5_RKIND * ELONG * ASPCT**2 / (RC * CQ0)
     CPP   = - 2._RKIND * SPSI0 * (1._RKIND + ELONG**2) /(ASPCT*RC*ELONG)**2
     !
     BPS(1 ) = R0
     BPS(2 ) = RC
     BPS(3 ) = 0._RKIND
     BPS(4 ) = ASPCT
     BPS(5 ) = ELONG
     BPS(6 ) = 0._RKIND
     BPS(7 ) = 0._RKIND
     BPS(8 ) = 0._RKIND
     BPS(9 ) = 0._RKIND
     BPS(10) = 0._RKIND
     BPS(11) = 0._RKIND
     BPS(12) = 0._RKIND
     !
  ELSE IF (NSURF .EQ. 2) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 2. INTOR - LIKE PLASMA SURFACE                                      *
     !                                                                     *
     !**********************************************************************
     !
     IF (BEANS .NE. 0._RKIND) R0 = R0 + ASPCT * BEANS
     !
     BPS(1 ) = R0
     BPS(2 ) = RC
     BPS(3 ) = BPS(2) - BPS(1)
     BPS(4 ) = ASPCT
     BPS(5 ) = ELONG
     BPS(6 ) = ASPCT * RC
     BPS(7 ) = TRIANG
     BPS(8 ) = BEANS
     BPS(9 ) = XI
     BPS(10) = CETA
     BPS(11) = 0.0_RKIND
     BPS(12) = 0.0_RKIND
     !
  ELSE IF (NSURF .EQ. 3) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 3. RACETRACK PLASMA SURFACE                                         *
     !                                                                     *
     !**********************************************************************
     !
     IF (BEANS .NE. 0._RKIND) R0 = R0 + ASPCT * BEANS
     !
     BPS(1 ) = R0
     BPS(2 ) = RC
     BPS(3 ) = BPS(2) - BPS(1)
     BPS(4 ) = ASPCT
     BPS(5 ) = ELONG
     BPS(6 ) = ASPCT * RC
     BPS(7 ) = TRIANG
     BPS(8 ) = BEANS
     BPS(9 ) = CETA
     BPS(10) = SGMA
     BPS(11) = TRIPLT
     BPS(12) = 0._RKIND
     BPS(13) = 0._RKIND
     BPS(14) = 0._RKIND
     !
  ELSE IF (NSURF .EQ. 4) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 4. X - POINT PLASMA SURFACE                                         *
     !                                                                     *
     !**********************************************************************
     !
     IF (BEANS .NE. 0._RKIND) R0 = R0 + ASPCT * BEANS
     !
     BPS(1 ) = R0
     BPS(2 ) = RC
     BPS(3 ) = BPS(2) - BPS(1)
     BPS(4 ) = ASPCT
     BPS(5 ) = ELONG
     BPS(6 ) = ASPCT * RC
     BPS(7 ) = RNU
     BPS(8 ) = XI
     BPS(9 ) = THETA0
     BPS(10) = SGMA
     BPS(11) = DELTA
     BPS(12) = 0.0_RKIND
     BPS(13) = TRIANG
     BPS(14) = BEANS
     !
     !**********************************************************************
     !                                                                     *
     ! 4.1 TEST IF BPS(11) AND BPS(8) ARE EQUAL TO 0                       *
     !                                                                     *
     !**********************************************************************
     !
     IF (BPS(11) .LT. EPSMCH)  BPS(11) = EPSMCH
     IF (BPS(8)  .LT. EPSMCH)  BPS(8)  = EPSMCH
     !
  ELSE IF (NSURF .EQ. 5) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 5. OCTOPOLE PLASMA SURFACE                                          *
     !                                                                     *
     !**********************************************************************
     !
     BPS(1 ) = R0
     BPS(2 ) = RC
     BPS(3 ) = BPS(2) - BPS(1)
     BPS(4 ) = ASPCT 
     BPS(5 ) = SGMA
     BPS(6 ) = ASPCT * RC
     BPS(7 ) = DELTA
     BPS(8 ) = THETA0
     BPS(9 ) = 0._RKIND
     BPS(10) = 0._RKIND
     BPS(11) = 0._RKIND
     BPS(12) = 0._RKIND
     BPS(13) = 0._RKIND
     BPS(14) = 0._RKIND
     !
  ELSE IF (NSURF .EQ. 6) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 6. PLASMA SURFACE DEFINED BY NBPS (R,Z) COORDINATES                 *
     !                                                                     *
     !**********************************************************************
     !
     !SYM FOR SYMMETRIC EQUILIBRIA, SHIFT BOUNDARY POINTS SO THAT
     !    Z=0 IS (ZMAX+ZMIN)/2 OF BOUNDARY
     !
     IF (NSYM.EQ.1) THEN
        CALL SUBSZ
        RZ0 = 0._RKIND
     ENDIF
     !
     BPS(1 ) = R0
     BPS(12) = RZ0
     !
     !   FIT BOUNDARY WITH CUBIC SPLINES
     !
     CALL BNDSPL
     !
  ELSE IF (NSURF .EQ. 7) THEN
     !
     !**********************************************************************
     !                                                                     *
     ! 7. PLASMA SURFACE DEFINED BY FOURIER COEFFICIENTS                   *
     !                                                                     *
     !**********************************************************************
     !
     BPS(1 ) = R0
     BPS(2 ) = RC
     BPS(3 ) = BPS(2) - BPS(1)
     BPS(6 ) = RZ0C
     BPS(12) = RZ0
     !
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  !  ADJUST BPS(4) AND BPS(5) SUCH THAT (RMIN+RMAX)/(RMAX-RMIN)=ASPCT   *
  !                                                                     *
  !**********************************************************************
  !
  IF (NSURF .EQ. 1 .OR. NSURF .EQ. 2 .OR. NSURF .EQ. 6 .OR. &
       &        NSURF .EQ. 7) RETURN
  ! 
  !%OS         IF (NSURF .EQ. 5) THEN
  !
  PRINT*,'**************WARNING***************************'
  PRINT*,' '
  PRINT*,'ASPECT RATIO IS CHANGED IN SUBROUTINE AUXVAL'
  PRINT*,'SUCH THAT ASPCT = (RMAX - RMIN) / (RMAX + RMIN)'
  PRINT*,'WHERE RMAX AND RMIN ARE THE MAXIMUM AND MINIMUM'
  PRINT*,'R VALUE OF THE PLASMA CROSS-SECTION'
  PRINT*,' '
  PRINT*,'**************WARNING***************************'
  !
  !%OS         ENDIF
  !
  DO J3=1,100
     !
     ZT(J3) = (J3 - 1) * CPI / 50._RKIND
     !
  END DO
  !
  CALL BOUND(100,ZT,ZR)
  !
  DO J4=1,100
     !
     ZR(J4) = BPS(1) + ZR(J4) * COS(ZT(J4))
     !
  END DO
  !
  IMX = ISMAX(100,ZR,1)
  IMN = ISMIN(100,ZR,1)
  ZRX = (ZR(IMX) - BPS(1) - BPS(3)) / BPS(6)
  ZRM = (ZR(IMN) - BPS(1) - BPS(3)) / BPS(6)
  !
  BPS(6) = 2._RKIND * BPS(2) / ((ZRX - ZRM) / ASPCT - (ZRX + ZRM))
  BPS(4) = BPS(6) / BPS(2)
  !
  RETURN
END SUBROUTINE AUXVAL
