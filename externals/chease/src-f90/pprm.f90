!*DECK C2SP10
!*CALL PROCESS
SUBROUTINE PPRM(KPPR1,KFIN)
  !        #####################
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  !  C2SP10  MODIFY P' DURING BALLOONING OPTIMIZATION ACCORDING TO THE  *
  !          ALGORITHM DESCRIBED IN SECTION 5.5 OF THE PUBLICATION      *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     KPPR1, KFIN
  !
  REAL(RKIND)      ::     ZDIFF(KPPR1), ZWORK(KPPR1)
  INTEGER          ::     ISTAB, ISUM, IP01, J960, J910
  INTEGER          ::     J, J11, J12, J13, J14, J15, J16, J17, J18
  REAL(RKIND)      ::     ZEPS, ZERR
  PARAMETER(ZEPS = 1.E-3_RKIND)
  !
  IF (KPPR1 .NE. NPPR+1) THEN
    PRINT *,'PROBLEM IN PPRM, KPPR1=',KPPR1,' .NE. NPPR+1=',NPPR+1
    STOP
  END IF
  !
  ZERR  = 0._RKIND
  ISTAB = 10
  !
  WRITE(*,*) (NP4(J),NP3(J),NP2(J),NP1(J),NP0(J),J=1,NPPR+1)
  WRITE(6,100) 'UNSTABLES VALUES : '
  !
  DO J11=1,NPPR+1
     !
     XP3(J11) = XP2(J11)
     XP2(J11) = XP1(J11)
     XP1(J11) = XP0(J11)
     XP0(J11) = RPRM(J11)
     !
     NP3(J11) = NP2(J11)
     NP2(J11) = NP1(J11)
     NP1(J11) = NP0(J11)
     !
     IF (SMERCI(J11) .GE. 0._RKIND .AND. NCBAL(J11) .EQ. 0) THEN 
        ! STABLE
        IF (NP0(J11) .NE. 0) NP0(J11) = 1
        !
        IF (XP0(J11).NE.0.0_RKIND) THEN
           !
           IF (ISTAB.EQ.10) ISTAB=1
           IF (ISTAB.EQ.2)  ISTAB=3
           !
        ENDIF
        !
     ELSE IF (SMERCI(J11) .LT. 0._RKIND .OR. NCBAL(J11) .GE. 1) THEN 
        ! UNSTABLE
        WRITE(6,200) J11,XP0(J11)
        !
        IF (NP0(J11) .NE. 0) NP0(J11) = - 1
        !
        XP4(J11) = XP0(J11)
        NP4(J11) = NP0(J11)
        !
        IF (XP0(J11) .NE. 0._RKIND) THEN
           !
           IF (ISTAB .EQ. 10) ISTAB=2
           IF (ISTAB .EQ. 1)  ISTAB=3
           !
        ENDIF
     ENDIF
     !
  END DO
  !
  WRITE(95,400) (PCSM(J),XP0(J),J=1,NPPR+1)
  WRITE(95,410) (PCSM(J),NP0(J),J=1,NPPR+1)
  !
  WRITE(6,*)
  !
  !***********************************************************************
  !                                                                      *
  ! ISTAB=1 IF THE PROFILE IS STABLE   EVERYWHERE                        * 
  !      =2 IF THE PROFILE IS UNSTABLE EVERYWHERE                        * 
  !      =3 OTHERWISE                                                    * 
  !                                                                      *
  !***********************************************************************
  !
  IF (ISTAB .EQ. 1) THEN
     !
     IF (NSRCH .EQ. 2) THEN
        !
        DO J12=1,NPPR+1
           !
           XPPRMN(J12) = XP0(J12)
           XLAMB(J12)  = 0._RKIND
           XP0(J12)    = XP4(J12)
           NP0(J12)    = NP4(J12)
           !
        END DO
        !
        WRITE(*,*) 'MINIMUM FOUND'
        WRITE(*,'(F15.8)') (XPPRMN(J),J=1,NPPR+1)
        !
        NSRCH = 2
        !
     ENDIF
     !
  ENDIF
  !
  IF (ISTAB .EQ. 2) THEN
     !
     IF (NSRCH .EQ. 1) THEN
        !
        DO J13=1,NPPR+1
           !
           XPPRMN(J13) = 0._RKIND
           XPPRMX(J13) = XP0(J13)
           XPPRDF(J13) = XPPRMX(J13) - XPPRMN(J13)
           XLAMB(J13)  = 1._RKIND
           NP1(J13)    = 0
           !
        END DO
        !
        WRITE(*,*)
        WRITE(*,*) 'MAXIMUM FOUND'
        !
        NSRCH = 3
        !
     ENDIF
     !
  ENDIF
  !
  !***********************************************************************
  !                                                                      *
  ! KFIN = -1 : FIRST ITERATION                                          *
  !         0 : ERROR LARGER THAN ZEPS                                   *
  !         1 : ERROR SMALLER THAN ZEPS BUT NOT STABLE EVERYWHERE        *
  !         2 : ERROR SMALLER THAN ZEPS AND STABLE EVERYWHERE  OR        *
  !             ERROR SMALLER THAN ZEPS / 10.0                           *
  !         3 : CONVERGED                                                *
  !                                                                      *
  !***********************************************************************
  !
  IF (KFIN .EQ. -1) THEN
     !
     DO J14=1,NPPR+1
        !
        IF (PCSM(J14) .LT. .1_RKIND) THEN
           !
           XP0(J14) = 0._RKIND
           NP0(J14) = 0
           !
        ENDIF
        !
        XP4(J14) = XP0(J14)
        NP4(J14) = NP0(J14)
        !
        XPPRMN(J14) = 0._RKIND
        XPPRMX(J14) = XP0(J14)
        XPPRDF(J14) = XP0(J14)
        !
     END DO
     !
     KFIN = 0
     !
  ENDIF
  !
  !***********************************************************************
  !                                                                      *
  ! IF NSRCH=1, SEARCH A COMPLETELY UNSTABLE PROFILE                     *
  !          2,                     STABLE   PROFILE                     *
  !          3, SEARCH OPTIMIZED PROFILE (1st time)                      *
  !          4,        OPTIMIZED PROFILE                                 *
  !                                                                      *
  !***********************************************************************
  !
  IF (NSRCH .EQ. 1) THEN
     !
     DO J15=1,NPPR+1
        !
        IF (NP0(J15) .EQ. 1 .AND. XP0(J15) .NE. 0._RKIND) THEN
           !
           XLAMB(J15) = 1.1_RKIND
           !
           IF (XP0(J15) .NE. 0._RKIND) XP0(J15) = XP0(J15) - 0.02_RKIND
           !
        ELSE
           !
           XLAMB(J15)=1._RKIND
           !
        ENDIF
        !
        RPRM(J15) = XLAMB(J15) * XP0(J15)
        !
        IF (RPRM(J15) .GT. 0._RKIND) RPRM(J15) = 0._RKIND
        !
        ZDIFF(J15) = ABS(RPRM(J15) - XP0(J15))
        !
        IF (ZDIFF(J15) .GT. ZERR) ZERR = ZDIFF(J15)
        !
     END DO
     !
     GOTO 9000
     !      
  ELSE IF (NSRCH .EQ. 2) THEN
     !
     DO J16=1,NPPR+1
        !
        IF (NP0(J16) .EQ. -1 .AND. XP0(J16) .NE. 0._RKIND) THEN
           !
           XLAMB(J16) = .5_RKIND
           !
        ELSE
           ! 
           XLAMB(J16) = 1._RKIND
           !
        ENDIF
        !
        RPRM(J16) = XLAMB(J16)*XP0(J16) 
        !
        IF (RPRM(J16) .GT. 0._RKIND) RPRM(J16) = 0._RKIND
        !
        ZDIFF(J16) = ABS(RPRM(J16) - XP0(J16))
        !
        IF (ZDIFF(J16) .GT. ZERR) ZERR = ZDIFF(J16)
        !
     END DO
     !      
     GOTO 9000
     !
  ELSE IF (NSRCH .EQ. 3) THEN
     !
     DO J17=1,NPPR+1
        !
        X2SRCH(J17) = 0.1_RKIND
        XLAMB(J17)  = 1._RKIND
        !
        IF (NP0(J17) .EQ. -1) THEN
           !
           IF (XP0(J17) .GT. -.005_RKIND .AND. XP0(J17-1) .EQ. 0._RKIND) THEN
              !
              WRITE(*,*) J17,XP0(J17),' MIS A ZERO'
              !
              XP0(J17)    = 0._RKIND
              XPPRMN(J17) = 0._RKIND
              XPPRDF(J17) = 0._RKIND
              !
           ENDIF
           !
           XLAMB(J17) = XLAMB(J17)  - X2SRCH(J17)
           !
        ELSE 
           !
           XLAMB(J17) = XLAMB(J17)  + X2SRCH(J17)
           !
        ENDIF

        !
        RPRM(J17)  = XPPRMN(J17) + XLAMB(J17) * XPPRDF(J17)
        !
        IF (RPRM(J17) .GT. 0._RKIND) RPRM(J17) = 0._RKIND
        !
        ZDIFF(J17) = ABS(RPRM(J17) - XP0(J17))
        !
        IF (ZDIFF(J17) .GT. ZERR) ZERR = ZDIFF(J17)
        !
     END DO
     !
     NSRCH = 4
     !
     GOTO 9500
     !  
  ELSE IF (NSRCH .EQ. 4) THEN
     !
     DO J18=1,NPPR+1
        !
        IF (XPPRDF(J18) .EQ. 0._RKIND) GOTO 18
        !
        IP01 = NP0(J18) * NP1(J18)
        ISUM = ABS(NP1(J18) + NP2(J18) + NP3(J18))
        !
        IF (IP01 .LT. 0) THEN
           ! 
           X2SRCH(J18) = .5_RKIND * X2SRCH(J18)
           !
        ELSE IF (ISUM .EQ. 3 .AND. KFIN .EQ. 0) THEN
           !
           X2SRCH(J18) = 1.2_RKIND * X2SRCH(J18)
           !
        ENDIF
        !
        IF (NP0(J18) .EQ. -1) THEN
           !
           IF (XP0(J18) .GT. -.005_RKIND .AND. XP0(J18-1) .EQ. 0._RKIND) THEN
              !
              WRITE(*,*) J18,XP0(J18),' MIS A ZERO'
              !
              XP0(J18)    = 0._RKIND
              XPPRMN(J18) = 0._RKIND
              XPPRDF(J18) = 0._RKIND
              !
           ENDIF
           !
           XLAMB(J18) = XLAMB(J18) - X2SRCH(J18)
           RPRM(J18)  = XPPRMN(J18) + XLAMB(J18) * XPPRDF(J18)
           !
           IF (RPRM(J18) .GT. 0._RKIND) RPRM(J18) = 0._RKIND
           !
        ELSE IF (KFIN .EQ. 0 .AND. NP0(J18) .EQ. 1) THEN
           !
           XLAMB(J18) = XLAMB(J18)  + X2SRCH(J18)
           RPRM(J18)  = XPPRMN(J18) + XLAMB(J18) * XPPRDF(J18)
           !
           IF (RPRM(J18) .GT. 0._RKIND) RPRM(J18) = 0._RKIND
           !
        ENDIF
        !
        ZDIFF(J18) = ABS(RPRM(J18) - XP0(J18))
        !
        IF (ZDIFF(J18) .GT. ZERR) ZERR = ZDIFF(J18)
        !
18      CONTINUE 
     END DO
     !
     GOTO 9500
     !
  ENDIF
  !      
  !***********************************************************************
  !                                                                      *
  !                            OUTPUT                                    *
  !                                                                      *
  !***********************************************************************
  !
9000 CONTINUE 
  !
  WRITE(*,*) 
  WRITE(*,*) 'ISTAB : ',ISTAB,NSRCH
  WRITE(*,*) 
  WRITE(*,*) '   I       CSM       P1P  IP1', &
       &             '       P0P  IP0', &
       &             '      LAMBDA          PP'
  DO J910=1,NPPR+1
     WRITE(*,250) J910,PCSM(J910),XP1(J910),NP1(J910), &
          &                   XP0(J910),NP0(J910), &
          &                   XLAMB(J910),RPRM(J910)
  END DO
  WRITE(*,*) 
  WRITE(*,*) 'ERROR  : ',ZERR,ZEPS
  !
  CALL SPLINE(NPPR+1,PCSM,RPRM,D2RPRM,ZWORK)
  !
  RETURN
  !
9500 CONTINUE 
  !
  WRITE(*,*) 
  WRITE(*,*) 'ISTAB : ',ISTAB,NSRCH-3
  WRITE(*,*) 
  WRITE(*,*) '   I          CSM               PP0      IP0    ', &
       &             '     MIN               MAX                 LAMBDA', &
       &             '        DIFF                PP'
  DO J960=1,NPPR+1
     WRITE(*,260) J960,PCSM(J960),XP0(J960),NP0(J960), &
          &                   XPPRMN(J960),XPPRMX(J960),XLAMB(J960), &
          &                   ZDIFF(J960),RPRM(J960)
  END DO
  !
  WRITE(*,*) 
  !
  IF (ZERR .LE. ZEPS) THEN
     !
     IF (ZERR .EQ. 0._RKIND .AND. ISTAB .NE. 1) STOP
     !
     WRITE(6,110) 'EPSILON > ERROR > EPSILON / 5._RKIND '
     WRITE(6,*) ZEPS,ZERR,ZEPS / 5._RKIND
     !
     KFIN = 3
     !
  ELSE
     !
     WRITE(*,*) 'ERROR > EPSILON ',ZERR,ZEPS
     !
  ENDIF
  !
  CALL SPLINE(NPPR+1,PCSM,RPRM,D2RPRM,ZWORK)
  !
  RETURN
  !
100 FORMAT (A)
110 FORMAT (A,/)
120 FORMAT (A,F10.3,A)
  !
200 FORMAT (I8,2X,F15.8,/)
240 FORMAT (I5,3(2X,F15.8),I5)
250 FORMAT (I5,2X,F15.8,2(2X,F15.8,I5),2(2X,F15.8))
260 FORMAT (I5,2(2X,F15.8),I5,5(2X,F15.8))
  !
300 FORMAT (F6.2) 
400 FORMAT (E15.8,2X,E15.8)
410 FORMAT (E15.8,2X,I5)
  !
500 FORMAT ('THE ',F10.4,' % OPT PROFILE STABLE EVERYWHERE')
510 FORMAT ('THE ',F10.4,' % OPT PROFILE NOT STABLE EVERYWHERE')
520 FORMAT ('THE ',F10.4,' % OPT PROFILE NOT UNSTABLE EVERYWHERE')
530 FORMAT ('THE ',F10.4,' % OPT PROFILE UNSTABLE EVERYWHERE')
  !
610 FORMAT (I4) 
620 FORMAT (1PE15.8) 
  !
END SUBROUTINE PPRM
