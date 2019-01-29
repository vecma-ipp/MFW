SUBROUTINE NERAT
  !        ################
  !                                        AUTHORS:
  !                                        A. ROY,  CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SB05  PRODUCE ERATO NAMELIST                                      *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  !
  !
  !**********************************************************************
  !                                                                     *
  !     C.21   ERATO NAMELIST                                           *
  !                                                                     *
  !**********************************************************************
  !
  REAL(rkind) AL0,      ANGLE,    ARROW, &
       &   EPSCON,   EPSMAC,   QIAXE,    QSURF,   &
       &   TSURF,    WNTORE,   WALL,     P0
  INTEGER NAL0AUTO, &
       &   ITEST,    &
       &   NFIG,     NITMAX,   NUA1, &
       &   NUA2,     NUB1,     NUB2,     NUX,      NUPL,     NUSG, &
       &   NUWA,     NVIT,     NWALL

  DIMENSION &
       &   ANGLE(16),   QTILDA(NPPSI1),  QS(NPPSI1),  WALL(10)
  !
  LOGICAL   NLDIAG(1:20), &
       &   NLEINQ,   NLGREN,        NLSYM
  !
  NAMELIST /NEWRUN/ &
       &   AL0,      ANGLE,    ARROW,    BETA,     BETAP,    CPSRF, &
       &   EPSCON,   EPSMAC,   QIAXE,    QSURF,    REXT,     RITOT, &
       &   TSURF,    WNTORE,   WALL,     RINOR,    QCYL,     P0, &
       &   NAL0AUTO, &
       &   NER,      NEGP,     ITEST,    MEQ,      NCHI,     NDES, &
       &   NFIG,     NITMAX,   NPRNT,    NPSI,     NSAVE,    NUA1, &
       &   NUA2,     NUB1,     NUB2,     NUX,      NUPL,     NUSG, &
       &   NV,       NUWA,     NVAC,     NVIT,     NWALL, &
       &   NLDIAG,   NLEINQ,   NLGREN,   NLSYM
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !
  !**********************************************************************
  !                                                                     *
  !  . COMPUTE RINOR AND QCYL FOR ERATO DIAGNOSTICS                     *
  !                                                                     *
  !**********************************************************************
  !
  INTEGER          ::     J
  INTEGER          ::     J4
  REAL(RKIND)      ::     Z4
  REAL(RKIND)      ::     Z3
  REAL(RKIND)      ::     Z2
  REAL(RKIND)      ::     Z1
  INTEGER          ::     J3
  REAL(RKIND)      ::     SMAX
  INTEGER          ::     NPLEQ
  REAL(RKIND)      ::     WVERT
  REAL(RKIND)      ::     WCURV
  REAL(RKIND)      ::     WASPCT
  REAL(RKIND)      ::     WNL
  REAL(RKIND)      ::     B2R2
  REAL(RKIND)      ::     CST
  REAL(RKIND)      ::     ALARG
  REAL(RKIND)      ::     QS
  REAL(RKIND)      ::     QTILDA
  !
  IF (NIDEAL.NE.1 .AND. NIDEAL.NE.2) RETURN
  !
  CALL VZERO(ANGLE,16)
  CALL VZERO(QTILDA,NPPSI1)
  CALL VZERO(QS,NPPSI1)
  !
  ALARG     = 24._RKIND
  AL0       = -1.E-04_RKIND
  ANGLE(1)  = 0._RKIND
  ANGLE(2)  = 90._RKIND
  !
  CALL VZERO(ANGLE(3),13)
  !
  ARROW     = 0.05_RKIND
  CST       = 1._RKIND / (Q0 * CPSRF)
  B2R2      = 0._RKIND
  EPSCON    = 1.E-04_RKIND
  EPSMAC    = RC1M12
  REXT      = 10_RKIND
  P0        = 0._RKIND
  QIAXE     = 1._RKIND / Q0
  WNL       = 0._RKIND
  WNTORE    = 1._RKIND
  WASPCT    = 1.1_RKIND
  WCURV     = 0._RKIND
  WVERT     = 11._RKIND
  ITEST     = 1
  NAL0AUTO  = 1
  NPLEQ     = 0
  SMAX      = 2._RKIND
  NFIG      = 2
  NITMAX    = 10
  NV        = 40
  NVIT      = 1
  NUA1      = 31
  NUA2      = 32
  NUB1      = 33
  NUB2      = 34
  NUX       = 35
  !
  !  DISK UNITS set in preset
  !
  CALL VZERO(WALL,10)
  !
  NLDIAG(1:11) = .TRUE.
  NLDIAG(2) = .TRUE.
  !
  NUPL      = 21
  NUSG      = 22
  NUWA      = 66
  NWALL     = 1
  !
  NLEINQ    = .FALSE.
  NLGREN    = .FALSE.
  !
  IF (NSYM .EQ. 0) THEN
     !
     NLSYM = .FALSE.
     !
  ELSE
     !
     NLSYM = .TRUE.
     !
  ENDIF
  !
  QS(1)     = Q0
  QS(2)     = FCCCC0(QPSI(1),QPSI(2),QPSI(3),QPSI(4), &
       &                      SMISO(1),SMISO(2),SMISO(3),SMISO(4),SMISOP1(2))
  QTILDA(1) = QPSI(1)
  QTILDA(2) = QPSI(2)
  !
  DO J3=3,NISO1EFF-1
     !
     Z1        = QPSI(J3-2)
     Z2        = QPSI(J3-1)
     Z3        = QPSI(J3  )
     Z4        = QPSI(J3+1)
     !
     QS(J3)     = FCCCC0(Z1,Z2,Z3,Z4,SMISO(J3-2), &
          &                       SMISO(J3-1),SMISO(J3),SMISO(J3+1),SMISOP1(J3))
     QTILDA(J3) = QPSI(J3)
     !
  END DO
  !
!!$  Z1        = QPSI(NISO1EFF-3)
!!$  Z2        = QPSI(NISO1EFF-2)
!!$  Z3        = QPSI(NISO1EFF-1)
!!$  Z4        = QPSI(NISO1EFF)
  !
  QS(NISO1EFF)  = FCCCC0(Z1,Z2,Z3,Z4,SMISO(NISO1EFF-3), &
    &                      SMISO(NISO1EFF-2),SMISO(NISO1EFF-1),SMISO(NISO1EFF),SMISOP1(NISO1EFF))
  QTILDA(NISO1EFF)  = QPSI(NISO1EFF-1)
  QS(NISO1EFF1)     = QPSI(NISO1EFF)
  QTILDA(NISO1EFF1) = QPSI(NISO1EFF)
  QSURF         = QPSI(NISO1EFF)
  TSURF         = TMF(NISO1EFF)
  !
  DO J4=3,20
     !
     NLDIAG(J4) = .FALSE.
     !
  END DO
  !
  print *,' newrun not run anymore, comment next line if needed'
  print *,' contact olivier.sauter@epfl.ch'
  ! READ(5,NEWRUN)
  !
  IF (NLGREN .OR. (REXT .LE. 1._RKIND)) NV = 0
  print *,'NV= ',NV
  IF ((.NOT. NLGREN) .AND. (NV.EQ.0) .AND. (REXT.GT.1._RKIND)) STOP 'NERAT'
  !
  OPEN(UNIT=NSAVE,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='NSAVE')
  WRITE(NSAVE,NEWRUN)
  WRITE(NSAVE,98) NISO1EFF1
  WRITE(NSAVE,99) (QS(J),J=1,NISO1EFF1)
  WRITE(NSAVE,98) NISO1EFF1
  WRITE(NSAVE,99) (QTILDA(J),J=1,NISO1EFF1)
  CLOSE(UNIT=NSAVE,STATUS='KEEP')
  !
  OPEN(UNIT=NPRNT,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='NPRNT')
  WRITE(NPRNT,*)
  WRITE(NPRNT,*)
  WRITE(NPRNT,*)
  WRITE(NPRNT,*)
  WRITE(NPRNT,NEWRUN)
  CLOSE(UNIT=NPRNT,STATUS='KEEP')
  !
  CALL RARRAY('QS',QS,NISO1EFF)
  CALL RARRAY('QTILDA',QTILDA,NISO1EFF)
  !
98 FORMAT(I5)
99 FORMAT(4E30.13)
  !
  RETURN
END SUBROUTINE NERAT
