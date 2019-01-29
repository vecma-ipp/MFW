!*DECK C1S06
!*CALL PROCESS
SUBROUTINE COTROL
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C1S06 CONTROL READ IN PARAMETERS                                    *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER          ::     ISSUM
  INTEGER          ::     LIMSUM
  INTEGER          ::     ILIMIT
  DIMENSION ILIMIT(140)
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  IF (NPROFZ .EQ. 1) THEN
     !
     PRINT *,' NPROFZ = 1 DOES NOT WORK. SPECIFYING TE AND NE'
     PRINT *,' IS PROBLEMATIC BECAUSE THE EQUILIBRIUM QUANTITY'
     PRINT *,' PSI-MIN APPEARS IN ONE OF THE TERMS OF THE'
     PRINT *,' CURRENT DENSITY'
     PRINT *,' ONE WAY THE SOLVE THAT PROBLEM WOULD BE TO '
     PRINT *,' SPECIFY TE AND P-PRIME.'
     STOP
     !
  ENDIF
  !
  IF (NRFP .EQ. 1) THEN
     !
     PRINT *,' AT THE TIME PACCAGNELA ADDED THIS OPTION, '
     PRINT *,' T**2 AND P WERE SPECIFIED AS POLYNOMIALS. '
     PRINT *,' SINCE THEN, T*TPRIME AND P-PRIME ARE GIVEN '
     PRINT *,' AS POLYNOMIALS AND THE REVERSED FIELD PINCH'
     PRINT *,' OPTION HAS NEVER BEEN CHECKED.'
     STOP
     !
  ENDIF
  !
  !OS can have analytical inputs in kepler as well: IF (NITMOPT.EQ.22 .OR. mod(NITMOPT,10).EQ.1) THEN 
  IF (mod(NITMOPT,10).EQ.1) THEN 
    ! Assume 2nd digit of NITMOPT=1 means reading equilibrium from cpo thus from profiles
    ! uses database structure as input, thus (R,Z) and arrays given
    NSURF  = 6
    if ((NPPFUN .NE. 4) .AND. (NPPFUN.NE.8)) NPPFUN = 4
    NFUNC  = 4
    NSYM = 0
  ENDIF
  !
  IF (NSURF .NE. 1) THEN
     !
     NANAL = 0
     NTEST = 0
     !
     IF (NPROFZ .EQ. 1) NSTTP = 2
     !
  ELSE
     !
     NCSCAL = 2
     NBLOPT = 0
     NFUNC  = 1
     NPPFUN = 1
     !
     IF (NANAL .EQ. 1) NTEST = 0
     !
  ENDIF
  !
  IF (NSURF .EQ. 4 .OR. NSURF .EQ. 6 .OR. NSURF .EQ. 7)  NSYM = 0
  IF (NSYM .EQ. 1)   RZ0  = 0._RKIND
  IF (NPROFZ .EQ. 1) NBLOPT = 0
  IF (NBSOPT .NE. 0) NBLOPT = 0
  IF (NBLOPT .NE. 0) NBSOPT = 0
  !
  IF (PSISCL .LE. 0._RKIND .OR. PSISCL .GT. 1._RKIND) THEN
     !
     PRINT *,' PSISCL SHOULD BE BETWEEN 0._RKIND AND 1._RKIND'
     STOP
     !
  ENDIF
  !
  IF ((NSTTP.EQ.3 .OR. NSTTP.EQ.4) .AND. CSSPEC .NE. 0._RKIND .AND. &
       &       (NCSCAL .EQ. 1 .OR. NCSCAL .EQ. 3) .AND. &
       &       (NOPT .EQ. 0 .OR. (NOPT .EQ. 1 .AND. &
       &        (NBLOPT .NE. 0 .AND. CPRESS .NE. 1._RKIND)))) THEN
     !
     CALL IVAR('NCSCAL',NCSCAL)
     CALL IVAR('NSTTP',NSTTP)
     CALL IVAR('NOPT',NOPT)
     CALL IVAR('NBLOPT',NBLOPT)
     CALL RVAR('CSSPEC',CSSPEC)
     CALL RVAR('CPRESS',CPRESS)
     !
     IF (NVERBOSE .GE. 0) THEN
       write(0,*) 'EQUILIBRIUM SCALING WITH OPTION NCSCAL=1 OR 3 '
       write(0,*) 'ONLY POSSIBLE WITH CSSPEC = 0 IF NSTTP = 3 or 4 AND'
       write(0,*) 'NOPT = 0 OR NOPT = 1 AND CPRESS .NE. 1._RKIND'
       !            STOP
     END IF
     !
  ENDIF
  !
  IF (NSOUR .LT. 1 .AND. (NFUNC .EQ. 1 .OR. NPPFUN .EQ. 1) .AND. &
       &       NSURF .NE. 1) THEN
     !
     CALL IVAR('NSOUR',NSOUR)
     CALL IVAR('NFUNC',NFUNC)
     CALL IVAR('NPPFUN',NPPFUN)
     CALL IVAR('NSURF',NSURF)
     !
     PRINT *,'NSOUR MUST BE LARGER THAN 0, OTHERWISE CURRENT=0'
     STOP
     !
  ENDIF
  !
  IF (NSOUR .GT. 10 .AND. (NFUNC .EQ. 1 .OR. NPPFUN .EQ. 1)) THEN
     !
     CALL IVAR('NSOUR',NSOUR)
     CALL IVAR('NFUNC',NFUNC)
     CALL IVAR('NPPFUN',NPPFUN)
     !
     PRINT *,'DIMENSION OF AT''S AND AP''S NOT LARGE ENOUGH'
     STOP
     !
  ENDIF
  !
  IF (NPOPULATIONS .GT. 10) THEN
     CALL IVAR('NPOPULATIONS',NPOPULATIONS)
     !
     PRINT *,'NPOPULATIONS MUST BE SMALLER THAN 11'
     STOP
     !
  ENDIF
  !
  IF (MOD(NT,2) .EQ. 1) THEN
     !
     CALL IVAR('NT',NT)
     !
     PRINT *,' NT MUST BE EVEN '
     STOP
     !
  ENDIF
  !
  IF (MOD(NCHI,2) .EQ. 1) THEN
     !
     CALL IVAR('NCHI',NCHI)
     !
     PRINT *,' NCHI MUST BE EVEN '
     STOP
     !
  ENDIF
  !
  IF (NIDEAL .EQ. 1 .AND. NDEQ .NE. 25) THEN
     CALL IVAR('NDEQ',NDEQ)
     PRINT *,'FOR ERATO, PARAMETER NDEQ MUST BE 25'
     STOP
  ENDIF
  !
  IF (NIDEAL .EQ. 2 .AND. NDEQ .NE. 29) THEN
     CALL IVAR('NDEQ',NDEQ)
     PRINT *,'FOR LION, PARAMETER NDEQ MUST BE 29'
     STOP
  ENDIF
  !
  IF ((NIDEAL .EQ. 9) .AND. ((NER.NE.2) .OR. (NEGP.NE.0))) THEN
     PRINT *,'FOR OGYROPSI, REQUIRES STRAIGHT FIELD LINE JACOBIAN WITH NER=2 AND NEGP=0'
     STOP
  ENDIF
  !
  IF ((NIDEAL .EQ. 10) .AND. ((NER.NE.0) .OR. (NEGP.NE.0))) THEN
     PRINT *, 'FOR HAMADA, JACOBIAN HAS TO BE A FLUX LABEL: NER=0 AND NEGP=0 REQUIRED'
     STOP
  ENDIF
  IF ((NIDEAL .EQ. 10) .AND. (COCOS_OUT .NE. 13)) THEN
     PRINT *, 'NIDEAL=10 has been tested with COCOS_OUT=13 equilibrium only so far, so force COCOS_OUT=13'
     COCOS_OUT = 13
  ENDIF
  !
  IF (NFUNRHO .GE. 1) THEN
    IF (NVERBOSE .GE. 1) write(*,*) 'EXP. PROFILES MAY BE NOT GIVEN IN TERMS OF RHO PSI'
    IF (((NPPFUN .NE. 4) .AND. (NPPFUN .NE. 8)) .OR. (NFUNC .NE. 4)) THEN
      PRINT *,' NFUNRHO>=1 ONLY WITH NPPFUN=4 or 8 AND NFUNC=4'
      STOP
    ENDIF
  ENDIF
  !     
  !     CHECK FLAGS IN COMDIM
  !
  IF (NIDEAL.EQ.0 .AND. MFLGMAR.NE.1) THEN
     CALL IVAR('MFLGMAR',MFLGMAR)
     PRINT *,' MFLGMAR SHOULD BE 1 IN COMDIM FOR NIDEAL=0'
     STOP 'MFLGMAR'
  ENDIF
  !
  IF ((NIDEAL.EQ.1 .OR. NIDEAL.EQ.2) .AND. MFLGERL.NE.1) THEN
     CALL IVAR('MFLGERL',MFLGERL)
     PRINT *,' MFLGERL SHOULD BE 1 IN COMDIM FOR NIDEAL=1 OR 2'
     STOP 'MFLGERL'
  ENDIF
  !
  IF (NIDEAL.EQ.3 .AND. MFLGNVW.NE.1) THEN
     CALL IVAR('MFLGNVW',MFLGNVW)
     PRINT *,' MFLGNVW SHOULD BE 1 IN COMDIM FOR NIDEAL=3'
     STOP 'MFLGNVW'
  ENDIF
  !
  !
  !**********************************************************************
  !                                                                     *
  ! 1. MAXIMUM NUMBER OF INTERVALS                                      *
  !                                                                     *
  !**********************************************************************
  !
  CALL RESETI(ILIMIT,140,0)
  !
  IF (NS     .GT. NPS)    ILIMIT(20)= 1
  IF (NT     .GT. NPT)    ILIMIT(22)= 1
  IF (NPSI   .GT. NPPSI)  ILIMIT(24)= 1
  IF (NCHI   .GT. NPCHI)  ILIMIT(26)= 1
  IF (NISO   .GT. NPISOEFF)  ILIMIT(28)= 1
  IF (NSMAX  .GT. NPSMAX) ILIMIT(30)= 1
  IF (MSMAX  .GT. MPSMAX) ILIMIT(32)= 1
  IF (NTURN  .GT. NPTURN) ILIMIT(34)= 1
  IF (NV     .GT. NPV)    ILIMIT(36)= 1
  IF (NBLC0  .GT. NPBLC0) ILIMIT(38)= 1
  IF (NPPR   .GT. NPPSI)  ILIMIT(40)= 1
  IF (NPPF   .GT. NPBPS)  ILIMIT(42)= 1
  IF (NTNOVA .GT. NPCHI)  ILIMIT(44)= 1
  !
  IF (NIDEAL .EQ. 4) THEN
     IF (NPSI*(NMGAUS+2)+1 .GT. NPISOEFF)  ILIMIT(46)= 1
  ENDIF
  !
  !**********************************************************************
  !                                                                     *
  ! 2. CHECK CONTROL VARIABLES                                          *
  !                                                                     *
  !**********************************************************************
  !
  IF (NANAL  .LT. 0 .OR.  NANAL  .GT. 1) ILIMIT(60) = 1
  IF (NBAL   .LT. 0 .OR.  NBAL   .GT. 1) ILIMIT(62) = 1
  IF (NCSCAL .LT. 1 .OR.  NCSCAL .GT. 4) ILIMIT(66) = 1
  IF (NDIFPS .LT. 0 .OR.  NDIFPS .GT. 1) ILIMIT(68) = 1
  IF (NDIFT  .LT. 0 .OR.  NDIFT  .GT. 2) ILIMIT(70) = 1
  IF (NFUNC  .LT. 1 .OR.  NFUNC  .GT. 5) ILIMIT(71) = 1
  IF (NIDEAL .LT. 0 .OR.  NIDEAL .GT. 10) ILIMIT(72) = 1
  IF (NIPR   .LT. 1 .OR.  NIPR   .GT. 4) ILIMIT(74) = 1
  IF (NOPT   .LT.-2 .OR.  NOPT   .GT. 1) ILIMIT(76) = 1
  IF (NPLOT  .LT. 0 .OR.  NPLOT  .GT. 1) ILIMIT(78) = 1
  IF (NBLOPT .LT. 0 .OR.  NBLOPT .GT. 3) ILIMIT(80) = 1
  IF (NPP    .LT. 1 .OR.  NPP    .GT. 3) ILIMIT(82) = 1
  IF (NPPFUN .LT. 1 .OR.  NPPFUN .GT. 8) ILIMIT(83) = 1
  IF (NPROFZ .LT. 0 .OR.  NPROFZ .GT. 1) ILIMIT(84) = 1
  IF (NPRPSI .LT. 0 .OR.  NPRPSI .GT. 1) ILIMIT(86) = 1
  IF (NRSCAL .LT. 0 .OR.  NRSCAL .GT. 1) ILIMIT(88) = 1
  IF (NSTTP  .LT. 1 .OR.  NSTTP  .GT. 4) ILIMIT(90) = 1
  IF (NSURF  .LT. 1 .OR.  NSURF  .GT. 7) ILIMIT(92) = 1
  IF (NSYM   .LT. 0 .OR.  NSYM   .GT. 1) ILIMIT(94) = 1
  IF (NTCASE .LT. 0 .OR.  NTCASE .GT. 4) ILIMIT(96) = 1
  IF (NTEST  .LT. 0 .OR.  NTEST  .GT. 1) ILIMIT(98) = 1
  IF (NTMF0  .LT. 0 .OR.  NTMF0  .GT. 1) ILIMIT(100)= 1
  IF (NPROPT .LT. -4 .OR.  NPROPT .GT. 4) ILIMIT(102)= 1
  IF (NBSOPT .LT. 0 .OR.  NBSOPT .GT. 2) ILIMIT(104)= 1
  IF (NBSTRP .LT. 1 .OR.  NBSTRP .GT. 2) ILIMIT(106)= 1
  IF (NBSFUN .LT. 1 .OR.  NBSFUN .GT. 3) ILIMIT(108)= 1
  !
  IF ((NIDEAL .EQ. 1 .OR. NIDEAL .EQ. 2) .AND. &
       &       NRSCAL .NE. 1) ILIMIT(120)= 1
  IF ((NIDEAL .EQ. 1 .OR. NIDEAL .EQ. 2 .OR. NIDEAL .EQ. 5) .AND. &
       &       NTMF0  .NE. 1) ILIMIT(122)= 1
  IF ((NIDEAL .EQ. 5 .OR. NIDEAL .EQ. 8) .AND. NRSCAL .NE. 0) ILIMIT(124)= 1
  IF (NIDEAL .EQ. 8 .AND. NTMF0 .NE. 0) ILIMIT(125)= 1
  IF (NIDEAL .EQ. 8 .AND. (NER .NE. 1 .OR. NEGP .ne. -1)) ILIMIT(126)= 1
  !
  !***********************************************************************
  !                                                                      *
  ! PRINTS ERROR MESSAGES (IF ANY)                                       *
  !                                                                      *
  !***********************************************************************
  !
  LIMSUM = ISSUM(140,ILIMIT,1)
  !
  IF (LIMSUM .GT. 0) THEN
    !
    IF (NVERBOSE .GE. 0) THEN
      IF (NVERBOSE .GE. 1) CALL OUTPUT(1)
      !
      IF (ILIMIT(10).EQ.1) WRITE(*,110) 
      !                                                                       
      IF (ILIMIT(20).EQ.1) WRITE(*,120) 'NS   ','NPS   ','NPS   ' &
        &                                         ,NS,     NPS
      IF (ILIMIT(22).EQ.1) WRITE(*,120) 'NT   ','NPT   ','NPT   ' &
        &                                         ,NT,     NPT
      IF (ILIMIT(24).EQ.1) WRITE(*,120) 'NPSI ','NPPSI ','NPPSI ' &
        &                                         ,NPSI,   NPPSI
      IF (ILIMIT(26).EQ.1) WRITE(*,120) 'NCHI ','NPCHI ','NPCHI ' &
        &                                         ,NCHI,   NPCHI
      IF (ILIMIT(28).EQ.1) WRITE(*,128) 'NISO ','NPISOEFF','NPPSI ' &
        &                                         ,NISO,   NPISOEFF
      IF (ILIMIT(30).EQ.1) WRITE(*,120) 'NSMAX','NPSMAX','NPSMAX' &
        &                                         ,NSMAX,  NPSMAX
      IF (ILIMIT(32).EQ.1) WRITE(*,120) 'MSMAX','MPSMAX','MPSMAX' &
        &                                         ,MSMAX,  MPSMAX
      IF (ILIMIT(34).EQ.1) WRITE(*,120) 'NTURN','NPTURN','NPTURN' &
        &                                         ,NTURN,  NPTURN
      IF (ILIMIT(36).EQ.1) WRITE(*,120) 'NV   ','NPV   ','NPV   ' &
        &                                         ,NV,     NPV
      IF (ILIMIT(38).EQ.1) WRITE(*,120) 'NBLC0','NPBLC0','NPBLC0' &
        &                                         ,NBLC0,  NPBLC0
      IF (ILIMIT(40).EQ.1) WRITE(*,121) 'NPPR','NPPSI','NPPR' &
        &                                         ,NPPSI
      IF (ILIMIT(40).EQ.1) WRITE(*,121) 'NPPV','NPPSI','NPPV' &
        &                                         ,NPPSI
      IF (ILIMIT(44).EQ.1) WRITE(*,121) 'NTNOVA','NPCHI','NTNOVA' &
        &                                         ,NPCHI
      IF (ILIMIT(46).EQ.1) WRITE(*,120) 'NPSI*(NMGAUS+2)+1', &
        &        'NPISOEFF','NPISOEFF',NPSI*(NMGAUS+2)+1,NPISOEFF
      !
      IF (ILIMIT(60).EQ.1)  WRITE(*,160) 'NANAL '
      IF (ILIMIT(62).EQ.1)  WRITE(*,160) 'NBAL  '
      IF (ILIMIT(66).EQ.1)  WRITE(*,174) 'NCSCAL'
      IF (ILIMIT(68).EQ.1)  WRITE(*,160) 'NDIFPS'
      IF (ILIMIT(70).EQ.1)  WRITE(*,170) 'NDIFT '
      IF (ILIMIT(71).EQ.1)  WRITE(*,181) 'NFUNC '
      IF (ILIMIT(72).EQ.1)  WRITE(*,192) 'NIDEAL'
      IF (ILIMIT(74).EQ.1)  WRITE(*,174) 'NIPR  '
      IF (ILIMIT(76).EQ.1)  WRITE(*,158) 'NOPT  '
      IF (ILIMIT(78).EQ.1)  WRITE(*,160) 'NPLOT '
      IF (ILIMIT(80).EQ.1)  WRITE(*,180) 'NBLOPT '
      IF (ILIMIT(82).EQ.1)  WRITE(*,166) 'NPP   '
      IF (ILIMIT(83).EQ.1)  WRITE(*,193) 'NPPFUN'
      IF (ILIMIT(84).EQ.1)  WRITE(*,160) 'NPROFZ'
      IF (ILIMIT(86).EQ.1)  WRITE(*,160) 'NPRPSI'
      IF (ILIMIT(88).EQ.1)  WRITE(*,160) 'NRSCAL'
      IF (ILIMIT(90).EQ.1)  WRITE(*,166) 'NSTTP '
      IF (ILIMIT(92).EQ.1)  WRITE(*,193) 'NSURF '
      IF (ILIMIT(94).EQ.1)  WRITE(*,160) 'NSYM  '
      IF (ILIMIT(96).EQ.1)  WRITE(*,174) 'NTCASE'
      IF (ILIMIT(98).EQ.1)  WRITE(*,160) 'NTEST '
      IF (ILIMIT(100).EQ.1) WRITE(*,160) 'NTMF0 '
      IF (ILIMIT(102).EQ.1) WRITE(*,166) 'NPROPT'
      IF (ILIMIT(104).EQ.1) WRITE(*,170) 'NBSOPT'
      IF (ILIMIT(106).EQ.1) WRITE(*,162) 'NBSTRP'
      IF (ILIMIT(108).EQ.1) WRITE(*,166) 'NBSFUN'
      !
      IF (ILIMIT(120).EQ.1) WRITE(*,220) 'NRSCAL'
      IF (ILIMIT(122).EQ.1) WRITE(*,220) 'NTFM0 '
      IF (ILIMIT(124).EQ.1) WRITE(*,222) 'NRSCAL'
      IF (ILIMIT(125).EQ.1) WRITE(*,222) 'NTFM0'
      IF (ILIMIT(126).EQ.1) WRITE(*,223) 'NER', 'NEGP'
      !
    END IF
    STOP
    !
  ENDIF
  !
  RETURN
  !
110 FORMAT('NSOUR MUST BE LARGER THAN 1, OTHERWISE CURRENT=0')
120 FORMAT(A,' .GT. ',A,'        RECOMPILE WITH LARGER ',A,' ',2I5)
121 FORMAT(A,' .GT. ',A,'        RUN AGAIN WITH SMALLER ',A,' ',I5)
128 FORMAT(A,' .GT. ',A,'NPPSI+1 RECOMPILE WITH LARGER ',A,' ',2I5)
158 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE -2, -1, 0 OR 1')
160 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 0 OR 1')
162 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 1 OR 2')
166 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 1,2 OR 3')
170 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 0,1 OR 2')
174 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 1,2,3 OR 4')
180 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 0,1,2 OR 3')
181 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 1,2,3,4 OR 5')
192 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 1,2,3,4,5,6,7,8,9 OR 10')
193 FORMAT('WRONG VALUE FOR ',A,' IT HAS TO BE 1,2,3,4,5,6 OR 7')
220 FORMAT('FOR STABILITY CODE ERATO, ',A,' MUST BE 1')
222 FORMAT('FOR STABILITY CODE XTOR, ',A,' MUST BE 0')
223 FORMAT(/,'FOR ELITE CODE,  ',A,' MUST BE 1 and ',A,' must be -1',/)
  !
END SUBROUTINE COTROL
