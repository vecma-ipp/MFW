!*DECK C3SB02
!*CALL PROCESS
SUBROUTINE WRTPLOT
  !        ##################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !**********************************************************************
  !                                                                     *
  ! C3SB02  WRITE EQUILIBRIUM PLOT QUANTITIES INTO FILE NUPLO           *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  INCLUDE 'COMDAT.inc'
  !
  INTEGER          ::     J301
  INTEGER          ::     I
  INTEGER          ::     J
  INTEGER          ::     J297
  INTEGER          ::     INTEXT
  INTEGER          ::     INR
  INTEGER          ::     INS
  INTEGER          ::     INSUR
  REAL(RKIND)      ::     ZABISG
  INTEGER          ::     J296
  REAL(RKIND)      ::     ZCHI
  REAL(RKIND)      ::     ZABIC
  INTEGER          ::     J295
  INTEGER          ::     J294
  REAL(RKIND)      ::     ZABIPR
  REAL(RKIND)      ::     ZCSIPR
  REAL(RKIND)      ::     ZZCURV
  REAL(RKIND)      ::     ZRCURV
  INTEGER          ::     J293
  INTEGER          ::     IBALL
  INTEGER          ::     IMERCR
  INTEGER          ::     IMERCI
  INTEGER          ::     J289
  INTEGER          ::     INBCHI
  REAL(RKIND)      ::     ZZCHI
  REAL(RKIND)      ::     ZRCHI
  INTEGER          ::     J287
  INTEGER          ::     JNB
  INTEGER          ::     J288
  INTEGER          ::     JJ
  INTEGER          ::     JSCHI
  REAL(RKIND)      ::     ZOARS
  REAL(RKIND)      ::     ZOSHS
  REAL(RKIND)      ::     ZOBETS
  REAL(RKIND)      ::     ZABS
  INTEGER          ::     J285
  REAL(RKIND)      ::     ZOJBSS
  REAL(RKIND)      ::     ZOJPS
  REAL(RKIND)      ::     ZOIPS
  REAL(RKIND)      ::     ZOTS
  REAL(RKIND)      ::     ZOTTS
  REAL(RKIND)      ::     ZOPS
  REAL(RKIND)      ::     ZOPPS
  REAL(RKIND)      ::     ZODRS
  REAL(RKIND)      ::     ZODIS
  REAL(RKIND)      ::     ZOHS
  REAL(RKIND)      ::     ZOTRS
  REAL(RKIND)      ::     ZOJBS
  REAL(RKIND)      ::     ZODQS
  REAL(RKIND)      ::     ZOQS
  REAL(RKIND)      ::     ZABIS
  REAL(RKIND)      ::     ZABSM
  REAL(RKIND)      ::     ZOTRR
  REAL(RKIND)      ::     ZOJBSR
  REAL(RKIND)      ::     ZOJPR
  REAL(RKIND)      ::     ZOBETR
  REAL(RKIND)      ::     ZOJR
  REAL(RKIND)      ::     ZOFR
  REAL(RKIND)      ::     ZOSHR
  REAL(RKIND)      ::     ZODQR
  REAL(RKIND)      ::     ZOJBR
  REAL(RKIND)      ::     ZOIPR
  REAL(RKIND)      ::     ZOTR
  REAL(RKIND)      ::     ZOTTR
  REAL(RKIND)      ::     ZOPR
  REAL(RKIND)      ::     ZOPPR
  REAL(RKIND)      ::     ZOQR
  REAL(RKIND)      ::     ZABR
  INTEGER          ::     I2
  INTEGER          ::     I1
  INTEGER          ::     J284
  REAL(RKIND)      ::     ZTET1
  REAL(RKIND)      ::     ZSIG1
  REAL(RKIND)      ::     ZPAR
  REAL(RKIND)      ::     ZZSUR
  INTEGER          ::     J283
  REAL(RKIND)      ::     ZBND2
  REAL(RKIND)      ::     ZBND1
  REAL(RKIND)      ::     ZRSUR
  REAL(RKIND)      ::     ZTSUR
  INTEGER          ::     J282
  REAL(RKIND)      ::     ZDT
  REAL(RKIND)      ::     ZZMIN
  REAL(RKIND)      ::     ZZMAX
  INTEGER          ::     ISMIN
  REAL(RKIND)      ::     ZRMIN
  INTEGER          ::     ISMAX
  REAL(RKIND)      ::     ZRMAX
  REAL(RKIND)      ::     ZTET
  REAL(RKIND)      ::     ZZTET
  REAL(RKIND)      ::     ZRTET
  REAL(RKIND)      ::     ZZ
  REAL(RKIND)      ::     ZR
  INTEGER          ::     J281
  REAL(RKIND)      ::     ZRHOS
  REAL(RKIND)      ::     ZABIT
  REAL(RKIND)      ::     ZOART
  REAL(RKIND)      ::     ZBBS
  REAL(RKIND)      ::     ZCBS2
  REAL(RKIND)      ::     ZCBS1
  REAL(RKIND)      ::     ZCBS
  REAL(RKIND)      ::     ZBSFC
  REAL(RKIND)      ::     ZBSF
  REAL(RKIND)      ::     ZLI1
  REAL(RKIND)      ::     ZBPOL1
  REAL(RKIND)      ::     ZGMX
  REAL(RKIND)      ::     ZGMSTA
  REAL(RKIND)      ::     ZGM
  REAL(RKIND)      ::     ZIBSNO
  REAL(RKIND)      ::     ZINORM
  REAL(RKIND)      ::     ZBXPER
  REAL(RKIND)      ::     ZBSPER
  REAL(RKIND)      ::     ZBPERC
  REAL(RKIND)      ::     ZMU0
  CHARACTER*180 TEXT(1:130)
  CHARACTER*8   CDATE,   CCLOK
  CHARACTER*24  CDATESUN
  !
  DIMENSION &
       &   IBALL(NPISOEFF),     IMERCI(NPISOEFF),    IMERCR(NPISOEFF)
  DIMENSION &
       &   ZABIC(NPCHI1),     ZABIPR(NPISOEFF),   ZABIS(NPISOEFF), &
       &   ZABISG(NSP1),      ZABIT(NTP1),       ZABR (2*NPISOEFF+1), &
       &   ZABS (NPISOEFF),     ZABSM(NPISOEFF),   ZCHI(NPCHI1), &
       &   ZCSIPR(NPISOEFF), &
       &   ZOARS(NPISOEFF),     ZOART(NTP1),       ZOBETS(NPISOEFF), &
       &   ZOSHR(2*NPISOEFF+1), ZOSHS(NPISOEFF),   ZOJBR(2*NPISOEFF+1), &
       &   ZOJBS(NPISOEFF),   ZOJPR(2*NPISOEFF+1), ZOJPS(NPISOEFF), &
       &   ZOJBSR(2*NPISOEFF+1,4),ZOJBSS(NPISOEFF,4),  ZODIS(NPISOEFF), &
       &   ZODRS(NPISOEFF),   ZOBETR(2*NPISOEFF+1),ZOTRR(2*NPISOEFF+1), &
       &   ZOTRS(NPISOEFF),   ZODQR(2*NPISOEFF+1), &
       &   ZOFR (2*NPISOEFF+1), ZODQS(NPISOEFF),   ZOHS (NPISOEFF), &
       &   ZOIPR(2*NPISOEFF+1), ZOIPS(NPISOEFF),   ZOJR (2*NPISOEFF+1), &
       &   ZOPPR(2*NPISOEFF+1), ZOPPS(NPISOEFF),   ZOPR (2*NPISOEFF+1), &
       &   ZOPS (NPISOEFF),   ZOQR (2*NPISOEFF+1), ZOQS (NPISOEFF), &
       &   ZOTR(2*NPISOEFF+1),  ZOTS(NPISOEFF),    ZOTTR(2*NPISOEFF+1), &
       &   ZOTTS(NPISOEFF),   ZPAR(2*NPISOEFF+1), &
       &   ZR(12*NPT+1),      ZRCHI(NPCHI1,NPISOEFF),  ZRCURV(4*2*NPISOEFF), &
       &   ZRHOS(NTP1), &
       &   ZRSUR(6*NPT),      ZRTET(NPT),        ZSIG1(NPISOEFF), &
       &   ZTET(NTP1),        ZTET1(NPISOEFF),     ZTSUR(6*NPT), &
       &   ZZ(12*NPT+1),      ZZCHI(NPCHI1,NPISOEFF),  ZZCURV(4*2*NPISOEFF), &
       &   ZZTET(NPT), &
       &   ZZSUR(6*NPT)
!cgv
        integer                  :: jp
        real(rkind)              :: zcrv, zczv
!cgv

  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !  WRITE QUANTITIES FOR PLOTS
  !
  OPEN(UNIT=NUPLO,ACCESS='SEQUENTIAL',FORM='FORMATTED',FILE='NUPLO')
  !
  ZMU0 = 1.256_RKIND
  ZBPERC = 100._RKIND * BETA
  ZBSPER = 100._RKIND * BETAS
  ZBXPER = 100._RKIND * BETAX
  ZINORM = RINOR/ZMU0
  ZIBSNO = RIBSNOR/ZMU0
  ZGM    = ZBPERC/ZINORM
  ZGMSTA = ZBSPER/ZINORM
  ZGMX   = ZBXPER/ZINORM
  ZBPOL1 = BETAP*CONVF
  ZLI1   = eqchease_out(1)%profiles_1d%li(NISO1EFF1)*CONVF
  ZBSF   = RITBS/RITOT
  ZBSFC  = RITBSC/RITOT
  ZCBS   = 0._RKIND
  IF (BETA.GT.RC0P) ZCBS1 = ZIBSNO*ZINORM/(ZBXPER*SQRT(ASPCT))
  IF (BETA.GT.RC0P) ZCBS2 = ZBSF/(SQRT(ASPCT)*ZBPOL1)
  ZBBS  = ZBXPER * ZBSF
  !
  TEXT(1) = 'CHEASE - V12.95'
  !
  WRITE (TEXT(2),1111) '        '
  WRITE (TEXT(3),1111) '        '
  !
  CALL DATE_AND_TIME(CDATE)
  WRITE (TEXT(2),1111) CDATE
  WRITE (TEXT(3),1111) '        '
  !
  CALL WHTEXT(LABEL1,TEXT(4))
  CALL WHTEXT(LABEL2,TEXT(5))
  CALL WHTEXT(LABEL3,TEXT(6))
  CALL WHTEXT(LABEL4,TEXT(7))
  !
  CALL WHTEXT('PLASMA SURFACE :',TEXT(8))
  CALL WITEXT(NSURF, 'NSURF', TEXT(9),1)
  CALL WRTEXT(ASPCT, 'ASPCT', TEXT(10),1,1)
  CALL WRTEXT(ELONG, 'ELONG', TEXT(11),1,1)
  CALL WRTEXT(TRIANG,'TRIANG',TEXT(12),1,1)
  CALL WRTEXT(DELTA, 'DELTA', TEXT(13),1,1)
  CALL WRTEXT(THETA0,'THETA0',TEXT(14),1,1)
  CALL WRTEXT(BEANS, 'BEANS', TEXT(15),1,1)
  CALL WRTEXT(CETA,  'CETA',  TEXT(16),1,1)
  CALL WRTEXT(SGMA,  'SGMA',  TEXT(17),1,1)
  CALL WRTEXT(TRIPLT,'TRIPLT',TEXT(18),1,1)
  CALL WRTEXT(RNU,   'RNU',   TEXT(19),1,1)
  CALL WRTEXT(XI,    'XI',    TEXT(20),1,1)
  CALL WRTEXT(AREA,  'AREA',  TEXT(21),1,1)
  CALL WHTEXT('EQUILIBRIUM SOLUTION :',TEXT(22))
  CALL WRTEXT(RMAG,       'RMAG',  TEXT(23),1,1)
  CALL WRTEXT(RZMAG,      'ZMAG',  TEXT(24),1,1)
  CALL WRTEXT(PSI0,       'PSIMIN',TEXT(25),1,1)
  CALL WRTEXT(PSISCL,     'PSISCL',TEXT(26),1,1)
  CALL WRTEXT(Q0,         'Q0',    TEXT(27),1,1)
  CALL WRTEXT(QPSI(NISO1EFF),'QSURF', TEXT(28),1,1)
  CALL WRTEXT(QCYL,       'QCYL',  TEXT(29),1,1)
  CALL WRTEXT(T0,         'T0',    TEXT(30),1,1)
  CALL WRTEXT(TMF(NISO1EFF), 'TSURF', TEXT(31),1,1)
  CALL WRTEXT(RITOT, 'TOT. CUR.',             TEXT(32),1,1)
  CALL WRTEXT(RINOR, 'NORM. CUR.',            TEXT(33),1,1)
  CALL WRTEXT(ZINORM,'IN (MA,T,M)',           TEXT(34),1,1)
  CALL WRTEXT(ZBSF,  'I-B.S.(0)/I-TOT',       TEXT(35),1,1)
  CALL WRTEXT(ZBSFC, 'I-B.S.(NUE*)/I-TOT',    TEXT(36),1,1)
  CALL WRTEXT(ZBSFC, 'I-B.S.(NUE*,ne''/ne)/I-TOT',TEXT(121),1,1)
  CALL WRTEXT(RZION, 'ION CHARGE',            TEXT(37),1,1)
  CALL WRTEXT(ETAEI, 'D(LOG(T))/D(LOG(N))',   TEXT(38),1,1)
  CALL WRTEXT(ZCBS1, 'CBS1=IB.S./(G*SQRT(E))',TEXT(39),1,1)
  CALL WRTEXT(ZCBS2, 'CBS2=F/(BP(1)*SQR(E))', TEXT(40),1,1)
  CALL WRTEXT(ZBBS,  'BBS',                   TEXT(41),1,1)
  CALL WRTEXT(CONVF, 'CONV. FACT.',           TEXT(42),1,1)
  CALL WRTEXT(eqchease_out(1)%profiles_1d%li(NISO1EFF1),'LI',              TEXT(43),1,1)
  CALL WRTEXT(ZLI1,         'LI (G.A.)',       TEXT(44),1,1)
  CALL WRTEXT(ZBPERC,       'BETA',            TEXT(45),2,1)
  CALL WRTEXT(ZBSPER,       'BETA*',           TEXT(46),2,1)
  CALL WRTEXT(ZBXPER,       'BETA EXP.',       TEXT(47),2,1)
  CALL WRTEXT(ZGM,          'G (MA,T,M).',     TEXT(48),1,1)
  CALL WRTEXT(ZGMSTA,       'G* (MA,T,M).',    TEXT(49),1,1)
  CALL WRTEXT(ZGMX,         'G EXP. (MA,T,M).',TEXT(50),1,1)
  CALL WRTEXT(BETAP,        'BETA POL.',       TEXT(51),1,1)
  CALL WRTEXT(ZBPOL1,       'BETA POL. (G.A.)',TEXT(52),1,1)
  !
  CALL WHTEXT('PROFILES :',TEXT(53))
  CALL WITEXT(NFUNC, 'NFUNC', TEXT(54),1)
  CALL WITEXT(NSTTP, 'NSTTP', TEXT(55),1)
  CALL WITEXT(NIPR,  'NIPR',  TEXT(56),1)
  CALL WITEXT(NSOUR, 'NSOUR', TEXT(57),1)
  CALL WITEXT(NPPFUN,'NPPFUN',TEXT(58),1)
  CALL WITEXT(NPP,   'NPP',   TEXT(59),1)
  CALL WRTEXT(PREDGE,'PREDGE',TEXT(60),1,1)
  CALL WITEXT(NBSOPT,'NBSOPT',TEXT(61),1)
  CALL WITEXT(NBSFUN,'NBSFUN',TEXT(62),1)
  CALL WITEXT(NBSTRP,'NBSTRP',TEXT(63),1)
  CALL WITEXT(NBLOPT,'NBLOPT',TEXT(64),1)
  CALL WITEXT(NBLC0, 'NBLC0', TEXT(65),2)
  CALL WITEXT(NTURN, 'NTURN', TEXT(66),2)
  CALL WITEXT(NPPR  ,'NPPR'  ,TEXT(67),2)
  CALL WRTEXT(CFBAL, 'CFBAL', TEXT(68),1,1)
  CALL WRTEXT(CPRESS,'CPRESS',TEXT(69),1,1)
  CALL WRTEXT(BSFRAC,'BSFRAC',TEXT(70),1,1)
  CALL WRTEXT(AT     ,'AT',   TEXT(71),3,13)
  CALL WRTEXT(AT2    ,'AT2',  TEXT(72),3,13)
  CALL WRTEXT(AT3    ,'AT3',  TEXT(73),3,13)
  CALL WRTEXT(AT4    ,'AT4',  TEXT(74),3,13)
  CALL WRTEXT(AP     ,'AP',   TEXT(75),3,13)
  CALL WRTEXT(AP2    ,'AP2',  TEXT(76),3,13)
  CALL WRTEXT(AFBS   ,'AFBS', TEXT(77),3,13)
  CALL WRTEXT(AFBS2  ,'AFBS2',TEXT(78),3,13)
  CALL WHTEXT('MESHES :',TEXT(79))
  CALL WRTEXT(R0,    'R0',    TEXT(80),1,1)
  CALL WRTEXT(RZ0,   'Z0',    TEXT(81),1,1)
  CALL WRTEXT(EPSLON,'EPSLON',TEXT(82),1,1)
  CALL WITEXT(NS,    'NS',    TEXT(83),2)
  CALL WITEXT(NT,    'NT',    TEXT(84),2)
  CALL WITEXT(NISO,  'NISO',  TEXT(85),2)
  CALL WITEXT(NTNOVA,'NTNOVA',TEXT(86),2)
  CALL WITEXT(NISO1EFF-1,  'NPSI',  TEXT(87),2)
  CALL WITEXT(NCHI,  'NCHI',  TEXT(88),2)
  CALL WITEXT(NER,   'NER',   TEXT(89),2)
  CALL WITEXT(NEGP,  'NEGP',  TEXT(90),2)
  CALL WHTEXT('S-PACKING :',TEXT(91))
  CALL WITEXT(NMESHA,'NMESHA',TEXT(92),1)
  CALL WRTEXT(SOLPDA,'SOLPDA',TEXT(93),1,1)
  CALL WITEXT(NPOIDA,'NPOIDA',TEXT(94),3)
  CALL WITEXT(NDIFPS,'NDIFPS',TEXT(95),1)
  CALL WHTEXT('I*-PACKING :',TEXT(96))
  CALL WITEXT(NMESHB,'NMESHB',TEXT(97),1)
  CALL WRTEXT(SOLPDB,'SOLPDB',TEXT(98),1,1)
  CALL WITEXT(NPOIDB,'NPOIDB',TEXT(99),3)
  CALL WHTEXT('SIGMA-PACKING :',TEXT(100))
  CALL WITEXT(NMESHC,'NMESHC',TEXT(101),1)
  CALL WRTEXT(SOLPDC,'SOLPDC',TEXT(102),1,1)
  CALL WITEXT(NPOIDC,'NPOIDC',TEXT(103),3)
  CALL WHTEXT('THETA-PACKING :',TEXT(104))
  CALL WITEXT(NMESHD,'NMESHD',TEXT(105),1)
  CALL WRTEXT(SOLPDD,'SOLPDD',TEXT(106),1,1)
  CALL WITEXT(NPOIDD,'NPOIDD',TEXT(107),3)
  CALL WITEXT(NDIFT, 'NDIFT', TEXT(108),1)
  CALL WHTEXT('CHI-PACKING :',TEXT(109))
  CALL WITEXT(NMESHE,'NMESHE',TEXT(110),1)
  CALL WRTEXT(SOLPDE,'SOLPDE',TEXT(111),1,1)
  CALL WITEXT(NPOIDE,'NPOIDE',TEXT(112),3)
  CALL WHTEXT('NORMALIZATION :',TEXT(113))
  CALL WITEXT(NCSCAL,'NCSCAL',TEXT(114),1)
  CALL WITEXT(NTMF0, 'NTMF0', TEXT(115),1)
  CALL WITEXT(NRSCAL,'NRSCAL',TEXT(116),1)
  CALL WRTEXT(SCALE, 'SCALE', TEXT(117),1,1)
  CALL WRTEXT(CSSPEC,'CSSPEC',TEXT(118),1,1)
  CALL WRTEXT(QSPEC, 'QSPEC', TEXT(119),1,1)
  CALL WRTEXT(CURRT, 'CURRT', TEXT(120),1,1)
  !
  !  COMPUTE MAXIMUM AND MINIMUM OF R AND Z
  !  REFERENCE IS THE MAGNETIC AXIS
  !
  ZOART(1) = 0._RKIND
  ZABIT(1) = 0._RKIND
  !
  CALL BOUND(NT1,CT,ZRHOS)
  !
  DO J281=1,NT
     !
     ZR(J281)      = ZRHOS(J281) * COS(CT(J281))
     ZZ(J281)      = ZRHOS(J281) * SIN(CT(J281))
     ZRTET(J281)   = ZR(J281)
     ZZTET(J281)   = ZZ(J281)
     ZTET(J281)    = CT(J281) - CT(1)
     ZOART(J281+1) = ZOART(J281) + .25_RKIND * (CT(J281+1) - CT(J281)) * &
          &                   (ZRHOS(J281)**2 + ZRHOS(J281+1)**2)
     ZABIT(J281+1) = REAL(J281,RKIND) / REAL(NT,RKIND)
     !
  END DO
  !
  ZTET(NT1) = 2._RKIND * CPI
  !
  ZRMAX = ZR(ISMAX(NT,ZR,1))
  ZRMIN = ZR(ISMIN(NT,ZR,1))
  ZZMAX = ZZ(ISMAX(NT,ZZ,1))
  ZZMIN = ZZ(ISMIN(NT,ZZ,1))
  !
  !  COMPUTE THE SURFACE
  !
  BPS( 1) = RMAG
  BPS(12) = RZMAG
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  ZDT = 2._RKIND * CPI / (6 * NT - 1)
  !
  DO J282=1,6*NT
     !
     ZTSUR(J282) = (J282 - 1) * ZDT
     !
  END DO
  !
  CALL BOUND(6*NT,ZTSUR,ZRSUR)
  CALL BOUND(1,PANGLE    ,ZBND1)
  CALL BOUND(1,PANGLE+CPI,ZBND2)
  !
  BPS( 1) = R0
  BPS(12) = RZ0
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  DO J283=1,6*NT
     !
     ZZSUR(J283) = ZRSUR(J283) * SIN(ZTSUR(J283))
     ZRSUR(J283) = ZRSUR(J283) * COS(ZTSUR(J283))
     !
  END DO
!cgv
!cgv..write plasma boundary coordinates
!cgv
         open(81,file='plasma_wall_boundary.data',form='formatted')
         rewind(81)
         write(81,*) 'cgv---- plasma and wall boundary------------------'
         write(81,*) 'r0,rz0,r0exp,r0w,rz0w'
         write(81,*) r0,rz0,r0exp,r0w,rz0w
         write(81,*) 'plasma mesh center: r0exp*r0,r0exp*rz0'
         write(81,*) r0exp*r0,r0exp*rz0
         write(81,*) 'wall center: r0exp*r0w,r0exp*rz0w'
         write(81,*) r0exp*r0w,r0exp*rz0w
         write(81,*) 'R_plasma Z_plasma R_wall Z_wall'
         do j=1,6*nt
           write(81,990) r0exp*(r0+zrsur(j)),r0exp*(rz0+zzsur(j)), &
                         r0exp*(r0w+rext*(r0+zrsur(j)-r0w)), &
                         r0exp*(rz0w+rext*(rz0+zzsur(j)-rz0w))
  990      format(1x,1p4e13.5)
         enddo
         close(81)
!cgv
  !
  !  COMPUTE THE VALUES OF R ON RADIUS USED FOR PROFILE DEFINITION
  !
  CALL RMRAD(NISO1EFF,RC0P,CPSRF,PANGLE,ZPAR(1),ZSIG1,ZTET1,1)
  CALL RMRAD(NISO1EFF,RC0P,CPSRF,PANGLE+CPI,ZPAR(NISO1EFF+1),ZSIG1, &
       &              ZTET1,1)
  !
  DO J284=1,NISO1EFF
     !
     I1 = NISO1EFF + J284 + 1
     I2 = NISO1EFF - J284 + 1
     !
     ZABR(I1)  =   ZPAR(J284)       * ZBND1
     ZABR(I2)  = - ZPAR(NISO1EFF+J284) * ZBND2
     ZOQR(I2)  = QPSI(J284)
     ZOQR(I1)  = QPSI(J284)
     ZOPPR(I2) = CPPR(J284)
     ZOPPR(I1) = CPPR(J284)
     ZOPR(I2)  = CPR(J284)
     ZOPR(I1)  = CPR(J284)
     ZOTTR(I2) = TTP(J284)
     ZOTTR(I1) = TTP(J284)
     ZOTR(I2)  = TMF(J284)
     ZOTR(I1)  = TMF(J284)
     ZOIPR(I2) = RIPR(J284)
     ZOIPR(I1) = RIPR(J284)
     ZOJBR(I2) = RJDOTB(J284)
     ZOJBR(I1) = RJDOTB(J284)
     ZODQR(I2) = CDQ(J284)
     ZODQR(I1) = CDQ(J284)
     ZOSHR(I2) = CDRQ(J284)
     ZOSHR(I1) = CDRQ(J284)
     ZOFR(I2)  = SMISO(J284)**2 * CPSRF
     ZOFR(I1)  = SMISO(J284)**2 * CPSRF
     ZOJR(I1)  = - (ZABR(I1) + RMAG) * ZOPPR(I1) - &
          &                  ZOTTR(I1) / (ZABR(I1) + RMAG)
     ZOJR(I2)  = - (ZABR(I2) + RMAG) * ZOPPR(I2) - &
          &                  ZOTTR(I2) / (ZABR(I2) + RMAG)
     ZOBETR(I2) = BETAB(J284)
     ZOBETR(I1) = BETAB(J284)
     ZOJPR(I2)  = RJPAR(J284)
     ZOJPR(I1)  = RJPAR(J284)
     ZOJBSR(I2,1:4) = RJBSOS(J284,1:4)
     ZOJBSR(I1,1:4) = RJBSOS(J284,1:4)
     ZOTRR(I2)  = 1._RKIND - RFCIRC(J284)
     ZOTRR(I1)  = 1._RKIND - RFCIRC(J284)
     !
  END DO
  !
  ZABR(NISO1EFF+1)  = 0._RKIND
  ZOQR(NISO1EFF+1)  = Q0
  ZOJBR(NISO1EFF+1) = RJDTB0
  ZOPPR(NISO1EFF+1) = DPDP0
  ZOPR(NISO1EFF+1)  = CP0
  ZOTTR(NISO1EFF+1) = DTTP0
  ZOTR(NISO1EFF+1)  = T0
  ZOIPR(NISO1EFF+1) = RIPR0
  ZOFR(NISO1EFF+1)   = 0._RKIND
  ZOJR(NISO1EFF+1)   = - (RMAG * DPDP0 + DTTP0 / RMAG)
  ZODQR(NISO1EFF+1)  = DQDP0
  ZOSHR(NISO1EFF+1)  = 0._RKIND
  ZOBETR(NISO1EFF+1) = BETAB(1)
  ZOTRR(NISO1EFF+1)  = 0._RKIND
  ZOJPR(NISO1EFF+1)  = RJPAR(1)
  ZOJBSR(NISO1EFF+1,1:4) = 0._RKIND
  !
  !  SET S-VALUES IN ZABS
  !
  ZABSM(1) = 0._RKIND
  ZABIS(1) = 0._RKIND
  ZOQS(1)  = Q0
  ZODQS(1) = DQDP0
  ZOJBS(1) = RJDTB0
  ZOTRS(1) = 0._RKIND
  ZOHS(1)  = FCCCC0(HMERCR(1),HMERCR(2),HMERCR(3),HMERCR(4), &
       &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ZODIS(1) = FCCCC0(SMERCI(1),SMERCI(2),SMERCI(3),SMERCI(4), &
       &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ZODRS(1) = FCCCC0(SMERCR(1),SMERCR(2),SMERCR(3),SMERCR(4), &
       &                     SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ZOPPS(1) = DPDP0
  ZOPS(1)  = CP0
  ZOTTS(1) = DTTP0
  ZOTS(1)  = T0
  ZOIPS(1) = RIPR0
  ZOIPS(1) = RIPR0
  ZOJPS(1) = RJPAR(1)
  ZOJBSS(1,:) = 0._RKIND
  !
  DO J285=1,NISO1EFF
     !
     ZABSM(J285+1) = SMISO(J285)
     ZABS(J285)    = SMISO(J285)
     ZABIS(J285+1) = (J285 - .5_RKIND) / REAL(NISO1EFF-1,RKIND)
     ZOHS(J285+1)  = HMERCR(J285)
     ZODIS(J285+1) = SMERCI(J285)
     ZODRS(J285+1) = SMERCR(J285)
     ZOBETS(J285)  = BETAB(J285)
     ZOQS(J285+1)  = QPSI(J285)
     ZODQS(J285+1) = CDQ(J285)
     ZOSHS(J285)   = CDRQ(J285)
     ZOPPS(J285+1) = CPPR(J285)
     ZOPS(J285+1)  = CPR(J285)
     ZOTTS(J285+1) = TTP(J285)
     ZOTS(J285+1)  = TMF(J285)
     ZOIPS(J285+1) = RIPR(J285)
     ZOJBS(J285+1) = RJDOTB(J285)
     ZOARS(J285)   = eqchease_out(1)%profiles_1d%rho_vol(J285)
     ZOJPS(J285+1) = RJPAR(J285)
     ZOJBSS(J285+1,1:4)= RJBSOS(J285,1:4)
     ZOTRS(J285+1) = 1._RKIND - RFCIRC(J285)
     !
  END DO
  !
  ZABIS(NISO1EFF+1) = 1._RKIND
  !
  !  CONSTANT CHI LINES
  !
  !%OS         JSCHI = NCHI / 25 + 1
  JSCHI = 1
  do jj=1,nchi
     write(14,'(a,i5,a,i5,a)') ' jchi = ',jj,' ipsi=1,',niso1eff,':'
     write(14,'(1p2e13.5)') (cr(jj,j288),cz(jj,j288),j288=1,niso1eff)
  end do

!cgv
if (nideal.eq.0) then

! if nideal.eq.0, i.e., mapping for MARS, prepare also mappa_rz files
! i.e., R,Z(s,chi) files for plasma
!
! Note: now all quantities are already computed on both integer
!       and half-integer meshes, thus there is no need for interpolation
! Note: odd elements of cr, cz correspond to cs mesh, even ones to csm!

      open(114,file='mappa_rz_chicsm_mars',form='formatted')
      write(114,'(a)') 'map of R,Z(chi,csm)'
      write(114,'(a,i5)') 'npsi1=',npsi1
      do jp=1,npsi1
        write(114,'(1pe24.16)') csm(jp)
      enddo
      write(114,'(a,i5)') 'nchi=',nchi
      do jj=1,nchi
        write(114,'(1pe24.16)') chim(jj)
      enddo
      write(114,'(a)') 'cr(jchi,jp), cz(jchi,jp)'
      do jp=2,niso1eff1,2
        write(114,'(a,i5)') 'jp=',jp
        do j288=1,nchi
          write(114,'(1p2e24.16)') cr(j288,jp),cz(j288,jp)
        enddo
      enddo
! add last point s=1
      jp=niso1eff1
      write(114,'(a,i5)') 'jp=',jp
      do j288=1,nchi
        write(114,'(1p2e24.16)') cr(j288,jp),cz(j288,jp)
      enddo
      write(114,'(a,1pe24.16,a,1pe24.16)') 'r0exp=',r0exp,' b0exp=',b0exp
      close(114)

      open(115,file='mappa_rz_chics_mars',form='formatted')
      write(115,'(a)') 'map of R,Z(chi,cs)'
      write(115,'(a,i5)') 'npsi1=',npsi1
      do jp=1,npsi1
        write(115,'(1pe24.16)') cs(jp)
      enddo
      write(115,'(a,i5)') 'nchi=',nchi
      do jj=1,nchi
        write(115,'(1pe24.16)') chim(jj)
      enddo
      write(115,'(a)') 'cr(jchi,jp), cz(jchi,jp)'
      do jp=1,niso1eff1,2
        write(115,'(a,i5)') 'jp=',jp
        do j288=1,nchi
          write(115,'(1p2e24.16)') cr(j288,jp),cz(j288,jp)
        enddo
      enddo
      write(115,'(a,1pe24.16,a,1pe24.16)') 'r0exp=',r0exp,' b0exp=',b0exp
      close(115)

! now vacuum mappa files if vacuum mesh is computed,
! i.e., R_vac,Z_vac(s,chi) files for vacuum

      if (rext.gt.1._rkind) then
        open(214,file='mappa_rz_chicsmv_mars',form='formatted')
        write(214,'(a)') 'map of R,Z(chi,vcsm)'
        write(214,'(a,i5)') 'nv1=',nv1
        do jp=1,nv
          write(214,'(1pe24.16)') csmv(jp)
        enddo
!.....add by hand last point on half-integer mesh equal to csv(nv1)
        write(214,'(1pe24.16)') csv(nv1)
        write(214,'(a,i5)') 'nchi=',nchi
        do jj=1,nchi
          write(214,'(1pe24.16)') chim(jj)
        enddo
        write(214,'(a)') 'crv(jchi,jp), czv(jchi,jp)'
        do jp=1,nv
          write(214,'(a,i5)') 'jp=',jp
          do j288=1,nchi
            zcrv= r0w+csmv(jp)*(cr(j288,niso1eff1)-r0w)
            zczv=rz0w+csmv(jp)*(cz(j288,niso1eff1)-rz0w)
            write(214,'(1p2e24.16)') zcrv, zczv
          enddo
        enddo
! add last point s=1
        jp=nv1
        write(214,'(a,i5)') 'jp=',jp
        do j288=1,nchi
          zcrv= r0w+csv(jp)*(cr(j288,niso1eff1)-r0w)
          zczv=rz0w+csv(jp)*(cz(j288,niso1eff1)-rz0w)
          write(214,'(1p2e24.16)') zcrv, zczv
        enddo
        write(214,'(a,1pe24.16,a,1pe24.16)') 'r0exp=',r0exp,' b0exp=',b0exp
        close(214)
!
        open(215,file='mappa_rz_chicsv_mars',form='formatted')
        write(215,'(a)') 'map of R,Z(chi,vcs)'
        write(215,'(a,i5)') 'nv1=',nv1
        do jp=1,nv1
          write(215,'(1pe24.16)') csv(jp)
        enddo
        write(215,'(a,i5)') 'nchi=',nchi
        do jj=1,nchi
          write(215,'(1pe24.16)') chim(jj)
        enddo
        write(215,'(a)') 'crv(jchi,jp), czv(jchi,jp)'
        do jp=1,nv1
          write(215,'(a,i5)') 'jp=',jp
          do j288=1,nchi
            zcrv= r0w+csv(jp)*(cr(j288,niso1eff1)-r0w)
            zczv=rz0w+csv(jp)*(cz(j288,niso1eff1)-rz0w)
            write(215,'(1p2e24.16)') zcrv, zczv
          enddo
        enddo
        write(215,'(a,1pe24.16,a,1pe24.16)') 'r0exp=',r0exp,' b0exp=',b0exp
        close(215)
      endif
endif
!cgv

  !
  DO J288=1,NISO1EFF
     !
     JNB = 0
     !
     DO J287=1,NCHI,JSCHI
        !
        JNB             = JNB + 1
        ZRCHI(JNB,J288) = CR(J287,J288) - R0
        ZZCHI(JNB,J288) = CZ(J287,J288) - RZ0
        !
     END DO
  END DO
  !
  INBCHI = JNB
  !
  !  BALLOONING AND MERCIER
  !
  DO J289=1,NISO1EFF
     !
     IMERCI(J289) = 0
     IMERCR(J289) = 0
     IBALL(J289) = 0
     !
     IF (SMERCI(J289) .LT. RC0P) IMERCI(J289) = 1
     IF (SMERCR(J289) .LT. RC0P) IMERCR(J289) = 1
     IF (NCBAL(J289) .NE. 0)  IBALL(J289) = 1
     !
  END DO
  !
  DO J293=1,NCURV
     !
     ZRCURV(J293) = RRCURV(J293) - R0
     ZZCURV(J293) = RZCURV(J293) - RZ0
     !
  END DO
  !
  ZCSIPR(1)      = 0._RKIND
  ZABIPR(NISO+1) = 1._RKIND
  !
  DO J294=1,NISO
     !
     ZABIPR(J294)   = REAL(J294-1,RKIND) / REAL(NISO,RKIND)
     ZCSIPR(J294+1) = CSIPR(J294)
     !
  END DO
  !
  DO J295=1,NCHI1
     !
     ZABIC(J295) = REAL(J295-1,RKIND) / REAL(NCHI,RKIND)
     ZCHI(J295)  = CHI(J295) - CHI(1)
     !
  END DO
  !
  DO J296=1,NS1
     !
     ZABISG(J296) = REAL(J296-1,RKIND) / REAL(NS,RKIND)
     !
  END DO
  !
  INSUR  = 6 * NT
  INS    = NISO1EFF + 1
  INR    = 2 * NISO1EFF + 1
  INTEXT = 121
  !
  !  WRITE DATA NECESSARY FOR PLOT ON THE FILE NUPLO
  !
  !
  WRITE(NUPLO,1003) INSUR,NCHI,NCHI1,NISO1EFF-1,NISO1EFF,NS,NS1,NT,NT1, &
       &                     INS,INR,INBCHI,INTEXT,NCURV,NMESHA,NMESHB, &
       &                     NMESHC,NMESHD,NMESHE,NPOIDA,NPOIDB, &
       &                     NPOIDC,NPOIDD,NPOIDE,NISO,NMGAUS
  DO J297=1,INTEXT
     !
     WRITE(NUPLO,1005) TEXT(J297)
     !
  END DO
  !
  WRITE(NUPLO,1004)(IBALL(J),J=1,NISO1EFF)
  WRITE(NUPLO,1004)(IMERCI(J),J=1,NISO1EFF)
  WRITE(NUPLO,1004)(IMERCR(J),J=1,NISO1EFF)
  !
  WRITE(NUPLO,1006) SOLPDA,SOLPDB,SOLPDC,SOLPDD,SOLPDE, &
       &                     ZRMAX,ZRMIN,ZZMAX,ZZMIN,PANGLE
  WRITE(NUPLO,1006)(APLACE(J),J=1,10)
  WRITE(NUPLO,1006)(AWIDTH(J),J=1,10)
  WRITE(NUPLO,1006)(BPLACE(J),J=1,10)
  WRITE(NUPLO,1006)(BWIDTH(J),J=1,10)
  WRITE(NUPLO,1006)(CPLACE(J),J=1,10)
  WRITE(NUPLO,1006)(CWIDTH(J),J=1,10)
  WRITE(NUPLO,1006)(DPLACE(J),J=1,10)
  WRITE(NUPLO,1006)(DWIDTH(J),J=1,10)
  WRITE(NUPLO,1006)(EPLACE(J),J=1,10)
  WRITE(NUPLO,1006)(EWIDTH(J),J=1,10)
  WRITE(NUPLO,1006) (ZTET(J),J=1,NT1)
  WRITE(NUPLO,1006) (CSIG(J),J=1,NS1)
  WRITE(NUPLO,1006) (SMISOP1(J),J=1,NISO1EFF+1)
  WRITE(NUPLO,1006) (ZCHI(J),J=1,NCHI1)
  WRITE(NUPLO,1006) (ZCSIPR(J),J=1,NISO+1)
  WRITE(NUPLO,1006)(ZRTET(J),J=1,NT)
  WRITE(NUPLO,1006)(ZZTET(J),J=1,NT)
  WRITE(NUPLO,1006)(ZRSUR(J),J=1,INSUR)
  WRITE(NUPLO,1006)(ZTSUR(J),J=1,INSUR)
  WRITE(NUPLO,1006)(ZZSUR(J),J=1,INSUR)
  WRITE(NUPLO,1006)(ZABIS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZABIT(J),J=1,NT1)
  WRITE(NUPLO,1006)(ZABIC(J),J=1,NCHI1)
  WRITE(NUPLO,1006)(ZOART(J),J=1,NT1)
  WRITE(NUPLO,1006)(ZABIPR(J),J=1,NISO+1)
  WRITE(NUPLO,1006)(ZABISG(J),J=1,NS1)
  WRITE(NUPLO,1006)(ZABSM(J),J=1,INS)
  WRITE(NUPLO,1006)(ZABR (J),J=1,INR)
  !%OS
  WRITE(21,*) INS,' = INS; SMISO(J),J=1,INS MESH, TO HAVE R(S)', &
       &     ' CORRESPONDANCE'
  WRITE(21,1006) (ZABSM (J),J=1,INS)
  WRITE(21,*) INR,' = INR; R(J),J=1,INR MESH OF SMISO AT Z=ZMAG'
  WRITE(21,1006) (ZABR (J),J=1,INR)
  !%OS
  WRITE(NUPLO,1006)(ZOQS (J),J=1,INS)
  WRITE(NUPLO,1006)(ZOQR (J),J=1,INR)
  WRITE(NUPLO,1006)(ZODQS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZODQR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOSHS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOSHR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOJBS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOJBR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOJBSS(J,1),J=1,INS)
  WRITE(NUPLO,1006)(ZOJBSS(J,2),J=1,INS)
  WRITE(NUPLO,1006)(ZOJBSS(J,3),J=1,INS)
  WRITE(NUPLO,1006)(ZOJBSS(J,4),J=1,INS)
  !%OS
!!$         DO J=1,NISO1EFF
!!$           ZOJBSS(J+1,2) = 0.0_RKIND
!!$         ENDDO
!!$! should suppress line below which contained dummy RJBSH in pchease
!!$         WRITE(NUPLO,1006)(ZOJBSS(J,2),J=1,INS)
  !%OS
  WRITE(NUPLO,1006)(ZOJBSR(J,1),J=1,INR)
  WRITE(NUPLO,1006)(ZOJBSR(J,2),J=1,INR)
  WRITE(NUPLO,1006)(ZOJBSR(J,3),J=1,INR)
  WRITE(NUPLO,1006)(ZOJBSR(J,4),J=1,INR)
  WRITE(NUPLO,1006)(ZOJPS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOJPR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOTRS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOTRR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOHS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZODIS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZODRS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOPPS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOPPR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOPS (J),J=1,INS)
  WRITE(NUPLO,1006)(ZOPR (J),J=1,INR)
  WRITE(NUPLO,1006)(ZOTTS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOTTR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOTS(J) ,J=1,INS)
  WRITE(NUPLO,1006)(ZOTR (J),J=1,INR)
  WRITE(NUPLO,1006)(ZOIPS(J),J=1,INS)
  WRITE(NUPLO,1006)(ZOIPR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOBETR(J),J=1,INR)
  WRITE(NUPLO,1006)(ZOBETS(J),J=1,NISO1EFF)
  WRITE(NUPLO,1006)(ZOFR (J),J=1,INR)
  WRITE(NUPLO,1006)(ZOARS(J),J=1,NISO1EFF)
  WRITE(NUPLO,1006)(ZOJR (J),J=1,INR)
  WRITE(NUPLO,1006)(ZABS(J),J=1,NISO1EFF)
  !
  ! substract mesh center coordinates to (R,Z) coordinates for plotting
  !
  RRISO(:,1:NISO1EFF)=RRISO(:,1:NISO1EFF) - R0
  RZISO(:,1:NISO1EFF)=RZISO(:,1:NISO1EFF) - RZ0
  CR(:,:)=CR(:,:) - R0
  CZ(:,:)=CZ(:,:) - RZ0
  !
  DO J301=1,NISO1EFF
     !
     WRITE(NUPLO,1006)(RRISO(I,J301),I=1,NMGAUS*NT1)
     WRITE(NUPLO,1006)(RZISO(I,J301),I=1,NMGAUS*NT1)
     !
  END DO
  !
  WRITE(NUPLO,1006)(ZRCURV(J),J=1,NCURV)
  WRITE(NUPLO,1006)(ZZCURV(J),J=1,NCURV)
  WRITE(NUPLO,1006)((ZRCHI(J,I),J=1,INBCHI),I=1,NISO1EFF)
  WRITE(NUPLO,1006)((ZZCHI(J,I),J=1,INBCHI),I=1,NISO1EFF)
  WRITE(NUPLO,1006)((RSHEAR(J,I),J=1,NCHI),I=1,NISO1EFF)
  WRITE(NUPLO,1006)((CR(J,I),J=1,NCHI),I=1,NISO1EFF)
  WRITE(NUPLO,1006)((CZ(J,I),J=1,NCHI),I=1,NISO1EFF)
  !
  CLOSE(UNIT=NUPLO,STATUS='KEEP')
  !
  ! Restore (R,Z) coodinates
  !
  RRISO(:,1:NISO1EFF)=RRISO(:,1:NISO1EFF) + R0
  RZISO(:,1:NISO1EFF)=RZISO(:,1:NISO1EFF) + RZ0
  CR(:,:)=CR(:,:) + R0
  CZ(:,:)=CZ(:,:) + RZ0
  !
  RETURN
  !
1003 FORMAT(1X,13I8)
1004 FORMAT((1X,40(I3)))
1005 FORMAT(A132)
1006 FORMAT((1X,8(1PE15.6)))
1007 FORMAT(13I8)
1111 FORMAT(A)
  !        
END SUBROUTINE WRTPLOT
