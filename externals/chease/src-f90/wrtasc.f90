SUBROUTINE WRTASC
  !#####################
  !
  !                                        AUTHORS:
  !                                        H. Lutjens, 
  !                                        P. Hennequin
  !                                        R. Arslanbekov 10/99
  !                      from WRTPLOT in chease.f
  !**********************************************************************
  !                                                                     *
  ! C3SB02  WRITE DATA NECESSARY FOR PLOT 
  !                            ON THE FILE chease.asc (ascii format)   *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  IMPLICIT NONE
  INCLUDE 'COMDAT.inc'
  !
  integer               ::      mp
  integer               ::      nnbbpp
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
  !
  REAL(RKIND)      ::     trriso
  REAL(RKIND)      ::     trziso
  REAL(RKIND)      ::     tcr
  REAL(RKIND)      ::     tcz
  REAL(RKIND)      ::     trshear
  !
  DIMENSION &
       IBALL(NPPSI1),     IMERCI(NPPSI1),    IMERCR(NPPSI1)
  DIMENSION &
       ZABIC(NPCHI1),     ZABIPR(NPISO+1),   ZABIS(NPPSI1+1), &
       ZABISG(NSP1),      ZABIT(NTP1),       ZABR (2*NPPSI1+1), &
       ZABS (NPPSI1),     ZABSM(NPPSI1+1),   ZCHI(NPCHI1), &
       ZCSIPR(NPISO+1), &
       ZOARS(NPPSI1),     ZOART(NTP1),       ZOBETS(NPPSI1), &
       ZOSHR(2*NPPSI1+1), ZOSHS(NPPSI1+1),   ZOJBR(2*NPPSI1+1), &
       ZOJBS(NPPSI1+1),   ZOJPR(2*NPPSI1+1), ZOJPS(NPPSI1+1), &
       ZOJBSR(2*NPPSI1+1,3),ZOJBSS(NPPSI1+1,3),  ZODIS(NPPSI1+1), &
       ZODRS(NPPSI1+1),   ZOBETR(2*NPPSI1+1),ZOTRR(2*NPPSI1+1), &
       ZOTRS(NPPSI1+1),   ZODQR(2*NPPSI1+1), &
       ZOFR (2*NPPSI1+1), ZODQS(NPPSI1),     ZOHS (NPPSI1+1), &
       ZOIPR(2*NPPSI1+1), ZOIPS(NPPSI1+1),   ZOJR (2*NPPSI1+1), & 
       ZOPPR(2*NPPSI1+1), ZOPPS(NPPSI1+1),   ZOPR (2*NPPSI1+1), &
       ZOPS (NPPSI1+1),   ZOQR (2*NPPSI1+1), ZOQS (NPPSI1+1), &
       ZOTR(2*NPPSI1+1),  ZOTS(NPPSI1+1),    ZOTTR(2*NPPSI1+1), & 
       ZOTTS(NPPSI1+1),   ZPAR(2*NPPSI1), &
       ZR(12*NPT+1),      ZRCHI(NPCHI,NPPSI1),  ZRCURV(4*NPPSI1), &
       ZRHOS(NTP1), &
       ZRSUR(6*NPT),      ZRTET(NPT),        ZSIG1(NPPSI1), &
       ZTET(NTP1),        ZTET1(NPPSI1),     ZTSUR(6*NPT), &
       ZZ(12*NPT+1),      ZZCHI(NPCHI,NPPSI1),  ZZCURV(4*NPPSI1), &
       ZZTET(NPT),  &
       ZZSUR(6*NPT) 
  !

  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !  28. WRITE QUANTITIES FOR PLOTS in binary format (IEEE)
  !
280 CONTINUE
  !
  !
  !     Open bin and dat-files for writing
  !
  write(6,*) 'Creating asc and dat files chease.* ...'
  mp = 101
  open(mp,  file ='chease.asc',  FORM = 'formatted')
  open(mp+1, file ='chease.dat', FORM = 'formatted')
  !      
  !  --------------------------------------------------------------------- 
  ZMU0 = 1.256_rkind
  ZBPERC = 100._rkind * BETA
  ZBSPER = 100._rkind * BETAS
  ZBXPER = 100._rkind * BETAX
  ZINORM = RINOR/ZMU0
  ZIBSNO = RIBSNOR/ZMU0
  ZGM    = ZBPERC/ZINORM
  ZGMSTA = ZBSPER/ZINORM
  ZGMX   = ZBXPER/ZINORM
  ZBPOL1 = BETAP*CONVF
  ZLI1   = RINDUC(NISO1EFF)*CONVF
  ZBSF   = RITBS/RITOT
  ZBSFC  = RITBSC/RITOT
  ZCBS   = 0._rkind
  IF (BETA.GT.RC0P) ZCBS1 = ZIBSNO*ZINORM/(ZBXPER*SQRT(ASPCT))
  IF (BETA.GT.RC0P) ZCBS2 = ZBSF/(SQRT(ASPCT)*ZBPOL1)
  ZBBS  = ZBXPER * ZBSF
  !  --------------------------------------------------------------------- 
  !   variables de caracterisation du run (pour ecriture texte)
  !  --------------------------------------------------------------------- 
  !          'PLASMA SURFACE :'
  !
  call ascwrtI(mp,'NSURF',1,1,NSURF)
  print*,'NSURF=', NSURF
  call ascwrtM(mp,'ASPCT',1,1,ASPCT)
  call ascwrtM(mp,'ELONG',1,1,ELONG)
  call ascwrtM(mp,'TRIANG',1,1,TRIANG)
  call ascwrtM(mp,'DELTA',1,1,DELTA)
  call ascwrtM(mp,'THETA0',1,1,THETA0)
  call ascwrtM(mp,'BEANS',1,1,BEANS)
  call ascwrtM(mp,'CETA',1,1,CETA)
  call ascwrtM(mp,'SGMA',1,1,SGMA)
  call ascwrtM(mp,'TRIPLT',1,1,TRIPLT)
  call ascwrtM(mp,'RNU',1,1,RNU)
  call ascwrtM(mp,'XI',1,1,XI)
  call ascwrtM(mp,'AREA',1,1,AREA)
  !          'EQUILIBRIUM SOLUTION :'
  call ascwrtM(mp,'RMAG',1,1,RMAG)
  call ascwrtM(mp,'RZMAG',1,1,RZMAG)
  call ascwrtM(mp,'PSIMIN',1,1,PSI0)
  call ascwrtM(mp,'PSISCL',1,1,PSISCL)
  call ascwrtM(mp,'Q0',1,1,Q0)
  call ascwrtM(mp,'QSURF',1,1,QPSI(NISO1EFF))
  call ascwrtM(mp,'QCYL',1,1,QCYL)
  call ascwrtM(mp,'T0',1,1,T0)
  call ascwrtM(mp,'TSURF',1,1,TMF(NISO1EFF))
  call ascwrtM(mp,'RITOT',1,1,RITOT)
  call ascwrtM(mp,'RINOR',1,1,RINOR)
  call ascwrtM(mp,'ZINORM',1,1,ZINORM)
  call ascwrtM(mp,'ZBSF',1,1,ZBSF)
  call ascwrtM(mp,'ZBSFC',1,1,ZBSFC)
  call ascwrtM(mp,'RZION',1,1,RZION)
  call ascwrtM(mp,'D(LOG(T))/D(LOG(N)',1,1,ETAEI)
  call ascwrtM(mp,'CBS1=IB.S./(G*SQRT(E)',1,1,ZCBS1)
  call ascwrtM(mp,'CBS2=F/(BP(1)*SQR(E)',1,1,ZCBS2)
  call ascwrtM(mp,'BBS',1,1,ZBBS)
  call ascwrtM(mp,'CONV. FACT',1,1,CONVF)
  call ascwrtM(mp,'LI',1,1,RINDUC(NISO1EFF))
  call ascwrtM(mp,'LI (G.A.)',1,1,ZLI1)
  call ascwrtM(mp,'BETA',1,1,ZBPERC)
  call ascwrtM(mp,'BETA*',1,1,ZBSPER)
  call ascwrtM(mp,'BETA EXP.',1,1,ZBXPER)
  call ascwrtM(mp,'G (MA,T,M).',1,1,ZGM)
  call ascwrtM(mp,'G* (MA,T,M).',1,1,ZGMSTA)
  call ascwrtM(mp,'G EXP. (MA,T,M).',1,1,ZGMX)
  call ascwrtM(mp,'BETA POL.',1,1,BETAP)
  call ascwrtM(mp,'BETA POL. (G.A.)',1,1,ZBPOL1)
  !            'PROFILES :'
  call ascwrtI(mp,'NFUNC',1,1,NFUNC)
  call ascwrtI(mp,'NSTTP',1,1,NSTTP)
  call ascwrtI(mp,'NIPR',1,1,NIPR)
  call ascwrtI(mp,'NSOUR',1,1,NSOUR)
  call ascwrtI(mp,'NPPFUN',1,1,NPPFUN)
  call ascwrtI(mp,'NPP',1,1,NPP)
  call ascwrtM(mp,'PREDGE',1,1,PREDGE)
  call ascwrtI(mp,'NBSOPT',1,1,NBSOPT)
  call ascwrtI(mp,'NBSFUN',1,1,NBSFUN)
  call ascwrtI(mp,'NBSTRP',1,1,NBSTRP)
  call ascwrtI(mp,'NBLOPT',1,1,NBLOPT)
  call ascwrtI(mp,'NBLC0',1,1,NBLC0)
  call ascwrtI(mp,'NTURN',1,1,NTURN)
  call ascwrtI(mp,'NPPR',1,1,NPPR)
  call ascwrtM(mp,'CFBAL',1,1,CFBAL)
  call ascwrtM(mp,'CPRESS',1,1,CPRESS)
  call ascwrtM(mp,'BSFRAC',1,1,BSFRAC)
  call ascwrtM(mp,'AT',15,1,AT)
  call ascwrtM(mp,'AT2',15,1,AT2)
  call ascwrtM(mp,'AT3',15,1,AT3)
  call ascwrtM(mp,'AT4',15,1,AT4)
  call ascwrtM(mp,'AP',15,1,AP)
  call ascwrtM(mp,'AP2',15,1,AP2)
  call ascwrtM(mp,'AFBS',15,1,AFBS)
  call ascwrtM(mp,'AFBS2',15,1,AFBS2)
  !                'MESHES :'
  call ascwrtM(mp,'R0',1,1,R0)
  call ascwrtM(mp,'RZ0',1,1,RZ0)
  call ascwrtM(mp,'EPSLON',1,1,EPSLON)
  call ascwrtI(mp,'NS',1,1,NS)
  call ascwrtI(mp,'NT',1,1,NT)
  call ascwrtI(mp,'NISO',1,1,NISO)
  call ascwrtI(mp,'NTNOVA',1,1,NTNOVA)
  call ascwrtI(mp,'NPSI',1,1,NPSI)
  call ascwrtI(mp,'NCHI',1,1,NCHI)
  call ascwrtI(mp,'NER',1,1,NER)
  call ascwrtI(mp,'NEGP',1,1,NEGP)
  !                   'S-PACKING :'
  call ascwrtI(mp,'NMESHA',1,1,NMESHA)
  call ascwrtM(mp,'SOLPDA',1,1,SOLPDA)
  call ascwrtI(mp,'NPOIDA',1,1,NPOIDA)
  call ascwrtI(mp,'NPOIDQ',1,1,NPOIDQ)
  call ascwrtI(mp,'NDIFPS',1,1,NDIFPS)
  !              'I*-PACKING :'
  call ascwrtI(mp,'NMESHB',1,1,NMESHB)
  call ascwrtM(mp,'SOLPDB',1,1,SOLPDB)
  call ascwrtI(mp,'NPOIDB',1,1,NPOIDB)
  !                 'SIGMA-PACKING :'
  call ascwrtI(mp,'NMESHB',1,1,NMESHB)
  call ascwrtI(mp,'SOLPDB',1,1,SOLPDB)
  call ascwrtI(mp,'NPOIDB',1,1,NPOIDB)
  !                'THETA-PACKING :'
  call ascwrtI(mp,'NMESHC',1,1,NMESHC)
  call ascwrtM(mp,'SOLPDC',1,1,SOLPDC)
  call ascwrtI(mp,'NPOIDC',1,1,NPOIDC)
  !                 'THETA-PACKING :'
  call ascwrtI(mp,'NMESHD',1,1,NMESHD)
  call ascwrtM(mp,'SOLPDD',1,1,SOLPDD)
  call ascwrtI(mp,'NPOIDD',1,1,NPOIDD)
  call ascwrtI(mp,'NDIFT',1,1,NDIFT)
  !                 'CHI-PACKING :'
  call ascwrtI(mp,'NMESHE',1,1,NMESHE)
  call ascwrtM(mp,'SOLPDE',1,1,SOLPDE)
  call ascwrtI(mp,'NPOIDE',1,1,NPOIDE)
  !                    'NORMALIZATION :'
  call ascwrtI(mp,'NCSCAL',1,1,NCSCAL)
  call ascwrtI(mp,'NTMF0',1,1,NTMF0)
  call ascwrtI(mp,'NRSCAL',1,1,NRSCAL)
  call ascwrtM(mp,'SCALE',1,1,SCALE)
  call ascwrtM(mp,'CSSPEC',1,1,CSSPEC)
  call ascwrtM(mp,'QSPEC',1,1,QSPEC)
  call ascwrtM(mp,'CURRT',1,1,CURRT)
  !
  !  COMPUTE MAXIMUM AND MINIMUM OF R AND Z
  !  REFERENCE IS THE MAGNETIC AXIS
  !
  ZOART(1) = 0._rkind
  ZABIT(1) = 0._rkind
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
     ZOART(J281+1) = ZOART(J281) + .25_rkind * (CT(J281+1) - CT(J281)) * &
          (ZRHOS(J281)**2 + ZRHOS(J281+1)**2)
     ZABIT(J281+1) = REAL(J281,rkind) / real(NT,rkind)
     !
  END DO
  !
  call ascwrtM(mp,'ZOART',NT1,1,ZOART)
  call ascwrtM(mp,'ZABIT',NT1,1,ZABIS)
  call ascwrtM(mp,'ZRTET',NT,1,ZRTET)
  call ascwrtM(mp,'ZZTET',NT,1,ZZTET)
  !
  ZTET(NT1) = 2._rkind * CPI
  call ascwrtM(mp,'ZTET',NT1,1,ZTET)
  !
  ZRMAX = ZR(ISMAX(NT,ZR,1))
  ZRMIN = ZR(ISMIN(NT,ZR,1))
  ZZMAX = ZZ(ISMAX(NT,ZZ,1))
  ZZMIN = ZZ(ISMIN(NT,ZZ,1))
  !
  call ascwrtM(mp,'ZRMAX',1,1,ZRMAX)
  call ascwrtM(mp,'ZRMIN',1,1,ZRMIN)
  call ascwrtM(mp,'ZZMAX',1,1,ZZMAX)
  call ascwrtM(mp,'ZZMIN',1,1,ZZMIN)
  !
  !  COMPUTE THE SURFACE
  !
  BPS( 1) = RMAG
  BPS(12) = RZMAG
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  ZDT = 2._rkind * CPI / real(6 * NT - 1,rkind)
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
  !
  INSUR  = 6 * NT
  call ascwrtI(mp,'INSUR',1,1,INSUR)
  !                     Z-VECTOR FOR PLASMA SURFACE
  call ascwrtM(mp,'ZRSUR',INSUR,1,ZRSUR)
  call ascwrtM(mp,'ZTSUR',INSUR,1,ZTSUR)
  call ascwrtM(mp,'ZZSUR',INSUR,1,ZZSUR)
  !
  !  COMPUTE THE VALUES OF R ON RADIUS USED FOR PROFILE DEFINITION
  !
  CALL RMRAD(NISO1EFF,RC0P,CPSRF,PANGLE,ZPAR(1),ZSIG1,ZTET1,1)
  CALL RMRAD(NISO1EFF,RC0P,CPSRF,PANGLE+CPI,ZPAR(NISO1EFF+1),ZSIG1,ZTET1,1)
  !
  call ascwrtM(mp,'PANGLE',1,1,PANGLE)
  !
  INR    = 2 * NISO1EFF + 1
  call ascwrtI(mp,'INR',1,1,INR)
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
          ZOTTR(I1) / (ZABR(I1) + RMAG)
     ZOJR(I2)  = - (ZABR(I2) + RMAG) * ZOPPR(I2) - &
          ZOTTR(I2) / (ZABR(I2) + RMAG)
     ZOBETR(I2) = BETAB(J284)
     ZOBETR(I1) = BETAB(J284)
     ZOJPR(I2)  = RJPAR(J284)
     ZOJPR(I1)  = RJPAR(J284)
     ZOJBSR(I2,1:3) = RJBSOS(J284,1:3)
     ZOJBSR(I1,1:3) = RJBSOS(J284,1:3)
     ZOTRR(I2)  = 1. - RFCIRC(J284)
     ZOTRR(I1)  = 1. - RFCIRC(J284)
     !
  END DO
  !
  ZABR(NISO1EFF+1)  = 0._rkind
  ZOQR(NISO1EFF+1)  = Q0
  ZOJBR(NISO1EFF+1) = RJDTB0
  ZOPPR(NISO1EFF+1) = DPDP0
  ZOPR(NISO1EFF+1)  = CP0
  ZOTTR(NISO1EFF+1) = DTTP0
  ZOTR(NISO1EFF+1)  = T0
  ZOIPR(NISO1EFF+1) = RIPR0
  ZOFR(NISO1EFF+1)   = 0._rkind
  ZOJR(NISO1EFF+1)   = - (RMAG * DPDP0 + DTTP0 / RMAG)
  ZODQR(NISO1EFF+1)  = DQDP0
  ZOSHR(NISO1EFF+1)  = 0._rkind
  ZOBETR(NISO1EFF+1) = BETAB(1)
  ZOTRR(NISO1EFF+1)  = 0._rkind
  ZOJPR(NISO1EFF+1)  = RJPAR(1)
  ZOJBSR(NISO1EFF+1,1) = 0._rkind
  ZOJBSR(NISO1EFF+1,2) = 0._rkind
  !  R-VECTOR FOR PROFILES (LENGTH = INR)
  call ascwrtM(mp,'ZABR',INR,1,ZABR)
  !  Q(R)-PROFILE
  call ascwrtM(mp,'ZOQR',INR,1,ZOQR)
  !  Q'(R)-PROFILE
  call ascwrtM(mp,'ZODQR',INR,1,ZODQR)
  !  SHEAR S(R)-PROFILE
  call ascwrtM(mp,'ZOSHR',INR,1,ZOSHR)

  call ascwrtM(mp,'ZOJBR',INR,1,ZOJBR)
  !  BOOTSTRAP CURRENT <JBOOT.B> R-PROFILE
  call ascwrtM(mp,'ZOJBSR',INR,3,ZOJBSR)
  !  TOTAL CURRENT <JTOT.B> R-PROFILE
  call ascwrtM(mp,'ZOJPR',INR,1,ZOJPR)
  !  TRAPPED FRACTION R-PROFILE
  call ascwrtM(mp,'ZOTRR',INR,1,ZOTRR)
  !  P'(R)-PROFILE
  call ascwrtM(mp,'ZOPPR',INR,1,ZOPPR)
  !  P(R)-PROFILE
  call ascwrtM(mp,'ZOPR',INR,1,ZOPR)
  !  TT'(R)-PROFILE
  call ascwrtM(mp,'ZOTTR',INR,1,ZOTTR)
  !  T(R)-PROFILE
  call ascwrtM(mp,'ZOTR',INR,1,ZOTR)
  !  I*(R)-PROFILE
  call ascwrtM(mp,'ZOIPR',INR,1,ZOIPR)
  !  BETA POLOIDAL R-PROFILE
  call ascwrtM(mp,'ZOBETR',INR,1,ZOBETR)
  !  POLOIDAL FLUX R-PROFILE
  call ascwrtM(mp,'ZOFR',INR,1,ZOFR)
  !  TOROIDAL CURRENT DENSITY R-PROFILE
  call ascwrtM(mp,'ZOJR',INR,1,ZOJR)
  !
  !  SET S-VALUES IN ZABS
  !
  INS    = NISO1EFF + 1
  call ascwrtI(mp,'INS',1,1,INS)
  !
  ZABSM(1) = 0._rkind
  ZABIS(1) = 0._rkind
  ZOQS(1)  = Q0
  ZODQS(1) = DQDP0
  ZOJBS(1) = RJDTB0
  ZOTRS(1) = 0._rkind
  ZOHS(1)  = FCCCC0(HMERCR(1),HMERCR(2),HMERCR(3),HMERCR(4), &
       SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ZODIS(1) = FCCCC0(SMERCI(1),SMERCI(2),SMERCI(3),SMERCI(4), &
       SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ZODRS(1) = FCCCC0(SMERCR(1),SMERCR(2),SMERCR(3),SMERCR(4), &
       SMISO(1),SMISO(2),SMISO(3),SMISO(4),RC0P)
  ZOPPS(1) = DPDP0
  ZOPS(1)  = CP0
  ZOTTS(1) = DTTP0
  ZOTS(1)  = T0
  ZOIPS(1) = RIPR0
  ZOIPS(1) = RIPR0
  ZOJPS(1) = RJPAR(1)
  ZOJBSS(1,:) = 0._rkind
  !
  ! SMISO does not include values on axis but the edge point
  ! use NISO1EFF1 and SMISOP1 to have values from axis to edge, 
  ! but then use eqchease_out(1)%profiles_1d%q instead of qpsi for example
  DO J285=1,NISO1EFF
     !
     ZABSM(J285+1) = SMISO(J285)
     ZABS(J285)    = SMISO(J285)
     ZABIS(J285+1) = (J285 - .5_rkind) / real(NISO1EFF-1,rkind)
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
     ZOARS(J285)   = RSURF(J285)
     ZOJPS(J285+1) = RJPAR(J285)
     ZOJBSS(J285+1,1:3)= RJBSOS(J285,1:3)
     ZOTRS(J285+1) = 1._rkind - RFCIRC(J285)
     !
  END DO
  !
  ZABIS(NISO1EFF+1) = 1._rkind
  !
  !
  !  S-VECTOR FOR PROFILES (LENGTH = INS)
  call ascwrtM(mp,'ZABSM',INS,1,ZABSM)
  !  Q(S)-PROFILE
  call ascwrtM(mp,'ZOQS',INS,1,ZOQS)
  !  Q'(S)-PROFILE
  call ascwrtM(mp,'ZODQS',INS,1,ZODQS)
  !  SHEAR S(S)-PROFILE
  call ascwrtM(mp,'ZOSHS',INS,1,ZOSHS)

  call ascwrtM(mp,'ZOJBS',INS,1,ZOJBS)
  !  BOOTSTRAP CURRENT <JBOOT.B> S-PROFILE

  call ascwrtM(mp,'ZOJBSS',INS,3,ZOJBSS)
  !  TOTAL CURRENT <JTOT.B> S-PROFILE
  call ascwrtM(mp,'ZOJPS',INS,1,ZOJPS)
  ! TRAPPED FRACTION S-PROFILE
  call ascwrtM(mp,'ZOTRS',INS,1,ZOTRS)
  !  H OF GLASSER-GREENE-JOHNSON S-PROFILE
  call ascwrtM(mp,'ZOHS',INS,1,ZOHS)
  !  IDEAL MERCIER COEFFICIENT -DI(S)
  call ascwrtM(mp,'ZODIS',INS,1,ZODIS)
  !  RESISTIVE MERCIER COEFFICIENT -DR(S)
  call ascwrtM(mp,'ZODRS',INS,1,ZODRS)
  !  P'(S)-PROFILE
  call ascwrtM(mp,'ZOPPS',INS,1,ZOPPS)
  !  P(S)-PROFILE
  call ascwrtM(mp,'ZOPS',INS,1,ZOPS)
  !  TT'(S)-PROFILE
  call ascwrtM(mp,'ZOTTS',INS,1,ZOTTS)
  !  T(S)-PROFILE
  call ascwrtM(mp,'ZOTS',INS,1,ZOTS)
  !  I*(S)-PROFILE
  call ascwrtM(mp,'ZOIPS',INS,1,ZOIPS)
  !  BETA POLOIDAL S-PROFILE
  call ascwrtM(mp,'ZOBETS',NISO1EFF,1,ZOBETS)
  !
  !  SQRT(VOLUME OF FLUX TUBE) S-PROFILE
  call ascwrtM(mp,'ZOARS',NISO1EFF,1,ZOARS)
  !  S-VECTOR FOR PROFILES (LENGTH = NISO1EFF)
  call ascwrtM(mp,'ZABS',NISO1EFF,1,ZABS)
  !
  !  CONSTANT CHI LINES
  !
  !%OS         JSCHI = NCHI / 25 + 1
  JSCHI = 1
  do jj=1,nchi
     write(14,'(a,i5,a,i5,a)') ' jchi = ',jj,' ipsi=1,',NISO1EFF,':'
     write(14,'(1p2e13.5)') (cr(jj,j288),cz(jj,j288),j288=1,NISO1EFF)
  end do
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
  call ascwrtI(mp,'INBCHI',1,1,INBCHI)
  !  R-MATRIX FOR ISOCHI LINES INTERSECTIONS WITH ISOPSI SURFACES
  call ascwrtM(mp,'ZRCHI',INBCHI,NISO1EFF,ZRCHI)
  !  Z-MATRIX FOR ISOCHI LINES INTERSECTIONS WITH ISOPSI SURFACES
  call ascwrtM(mp,'ZZCHI',INBCHI,NISO1EFF,ZZCHI)
  !
  call ascwrtM(mp,'CSIG',NS1,1,CSIG)
  call ascwrtM(mp,'SMISO',,1,SMISO)
  call ascwrtM(mp,'ZCHI',NCHI1,1,ZCHI)
  call ascwrtM(mp,'ZCSIPR',NISO,1,ZCSIPR)
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
  !  BALLOONING (IN)STABILITY FLAG ON EACH FLUX SURFACE: (1)0
  call ascwrtI(mp,'IBALL',NISO1EFF,1,IBALL)
  call ascwrtI(mp,'NCBAL',NISO1EFF,1,NCBAL)

  !  IDEAL INTERCHANGE (IN)STABILITY FLAG: (1)0
  call ascwrtI(mp,'IMERCI',NISO1EFF,1,IMERCI)

  !  RESISTIVE INTERCHANGE (IN)STABILITY FLAG: (1)0
  call ascwrtI(mp,'IMERCR',NISO1EFF,1,IMERCR)
  !
  DO J293=1,NCURV
     !
     ZRCURV(J293) = RRCURV(J293) - R0
     ZZCURV(J293) = RZCURV(J293) - RZ0
     !
  END DO
  !
  !  R-VECTOR FOR ZERO-CURVATURE CURVE
  call ascwrtM(mp,'ZRCURV',NCURV,1,ZRCURV)
  !  Z-VECTOR FOR ZERO-CURVATURE CURVE
  call ascwrtM(mp,'ZZCURV',NCURV,1,ZZCURV)
  !
  ZCSIPR(1)      = 0._rkind
  ZABIPR(NISO+1) = 1._rkind
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
  call ascwrtI(mp,'NMGAUS',1,1,NMGAUS)
  !
  ! substract mesh center coordinates to (R,Z) coordinates for plotting
  !
  RRISO(:,1:NISO1EFF)=RRISO(:,1:NISO1EFF) - R0
  RZISO(:,1:NISO1EFF)=RZISO(:,1:NISO1EFF) - RZ0
  CR(:,:)=CR(:,:) - R0
  CZ(:,:)=CZ(:,:) - RZ0
  !
  !  R-MATRIX FOR ISOPSI-SURFACES
  NNBBPP = NMGAUS*NT1
  call ascwrtM(mp,'RRISO',NNBBPP,NISO1EFF,tRRISO)
  !  Z-MATRIX FOR ISOPSI-SURFACES
  call ascwrtM(mp,'RZISO',NNBBPP,NISO1EFF,tRZISO)
  !  LOCAL_SHEAR-MATRIX ON (CR,CZ) MESH
  call ascwrtM(mp,'RSHEAR',NCHI,NISO1EFF,tRSHEAR)
  call ascwrtM(mp,'CR',NCHI,NISO1EFF,tCR)
  call ascwrtM(mp,'CZ',NCHI,NISO1EFF,tCZ)
  !
  !
  ! Restore (R,Z) coodinates
  !
  RRISO(:,1:NISO1EFF)=RRISO(:,1:NISO1EFF) + R0
  RZISO(:,1:NISO1EFF)=RZISO(:,1:NISO1EFF) + RZ0
  CR(:,:)=CR(:,:) + R0
  CZ(:,:)=CZ(:,:) + RZ0

  call ascwrtM(mp,'APLACE',10,1,APLACE)
  call ascwrtM(mp,'AWIDTH',10,1,AWIDTH)
  call ascwrtM(mp,'BPLACE',10,1,BPLACE)
  call ascwrtM(mp,'BWIDTH',10,1,BWIDTH)
  call ascwrtM(mp,'CPLACE',10,1,CPLACE)
  call ascwrtM(mp,'CWIDTH',10,1,CWIDTH)
  call ascwrtM(mp,'DPLACE',10,1,DPLACE)
  call ascwrtM(mp,'DWIDTH',10,1,DWIDTH)
  call ascwrtM(mp,'EPLACE',10,1,EPLACE)
  call ascwrtM(mp,'EWIDTH',10,1,EWIDTH)
  call ascwrtM(mp,'QPLACE',10,1,QPLACE)
  call ascwrtM(mp,'QWIDTH',10,1,QWIDTH)
  !
  close(mp)
  close(mp+1)     
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
END SUBROUTINE WRTASC
subroutine ascwrtI(mf,name,n,m,intvalue)
  !
  !                                        AUTHORS:
  !                                        R. Arslanbekov
  implicit none

  integer                 :: mf, n, m
  integer, dimension(n*m) :: intvalue
  character*(*)           :: name
  character(1)            :: info = '*'
  !
  write(mf,*) intvalue
  write(mf+1,'(A, 2X, A, 1X, I10, 1X, I10)') name, info, n, m
  !     
  return
end subroutine ascwrtI

subroutine ascwrtM(mf,name,n,m,realvalue)
  !
  !                                        AUTHORS:
  !                                        R. Arslanbekov
  implicit none

  integer                     :: mf, n, m
  character*(*)               :: name
  character(1)                :: info = '*'
  real(8), dimension(n*m) :: realvalue
  !     
  write(mf,*) realvalue
  write(mf+1,'(A, 2X, A, 1X, I10, 1X, I10)') name, info, n, m
  !
  return
end subroutine ascwrtM

subroutine ascwrtC(mf,name,charvalue)
  !
  !                                        AUTHORS:
  !                                        R. Arslanbekov
  implicit none

  integer                 :: mf, n = 1, m = 1
  character*(*)           :: name
  character*(*)           :: charvalue
  !     
  write(mf,*) 0._8
  write(mf+1,'(A, 2X, A, 1X, I10, 1X, I10)') name, charvalue, n, m
  !
  return
end subroutine ascwrtC

