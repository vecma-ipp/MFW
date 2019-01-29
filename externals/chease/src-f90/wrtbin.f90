SUBROUTINE WRTBIN
  !        #####################
  !
  !                                        AUTHORS:
  !                                        H. Lutjens, 
  !                                        P. Hennequin
  !                                        R. Arslanbekov 10/99
  !                      from WRTPLOT in chease.f
  !**********************************************************************
  !                                                                     *
  ! C3SB02  WRITE DATA NECESSARY FOR PLOT 
  !                            ON THE FILE chease.bin (binary format)   *
  !                                                                     *
  !**********************************************************************
  !
  USE globals
  USE interpol
  use interpos_module
  IMPLICIT NONE
  INCLUDE 'COMDAT.inc'
  !
  integer          ::      mp      ! <wrtbin.f90>
  integer          ::      nnbbpp  ! <wrtbin.f90>
  INTEGER          ::     J301    ! <wrtplot.f90>
  INTEGER          ::     I    ! <wrtplot.f90>
  INTEGER          ::     J    ! <wrtplot.f90>
  INTEGER          ::     J297    ! <wrtplot.f90>
  INTEGER          ::     INTEXT    ! <wrtplot.f90>
  INTEGER          ::     INR    ! <wrtplot.f90>
  INTEGER          ::     INSUR    ! <wrtplot.f90>
  INTEGER          ::     J296    ! <wrtplot.f90>
  INTEGER          ::     J295    ! <wrtplot.f90>
  INTEGER          ::     J294    ! <wrtplot.f90>
  INTEGER          ::     J293    ! <wrtplot.f90>
  INTEGER          ::     J289    ! <wrtplot.f90>
  INTEGER          ::     INBCHI    ! <wrtplot.f90>
  INTEGER          ::     J287    ! <wrtplot.f90>
  INTEGER          ::     JNB    ! <wrtplot.f90>
  INTEGER          ::     J288    ! <wrtplot.f90>
  INTEGER          ::     JJ    ! <wrtplot.f90>
  INTEGER          ::     JSCHI    ! <wrtplot.f90>
  INTEGER          ::     J285    ! <wrtplot.f90>
  INTEGER          ::     I2    ! <wrtplot.f90>
  INTEGER          ::     I1    ! <wrtplot.f90>
  INTEGER          ::     J284    ! <wrtplot.f90>
  INTEGER          ::     J283    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBND2    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBND1    ! <wrtplot.f90>
  INTEGER          ::     J282    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZDT    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZZMIN    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZZMAX    ! <wrtplot.f90>
  INTEGER          ::     ISMIN    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZRMIN    ! <wrtplot.f90>
  INTEGER          ::     ISMAX    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZRMAX    ! <wrtplot.f90>
  INTEGER          ::     J281    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBBS    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZCBS2    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZCBS1    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBSFC    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBSF    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZLI1    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBPOL1    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZGMX    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZGMSTA    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZGM    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZIBSNO    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZINORM    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBXPER    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBSPER    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBPERC    ! <wrtplot.f90>
  REAL(RKIND)      ::     ZBAXEPER! <wrtplot.f90>
  REAL(RKIND)      ::     ZMU0    ! <wrtplot.f90>
  !
  INTEGER, dimension(:), allocatable         ::     IBALL    ! <wrtplot.f90>
  INTEGER, dimension(:), allocatable         ::     IMERCR    ! <wrtplot.f90>
  INTEGER, dimension(:), allocatable         ::     IMERCI    ! <wrtplot.f90>
  !
  REAL(RKIND), dimension(:), allocatable     ::     ZABR    ! <wrtplot.f90>
  REAL(RKIND), dimension(:), allocatable     ::     ZPAR    ! <wrtplot.f90>
  REAL(RKIND), dimension(:,:), allocatable   ::     ZRCHI    ! <wrtplot.f90>
  REAL(RKIND), dimension(:,:), allocatable   ::     ZZCHI    ! <wrtplot.f90>
  REAL(RKIND), dimension(:), allocatable     ::     ZRHOS    ! <wrtplot.f90>
  REAL(RKIND), dimension(:), allocatable     ::     ZRSUR    ! <wrtplot.f90>
  REAL(RKIND), dimension(:), allocatable     ::     ZTSUR    ! <wrtplot.f90>
  REAL(RKIND), dimension(:), allocatable     ::     ZSIG1    ! <wrtplot.f90>
  REAL(RKIND), dimension(:), allocatable     ::     ZTET1    ! <wrtplot.f90>
  !
  real(rkind), dimension(:), allocatable     ::      aux
  !
  integer :: state
  !
  !----*----*----*---*----*----*----*----*----*----*----*----*----*----*-
  !  28. WRITE QUANTITIES FOR PLOTS in binary format (IEEE)
  !
  !     Open bin and dat-files for writing
  !
  write(6,*) 'Creating bin and dat files chease.* ...'
  mp = 101
  !  open(mp,  file ='chease.bin', & ! when "convert" not accepted by compiler
  open(mp,convert = 'big_endian', file ='chease.bin', &
       & FORM = 'unformatted')
  open(mp+1, file ='chease.dat', FORM = 'formatted')
  !      
  !  --------------------------------------------------------------------- 
  ZMU0 = 1.256_rkind
  ZBPERC = 100._rkind * BETA
  ZBSPER = 100._rkind * BETAS
  ZBXPER = 100._rkind * BETAX
  ZBAXEPER = 2._rkind*100._rkind*cp0*( eqchease_out_add_1d(NISO1EFF1,iirgeo)/T0 )**2
  ZINORM = RINOR/ZMU0
  ZIBSNO = RIBSNOR/ZMU0
  ZGM    = ZBPERC/ZINORM
  ZGMSTA = ZBSPER/ZINORM
  ZGMX   = ZBXPER/ZINORM
  ZBPOL1 = BETAP*CONVF
  ZLI1   = eqchease_out(1)%profiles_1d%li(NISO1EFF1)*CONVF
  ZBSF   = RITBS/RITOT
  ZBSFC  = RITBSC/RITOT
  ZCBS1  = 0._RKIND
  ZCBS2  = 0._RKIND
  IF (BETAX .GT. 0._RKIND) ZCBS1 = ZIBSNO*ZINORM/(ZBXPER*SQRT(ASPCT))
  IF (BETAP .GT. 0._RKIND) ZCBS2 = ZBSF/(SQRT(ASPCT)*ZBPOL1)
  ZBBS  = ZBXPER * ZBSF
  !  --------------------------------------------------------------------- 
  !   variables de caracterisation du run (pour ecriture texte)
  !  --------------------------------------------------------------------- 
  !          'PLASMA SURFACE :'
  !
  call binwrtI(mp,'NSURF',1,1,NSURF)
  call binwrtM(mp,'ASPCT',1,1,ASPCT)
  call binwrtM(mp,'RC',1,1,RC)
  call binwrtM(mp,'ELONG',1,1,ELONG)
  call binwrtM(mp,'TRIANG',1,1,TRIANG)
  call binwrtM(mp,'DELTA',1,1,DELTA)
  call binwrtM(mp,'THETA0',1,1,THETA0)
  call binwrtM(mp,'BEANS',1,1,BEANS)
  call binwrtM(mp,'CETA',1,1,CETA)
  call binwrtM(mp,'SGMA',1,1,SGMA)
  call binwrtM(mp,'TRIPLT',1,1,TRIPLT)
  call binwrtM(mp,'RNU',1,1,RNU)
  call binwrtM(mp,'XI',1,1,XI)
  call binwrtM(mp,'AREA',1,1,AREA)
  call binwrtM(mp,'REXT',1,1,REXT)
  call binwrtM(mp,'TENSBND',1,1,TENSBND)
  call binwrtI(mp,'NBPSOUT',1,1,NBPSOUT)
  call binwrtI(mp,'NBSEXPQ',1,1,NBSEXPQ)
  call binwrtI(mp,'NFIXWALL',1,1,NFIXWALL)
  call binwrtI(mp,'NSYM',1,1,NSYM)
  !          'EQUILIBRIUM SOLUTION :'
  call binwrtM(mp,'B0EXP',1,1,B0EXP)
  call binwrtM(mp,'R0EXP',1,1,R0EXP)
  call binwrtM(mp,'SCEXP',1,1,SCEXP)
  call binwrtM(mp,'SIGNB0XP',1,1,SIGNB0XP)
  call binwrtM(mp,'SIGNIPXP',1,1,SIGNIPXP)
  call binwrtM(mp,'EPSLON',1,1,EPSLON)
  call binwrtM(mp,'RELAX',1,1,RELAX)
  call binwrtM(mp,'RMAG',1,1,RMAG)
  call binwrtM(mp,'RZMAG',1,1,RZMAG)
  call binwrtM(mp,'PSIMIN',1,1,PSI0)
  call binwrtM(mp,'PSISCL',1,1,PSISCL)
  call binwrtM(mp,'SNUMBER',1,1,SNUMBER)
  call binwrtM(mp,'SLIMIT',1,1,SLIMIT)
  call binwrtM(mp,'SHIFT_P',1,1,SHIFT_P)
  call binwrtM(mp,'Q0',1,1,Q0)
  call binwrtM(mp,'QSURF',1,1,QPSI(NISO1EFF))
  call binwrtM(mp,'QCYL',1,1,QCYL)
  call binwrtM(mp,'QMIN',1,1,QMIN)
  call binwrtM(mp,'CSQMIN',1,1,CSQMIN)
  call binwrtM(mp,'Q95',1,1,Q95)
  call binwrtM(mp,'T0',1,1,T0)
  call binwrtM(mp,'TSURF',1,1,TMF(NISO1EFF))
  call binwrtM(mp,'RITOT',1,1,RITOT)
  call binwrtM(mp,'RINOR',1,1,RINOR)
  call binwrtM(mp,'ZINORM',1,1,ZINORM)
  call binwrtM(mp,'ZBSF',1,1,ZBSF)
  call binwrtM(mp,'ZBSFC',1,1,ZBSFC)
  call binwrtM(mp,'RZION',1,1,RZION)
  call binwrtM(mp,'RPEOP',1,1,RPEOP)
  call binwrtM(mp,'SCALNE',1,1,SCALNE)
  call binwrtM(mp,'ETAEI',1,1,ETAEI)
  call binwrtM(mp,'GAMMA',1,1,GAMMA)
  call binwrtM(mp,'CBS1',1,1,ZCBS1)
  call binwrtM(mp,'CBS2',1,1,ZCBS2)
  call binwrtM(mp,'BBS',1,1,ZBBS)
  call binwrtM(mp,'CONV_FACT',1,1,CONVF)
  call binwrtM(mp,'LI',1,1,eqchease_out(1)%profiles_1d%li(NISO1EFF1))
  call binwrtM(mp,'LI_GA',1,1,ZLI1)
  call binwrtM(mp,'BETA',1,1,ZBPERC)
  call binwrtM(mp,'BETAstar',1,1,ZBSPER)
  call binwrtM(mp,'BETA_EXP',1,1,ZBXPER)
  call binwrtM(mp,'BETA_AXIS',1,1,ZBAXEPER)
  call binwrtM(mp,'G_in_MA_T_M',1,1,ZGM)
  call binwrtM(mp,'Gstar_in_MA_T_M',1,1,ZGMSTA)
  call binwrtM(mp,'G_EXP_MA_T_M',1,1,ZGMX)
  call binwrtM(mp,'BETA_POL',1,1,BETAP)
  call binwrtM(mp,'BETA_POL_GA',1,1,ZBPOL1)
  call binwrtM(mp,'CPBAR',1,1,CPBAR)
  call binwrtM(mp,'CPPF',1,1,CPPF)

  call binwrtI(mp,'NANAL',1,1,NANAL)
  call binwrtI(mp,'NDIAGOP',1,1,NDIAGOP)
  call binwrtI(mp,'NINMAP',1,1,NINMAP)
  call binwrtI(mp,'NINSCA',1,1,NINSCA)
  call binwrtI(mp,'NOPT',1,1,NOPT)
  call binwrtI(mp,'NSMOOTH',1,1,NSMOOTH)
  call binwrtI(mp,'NTCASE',1,1,NTCASE)
  call binwrtI(mp,'NTEST',1,1,NTEST)
  !            'PROFILES :'
  call binwrtI(mp,'NFUNC',1,1,NFUNC)
  call binwrtI(mp,'NSTTP',1,1,NSTTP)
  call binwrtI(mp,'NIPR',1,1,NIPR)
  call binwrtI(mp,'NSOUR',1,1,NSOUR)
  call binwrtI(mp,'NPPFUN',1,1,NPPFUN)
  call binwrtI(mp,'NPP',1,1,NPP)
  call binwrtM(mp,'PREDGE',1,1,PREDGE)
  call binwrtI(mp,'NBSOPT',1,1,NBSOPT)
  call binwrtI(mp,'NBSFUN',1,1,NBSFUN)
  call binwrtI(mp,'NBSTRP',1,1,NBSTRP)
  call binwrtI(mp,'NBLOPT',1,1,NBLOPT)
  call binwrtI(mp,'NBLC0',1,1,NBLC0)
  call binwrtI(mp,'NTURN',1,1,NTURN)
  call binwrtI(mp,'NPPR',1,1,NPPR)
  call binwrtI(mp,'NBAL',1,1,NBAL)
  call binwrtI(mp,'NEONBQS',1,1,NEONBQS)
  call binwrtI(mp,'NFUNRHO',1,1,NFUNRHO)
  call binwrtM(mp,'CFBAL',1,1,CFBAL)
  call binwrtM(mp,'CPRESS',1,1,CPRESS)
  call binwrtM(mp,'CPRESSO',1,1,CPRESSO)
  call binwrtM(mp,'CQ0',1,1,CQ0)
  call binwrtM(mp,'BSFRAC',1,1,BSFRAC)
  call binwrtM(mp,'CFNRESS',1,1,CFNRESS)
  call binwrtM(mp,'CFNRESSO',1,1,CFNRESSO)
  call binwrtI(mp,'COCOS_IN',1,1,COCOS_IN)
  call binwrtI(mp,'COCOS_OUT',1,1,COCOS_OUT)
  call binwrtM(mp,'AT',size(AT),1,AT)
  call binwrtM(mp,'AT2',size(AT2),1,AT2)
  call binwrtM(mp,'AT3',size(AT3),1,AT3)
  call binwrtM(mp,'AT4',size(AT4),1,AT4)
  call binwrtM(mp,'AP',size(AP),1,AP)
  call binwrtM(mp,'AP2',size(AP2),1,AP2)
  call binwrtM(mp,'AFBS',size(AFBS),1,AFBS)
  call binwrtM(mp,'AFBS2',size(AFBS2),1,AFBS2)
  call binwrtM(mp,'BENTAXIS',1,1,BENTAXIS)
  call binwrtM(mp,'BENTQPROFILE',1,1,BENTQPROFILE)
  call binwrtM(mp,'BENTRADIUS',1,1,BENTRADIUS)
  call binwrtM(mp,'RODABYROD0',1,1,RODABYROD0)
  call binwrtM(mp,'TENSPROF',1,1,TENSPROF)
  call binwrtI(mp,'NPOPULATIONS',1,1,NPOPULATIONS)
  call binwrtI(mp,'NPROF2D',1,1,NPROF2D)
  call binwrtI(mp,'NPROFZ',1,1,NPROFZ)
  call binwrtI(mp,'NPROPT',1,1,NPROPT)
  call binwrtI(mp,'NPRPSI',1,1,NPRPSI)
  call binwrtI(mp,'NRFP',1,1,NRFP)
  !                'MESHES :'
  call binwrtM(mp,'R0',1,1,R0)
  call binwrtM(mp,'R0W',1,1,R0)
  call binwrtM(mp,'RZ0',1,1,RZ0)
  call binwrtM(mp,'RZ0W',1,1,RZ0W)
  call binwrtM(mp,'RBOXLEN',1,1,RBOXLEN)
  call binwrtM(mp,'ZBOXLEN',1,1,ZBOXLEN)
  call binwrtM(mp,'RBOXLFT',1,1,RBOXLFT)
  call binwrtI(mp,'NS',1,1,NS)
  call binwrtI(mp,'NT',1,1,NT)
  call binwrtI(mp,'NISO',1,1,NISO)
  call binwrtI(mp,'NTNOVA',1,1,NTNOVA)
  call binwrtI(mp,'NPSI',1,1,NISO1EFF-1)
  call binwrtI(mp,'NCHI',1,1,NCHI)
  call binwrtI(mp,'NER',1,1,NER)
  call binwrtI(mp,'NEGP',1,1,NEGP)
  call binwrtI(mp,'MDT',1,1,MDT)
  call binwrtI(mp,'MSMAX',1,1,MSMAX)
  call binwrtI(mp,'NRBOX',1,1,NRBOX)
  call binwrtI(mp,'NZBOX',1,1,NZBOX)
  call binwrtI(mp,'NRBOX_XTOR',size(NRBOX_XTOR),1,NRBOX_XTOR)
  call binwrtI(mp,'NZBOX_XTOR',size(NZBOX_XTOR),1,NZBOX_XTOR)
  call binwrtI(mp,'NRHOMESH',1,1,NRHOMESH)
  call binwrtI(mp,'NV',1,1,NV)
  call binwrtI(mp,'NVEXP',1,1,NVEXP)
  !
  call binwrtI(mp,'NEQDSK',1,1,NEQDSK)
  call binwrtI(mp,'NITMOPT',1,1,NITMOPT)
  call binwrtI(mp,'NITMRUN',size(NITMRUN),1,NITMRUN)
  call binwrtI(mp,'NITMSHOT',size(NITMSHOT),1,NITMSHOT)
  call binwrtI(mp,'NIDEAL',1,1,NIDEAL)
  call binwrtI(mp,'NOUTXTOR',1,1,NOUTXTOR)
  call binwrtI(mp,'NPLOT',1,1,NPLOT)
  call binwrtI(mp,'NFFTOPT',1,1,NFFTOPT)
  call binwrtI(mp,'NEQDXTPO',1,1,NEQDXTPO)
  call binwrtI(mp,'NVERBOSE',1,1,NVERBOSE)
  !                   'S-PACKING :'
  call binwrtI(mp,'NMESHA',1,1,NMESHA)
  call binwrtM(mp,'SOLPDA',1,1,SOLPDA)
  call binwrtI(mp,'NPOIDA',1,1,NPOIDA)
  call binwrtI(mp,'NPOIDQ',1,1,NPOIDQ)
  call binwrtI(mp,'NDIFPS',1,1,NDIFPS)
  !              'I*-PACKING :'
  call binwrtI(mp,'NMESHB',1,1,NMESHB)
  call binwrtM(mp,'SOLPDB',1,1,SOLPDB)
  call binwrtI(mp,'NPOIDB',1,1,NPOIDB)
  !                 'SIGMA-PACKING :'
  call binwrtI(mp,'NMESHB',1,1,NMESHB)
  call binwrtM(mp,'SOLPDB',1,1,SOLPDB)
  call binwrtI(mp,'NPOIDB',1,1,NPOIDB)
  !                'THETA-PACKING :'
  call binwrtI(mp,'NMESHC',1,1,NMESHC)
  call binwrtM(mp,'SOLPDC',1,1,SOLPDC)
  call binwrtI(mp,'NPOIDC',1,1,NPOIDC)
  !                 'THETA-PACKING :'
  call binwrtI(mp,'NMESHD',1,1,NMESHD)
  call binwrtM(mp,'SOLPDD',1,1,SOLPDD)
  call binwrtI(mp,'NPOIDD',1,1,NPOIDD)
  call binwrtI(mp,'NDIFT',1,1,NDIFT)
  !                 'CHI-PACKING :'
  call binwrtI(mp,'NMESHE',1,1,NMESHE)
  call binwrtM(mp,'SOLPDE',1,1,SOLPDE)
  call binwrtI(mp,'NPOIDE',1,1,NPOIDE)
  !                    'NORMALIZATION :'
  call binwrtI(mp,'NCSCAL',1,1,NCSCAL)
  call binwrtI(mp,'NTMF0',1,1,NTMF0)
  call binwrtI(mp,'NRSCAL',1,1,NRSCAL)
  call binwrtM(mp,'SCALE',1,1,SCALE)
  call binwrtM(mp,'CSSPEC',1,1,CSSPEC)
  call binwrtM(mp,'QSPEC',1,1,QSPEC)
  call binwrtM(mp,'CURRT',1,1,CURRT)
  !
  !  COMPUTE MAXIMUM AND MINIMUM OF R AND Z
  !  REFERENCE IS THE MAGNETIC AXIS
  !
  allocate(aux(NTP1),ZRHOS(NTP1),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 1 =',state
  !
  CALL BOUND(NT1,CT,ZRHOS)
  !
  aux(1) = 0._rkind
  DO J281=1,NT
     !
     aux(J281+1) = aux(J281) + .25_rkind * (CT(J281+1) - CT(J281)) * &
          (ZRHOS(J281)**2 + ZRHOS(J281+1)**2)
     !
  END DO
  !
  call binwrtM(mp,'ZOART',NT1,1,aux)
  !
  aux(:)= (/ 0._rkind, (REAL(i,rkind)/real(NT,rkind), i=1,NT) /)
  call binwrtM(mp,'ZABIT',NT1,1,aux)
  !
  aux(:)= (/ (ZRHOS(i) * COS(CT(i)), i=1,NT) /)
  call binwrtM(mp,'ZRTET',NT,1,aux)
  !
  ZRMAX=maxval(aux(1:NT))
  ZRMIN=minval(aux(1:NT))
  call binwrtM(mp,'ZRMAX',1,1,ZRMAX)
  call binwrtM(mp,'ZRMIN',1,1,ZRMIN)
  !
  aux(:)= (/ (ZRHOS(i) * SIN(CT(i)), i=1,NT) /)
  call binwrtM(mp,'ZZTET',NT,1,aux)
  !
  ZZMAX=maxval(aux(1:NT))
  ZZMIN=minval(aux(1:NT))
  call binwrtM(mp,'ZZMAX',1,1,ZZMAX)
  call binwrtM(mp,'ZZMIN',1,1,ZZMIN)
  !
  aux(:)= (/ (CT(i)-CT(1), i=1,NT), 2._rkind * CPI /)
  call binwrtM(mp,'ZTET',NT1,1,aux)
  !
  deallocate(aux,ZRHOS)
  !
  !  COMPUTE THE SURFACE
  !
  INSUR  = 6 * NT
  call binwrtI(mp,'INSUR',1,1,INSUR)
  !
  allocate(aux(INSUR),ZTSUR(INSUR),ZRSUR(INSUR),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 2 =',state
  !
  BPS( 1) = RMAG
  BPS(12) = RZMAG
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  ZDT = 2._rkind * CPI / real(6 * NT - 1,rkind)
  !
  ztsur(:) = (/ ((i-1)*ZDT, i=1,INSUR) /)
  call binwrtM(mp,'ZTSUR',INSUR,1,ZTSUR)
  !
  CALL BOUND(6*NT,ZTSUR,ZRSUR)
  CALL BOUND(1,PANGLE    ,ZBND1)
  CALL BOUND(1,PANGLE+CPI,ZBND2)
  !
  ! OS comment bug with ZTSUR(J283) used without being defined, assumed should be i
  aux(:) = (/ (ZRSUR(i) * COS(ZTSUR(i)), i=1,INSUR) /)
  call binwrtM(mp,'ZRSUR',INSUR,1,aux)
  !
  ! OS comment bug with ZTSUR(J283) used without being defined, assumed should be i
  aux(:) = (/ (ZRSUR(i) * SIN(ZTSUR(i)), i=1,INSUR) /)
  call binwrtM(mp,'ZZSUR',INSUR,1,aux)
  !
  BPS( 1) = R0
  BPS(12) = RZ0
  !
  IF (NSURF .NE. 1) BPS(3) = BPS(2) - BPS(1)
  IF (NSURF .EQ. 6) CALL BNDSPL
  !
  deallocate(aux,ztsur,zrsur)
  !
  !  COMPUTE THE VALUES OF R ON RADIUS USED FOR PROFILE DEFINITION
  !
  INR    = 2 * NISO1EFF + 1
  !
  call binwrtI(mp,'INR',1,1,INR)
  call binwrtI(mp,'INS',1,1,NISO1EFF1)
  !
  allocate(aux(INR),ZABR(INR),ZPAR(INR),ZSIG1(NPISOEFF),ZTET1(NPISOEFF),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 3 =',state
  !
  CALL RMRAD(NISO1EFF,RC0P,CPSRF,PANGLE,ZPAR(1),ZSIG1,ZTET1,1)
  CALL RMRAD(NISO1EFF,RC0P,CPSRF,PANGLE+CPI,ZPAR(NISO1EFF1),ZSIG1,ZTET1,1)
  !
  call binwrtM(mp,'PANGLE',1,1,PANGLE)
  !
  !
  !  R-VECTOR FOR PROFILES (LENGTH = INR)
  ZABR(:) = (/ (-ZPAR(2*NISO1EFF+1-i)*ZBND2, i=1,NISO1EFF), 0._rkind, (ZPAR(i)*ZBND1, i=1,NISO1EFF) /)
  call binwrtM(mp,'ZABR',INR,1,ZABR)
  !
  !  Q(R)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%q(i), i=NISO1EFF1,2,-1),  &
              (eqchease_out(1)%profiles_1d%q(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOQR',INR,1,aux)
  !
  !  Q'(R)-PROFILE
  aux(:) = (/ (eqchease_out_add_1d(i,iidqdpsi), i=NISO1EFF1,2,-1), &
              (eqchease_out_add_1d(i,iidqdpsi), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZODQR',INR,1,aux)
  !
  !  SHEAR S(R)-PROFILE
  aux(:) = (/ (eqchease_out_add_1d(i,iishear), i=NISO1EFF1,2,-1), &
              (eqchease_out_add_1d(i,iishear), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOSHR',INR,1,aux)
  !
  !  J//=<J.B>/B0(R) PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%jparallel(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%jparallel(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOJBR',INR,1,aux)
  !
  !  BOOTSTRAP CURRENT PROFILE's
  aux(:) = (/ (RJBSOS(i,1), i=NISO1EFF,1,-1), 0._rkind, (RJBSOS(i,1), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSR1',INR,1,aux)
  aux(:) = (/ (RJBSOS(i,2), i=NISO1EFF,1,-1), 0._rkind, (RJBSOS(i,2), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSR2',INR,1,aux)
  aux(:) = (/ (RJBSOS(i,3), i=NISO1EFF,1,-1), 0._rkind, (RJBSOS(i,3), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSR3',INR,1,aux)
  aux(:) = (/ (RJBSOS(i,4), i=NISO1EFF,1,-1), 0._rkind, (RJBSOS(i,4), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSR4',INR,1,aux)
  !
  ! NUSTAR (WITHOUT ZEFF)
  aux(:) = (/ (RNUSTAR(i), i=NISO1EFF,1,-1), 0._rkind, (RNUSTAR(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'RNUSTARR',INR,1,aux)
  !
  !  TRAPPED FRACTION R-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%ftrap(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%ftrap(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTRR',INR,1,aux)
  !
  !  IDEAL MERCIER COEFFICIENT -DI(S), full formula
  aux(:) = (/ (SMERCI(i), i=NISO1EFF,1,-1), &
              flinear(eqchease_out(1)%profiles_1d%rho_vol(2),eqchease_out(1)%profiles_1d%rho_vol(3),SMERCI(1),SMERCI(2),0._rkind), &
              (SMERCI(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZODIR',INR,1,aux)
  !
  !  IDEAL MERCIER COEFFICIENT -DI(S), L.A. with Elongation
  aux(:) = (/ (rdi(i), i=NISO1EFF,1,-1), rdi(1), (rdi(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZODILAR',INR,1,aux)
  !
  !  IDEAL MERCIER COEFFICIENT -DI(S), Shafranov Yourchenko
  aux(:) = (/(rsy(i), i=NISO1EFF,1,-1), rsy(1), (rsy(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZORSYR',INR,1,aux)
  !
  !  RESISTIVE MERCIER COEFFICIENT -DR(S)
  aux(:) = (/ (smercr(i), i=NISO1EFF,1,-1), SMERCR(1), (smercr(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZODRR',INR,1,aux)
  !
  !  H OF GLASSER-GREENE-JOHNSON R-PROFILE
  aux(:) = (/ (hmercr(i), i=NISO1EFF,1,-1), HMERCR(1), (hmercr(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOHH',INR,1,aux)
  !
  !  P'(R)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%pprime(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%pprime(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOPPR',INR,1,aux)
  !
  !  P(R)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%pressure(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%pressure(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOPR',INR,1,aux)
  !
  !  TT'(R)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%ffprime(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%ffprime(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTTR',INR,1,aux)
  !
  !  T(R)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%F_dia(i), i=NISO1EFF1,2,-1), & 
              (eqchease_out(1)%profiles_1d%F_dia(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTR',INR,1,aux)
  !
  !  I*(R)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%jphi(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%jphi(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOIPR',INR,1,aux)
  !
  !  BETA POLOIDAL R-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%beta_pol(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%beta_pol(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOBETR',INR,1,aux)
  !
  !  SQRT(VOLUME OF FLUX TUBE) S-PROFILE
  aux(:) = (/(eqchease_out(1)%profiles_1d%rho_vol(i), i=NISO1EFF1,2,-1), &
             (eqchease_out(1)%profiles_1d%rho_vol(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOARR',INR,1,aux)
  !
  !  Elongation
  aux(:) = (/ (eqchease_out(1)%profiles_1d%elongation(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%elongation(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOELONGR',INR,1,aux)
  !
  !  ELLIPTICITY
  aux(:) = (/ (RELL(i), i=NISO1EFF,1,-1), &
               flinear(eqchease_out(1)%profiles_1d%rho_vol(2),eqchease_out(1)%profiles_1d%rho_vol(3),rell(1),rell(2),0._rkind), &
               (RELL(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZELLR',INR,1,aux)
  !
  !  d(ELLIPTICITY)/dr
  aux(:) = (/ (RDEDR(i), i=NISO1EFF,1,-1), &
               flinear(eqchease_out(1)%profiles_1d%rho_vol(2),eqchease_out(1)%profiles_1d%rho_vol(3),rdedr(1),rdedr(2),0._rkind), &
              (RDEDR(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZDEDRR',INR,1,aux)
  !
  !  Upper Triangularity
  aux(:) = (/ (eqchease_out(1)%profiles_1d%tria_upper(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%tria_upper(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTRIAUPR',INR,1,aux)
  !
  !  Lower Triangularity
  aux(:) = (/ (eqchease_out(1)%profiles_1d%tria_lower(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%tria_lower(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTRIALOWR',INR,1,aux)
  !
  !  Geometric center of flux tube S-PROFILE
  aux(:) = (/(eqchease_out_add_1d(i,iirgeo), i=NISO1EFF1,2,-1), &
             (eqchease_out_add_1d(i,iirgeo), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORGEOMR',INR,1,aux)
  !
  !  Inverse Aspect Ratio of Flux Tube
  aux(:) = (/ (eqchease_out_add_1d(i,iiamin)/eqchease_out_add_1d(i,iirgeo), i=NISO1EFF1,2,-1), &
              (eqchease_out_add_1d(i,iiamin)/eqchease_out_add_1d(i,iirgeo), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOASPCTR',INR,1,aux)
  !
  !  AREA of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%area(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%area(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOAREAR',INR,1,aux)
  !
  !  Volume of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%volume(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%volume(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOVOLR',INR,1,aux)
  !
  !  Normalized Volume of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%rho_vol(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%rho_vol(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORHOVOLR',INR,1,aux)
  !
  !  Volume-prime of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%vprime(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%vprime(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOVPRIMER',INR,1,aux)
  !
  !  rho_tor of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%rho_tor(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%rho_tor(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORHOTORR',INR,1,aux)
  !
  !  Normalized rho_tor of flux tube
  aux(:) = (/ (CSMTOR(i), i=NISO1EFF,1,-1), 0._rkind, (CSMTOR(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZORHOTORNR',INR,1,aux)
  !
  !  rho_tor-prime of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%dvdrho(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%dvdrho(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORHOTORPRIMER',INR,1,aux)
  !
  !  dpsi/drho_tor/Rgeom(a) of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%dpsidrho_tor(i)/eqchease_out_add_1d(NISO1EFF1,iirgeo), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%dpsidrho_tor(i)/eqchease_out_add_1d(NISO1EFF1,iirgeo), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZODPSIRHOTORR',INR,1,aux)
  !
  !  Ip Profile
  aux(:) = (/ (eqchease_out_add_1d(i,iiIplas), i=NISO1EFF1,2,-1), &
              (eqchease_out_add_1d(i,iiIplas), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOIPLR',INR,1,aux)
  !
  !  Li Profile
  aux(:) = (/ (eqchease_out(1)%profiles_1d%li(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%li(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOLIR',INR,1,aux)
  !
  !  Bmin Profile
  aux(:) = (/ (eqchease_out_add_1d(i,iiBmin), i=NISO1EFF1,2,-1), &
              (eqchease_out_add_1d(i,iiBmin), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOBMINR',INR,1,aux)
  !
  !  Bmax Profile
  aux(:) = (/ (eqchease_out_add_1d(i,iiBmax), i=NISO1EFF1,2,-1), &
              (eqchease_out_add_1d(i,iiBmax), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOBMAXR',INR,1,aux)
  !
  !  Rmin Profile
  aux(:) = (/ (eqchease_out(1)%profiles_1d%r_inboard(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%r_inboard(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORMINR',INR,1,aux)
  !
  !  Rmax Profile
  aux(:) = (/ (eqchease_out(1)%profiles_1d%r_outboard(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%r_outboard(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORMAXR',INR,1,aux)
  !
  !  POLOIDAL FLUX R-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%psi(i), i=NISO1EFF1,2,-1), &
              (eqchease_out(1)%profiles_1d%psi(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOFR',INR,1,aux)
  !
  !  TOROIDAL CURRENT DENSITY R-PROFILE
  !
  aux(NISO1EFF1)   = - (RMAG * DPDP0 + DTTP0 / RMAG)

  DO J284=1,NISO1EFF
     !
     I1 = NISO1EFF + J284 + 1
     I2 = NISO1EFF - J284 + 1
     !
     aux(I1)  = - (ZABR(I1) + RMAG) * CPPR(J284) - &
          TTP(J284) / (ZABR(I1) + RMAG)
     aux(I2)  = - (ZABR(I2) + RMAG) * CPPR(J284) - &
          TTP(J284) / (ZABR(I2) + RMAG)
     !
  END DO
  call binwrtM(mp,'ZOJR',INR,1,aux)
  !
  deallocate(aux,ZABR,ZPAR,ZSIG1,ZTET1)
  !
  !  SET S-VALUES IN ZABS
  !
  !
  allocate(aux(NISO1EFF1),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 4 =',state
  !
  !
  !  S-VECTOR FOR PROFILES (LENGTH = NISO1EFF1)
  aux(:) = (/0._rkind, (SMISO(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZABSM',NISO1EFF1,1,aux)
  !
  !  Q(S)-PROFILE
  aux(:) = (/(eqchease_out(1)%profiles_1d%q(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOQS',NISO1EFF1,1,aux)
  !
  !  Q'(S)-PROFILE
  aux(:) = (/ (eqchease_out_add_1d(i,iidqdpsi), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZODQS',NISO1EFF1,1,aux)
  !
  !  SHEAR S(S)-PROFILE
  aux(:) = (/ (eqchease_out_add_1d(i,iishear), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOSHS',NISO1EFF1,1,aux)
  !
  !  J//=<J.B>/B0 S-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%jparallel(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOJBS',NISO1EFF1,1,aux)
  !
  !  BOOTSTRAP CURRENT <JBOOT.B> S-PROFILE's
  aux(:) = (/ 0._rkind, (RJBSOS(i,1), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSS1',NISO1EFF1,1,aux)
  aux(:) = (/ 0._rkind, (RJBSOS(i,2), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSS2',NISO1EFF1,1,aux)
  aux(:) = (/ 0._rkind, (RJBSOS(i,3), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSS3',NISO1EFF1,1,aux)
  aux(:) = (/ 0._rkind, (RJBSOS(i,4), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOJBSS4',NISO1EFF1,1,aux)
  !
  ! NUSTAR (WITHOUT ZEFF)
  aux(:) = (/ 0._rkind, (RNUSTAR(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'RNUSTARS',NISO1EFF1,1,aux)
  !
  ! TRAPPED FRACTION S-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%ftrap(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTRS',NISO1EFF1,1,aux)
  !
  !  IDEAL MERCIER COEFFICIENT -DI(S), full formula
  aux(:) = (/ flinear(eqchease_out(1)%profiles_1d%rho_vol(2),eqchease_out(1)%profiles_1d%rho_vol(3),SMERCI(1),SMERCI(2),0._rkind), &
             (SMERCI(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZODIS',NISO1EFF1,1,aux)
  !
  !  IDEAL MERCIER COEFFICIENT -DI(S), L.A. with Elongation
  aux(:) = (/rdi(1), (rdi(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZODILAS',NISO1EFF1,1,aux)
  !
  !  IDEAL MERCIER COEFFICIENT -DI(S), Shafranov Yourchenko
  aux(:) = (/rsy(1), (rsy(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZORSYS',NISO1EFF1,1,aux)
  !
  !  RESISTIVE MERCIER COEFFICIENT -DR(S)
  aux(:) = (/SMERCR(1), (smercr(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZODRS',NISO1EFF1,1,aux)
  !
  !  H OF GLASSER-GREENE-JOHNSON S-PROFILE
  aux(:) = (/HMERCR(1), (hmercr(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZOHS',NISO1EFF1,1,aux)
  !
  !  P'(S)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%pprime(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOPPS',NISO1EFF1,1,aux)
  !
  !  P(S)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%pressure(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOPS',NISO1EFF1,1,aux)
  !
  !  TT'(S)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%ffprime(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTTS',NISO1EFF1,1,aux)
  !
  !  T(S)-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%F_dia(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTS',NISO1EFF1,1,aux)
  !  I*(S)-PROFILE
  !
  aux(:) = (/ (eqchease_out(1)%profiles_1d%jphi(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOIPS',NISO1EFF1,1,aux)
  !
  !  BETA POLOIDAL S-PROFILE
  aux(:) = (/(eqchease_out(1)%profiles_1d%beta_pol(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOBETS',NISO1EFF1,1,aux)
  !
  !  SQRT(VOLUME OF FLUX TUBE) S-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%rho_vol(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOARS',NISO1EFF1,1,aux)
  !
  !  POLOIDAL FLUX R-PROFILE
  aux(:) = (/ (eqchease_out(1)%profiles_1d%psi(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOFS',NISO1EFF1,1,aux)
  !
  !  Elongation
  aux(:) = (/ (eqchease_out(1)%profiles_1d%elongation(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOELONGS',NISO1EFF1,1,aux)
  !
  !  ELLIPTICITY
  aux(:) = (/ flinear(eqchease_out(1)%profiles_1d%rho_vol(2),eqchease_out(1)%profiles_1d%rho_vol(3),rell(1),rell(2),0._rkind), &
             (RELL(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZELLS',NISO1EFF1,1,aux)
  !
  !  d(ELLIPTICITY)/dr
  aux(:) = (/ flinear(eqchease_out(1)%profiles_1d%rho_vol(2),eqchease_out(1)%profiles_1d%rho_vol(3),rdedr(1),rdedr(2),0._rkind), &
             (RDEDR(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZDEDRS',NISO1EFF1,1,aux)
  !
  !  Lower Triangularity
  aux(:) = (/ (eqchease_out(1)%profiles_1d%tria_lower(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTRIALOWS',NISO1EFF1,1,aux)
  !
  !  Upper Triangularity
  aux(:) = (/ (eqchease_out(1)%profiles_1d%tria_upper(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOTRIAUPS',NISO1EFF1,1,aux)
  !
  !  Geometric center of flux tube
  aux(:) = (/ (eqchease_out_add_1d(i,iirgeo), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORGEOMS',NISO1EFF1,1,aux)
  !
  !  Inverse Aspect Ratio of Flux Tube
  aux(:) = (/ (eqchease_out_add_1d(i,iiamin)/eqchease_out_add_1d(i,iirgeo), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOASPCTS',NISO1EFF1,1,aux)
  !
  !  Area of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%area(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOAREAS',NISO1EFF1,1,aux)
  !
  !  Volume of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%volume(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOVOLS',NISO1EFF1,1,aux)
  !
  !  Normalized Volume of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%rho_vol(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORHOVOLS',NISO1EFF1,1,aux)
  !
  !  Volume-prime of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%vprime(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOVPRIMES',NISO1EFF1,1,aux)
  !
  !  rho_tor of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%rho_tor(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORHOTORS',NISO1EFF1,1,aux)
  !
  !  Normalized rho_tor of flux tube
  aux(:) = (/ 0._rkind, (CSMTOR(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZORHOTORNS',NISO1EFF1,1,aux)
  !
  !  rho_tor-prime of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%dvdrho(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORHOTORPRIMES',NISO1EFF1,1,aux)
  !
  !  dpsi/drho_tor/Rgeom(a) of flux tube
  aux(:) = (/ (eqchease_out(1)%profiles_1d%dpsidrho_tor(i)/eqchease_out_add_1d(NISO1EFF1,iirgeo), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZODPSIRHOTORS',NISO1EFF1,1,aux)
  !
  !  Ip Profile
  aux(:) = (/ (eqchease_out_add_1d(i,iiIplas), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOIPLS',NISO1EFF1,1,aux)
  !
  !  Li Profile
  aux(:) = (/ (eqchease_out(1)%profiles_1d%li(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOLIS',NISO1EFF1,1,aux)
  !
  !  Bmin Profile
  aux(:) = (/ (eqchease_out_add_1d(i,iiBmin), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOBMINS',NISO1EFF1,1,aux)
  !
  !  Bmax Profile
  aux(:) = (/ (eqchease_out_add_1d(i,iiBmax), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZOBMAXS',NISO1EFF1,1,aux)
  !
  !  Rmin Profile
  aux(:) = (/ (eqchease_out(1)%profiles_1d%r_inboard(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORMINS',NISO1EFF1,1,aux)
  !
  !  Rmax Profile
  aux(:) = (/ (eqchease_out(1)%profiles_1d%r_outboard(i), i=1,NISO1EFF1) /)
  call binwrtM(mp,'ZORMAXS',NISO1EFF1,1,aux)
  !
  !  S-VECTOR FOR PROFILES (LENGTH = NISO1EFF)
  aux(:) = (/0._rkind, (smiso(i), i=1,NISO1EFF) /)
  call binwrtM(mp,'ZABS',NISO1EFF1,1,aux)
  !
  deallocate(aux)
  !
  !  CONSTANT CHI LINES
  !
  allocate(ZRCHI(NPCHI,NPISOEFF),ZZCHI(NPCHI,NPISOEFF),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 5 =',state
  !
  !%OS         JSCHI = NCHI / 25 + 1
  JSCHI = 1
  do jj=1,nchi
     write(14,'(a,i5,a,i5,a)') ' jchi = ',jj,' ipsi=1,',niso1eff,':'
     write(14,'(1p2e13.5)') (cr(jj,j288),cz(jj,j288),j288=1,niso1eff)
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
  call binwrtI(mp,'INBCHI',1,1,INBCHI)
  !  R-MATRIX FOR ISOCHI LINES INTERSECTIONS WITH ISOPSI SURFACES
  call binwrtM(mp,'ZRCHI',INBCHI,NISO1EFF,ZRCHI)
  !  Z-MATRIX FOR ISOCHI LINES INTERSECTIONS WITH ISOPSI SURFACES
  call binwrtM(mp,'ZZCHI',INBCHI,NISO1EFF,ZZCHI)
  !
  deallocate(ZRCHI,ZZCHI)
  !
  !  BALLOONING AND MERCIER
  !
  allocate(IBALL(NPISOEFF),IMERCI(NPISOEFF),IMERCR(NPISOEFF),stat =state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 6 =',state
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
  call binwrtI(mp,'IBALL',NISO1EFF,1,IBALL)
  call binwrtI(mp,'NCBAL',NISO1EFF,1,NCBAL)

  !  IDEAL INTERCHANGE (IN)STABILITY FLAG: (1)0
  call binwrtI(mp,'IMERCI',NISO1EFF,1,IMERCI)

  !  RESISTIVE INTERCHANGE (IN)STABILITY FLAG: (1)0
  call binwrtI(mp,'IMERCR',NISO1EFF,1,IMERCR)
  deallocate(IBALL,IMERCI,IMERCR)
  !
  ! NCURV is evaluated in ERDATA, which is always called when NPLOT=1
  !
  allocate(aux(NCURV),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 7 =',state
  !
  !  R-VECTOR FOR ZERO-CURVATURE CURVE
  aux(:) = (/(RRCURV(i) - R0, i=1,NCURV) /)
  call binwrtM(mp,'ZRCURV',NCURV,1,aux)
  !
  !  Z-VECTOR FOR ZERO-CURVATURE CURVE
  aux(:) = (/(RZCURV(i) - RZ0, i=1,NCURV) /)
  call binwrtM(mp,'ZZCURV',NCURV,1,aux)
  !
  deallocate(aux)
  !
  call binwrtM(mp,'CSIG',NS1,1,CSIG)
  call binwrtM(mp,'SMISO',NISO1EFF,1,SMISO)
  !
  allocate(aux(NPCHI1),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 8 =',state
  !
  aux(:) = (/(CHI(i) - CHI(1), i=1,NCHI1) /)
  call binwrtM(mp,'ZCHI',NCHI1,1,aux)
  !
  !
  aux(:) = (/(REAL(i-1,RKIND) / REAL(NCHI,RKIND), i=1,NCHI1) /)
  call binwrtM(mp,'ZABIC',NCHI1,1,aux)
  !
  deallocate(aux)
  !
  ! NISO1EFF is linked to NPSI and not NISO, so NISO can be larger than NISO1EFF
  allocate(aux(NISO+1),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 9 =',state
  !
  aux(:) = (/0._rkind,(CSIPR(i), i=1,NISO) /)
  call binwrtM(mp,'CSIPR',NISO+1,1,aux)
  !
  aux(:) = (/(REAL(i-1,RKIND) / REAL(NISO,RKIND), i=1,NISO), 1._rkind /)
  call binwrtM(mp,'ZABIPR',NISO+1,1,aux)
  !
  deallocate(aux)
  !
  allocate(aux(NSP1),stat=state)
  if (state.ne.0) print*,'Allocation problem in WRTBIN ! stat 10 =',state
  !
  aux(:) = (/(REAL(i-1,RKIND) / REAL(NS,RKIND), i=1,NS1) /)
  call binwrtM(mp,'ZABISG',NS1,1,aux)
  !
  deallocate(aux)
  !
  call binwrtI(mp,'NMGAUS',1,1,NMGAUS)
  call binwrtI(mp,'NSGAUS',1,1,NSGAUS)
  call binwrtI(mp,'NTGAUS',1,1,NTGAUS)
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
  call binwrtI(mp,'NNBBPP',1,1,NNBBPP)
  !
  call binwrtM(mp,'RRISO',NNBBPP,NISO1EFF,RRISO)
  !  Z-MATRIX FOR ISOPSI-SURFACES
  call binwrtM(mp,'RZISO',NNBBPP,NISO1EFF,RZISO)
  !  LOCAL_SHEAR-MATRIX ON (CR,CZ) MESH
  call binwrtM(mp,'RSHEAR',NCHI,NISO1EFF,RSHEAR)
  call binwrtM(mp,'CR',NCHI,NISO1EFF,CR)
  call binwrtM(mp,'CZ',NCHI,NISO1EFF,CZ)
  !
  ! Restore (R,Z) coodinates
  !
  RRISO(:,1:NISO1EFF)=RRISO(:,1:NISO1EFF) + R0
  RZISO(:,1:NISO1EFF)=RZISO(:,1:NISO1EFF) + RZ0
  CR(:,:)=CR(:,:) + R0
  CZ(:,:)=CZ(:,:) + RZ0
  !
  call binwrtM(mp,'APLACE',size(APLACE),1,APLACE)
  call binwrtM(mp,'AWIDTH',size(AWIDTH),1,AWIDTH)
  call binwrtM(mp,'BPLACE',size(BPLACE),1,BPLACE)
  call binwrtM(mp,'BWIDTH',size(BWIDTH),1,BWIDTH)
  call binwrtM(mp,'CPLACE',size(CPLACE),1,CPLACE)
  call binwrtM(mp,'CWIDTH',size(CWIDTH),1,CWIDTH)
  call binwrtM(mp,'DPLACE',size(DPLACE),1,DPLACE)
  call binwrtM(mp,'DWIDTH',size(DWIDTH),1,DWIDTH)
  call binwrtM(mp,'EPLACE',size(EPLACE),1,EPLACE)
  call binwrtM(mp,'EWIDTH',size(EWIDTH),1,EWIDTH)
  call binwrtM(mp,'QPLACE',size(QPLACE),1,QPLACE)
  call binwrtM(mp,'QWIDTH',size(QWIDTH),1,QWIDTH)
  call binwrtM(mp,'QVALNEO',size(QVALNEO),1,QVALNEO)
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
END SUBROUTINE WRTBIN
subroutine binwrtI(mf,name,n,m,intvalue)
  !
  !                                        AUTHORS:
  !                                        R. Arslanbekov
  implicit none

  integer                 :: mf, n, m
  integer, dimension(n*m) :: intvalue
  character*(*)           :: name
  character(1)            :: info = '*'
  !
  write(mf) real(intvalue, 4)
  write(mf+1,'(A, 2X, A, 1X, I10, 1X, I10)') name, info, n, m
  !     
  return
end subroutine binwrtI

subroutine binwrtM(mf,name,n,m,realvalue)
  !
  !                                        AUTHORS:
  !                                        R. Arslanbekov
  implicit none

  integer                     :: mf, n, m
  character*(*)               :: name
  character(1)                :: info = '*'
  real(8), dimension(n*m) :: realvalue
  !     
  write(mf) real(realvalue,4)
  write(mf+1,'(A, 2X, A, 1X, I10, 1X, I10)') name, info, n, m
  !
  return
end subroutine binwrtM

subroutine binwrtC(mf,name,charvalue)
  !
  !                                        AUTHORS:
  !                                        R. Arslanbekov
  implicit none

  integer                 :: mf, n = 1, m = 1
  character*(*)           :: name
  character*(*)           :: charvalue
  !     
  write(mf) real(0._8,4)
  write(mf+1,'(A, 2X, A, 1X, I10, 1X, I10)') name, charvalue, n, m
  !
  return
end subroutine binwrtC

