!*DECK C1S03
!*CALL PROCESS
SUBROUTINE PRESET
  !        #################
  !
  !                                        AUTHORS:
  !                                        H. LUTJENS,  CRPP-EPFL
  !                                        A. BONDESON, CRPP-EPFL
  !
  !**********************************************************************
  !                                                                     *
  ! C1S02 SET UP THE DEFAULT CASE, I.E. A SOLOVEV EQUILIBRIUM (SEE      *
  !       SECTION 6.4.1 IN THE PUBLICATION)                             *
  !                                                                     *
  !**********************************************************************
  !
  !
  USE globals
  IMPLICIT NONE
  !
  !---*----*----*----*----*----*----*----*----*----*----*----*----*----*
  !
  !
  ! TAPE UNIT NUMBERS
  !
  INTEGER          ::     MDNEO
  REAL(RKIND)      ::     APLHA0
  INTEGER          ::     NCALL
  !
  !    SHOULD DEFINE ALL UNITS HERE TO MAKE SURE THERE IS NO OVERWRITE
  NRZPEL = 2
  MEQ    = 4
  NSAVE  = 8
  NPRNT  = 9
  NIN    = 10
  NOUT   = 11
  ! defined in chease_prog only         INXML = 12
  ! defined in chease_prog only         INAMELIST = 13
  NDES   = 16
  NVAC   = 17
  NO     = 21
  NOI    = 22
  NETVAC = 23
  NUPLO  = 33
  NUMAT  = 34
  NXTOR  = 36
  NXTORP = 37
  NELITE = 38
  NUEQDSK = 39
  NUEQDSKCOCOS = 40
  NUCIN  = 41
  NOUTGYRO = 42
  INP1   = 46
  JSOLVER = 47
  NXIN   = 48
  NPENN  = 49
  NXPQOUT = 50
  NXPQTNZ   = 51
  NXPQTOR = 52
  !
  ! NAMELIST VARIABLES
  !
  MDT    = 16
  MSMAX  = 1
  NANAL  = 0
  NBAL   = 1
  NBLC0  = 1
  NBLOPT = 0
  NBSEXPQ = 0
  NBSFUN = 1
  NBSOPT = 0
  NBPSOUT = MIN(300,NPBPS)
  NBSTRP = 1
  NCALL  = 0
  NCHI   = 100
  NCSCAL = 2
  NDIAGOP = 0
  NDIFPS = 1
  NDIFT  = 1
  NEGP   = -1
  NEQDSK = 0
  NEQDXTPO = 1
  NEQDZMG = 1
  NER    = 1
  NFFTOPT = 1
  NFUNC  = 1
  NFUNRHO = 0
  NIDEAL = 6
  NINMAP = 30
  NINSCA = 30
  NIPR   = 1
  NMESHA = 0
  NMESHB = 0
  NMESHC = 0
  NMESHD = 0
  NMESHE = 0
  ! New mesh packing using rho(theta)
  ! Setting NMESHPOL=1 overrides by default nmeshd and nmeshe in AUXVAL. So just set it to 1 in namelist
  ! NMESHPOL uses NMESHPOLEXP and SOLPDPOL which are already set to good defaults (6 and 25%)
  ! Leads to using NEMSHD=2 and NMESHE=2
  NMESHPOL = 0 ! if =1
  NMESHPOLEXP = 6
  NMGAUS = 4
  NISO   = 100
  NPISOEFF = 100
  NOPT   = 0
  NOUTXTOR = 0
  NPLOT  = 0
  NPOIDA = 0
  NPOIDB = 0
  NPOIDC = 0
  NPOIDD = 0
  NPOIDE = 0
  NPOIDQ = 0
  NPOPULATIONS = 1
  NPP    = 1
  NPPFUN = 1
  NPPR   = 30
  NPROF2d = 1 ! creates profiles_2d(2) or more if mod(nitmopt,10)>=1
  NPROFZ = 0
  NPROPT = 2 ! Istar in EXPEQ.OUT
  NPRPSI = 0
  NPSI   = 100
  NRBOX  = 101
  NRBOX_XTOR = 401
  NRFP   = 0
  NRSCAL = 0
  NS     = 40
  NSGAUS = 4
  NSMOOTH= 1
  NSOUR  = 2
  NSTTP  = 1
  NSURF  = 1
  NSYM   = 1
  NT     = 40
  NTCASE = 0
  NTGAUS = 4
  NTEST  = 0
  NTMF0  = 0
  NTNOVA = 64
  NTURN  = 10
  NV     = 60
  NVEXP  = 0
  NZBOX  = 65
  NZBOX_XTOR = 401
  !
  APLHA0    = 0._RKIND
  AP(1)     = 0.1_RKIND
  AP(2)     = 0.5_RKIND
  AP2(1)    = 0.1_RKIND
  AP2(2)    = 0.5_RKIND
  ASPCT     = .33333333333333_RKIND
  AT(1)     = 0.1_RKIND
  AT(2)     = 0.5_RKIND
  AT2(1)    = 0.1_RKIND
  AT2(2)    = 0.5_RKIND
  AT3(1)    = 0.1_RKIND
  AT3(2)    = 0.5_RKIND
  AT4(1)    = 0.1_RKIND
  AT4(2)    = 0.5_RKIND
  BEANS     = 0._RKIND
  BSFRAC    = 0.5_RKIND
  B0EXP     = 1.0_RKIND
  CETA      = 0._RKIND
  CFBAL     = 1._RKIND
  CFNRESS   = 1._RKIND
  CFNRESSO  = 1._RKIND
  CPRESS    = 1._RKIND
  CPRESSO   = 1._RKIND
  CQ0       = 0.75_RKIND
  CSSPEC    = 0._RKIND
  CTORSRF   = 1._RKIND
  CURRT     = 0.5_RKIND
  DELTA     = 0._RKIND
  ELONG     = 1._RKIND
  EPSLON    = 1.E-10_RKIND
  ETAEI     = 1.5_RKIND
  GAMMA     = 5._RKIND / 3._RKIND
  NRHOMESH  = 0
  PANGLE    = 0._RKIND
  PSISCL    = 1._RKIND
  QSPEC     = 1._RKIND
  PREDGE    = 0._RKIND
  RBOXLEN   = 0._RKIND
  RBOXLFT   = 0._RKIND
  RC        = 1._RKIND
  R0        = 1._RKIND
  R0EXP     = 1.0_RKIND
  R0W       = 1._RKIND
  RELAX     = 0._RKIND
  REXT      = 1._RKIND
  RNU       = 0._RKIND
  RODABYROD0= 1._RKIND
  RPEOP     = 0.5_RKIND
  RZION     = 1._RKIND
  RZ0       = 0._RKIND
  RZ0C      = 0._RKIND
  RZ0W      = 0._RKIND
  SCALE     = 1._RKIND
  SCALAC    = 1._RKIND
  SCALNE    = 0._RKIND
  SCEXP     = 1._RKIND
  SGMA      = 0._RKIND
  SLIMIT    = 1.E5_RKIND
  SNUMBER   = 1.E7_RKIND
  SOLPDA    = 0._RKIND
  SOLPDB    = 0._RKIND
  SOLPDC    = 0._RKIND
  SOLPDD    = 0._RKIND
  SOLPDE    = 0._RKIND
  SOLPDPOL  = 0.25_RKIND
  SHIFT_P   = 0._RKIND
  TENSPROF  = -0.3_RKIND
  TENSBND   = -0.3_RKIND
  TRIANG    = 0._RKIND
  TRIPLT    = 0._RKIND
  XI        = 0._RKIND
  ZBOXLEN   = 0._RKIND
  !
  !     EQDSK RELATED VARIABLES, EVENTUALLY SET DEFAULT WALL
  !
  NWALLPOS = 0
!!$         DO I=1,NPBPS
!!$           WALLPOSR(I) = 0._RKIND
!!$           WALLPOSZ(I) = 0._RKIND
!!$         END DO
  !
  ! AUXILIARY VARIABLES
  !
  NSMAX  = 1
  NWGAUS = NSGAUS * NTGAUS
  !
  CPI    = PI
  RC0P   = 0.E0_RKIND
  RC1P   = 1.E0_RKIND
  RC2P   = 2.E0_RKIND
  RC2PI  = CPI + CPI
  !
  !        EPNON0 IS MAXIMUM ERROR IN NONLINEAR ITERATION ON SMALL MESH
  !
  EPNON0 = 1.E-6_RKIND
  !
  !        EZMAG IS MAXIMUM ERROR IN ZMAG FOR UPDOWN SYMMETRIC CASE
  !        BEFORE RELAXATION PARAMETER RELAX IS INCREASED
  !
  EZMAG = 1.E-4_RKIND
  !
  !        MACHINEDEPENDENT NUMBERS BELOW
  !
  RC1M14 = 1.E-14_RKIND
  EPSMCH = 1.E-30_RKIND
  RC1M13 = 10.E0_RKIND*RC1M14
  RC1M12 = 100.E0_RKIND*RC1M14
  !
  !     NEOCLASSICAL STUFF AT GIVEN RATIONAL SURFACES
  !
  !     I=1 => Q_AXIS
  QVALNEO(2) = 1.0_RKIND
  QVALNEO(3) = 1.5_RKIND
  QVALNEO(4) = 2.0_RKIND
  QVALNEO(5) = 3.0_RKIND
  QVALNEO(6) = 4.0_RKIND
  !     I=NEONBQS => Q_EDGE
  NEONBQS = 7
  MDNEO = 50
  !
  RETURN
END SUBROUTINE PRESET
