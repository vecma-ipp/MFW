MODULE globals
  !
  !   Global vars and arrays
  !
  USE prec_const
  use euitm_schemas                       ! module containing the equilibrium type definitions
  IMPLICIT NONE
  !
!!! Parameters, Dimension constants
  !
  INTEGER, PARAMETER :: mflgdiag1 = 1
  INTEGER, PARAMETER :: npbps = 2300
  INTEGER, PARAMETER :: nppsbal = 10
  INTEGER, PARAMETER :: nppscub = 10
  INTEGER, PARAMETER :: npdianeo = 60
  INTEGER, PARAMETER :: npoutflg = 15
  !
!!! Init array dimensions
  !
  INTEGER, SAVE :: mflgerl = 0
  INTEGER, SAVE :: mflgmar = 0
  INTEGER, SAVE :: mflgnvw = 0
  INTEGER, SAVE :: mflgpen = 0
  INTEGER, SAVE :: nps = 40
  INTEGER, SAVE :: npt = 40
  INTEGER, SAVE :: npchi = 121
  INTEGER, SAVE :: nppsi = 121
  INTEGER, SAVE :: npchi1 =122 ! npchi+1
  INTEGER, SAVE :: nppsi1 =122 ! nppsi+1
  !
  ! Other integers
  !
  ! indices in eqchease_out_add_1d array
  INTEGER, SAVE ::                                            iirgeo       =1
  INTEGER, SAVE ::                                            iiamin       =2
  INTEGER, SAVE ::                                            iidqdpsi     =3
  INTEGER, SAVE ::                                            iid2qdpsi2   =4
  INTEGER, SAVE ::                                            iishear      =5
  INTEGER, SAVE ::                                            iidsheardpsi =6
  INTEGER, SAVE ::                                            iigradpsi_av =7
  INTEGER, SAVE ::                                            iia_av       =8
  INTEGER, SAVE ::                                            iiR_av       =9
  INTEGER, SAVE ::                                            iiBmax       =10
  INTEGER, SAVE ::                                            iiBmin       =11
  INTEGER, SAVE ::                                            iialpha      =12
  INTEGER, SAVE ::                                            iiIplas      =13
  INTEGER, SAVE ::                                            iiTe         =14
  INTEGER, SAVE ::                                            iidTedpsi    =15
  INTEGER, SAVE ::                                            iine         =16
  INTEGER, SAVE ::                                            iidnedpsi    =17
  INTEGER, SAVE ::                                            iiTi         =18
  INTEGER, SAVE ::                                            iidTidpsi    =19
  INTEGER, SAVE ::                                            iini         =20
  INTEGER, SAVE ::                                            iidnidpsi    =21
  INTEGER, SAVE ::                                            iizeff       =22
  INTEGER, SAVE ::                                            iinuestar    =23
  INTEGER, SAVE ::                                            iisigneo     =24
  INTEGER, SAVE ::                                            iijbsBav     =25
  ! indices in eqchease_out_add_2d array
  INTEGER, SAVE ::                                            iiB          =1
  INTEGER, SAVE ::                                            iidBdpsi     =2
  INTEGER, SAVE ::                                            iidBdchi     =3
  INTEGER, SAVE ::                                            iidpsidR     =4
  INTEGER, SAVE ::                                            iidpsidZ     =5
  INTEGER, SAVE ::                                            iidchidR     =6
  INTEGER, SAVE ::                                            iidchidZ     =7
  INTEGER, SAVE ::                                            iidRdpsi     =8
  INTEGER, SAVE ::                                            iidRdchi     =9
  INTEGER, SAVE ::                                            iidZdpsi     =10
  INTEGER, SAVE ::                                            iidZdchi     =11
  INTEGER, SAVE ::                                            iiAh         =12
  INTEGER, SAVE ::                                            iidAhdpsi    =13
  INTEGER, SAVE ::                                            iitheta      =14 ! change allocate in mappin.f90 if increased
  ! indices in eqchease_out_add_2d_RZ array (allocated with (NRBOX,NZBOX)
  INTEGER, SAVE ::                                            iiRZ_chi      =1
  !
  INTEGER, SAVE ::                                            COCOS_IN     = 2
  INTEGER, SAVE ::                                            COCOS_OUT    = 2
  INTEGER, SAVE ::                                            inp1         = 0
  INTEGER, SAVE ::                                            JSOLVER      = 0
  INTEGER, SAVE ::                                            mdt          = 0       ! HL for XTOR 
  INTEGER, SAVE ::                                            meq          = 0
  INTEGER, DIMENSION( :,:,:), ALLOCATABLE, SAVE ::            mpla
  INTEGER, SAVE ::                                            mpsmax = 0
  INTEGER, SAVE ::                                            msmax        = 0
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                n2bal
  INTEGER, SAVE ::                                            n4nsnt       = 0
  INTEGER, SAVE ::                                            n4nt = 0
  INTEGER, SAVE ::                                            n8nt = 0
  INTEGER, SAVE ::                                            n16nt = 0
  INTEGER, SAVE ::                                            nanal
  INTEGER, SAVE ::                                            nbal        = 0
  INTEGER, SAVE ::                                            nband       = 0
  INTEGER, SAVE ::                                            nblc0       = 0
  INTEGER, SAVE ::                                            nblopt      = 0
  INTEGER, SAVE ::                                            nbps        = 0
  INTEGER, SAVE ::                                            nbpsout     = 0
  INTEGER, SAVE ::                                            nbsexpq     = 0
  INTEGER, SAVE ::                                            nbsfun      = 0
  INTEGER, SAVE ::                                            nbsopt      = 0
  INTEGER, SAVE ::                                            nbstrp      = 0
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                ncbal
  INTEGER, DIMENSION( :,:), ALLOCATABLE, SAVE ::              ncblns
  INTEGER, SAVE ::                                            nchi        = 0
  INTEGER, SAVE ::                                            nchi1       = 0
  INTEGER, SAVE ::                                            nchieff     = 0
  INTEGER, SAVE ::                                            ncon        = 0
  INTEGER, SAVE ::                                            ncscal      = 0
  INTEGER, SAVE ::                                            ncurv       = 0
  INTEGER, SAVE ::                                            ndeq = 0
  INTEGER, SAVE ::                                            ndes        = 0
  INTEGER, SAVE ::                                            ndiaglas    = 0
  INTEGER, SAVE ::                                            ndiagop     = 0
  INTEGER, SAVE ::                                            ndiaperq    = 0
  INTEGER, SAVE ::                                            ndifps      = 0
  INTEGER, SAVE ::                                            ndift       = 0
  INTEGER, DIMENSION(5), SAVE ::                              ndim        = 0
  INTEGER, SAVE ::                                            negp        = 0
  INTEGER, SAVE ::                                            nelite      = 0
  INTEGER, SAVE ::                                            neonbglo    = 0
  INTEGER, SAVE ::                                            neonbqs     = 0
  INTEGER, SAVE ::                                            neonbval    = 0
  INTEGER, SAVE ::                                            neqdsk      = 0
  INTEGER, SAVE ::                                            neqdxtpo    = 0
  INTEGER, SAVE ::                                            neqdzmg     = 0
  INTEGER, SAVE ::                                            ner         = 0
  INTEGER, SAVE ::                                            netvac      = 0
  INTEGER, SAVE ::                                            nf          = 0
  INTEGER, SAVE ::                                            nfftopt     = 0
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                nflgaddia
  INTEGER, SAVE ::                                            nfourpb     = 0
  INTEGER, SAVE ::                                            nfunc       = 0
  INTEGER, SAVE ::                                            nfixwall    = 1
  INTEGER, SAVE ::                                            nfunrho     = 0
  INTEGER, SAVE ::                                            ni          = 0
  INTEGER, SAVE ::                                            nideal      = 6
  INTEGER, SAVE ::                                            nin         = 0
  INTEGER, SAVE ::                                            ninmap      = 0
  INTEGER, SAVE ::                                            ninsca      = 0
  INTEGER, SAVE ::                                            nipr        = 0
  INTEGER, SAVE ::                                            nis2 = 0
  INTEGER, SAVE ::                                            niso        = 0
  INTEGER, SAVE ::                                            nisoo       = 0
  INTEGER, SAVE ::                                            niso1eff    = 0
  INTEGER, SAVE ::                                            niso1eff1   = 0
  INTEGER, SAVE ::                                            nitmopt     = 22
  INTEGER, DIMENSION(2), SAVE ::                              nitmrun     = -1
  INTEGER, DIMENSION(2), SAVE ::                              nitmshot    = -1
  INTEGER, SAVE ::                                            nmag        = 0
  INTEGER, SAVE ::                                            nmesha      = 0
  INTEGER, SAVE ::                                            nmeshb      = 0
  INTEGER, SAVE ::                                            nmeshc      = 0
  INTEGER, SAVE ::                                            nmeshd      = 0
  INTEGER, SAVE ::                                            nmeshe      = 0
  INTEGER, SAVE ::                                            nmeshpol    = 0
  INTEGER, SAVE ::                                            nmeshpolexp = -1
  INTEGER, SAVE ::                                            nmgaus      = 0
  INTEGER, SAVE ::                                            no          = 0
  INTEGER, SAVE ::                                            noi         = 0
  INTEGER, SAVE ::                                            nopt        = 0
  INTEGER, SAVE ::                                            nout        = 0
  INTEGER, SAVE ::                                            noutgyro    = 0
  INTEGER, SAVE ::                                            noutxtor    = 0
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np0
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np1
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np2
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np3
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np4
  INTEGER, SAVE ::                                            np4nst = 0
  INTEGER, SAVE ::                                            npband = 0
  INTEGER, SAVE ::                                            npblc0 = 16
  INTEGER, SAVE ::                                            npdiag1 = 0
  INTEGER, SAVE ::                                            npenn       = 0
  INTEGER, SAVE ::                                            npgaus = 0
  INTEGER, SAVE ::                                            npisoeff = 0
  INTEGER, DIMENSION( :,:), ALLOCATABLE, SAVE ::              nplac
  INTEGER, SAVE ::                                            nplot       = 0
  INTEGER, SAVE ::                                            npmgs = 4
  INTEGER, SAVE ::                                            npoida      = 0
  INTEGER, SAVE ::                                            npoidb      = 0
  INTEGER, SAVE ::                                            npoidc      = 0
  INTEGER, SAVE ::                                            npoidd      = 0
  INTEGER, SAVE ::                                            npoide      = 0
  INTEGER, SAVE ::                                            npoidq      = 0
  INTEGER, SAVE ::                                            npopulations= 1
  INTEGER, SAVE ::                                            npp         = 0
  INTEGER, SAVE ::                                            nppf        = 0
  INTEGER, SAVE ::                                            nppfun      = 0
  INTEGER, SAVE ::                                            nppr        = 0
  INTEGER, SAVE ::                                            nppsnvw = 0
  INTEGER, SAVE ::                                            nprnt       = 0
  INTEGER, SAVE ::                                            nprof2d     = 1
  INTEGER, SAVE ::                                            nprofz      = 0
  INTEGER, SAVE ::                                            npropt      = 0
  INTEGER, SAVE ::                                            nprpsi      = 0
  INTEGER, SAVE ::                                            npsgs = 4
  INTEGER, SAVE ::                                            npsi        = 0
  INTEGER, SAVE ::                                            npsi1       = 0
  INTEGER, SAVE ::                                            npsmax = 0
  INTEGER, SAVE ::                                            nptgs = 4
  INTEGER, SAVE ::                                            npv = 41
  INTEGER, SAVE ::                                            npv1 = 0
  INTEGER, SAVE ::                                            nrbox       = 0
  INTEGER, DIMENSION(10), SAVE ::                             nrbox_xtor  = 0
  INTEGER, SAVE ::                                            npturn = 20
  INTEGER, SAVE ::                                            nrfp        = 0
  INTEGER, SAVE ::                                            nrhomesh    = 0
  INTEGER, SAVE ::                                            nrscal      = 0
  INTEGER, SAVE ::                                            nrzpel      = 0
  INTEGER, SAVE ::                                            ns          = 0
  INTEGER, SAVE ::                                            ns1         = 0
  INTEGER, SAVE ::                                            nsave       = 0
  INTEGER, SAVE ::                                            nsgaus      = 0
  INTEGER, SAVE ::                                            nsmax       = 0
  INTEGER, SAVE ::                                            nsmooth     = 0
  INTEGER, SAVE ::                                            nsnt = 0
  INTEGER, SAVE ::                                            nso         = 0
  INTEGER, SAVE ::                                            nsour       = 0
  INTEGER, SAVE ::                                            nsp1 = 0
  INTEGER, SAVE ::                                            nsrch       = 0
  INTEGER, SAVE ::                                            nstmax      = 0
  INTEGER, SAVE ::                                            nsttp       = 0
  INTEGER, SAVE ::                                            nsurf       = 0
  INTEGER, SAVE ::                                            nsym        = 0
  INTEGER, SAVE ::                                            nt          = 0
  INTEGER, SAVE ::                                            nt1         = 0
  INTEGER, SAVE ::                                            nt2         = 0
  INTEGER, SAVE ::                                            ntcase      = 0
  INTEGER, SAVE ::                                            ntest       = 0
  INTEGER, SAVE ::                                            ntgaus      = 0
  INTEGER, SAVE ::                                            ntmf0       = 0
  INTEGER, SAVE ::                                            ntnova      = 0
  INTEGER, SAVE ::                                            ntnzpt      = 0
  INTEGER, SAVE ::                                            nto         = 0
  INTEGER, SAVE ::                                            ntp1 = 0
  INTEGER, SAVE ::                                            ntp2 = 0
  INTEGER, SAVE ::                                            nturn       = 0
  INTEGER, SAVE ::                                            NUCIN       = 0
  INTEGER, SAVE ::                                            NUEQDSK     = 0
  INTEGER, SAVE ::                                            NUEQDSKCOCOS = 0
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                nupdwn
  INTEGER, SAVE ::                                            nuplo       = 0
  INTEGER, SAVE ::                                            numat       = 0
  INTEGER, SAVE ::                                            nv          = 0
  INTEGER, SAVE ::                                            nv1         = 0
  INTEGER, SAVE ::                                            nvac        = 0
  INTEGER, SAVE ::                                            nvexp       = 0
  INTEGER, SAVE ::                                            nwallpos    = 0
  INTEGER, SAVE ::                                            nwgaus      = 0
  INTEGER, SAVE ::                                            nxin        = 0
  INTEGER, SAVE ::                                            NXPQOUT     = 0
  INTEGER, SAVE ::                                            NXPQTNZ     = 0
  INTEGER, SAVE ::                                            NXPQTOR     = 0
  INTEGER, SAVE ::                                            nxtor       = 0
  INTEGER, SAVE ::                                            nxtorp      = 0
  INTEGER, SAVE ::                                            nzbox       = 0
  INTEGER, SAVE ::                                            NVERBOSE    = 2
  INTEGER, DIMENSION(10), SAVE ::                             nzbox_xtor  = 0
  !
  ! LOGICALS
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp1
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp2
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp3
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp4
!!! Static vars and arrays
  !
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          a
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        abal
  REAL(RKIND), DIMENSION(15), SAVE ::                         afbs        = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         afbs2       = 0._RKIND
  REAL(RKIND), DIMENSION(:,:),   ALLOCATABLE, SAVE ::         ah
  REAL(RKIND), DIMENSION(:,:),   ALLOCATABLE, SAVE ::         ahpr
  REAL(RKIND), DIMENSION(:,:),   ALLOCATABLE, SAVE ::         ahprcor
  REAL(RKIND), SAVE ::                                        alzero      = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         ap          = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         ap2         = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         aplace      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            aratio
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            areacsm
  REAL(RKIND), SAVE ::                                        area        = 0._RKIND
  REAL(RKIND), SAVE ::                                        aspct       = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         at          = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         at2         = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         at3         = 0._RKIND
  REAL(RKIND), DIMENSION(15), SAVE ::                         at4         = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         awidth      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            b
  REAL(RKIND), SAVE ::                                        b0exp       = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          bchin
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          bchio
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bchiso
  REAL(RKIND), SAVE ::                                        beans       = 0._RKIND
  REAL(RKIND), SAVE ::                                        bentaxis    = 1.015_RKIND
  REAL(RKIND), SAVE ::                                        bentradius  = 0.5_RKIND
  REAL(RKIND), SAVE ::                                        bentqprofile= 0._RKIND
  REAL(RKIND), SAVE ::                                        beta        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            betab
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            betapcsm
  REAL(RKIND), SAVE ::                                        betap       = 0._RKIND
  REAL(RKIND), SAVE ::                                        betas       = 0._RKIND
  REAL(RKIND), SAVE ::                                        betax       = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          bndiso
  REAL(RKIND), DIMENSION(10), SAVE ::                         bplace      = 0._RKIND
  REAL(RKIND), DIMENSION(14), SAVE ::                         bps         = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bpscos
  REAL(RKIND), DIMENSION(14), SAVE ::                         bpso        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bpssin
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2ne
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2te
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2ti
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2zef
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsdense
  REAL(RKIND), SAVE ::                                        bsfrac      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bstempe
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bstempi
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bszeff
  REAL(RKIND), DIMENSION(10), SAVE ::                         bwidth      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cdq
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cdrq
  REAL(RKIND), SAVE ::                                        ceps        = 0._RKIND
  REAL(RKIND), SAVE ::                                        ceta        = 0._RKIND
  REAL(RKIND), SAVE ::                                        cfbal       = 0._RKIND
  REAL(RKIND), SAVE ::                                        cfnress     = 0._RKIND
  REAL(RKIND), SAVE ::                                        cfnresso    = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chi
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chi0
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chiiso
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chim
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          chin
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          chio
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chiold
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid0
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid0o
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid2o
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidq
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidrtor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidrtoro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cipr
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnr1
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnr2
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnz1
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnz2
  REAL(RKIND), SAVE ::                                        convf       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cp
  REAL(RKIND), SAVE ::                                        cp0         = 0._RKIND
  REAL(RKIND), SAVE ::                                        cpbar       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpdp
  REAL(RKIND), SAVE ::                                        cpdp0       = 0._RKIND
  REAL(RKIND), SAVE ::                                        cpi         = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         cplace      = 0._RKIND
  REAL(RKIND), SAVE ::                                        cpp         = 0._RKIND
  REAL(RKIND), SAVE ::                                        cppf        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cppr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cppro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpr
  REAL(RKIND), SAVE ::                                        cpress      = 0._RKIND
  REAL(RKIND), SAVE ::                                        cpresso     = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsi
  REAL(RKIND), SAVE ::                                        cpsi0       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsi1t
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsicl
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsili
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsio
  REAL(RKIND), SAVE ::                                        cpsrf       = 0._RKIND
  REAL(RKIND), SAVE ::                                        cq0         = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cs
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csig
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csigo
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csipr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csipri
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csipro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csm
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csmtor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csmv
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cspen
  REAL(RKIND), SAVE                             ::            csqmin
  REAL(RKIND), SAVE                             ::            csspec      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csv
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ct
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cto
  REAL(RKIND), SAVE ::                                        ctorsrf     = 1._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ctpen
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ctxt
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ctxt_refined
  REAL(RKIND), SAVE ::                                        curold      = 0._RKIND
  REAL(RKIND), SAVE ::                                        currt       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cw
  REAL(RKIND), DIMENSION(10), SAVE ::                         cwidth      = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cz
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid0
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid0o
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid2o
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidq
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidrtor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidrtoro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cipr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cppr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cppro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cpr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2pstn
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2pstt
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rbps
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rfun
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rppf
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rprm
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2tmf
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2tmfo
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2ttp
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2zbps
  REAL(RKIND), SAVE ::                                        delta       = 0._RKIND
  REAL(RKIND), SAVE ::                                        dens0       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            densty
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dgniso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dgriso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dgziso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          diagars
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffds
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffdt
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffp
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffst
  REAL(RKIND), SAVE ::                                        dpdp0       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdsnu
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdsth
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdtnu
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdtth
  REAL(RKIND), DIMENSION(10), SAVE ::                         dplace      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dprime
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dpriso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dpsiso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dptiso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dpziso
  REAL(RKIND), SAVE ::                                        dqdp0       = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          drhopi
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          drniso
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            drsdt
  REAL(RKIND), SAVE ::                                        dttp0       = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         dwidth      = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dzeta
  REAL(RKIND), SAVE ::                                        elong       = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         eplace      = 0._RKIND
  REAL(RKIND), SAVE ::                                        epnon0      = 0._RKIND
  REAL(RKIND), SAVE ::                                        epslon      = 0._RKIND
  REAL(RKIND), SAVE ::                                        epsmch      = 0._RKIND
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        eq
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq13
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq22
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq24
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq3
  !
  REAL(RKIND), DIMENSION(:,:),   ALLOCATABLE, SAVE ::         eqchease_out_add_1d
  REAL(RKIND), DIMENSION(:,:,:), ALLOCATABLE, SAVE ::         eqchease_out_add_2d
  REAL(RKIND), DIMENSION(:,:,:), ALLOCATABLE, SAVE ::         eqchease_out_add_2d_rz
  ! varnames defined in mappin.f90 once allocated
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eqdspsi
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eqi
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eql
  REAL(RKIND), SAVE ::                                        etaei       = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         ewidth      = 0._RKIND
  REAL(RKIND), SAVE ::                                        ezmag       = 0._RKIND
  REAL(RKIND), DIMENSION( :,:,:,:), ALLOCATABLE, SAVE ::      fb
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            fcsm
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            fcsmtnz
  REAL(RKIND), DIMENSION(:,:),   ALLOCATABLE, SAVE ::         fm
  REAL(RKIND), SAVE ::                                        gamma       = 0._RKIND
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        gammaparxt
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            globneo
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          gpiso
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            hmercr
  REAL(RKIND), SAVE ::                                        pangle      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            pcs
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            pcsm
  REAL(RKIND), SAVE ::                                        predge      = 0._RKIND
  REAL(RKIND), SAVE ::                                        psi0        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            psiiso
  REAL(RKIND), SAVE ::                                        psiscl      = 0._RKIND
  REAL(RKIND), SAVE ::                                        q0          = 0._RKIND
  REAL(RKIND), SAVE ::                                        q95         = 0._RKIND
  REAL(RKIND), SAVE ::                                        qcyl        = 0._RKIND
  REAL(RKIND), SAVE ::                                        qmin        = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         qplace      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            qpsi
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            qpsiin
  REAL(RKIND), SAVE ::                                        qspec       = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         qvalneo     = 0._RKIND
  REAL(RKIND), DIMENSION(10), SAVE ::                         qwidth      = 0._RKIND
  REAL(RKIND), SAVE ::                                        r0          = 0._RKIND
  REAL(RKIND), SAVE ::                                        r0exp       = 0._RKIND
  REAL(RKIND), SAVE ::                                        r0o         = 0._RKIND
  REAL(RKIND), SAVE ::                                        r0w         = 0._RKIND
  REAL(RKIND), SAVE ::                                        r0wo        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rare
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rb2av
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rbdiag
  REAL(RKIND), SAVE ::                                        rboxlen     = -1._RKIND
  REAL(RKIND), SAVE ::                                        rboxlft     = -1._RKIND
  REAL(RKIND), SAVE ::                                        rboxlen_in  = 0._RKIND
  REAL(RKIND), SAVE ::                                        rboxlft_in  = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rbpol0
  REAL(RKIND), SAVE ::                                        rc          = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc0p        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc1m12      = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc1m13      = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc1m14      = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc1p        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc2p        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rc2pi       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rdedr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rdi
  REAL(RKIND), SAVE ::                                        relax       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rell
  REAL(RKIND), SAVE ::                                        resdps      = 0._RKIND
  REAL(RKIND), SAVE ::                                        resdpt      = 0._RKIND
  REAL(RKIND), SAVE ::                                        resdst      = 0._RKIND
  REAL(RKIND), SAVE ::                                        residu      = 0._RKIND
  REAL(RKIND), SAVE ::                                        resmap      = 0._RKIND
  REAL(RKIND), SAVE ::                                        respsi      = 0._RKIND
  REAL(RKIND), SAVE ::                                        rext        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rfcirc
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rfpbp
  REAL(RKIND), SAVE ::                                        rfpf        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rfpt        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rfun
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rhoiso
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        rhoparxt
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rhos
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rhovac
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rhovaci
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rhovacm
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rib
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rib2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ribi2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rir2
  REAL(RKIND), SAVE ::                                        ribsnor     = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            riie
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            riir
  REAL(RKIND), SAVE ::                                        rinor       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rip
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rip2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ripr
  REAL(RKIND), SAVE ::                                        ripr0       = 0._RKIND
  REAL(RKIND), SAVE ::                                        ritbs       = 0._RKIND
  REAL(RKIND), SAVE ::                                        ritbsc      = 0._RKIND
  REAL(RKIND), SAVE ::                                        ritbsc2      = 0._RKIND
  REAL(RKIND), SAVE ::                                        ritbsc3      = 0._RKIND
  REAL(RKIND), SAVE ::                                        ritot       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rivol
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj1
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj3
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj4
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj5
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj5p
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj6
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rj7s
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rjbsos
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rjdotb
  REAL(RKIND), SAVE ::                                        rjdtb0      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rjpar
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rleng
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rleng1
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rm
  REAL(RKIND), SAVE ::                                        rmag        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rmago       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rn
  REAL(RKIND), SAVE ::                                        rnu         = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rnustar
  REAL(RKIND), SAVE ::                                        rodabyrod0  = 1._RKIND
  REAL(RKIND), SAVE ::                                        rpeop       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rppf
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rprm
  REAL(RKIND), SAVE ::                                        rraxis      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rrbps
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rrbpsou
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rrcurv
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rriso
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rshear
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rsint
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rsy
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rtint
  REAL(RKIND), SAVE ::                                        rz0         = 0._RKIND
  REAL(RKIND), SAVE ::                                        rz0c        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rz0o        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rz0w        = 0._RKIND
  REAL(RKIND), SAVE ::                                        rz0wo       = 0._RKIND
  REAL(RKIND), SAVE ::                                        rzaxis      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rzbps
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rzbpsou
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rzcurv
  REAL(RKIND), SAVE ::                                        rzion       = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rziso
  REAL(RKIND), SAVE ::                                        rzmag       = 0._RKIND
  REAL(RKIND), SAVE ::                                        rzmago      = 0._RKIND
  REAL(RKIND), SAVE ::                                        rzmgeqd     = 0._RKIND
  REAL(RKIND), SAVE ::                                        scalac      = 0._RKIND
  REAL(RKIND), SAVE ::                                        scale       = 0._RKIND
  REAL(RKIND), SAVE ::                                        scalne      = 0._RKIND
  REAL(RKIND), SAVE ::                                        scexp       = 0._RKIND
  REAL(RKIND), SAVE ::                                        scheck      = 0._RKIND
  REAL(RKIND), SAVE ::                                        sgma        = 0._RKIND
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          sigchi
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          sigmap
  REAL(RKIND), SAVE ::                                        signb0xp    = 1._RKIND
  REAL(RKIND), SAVE ::                                        signipxp    = 1._RKIND
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        sigparxt
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            sigpen
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          sigpsi
  REAL(RKIND), SAVE ::                                        slimit      = 0._RKIND    !HL for xtor
  REAL(RKIND), SAVE ::                                        smag        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            smerci
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            smercr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            smiso
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            smisop1
  REAL(RKIND), SAVE ::                                        snumber     = 0._RKIND    !HL for xtor
  REAL(RKIND), SAVE ::                                        solpda      = 0._RKIND
  REAL(RKIND), SAVE ::                                        solpdb      = 0._RKIND
  REAL(RKIND), SAVE ::                                        solpdc      = 0._RKIND
  REAL(RKIND), SAVE ::                                        solpdd      = 0._RKIND
  REAL(RKIND), SAVE ::                                        solpde      = 0._RKIND
  REAL(RKIND), SAVE ::                                        solpdpol    = 0._RKIND
  REAL(RKIND), SAVE ::                                        shift_p     = 0._RKIND    !HL for xtor
  REAL(RKIND), SAVE ::                                        spsi0       = 0._RKIND
  REAL(RKIND), SAVE ::                                        spsim       = 0._RKIND
  REAL(RKIND), SAVE ::                                        spsimo      = 0._RKIND
  REAL(RKIND), SAVE ::                                        stime
  REAL(RKIND), SAVE ::                                        t0          = 0._RKIND
  REAL(RKIND), SAVE ::                                        temp0       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            temper
  REAL(RKIND), SAVE ::                                        tensprof    = 0._RKIND
  REAL(RKIND), SAVE ::                                        tensbnd     = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetbps
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetchi, tetchi_sorted
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetmap
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetpen
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        tetparxt
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetpsi
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetvac
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetvaci
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetvacm
  REAL(RKIND), SAVE ::                                        theta0      = 0._RKIND
  REAL(RKIND), SAVE ::                                        tmag        = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tmf
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tmfo
  REAL(RKIND), SAVE ::                                        triang      = 0._RKIND
  REAL(RKIND), SAVE ::                                        triplt      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ttp
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ttpo
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          valsneo
  REAL(RKIND), SAVE ::                                        volume      = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            wallposr
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            wallposz
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          wgtpsi
  REAL(RKIND), SAVE ::                                        wmagp       = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            x2srch
  REAL(RKIND), SAVE ::                                        xi          = 0._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xlamb
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp0
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp1
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp2
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp3
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp4
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xpprdf
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xpprmn
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xpprmx
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          ydrsdt
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          yrst
  REAL(RKIND), SAVE ::                                        zboxlen     = -1._RKIND
  REAL(RKIND), SAVE ::                                        zboxmid     = -1.E+40_RKIND
  REAL(RKIND), SAVE ::                                        zboxlen_in  = 0._RKIND
  REAL(RKIND), SAVE ::                                        zboxmid_in  = -1.E+40_RKIND
  REAL(RKIND), SAVE ::                                        zone        = 0._RKIND
  !
  ! COMPLEX
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b2e
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2f
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2fc
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2fcm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2fm
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b2u
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b3e
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3f
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3fc
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3fcm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3fm
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b3u
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11lmv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11lv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12lmv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12lv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22lmv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22lv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33lmv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33lv
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       djcof
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       djcofm
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     dpeds
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     dpedsm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drhos
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drhosm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drot
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drotm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrho
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrhom
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrot
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrotm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       feq
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       feqm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbr
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbrm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfc
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfcm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfs
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfsm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gchdz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gchdzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gscc
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsccm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gscs
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gscsm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsdz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsdzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfc
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfcm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfs
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfsm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy2
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy2m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy3
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy3m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrxx
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrxxm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idryx
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idryxm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrzx
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrzxm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig122
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig122m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig123
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig123m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igf22
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igf22m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx2
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx2m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx3
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx3m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy2
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy2m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy3
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy3m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qx
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qxm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qy
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qym
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxx
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxxm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxy
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxym
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inyy
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inyym
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inzz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inzzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irxz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irxzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iryx
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iryxm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irzy
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irzym
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs11
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs11m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs12
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs12m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs21
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs21m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs22
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs22m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq1
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq1m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq2
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq2m
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq3
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq3m
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j2e
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j2u
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j3e
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j3u
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacobi
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacobm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacof
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacofm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacos
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacosm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg11l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg11lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg12l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg12lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg22l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg22lm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg33l
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg33lm
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     peq
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     pre
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visxz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visxzm
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visyz
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visyzm
  !
  ! CHARACTERS
  CHARACTER(len=150), DIMENSION( :), ALLOCATABLE, SAVE ::     diagartx
  CHARACTER(LEN=132), DIMENSION(:), ALLOCATABLE, SAVE ::      eqchease_out_varnames
  CHARACTER(LEN=132), DIMENSION(:), ALLOCATABLE, SAVE ::      eqchease_out_add_1d_varnames
  CHARACTER(LEN=132), DIMENSION(:), ALLOCATABLE, SAVE ::      eqchease_out_add_2d_varnames 
  CHARACTER(LEN=132), DIMENSION(:), ALLOCATABLE, SAVE ::      eqchease_out_add_2d_rz_varnames
  CHARACTER(len=80), SAVE ::                                  label1      = ' '
  CHARACTER(len=80), SAVE ::                                  label2      = ' '
  CHARACTER(len=80), SAVE ::                                  label3      = ' '
  CHARACTER(len=80), SAVE ::                                  label4      = ' '
  CHARACTER(len=120), DIMENSION(4), SAVE ::                   comments      = ' '
  CHARACTER(len=150), DIMENSION( :), ALLOCATABLE, SAVE ::     rbdiagtx
  CHARACTER(len=20), DIMENSION( :), ALLOCATABLE, SAVE ::      titglneo
  CHARACTER(len=20), DIMENSION( :), ALLOCATABLE, SAVE ::      titsneo
  CHARACTER(len=120), DIMENSION(2), SAVE ::                   treeitm     ='euitm'
  !
  ! types
  type(type_equilibrium),pointer,  SAVE ::                    eqchease_in(:)
  type(type_equilibrium),pointer,  SAVE ::                    eqchease_out(:)
  !
END MODULE globals
