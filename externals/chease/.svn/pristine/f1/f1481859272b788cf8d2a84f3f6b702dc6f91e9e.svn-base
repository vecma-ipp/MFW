MODULE globals
!
!   Global vars and arrays
!
  USE prec_const
  use euitm_schemas                       ! module containing the equilibrium type definitions
  IMPLICIT NONE
!
!!! Dimension constants
!
  INTEGER, PARAMETER :: mflgdiag1 = 1    ! ?
  INTEGER, PARAMETER :: npbps = 1100      ! ?
  INTEGER, PARAMETER :: nppsbal = 10     ! ?
  INTEGER, PARAMETER :: nppscub = 10     ! ?
  INTEGER, PARAMETER :: npdianeo = 60    ! ?
  INTEGER, PARAMETER :: npoutflg = 15    ! ?
  INTEGER, PARAMETER :: nbnd2 = 5        ! ?
  INTEGER, PARAMETER :: nesh1 = 12       ! ?
  INTEGER, PARAMETER :: nicon = 34       ! ?
  INTEGER, PARAMETER :: niod = 17        ! ?
  INTEGER, PARAMETER :: nisu2 = 2        ! ?
  INTEGER, PARAMETER :: nnum = 5+26      ! ?
  INTEGER, PARAMETER :: nphy1 = 1        ! ?
  INTEGER, PARAMETER :: nphy2 = 67+8*15  ! ?
  INTEGER, PARAMETER :: nplo1 = 1        ! ?
  INTEGER, PARAMETER :: nsol1 = 3        ! ?
  INTEGER, PARAMETER :: nsol2 = 10       ! ?
  INTEGER, PARAMETER :: neqd2 = 2        ! ?
!
!!! Init array dimensions
!
  INTEGER, SAVE :: nps = 40
  INTEGER, SAVE :: npt = 40
  INTEGER, SAVE :: npchi = 121
  INTEGER, SAVE :: nppsi = 121
  INTEGER, SAVE :: npchi1 =122 ! npchi+1
  INTEGER, SAVE :: nppsi1 =122 ! nppsi+1
  INTEGER, SAVE :: mflgerl = 0
  INTEGER, SAVE :: mflgmar = 0
  INTEGER, SAVE :: mflgnvw = 0
  INTEGER, SAVE :: mflgpen = 0
  INTEGER, SAVE :: npblc0 = 16
  INTEGER, SAVE :: npturn = 20
  INTEGER, SAVE :: mpsmax = 0
  INTEGER, SAVE :: npsmax = 0
  INTEGER, SAVE :: npv = 41
  INTEGER, SAVE :: npmgs = 4
  INTEGER, SAVE :: npsgs = 4
  INTEGER, SAVE :: nptgs = 4
  INTEGER, SAVE :: npdiag1 = 0
  INTEGER, SAVE :: ndeq = 0
  INTEGER, SAVE :: npband = 0
  INTEGER, SAVE :: npgaus = 0
  INTEGER, SAVE :: nppsnvw = 0
  INTEGER, SAVE :: npiso = 0
  INTEGER, SAVE :: npv1 = 0
  INTEGER, SAVE :: nsnt = 0
  INTEGER, SAVE :: nsp1 = 0
  INTEGER, SAVE :: ntp1 = 0
  INTEGER, SAVE :: ntp2 = 0
  INTEGER, SAVE :: np4nst = 0
  INTEGER, SAVE :: n4nt = 0
  INTEGER, SAVE :: n8nt = 0
  INTEGER, SAVE :: n16nt = 0
  INTEGER, SAVE :: nbal1 = 0
  INTEGER, SAVE :: nbal2 = 0
  INTEGER, SAVE :: nbla = 0
  INTEGER, SAVE :: nbnd1 = 0
  INTEGER, SAVE :: nera = 0
  INTEGER, SAVE :: nesh2 = 0
  INTEGER, SAVE :: neta1 = 0
  INTEGER, SAVE :: neta2 = 0
  INTEGER, SAVE :: neta3 = 0
  INTEGER, SAVE :: neta4 = 0
  INTEGER, SAVE :: nint1 = 0
  INTEGER, SAVE :: nint2 = 0
  INTEGER, SAVE :: nis2 = 0
  INTEGER, SAVE :: nisur = 0
  INTEGER, SAVE :: nmap2 = 0
  INTEGER, SAVE :: nplo2 = 0
  INTEGER, SAVE :: ncvac = 0
  INTEGER, SAVE :: nvev = 0
  INTEGER, SAVE :: neqd1 = 0
!
!!! Static vars and arrays
!
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          a                         !/combla/
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        abal                      !/combal/
  REAL(RKIND), DIMENSION(15), SAVE ::                         afbs        = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(15), SAVE ::                         afbs2       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ageocsm                   !/comsur/
  REAL(RKIND), SAVE ::                                        alzero      = 0._RKIND    !/combnd/
  REAL(RKIND), DIMENSION(15), SAVE ::                         ap          = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(15), SAVE ::                         ap2         = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(10), SAVE ::                         aplace      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            aratio                    !/comsur/
  REAL(RKIND), SAVE ::                                        area        = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        aspct       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        aspctr      = 0._RKIND    !/cometa/
  REAL(RKIND), DIMENSION(15), SAVE ::                         at          = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(15), SAVE ::                         at2         = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(15), SAVE ::                         at3         = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(15), SAVE ::                         at4         = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(10), SAVE ::                         awidth      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            b                         !/combla/
  REAL(RKIND), SAVE ::                                        b0exp       = 0._RKIND    !/comesh/ /EQDATA/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b2e                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2f                       !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2fc                      !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2fcm                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b2fm                      !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b2u                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b3e                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3f                       !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3fc                      !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3fcm                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       b3fm                      !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     b3u                       !/cometa/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          bchin                     !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          bchio                     !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bchiso                    !/comiso/
  REAL(RKIND), SAVE ::                                        beans       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        bentaxis    = 1.015_RKIND !         /EQDATA/ 
  REAL(RKIND), SAVE ::                                        bentradius  = 0.5_RKIND   !         /EQDATA/ 
  REAL(RKIND), SAVE ::                                        bentqprofile= 0._RKIND    !         /EQDATA/ 
  REAL(RKIND), SAVE ::                                        beta        = 0._RKIND    !/comphy/ /NEWRUN/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            betab                     !/comsur/
  REAL(RKIND), SAVE ::                                        betap       = 0._RKIND    !/comphy/ /NEWRUN/
  REAL(RKIND), SAVE ::                                        betas       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        betax       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          bndiso                    !/comiso/
  REAL(RKIND), DIMENSION(10), SAVE ::                         bplace      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION(14), SAVE ::                         bps         = 0._RKIND    !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bpscos                    !/combnd/
  REAL(RKIND), DIMENSION(14), SAVE ::                         bpso        = 0._RKIND    !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bpssin                    !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2ne                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2te                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2ti                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsd2zef                   !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bsdense                   !/combal/
  REAL(RKIND), SAVE ::                                        bsfrac      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bstempe                   !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bstempi                   !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            bszeff                    !/combal/
  REAL(RKIND), DIMENSION(10), SAVE ::                         bwidth      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cdq                       !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cdrq                      !/comsur/
  REAL(RKIND), SAVE ::                                        ceps        = 0._RKIND    !/comsol/
  REAL(RKIND), SAVE ::                                        ceta        = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        cfbal       = 0._RKIND    !/combal/ /EQDATA/
  REAL(RKIND), SAVE ::                                        cfnress     = 0._RKIND    !/combal/ /EQDATA/
  REAL(RKIND), SAVE ::                                        cfnresso    = 0._RKIND    !/combal/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chi                       !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chi0                      !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chiiso                    !/comiso/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chim                      !/comesh/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          chin                      !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          chio                      !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            chiold                    !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid0                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid0o                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid2                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cid2o                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidq                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidr                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidrtor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cidrtoro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cipr                      !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnr1                      !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnr2                      !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnz1                      !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cnz2                      !/comera/
  REAL(RKIND), SAVE ::                                        convf       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cp                        !/comsur/
  REAL(RKIND), SAVE ::                                        cp0         = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        cpbar       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpdp                      !/comsur/
  REAL(RKIND), SAVE ::                                        cpdp0       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        cpi         = 0._RKIND    !/comsol/
  REAL(RKIND), DIMENSION(10), SAVE ::                         cplace      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        cpp         = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        cppf        = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cppr                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cppro                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpr                       !/comsur/
  REAL(RKIND), SAVE ::                                        cpress      = 0._RKIND    !/combal/ /EQDATA/
  REAL(RKIND), SAVE ::                                        cpresso     = 0._RKIND    !/combal/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsi                      !/combla/
  REAL(RKIND), SAVE ::                                        cpsi0       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsi1t                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsicl                    !/combla/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsili                    !/combla/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cpsio                     !/combla/
  REAL(RKIND), SAVE ::                                        cpsrf       = 0._RKIND    !/comphy/ /NEWRUN/
  REAL(RKIND), SAVE ::                                        cq0         = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cr                        !/comera/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cs                        !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csig                      !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csigo                     !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csipr                     !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csipri                    !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csipro                    !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csm                       !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csmtor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csmv                      !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cspen                     !/comesh/
  REAL(RKIND), SAVE                     	::  	      csqmin
  REAL(RKIND), SAVE ::                                        csspec      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cstor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            csv                       !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ct                        !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cto                       !/comesh/
  REAL(RKIND), SAVE ::                                        ctorsrf     = 1._RKIND
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ctpen                     !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ctxt                      !/comesh/
  REAL(RKIND), SAVE ::                                        curold      = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        currt       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            cw                        !/comint/
  REAL(RKIND), DIMENSION(10), SAVE ::                         cwidth      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          cz                        !/comera/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid0                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid0o                   !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid2                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cid2o                   !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidq                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidr                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidrtor
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cidrtoro
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cipr                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cppr                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cppro                   !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2cpr                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2pstn                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2pstt                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rbps                    !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rfun                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rppf                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2rprm                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2tmf                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2tmfo                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2ttp                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            d2zbps                    !/combnd/
  REAL(RKIND), SAVE ::                                        delta       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        dens0       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            densty                    !/comsur/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11lmv                   !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg11lv                    !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12lmv                   !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg12lv                    !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22lmv                   !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg22lv                    !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33lmv                   !/comvac/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       dg33lv                    !/comvac/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dgniso                    !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dgriso                    !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dgziso                    !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          diagars                   !/comdiag1/
  CHARACTER(len=150), DIMENSION( :), ALLOCATABLE, SAVE ::     diagartx                  !/comdia1tx/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffds                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffdt                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffp                     !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            diffst                    !/comvev/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       djcof                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       djcofm                    !/flow1/
  REAL(RKIND), SAVE ::                                        dpdp0       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdsnu                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdsth                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdtnu                    !/comvev/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dpdtth                    !/comvev/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     dpeds                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     dpedsm                    !/cometa/
  REAL(RKIND), DIMENSION(10), SAVE ::                         dplace      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            dprime                    !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dpriso                    !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dpsiso                    !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dptiso                    !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dpziso                    !/comiso/
  REAL(RKIND), SAVE ::                                        dqdp0       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          drhopi                    !/commap/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drhos                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drhosm                    !/flow1/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          drniso                    !/comiso/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drot                      !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       drotm                     !/flow1/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            drsdt                     !/comesh/
  REAL(RKIND), SAVE ::                                        dttp0       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION(10), SAVE ::                         dwidth      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          dzeta                     !/comint/
  REAL(RKIND), SAVE ::                                        elong       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(10), SAVE ::                         eplace      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        epnon0      = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        epslon      = 0._RKIND    !/comsol/ /EQDATA/
  REAL(RKIND), SAVE ::                                        epsmch      = 0._RKIND    !/comdec/
  REAL(RKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::        eq                        !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq13                      !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq22                      !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq24                      !/comera/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eq3                       !/cometa/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eqdspsi                   !/comeqd/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eqi                       !/cometa/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          eql                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrho                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrhom                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrot                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       eqrotm                    !/flow1/
  REAL(RKIND), SAVE ::                                        etaei       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(10), SAVE ::                         ewidth      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        ezmag       = 0._RKIND    !/comdec/
  REAL(RKIND), DIMENSION( :,:,:,:), ALLOCATABLE, SAVE ::      fb                        !/comint/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            fcsm                      !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            fcsmtnz                   !/combal/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       feq                       !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       feqm                      !/flow1/
  REAL(RKIND), SAVE ::                                        gamma       = 0._RKIND    !/comphy/ /EQDATA/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbr                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbrm                      !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbz                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gbzm                      !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfc                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfcm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfs                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gcfsm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gchdz                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gchdzm                    !/cometa/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            globneo                   !/comneo/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          gpiso                     !/comiso/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gscc                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsccm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gscs                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gscsm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsdz                      !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsdzm                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfc                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfcm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfs                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       gsfsm                     !/flow/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            hmercr                    !/combal/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy2                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy2m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy3                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idiy3m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrxx                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrxxm                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idryx                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idryxm                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrzx                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       idrzxm                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig122                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig122m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig123                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ig123m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igf22                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igf22m                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx2                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx2m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx3                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpx3m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy2                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy2m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy3                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       igpy3m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qx                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qxm                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qy                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qym                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qz                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ij0qzm                    !/flow1/
  INTEGER, SAVE ::                                            inp1        = 0           !/comiod/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxx                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxxm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxy                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inxym                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inyy                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inyym                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inzz                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       inzzm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irxz                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irxzm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iryx                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iryxm                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irzy                      !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       irzym                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs11                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs11m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs12                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs12m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs21                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs21m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs22                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       ivs22m                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq1                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq1m                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq2                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq2m                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq3                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       iwsq3m                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j2e                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j2u                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j3e                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     j3u                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacobi                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacobm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacof                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacofm                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacos                     !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jacosm                    !/flow1/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg11l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg11lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg12l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg12lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg22l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg22lm                    !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg33l                     !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       jg33lm                    !/cometa/
  CHARACTER(len=80), SAVE ::                                  label1      = ' '         !/comlab/
  CHARACTER(len=80), SAVE ::                                  label2      = ' '         !/comlab/
  CHARACTER(len=80), SAVE ::                                  label3      = ' '         !/comlab/
  CHARACTER(len=80), SAVE ::                                  label4      = ' '         !/comlab/
  CHARACTER(len=120), DIMENSION(4), SAVE ::                    comments      = ' '
  INTEGER, SAVE ::                                            meq         = 0           !/comiod/ /NEWRUN/
  INTEGER, DIMENSION( :,:,:), ALLOCATABLE, SAVE ::            mpla                      !/comint/
  INTEGER, SAVE ::                                            msmax       = 0           !/comnum/ /EQDATA/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                n2bal                     !/combal/
  INTEGER, SAVE ::                                            n4nsnt      = 0           !/comnum/
  INTEGER, SAVE ::                                            nanal                     !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nbal        = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nband       = 0           !/comnum/
  INTEGER, SAVE ::                                            nblc0       = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            nblopt      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nbps        = 0           !/combnd/
  INTEGER, SAVE ::                                            nbpsout     = 0           !/combnd/ /EQDATA/
  INTEGER, SAVE ::                                            nbsexpq     = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nbsfun      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nbsopt      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nbstrp      = 0           !/comcon/ /EQDATA/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                ncbal                     !/combal/
  INTEGER, DIMENSION( :,:), ALLOCATABLE, SAVE ::              ncblns                    !/combal/
  INTEGER, SAVE ::                                            nchi        = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            nchi1       = 0           !/comnum/
  INTEGER, SAVE ::                                            ncon        = 0           !/comsol/
  INTEGER, SAVE ::                                            ncscal      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            ncurv       = 0           !/complo/
  INTEGER, SAVE ::                                            ndes        = 0           !/comiod/ /NEWRUN/
  INTEGER, SAVE ::                                            ndiaglas    = 0           !/comdiag1/
  INTEGER, SAVE ::                                            ndiagop     = 0           !/comdiag1/ /EQDAT
  INTEGER, SAVE ::                                            ndiaperq    = 0           !/comdiag1/
  INTEGER, SAVE ::                                            ndifps      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            ndift       = 0           !/comcon/ /EQDATA/
  INTEGER, DIMENSION(5), SAVE ::                              ndim        = 0           !/comnum/
  INTEGER, SAVE ::                                            negp        = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nelite      = 0
  INTEGER, SAVE ::                                            neonbglo    = 0           !/comneo/
  INTEGER, SAVE ::                                            neonbqs     = 0           !/comneo/ /EQDATA/
  INTEGER, SAVE ::                                            neonbval    = 0           !/comneo/
  INTEGER, SAVE ::                                            neqdsk      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            neqdxtpo    = 0           !/comeqd/ /EQDATA/
  INTEGER, SAVE ::                                            neqdzmg     = 0           !/comeqd/
  INTEGER, SAVE ::                                            ner         = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            netvac      = 0           !/comiod/
  INTEGER, SAVE ::                                            nitmopt     = -1          !/EQDATA/
  INTEGER, DIMENSION(2), SAVE ::                              nitmrun     = -1          !/EQDATA/
  INTEGER, DIMENSION(2), SAVE ::                              nitmshot    = -1          !/EQDATA/
  INTEGER, SAVE ::                                            nf          = 0           !/combnd/
  INTEGER, SAVE ::                                            nfftopt     = 0           !/comcon/ /EQDATA/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                nflgaddia                 !/comdiag1/
  INTEGER, SAVE ::                                            nfourpb     = 0           !/combnd/
  INTEGER, SAVE ::                                            nfunc       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nfunrho     = 0
  INTEGER, SAVE ::                                            ni          = 0           !/combnd/
  INTEGER, SAVE ::                                            nideal      = 6           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nin         = 0           !/comiod/
  INTEGER, SAVE ::                                            ninmap      = 0           !/comsol/ /EQDATA/
  INTEGER, SAVE ::                                            ninsca      = 0           !/comsol/ /EQDATA/
  INTEGER, SAVE ::                                            nipr        = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            niso        = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            nisoo       = 0           !/comnum/
  LOGICAL, SAVE ::                                            nldiag1     = .FALSE.     !/comdiag1/
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp1                   !/comcon/
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp2                   !/comcon/
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp3                   !/comcon/
  LOGICAL, DIMENSION( :), ALLOCATABLE, SAVE ::                nloutp4                   !/comcon/
  INTEGER, SAVE ::                                            nmag        = 0           !/comesh/
  INTEGER, SAVE ::                                            nmesha      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            nmeshb      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            nmeshc      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            nmeshd      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            nmeshe      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            nmgaus      = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            no          = 0           !/comiod/
  INTEGER, SAVE ::                                            no3         = 0           !/comiod/
  INTEGER, SAVE ::                                            noi         = 0           !/comiod/
  INTEGER, SAVE ::                                            nopt        = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nout        = 0           !/comiod/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np0                       !/combal/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np1                       !/combal/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np2                       !/combal/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np3                       !/combal/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                np4                       !/combal/
  INTEGER, SAVE ::                                            npenn       = 0           !/comiod/
  INTEGER, DIMENSION( :,:), ALLOCATABLE, SAVE ::              nplac                     !/comint/
  INTEGER, SAVE ::                                            nplot       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            npoida      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            npoidb      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            npoidc      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            npoidd      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            npoide      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            npoidq      = 0           !/comesh/ /EQDATA/
  INTEGER, SAVE ::                                            npp         = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nppf        = 0           !/combal/
  INTEGER, SAVE ::                                            nppfun      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nppr        = 0           !/combal/ /EQDATA/
  INTEGER, SAVE ::                                            nprnt       = 0           !/comiod/ /NEWRUN/
  INTEGER, SAVE ::                                            nprofz      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            npropt      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nprpsi      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            npsi        = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            npsi1       = 0           !/comnum/
  INTEGER, SAVE ::                                            nrbox       = 0           !/comeqd/ /EQDATA/
  INTEGER, SAVE ::                                            nrfp        = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nrhomesh    = 0
  INTEGER, SAVE ::                                            nrscal      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nrzpel      = 0           !/comiod/
  INTEGER, SAVE ::                                            ns          = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            ns1         = 0           !/comnum/
  INTEGER, SAVE ::                                            nsave       = 0           !/comiod/ /NEWRUN/
  INTEGER, SAVE ::                                            nsgaus      = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            nsmax       = 0           !/comnum/
  INTEGER, SAVE ::                                            nsmooth     = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nso         = 0           !/comnum/
  INTEGER, SAVE ::                                            nsour       = 0           !/comphy/ /EQDATA/
  INTEGER, SAVE ::                                            nsrch       = 0           !/combal/
  INTEGER, SAVE ::                                            nstmax      = 0           !/comnum/
  INTEGER, SAVE ::                                            nsttp       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nsurf       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nsym        = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nt          = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            nt1         = 0           !/comnum/
  INTEGER, SAVE ::                                            nt2         = 0           !/comnum/
  INTEGER, SAVE ::                                            ntcase      = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            ntest       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            ntgaus      = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            ntmf0       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            ntnova      = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            ntnzpt      = 0           !/combal/
  INTEGER, SAVE ::                                            nto         = 0           !/comnum/
  INTEGER, SAVE ::                                            nturn       = 0           !/combal/ /EQDATA/
  INTEGER, DIMENSION( :), ALLOCATABLE, SAVE ::                nupdwn                    !/comint/
  INTEGER, SAVE ::                                            nuplo       = 0           !/comiod/
  INTEGER, SAVE ::                                            numat       = 0           !/comiod/
  INTEGER, SAVE ::                                            nv          = 0           !/comnum/ /EQDATA/
  INTEGER, SAVE ::                                            nv1         = 0           !/comnum/
  INTEGER, SAVE ::                                            nvac        = 0           !/comiod/ /NEWRUN/
  INTEGER, SAVE ::                                            nvexp       = 0           !/comcon/ /EQDATA/
  INTEGER, SAVE ::                                            nwallpos    = 0           !/comeqd/
  INTEGER, SAVE ::                                            nwgaus      = 0           !/comnum/
  INTEGER, SAVE ::                                            nxin        = 0           !/comiod/
  INTEGER, SAVE ::                                            nxout       = 0           !/comiod/
  INTEGER, SAVE ::                                            nxtnz       = 0           !/comiod/
  INTEGER, SAVE ::                                            nxtor       = 0           !/comiod/
  INTEGER, SAVE ::                                            nzbox       = 0           !/comeqd/ /EQDATA/
  REAL(RKIND), SAVE ::                                        pangle      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            pcs                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            pcsm                      !/combal/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     peq                       !/cometa/
  COMPLEX(CKIND), DIMENSION( :,:,:), ALLOCATABLE, SAVE ::     pre                       !/cometa/
  REAL(RKIND), SAVE ::                                        predge      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        psi0        = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            psiiso                    !/comsur/
  REAL(RKIND), SAVE ::                                        psiscl      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        q0          = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        q95         = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        qcyl        = 0._RKIND    !/comphy/ /NEWRUN/
  REAL(RKIND), SAVE ::                                        qmin        = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION(10), SAVE ::                         qplace      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            qpsi                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            qpsiin
  REAL(RKIND), SAVE ::                                        qspec       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION(10), SAVE ::                         qvalneo     = 0._RKIND    !/comneo/ /EQDATA/
  REAL(RKIND), DIMENSION(10), SAVE ::                         qwidth      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        r0          = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        r0exp       = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        r0o         = 0._RKIND    !/comesh/
  REAL(RKIND), SAVE ::                                        r0w         = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        r0wo        = 0._RKIND    !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rare                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rb2av                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rb2max                    !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rbdiag                    !/comdiag1/
  CHARACTER(len=150), DIMENSION( :), ALLOCATABLE, SAVE ::     rbdiagtx                  !/comdia1tx/
  REAL(RKIND), SAVE ::                                        rboxlen     = 0._RKIND    !/comeqd/ /EQDATA/
  REAL(RKIND), SAVE ::                                        rboxlft     = 0._RKIND    !/comeqd/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rbpol0                    !/comsur/
  REAL(RKIND), SAVE ::                                        rc          = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        rc0p        = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        rc1m12      = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        rc1m13      = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        rc1m14      = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        rc1p        = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        rc2p        = 0._RKIND    !/comdec/
  REAL(RKIND), SAVE ::                                        rc2pi       = 0._RKIND    !/comdec/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rdedr                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rdi                       !/comsur/
  REAL(RKIND), SAVE ::                                        relax       = 0._RKIND    !/comsol/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rell                      !/comsur/
  REAL(RKIND), SAVE ::                                        resdps      = 0._RKIND    !/comvev/
  REAL(RKIND), SAVE ::                                        resdpt      = 0._RKIND    !/comvev/
  REAL(RKIND), SAVE ::                                        resdst      = 0._RKIND    !/comvev/
  REAL(RKIND), SAVE ::                                        residu      = 0._RKIND    !/comsol/
  REAL(RKIND), SAVE ::                                        resmap      = 0._RKIND    !/comsol/
  REAL(RKIND), SAVE ::                                        respsi      = 0._RKIND    !/comvev/
  REAL(RKIND), SAVE ::                                        rext        = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rfcirc                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rfpbp                     !/comsur/
  REAL(RKIND), SAVE ::                                        rfpf        = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        rfpt        = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rfun                      !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rgeocsm                   !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rhoiso                    !/comiso/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rhos                      !/comesh/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rhovac                    !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rhovaci                   !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rhovacm                   !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rib2                      !/comsur/
  REAL(RKIND), SAVE ::                                        ribsnor     = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            riie                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            riir                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rinduc                    !/comsur/
  REAL(RKIND), SAVE ::                                        rinor       = 0._RKIND    !/comphy/ /NEWRUN/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rip                       !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rip2                      !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ripr                      !/comsur/
  REAL(RKIND), SAVE ::                                        ripr0       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        ritbs       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        ritbsc      = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        ritbsc2      = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        ritbsc3      = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        ritot       = 0._RKIND    !/comphy/ /NEWRUN/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rivol                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj1                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj2                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj3                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj4                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj5                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj5p                      !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rj6                       !/combal/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rjbsos                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rjdotb                    !/comsur/
  REAL(RKIND), SAVE ::                                        rjdtb0      = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rjpar                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rleng                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rleng1                    !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rm                        !/cometa/
  REAL(RKIND), SAVE ::                                        rmag        = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        rmago       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rn                        !/cometa/
  REAL(RKIND), SAVE ::                                        rnu         = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rnustar                   !/comsur/
  REAL(RKIND), SAVE ::                                        rpeop       = 0._RKIND    !/comsur/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rppf                      !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rprm                      !/combal/
  REAL(RKIND), SAVE ::                                        rraxis      = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rrbps                     !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rrbpsou                   !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rrcurv                    !/complo/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rriso                     !/comiso/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rshear                    !/complo/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rsint                     !/comint/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rsurf                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rsy                       !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rtint                     !/comint/
  REAL(RKIND), SAVE ::                                        rz0         = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        rz0c        = 0._RKIND    !/combnd/
  REAL(RKIND), SAVE ::                                        rz0o        = 0._RKIND    !/comesh/
  REAL(RKIND), SAVE ::                                        rz0w        = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        rz0wo       = 0._RKIND    !/comesh/
  REAL(RKIND), SAVE ::                                        rzaxis      = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rzbps                     !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rzbpsou                   !/combnd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            rzcurv                    !/complo/
  REAL(RKIND), SAVE ::                                        rzion       = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          rziso                     !/comiso/
  REAL(RKIND), SAVE ::                                        rzmag       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        rzmago      = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        rzmgeqd     = 0._RKIND    !/comeqd/
  REAL(RKIND), SAVE ::                                        scalac      = 0._RKIND    !/comsol/
  REAL(RKIND), SAVE ::                                        scale       = 0._RKIND    !/comsol/
  REAL(RKIND), SAVE ::                                        scalne      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        scexp       = 0._RKIND    !/comsol/ /EQDATA/
  REAL(RKIND), SAVE ::                                        scheck      = 0._RKIND    !/comsol/
  REAL(RKIND), SAVE ::                                        sgma        = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          sigchi                    !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          sigmap                    !/commap/
  REAL(RKIND), SAVE ::                                        signb0xp    = 1._RKIND    !/EQDATA/
  REAL(RKIND), SAVE ::                                        signipxp    = 1._RKIND    !/EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            sigpen                    !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          sigpsi                    !/comiso/
  REAL(RKIND), SAVE ::                                        slimit      = 0._RKIND    !HL for xtor /EQDATA/
  REAL(RKIND), SAVE ::                                        smag        = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            smerci                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            smercr                    !/combal/
  REAL(RKIND), SAVE ::                                        snumber     = 0._RKIND    !HL for xtor /EQDATA/
  REAL(RKIND), SAVE ::                                        solpda      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        solpdb      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        solpdc      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        solpdd      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        solpde      = 0._RKIND    !/comesh/ /EQDATA/
  REAL(RKIND), SAVE ::                                        spitzer     = 0._RKIND    !HL for xtor /EQDATA/
  REAL(RKIND), SAVE ::                                        spsi0       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        spsim       = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        spsimo      = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        t0          = 0._RKIND    !/comphy/
  REAL(RKIND), SAVE ::                                        temp0       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            temper                    !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetbps                    !/combnd/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetchi                    !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetmap                    !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetpen                    !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetpsi                    !/comiso/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetvac                    !/commap/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          tetvaci                   !/commap/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tetvacm                   !/commap/
  REAL(RKIND), SAVE ::                                        theta0      = 0._RKIND    !/comphy/ /EQDATA/
  CHARACTER(len=20), DIMENSION( :), ALLOCATABLE, SAVE ::      titglneo                  !/comneoc/
  CHARACTER(len=20), DIMENSION( :), ALLOCATABLE, SAVE ::      titsneo                   !/comneoc/
  REAL(RKIND), SAVE ::                                        tmag        = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tmf                       !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            tmfo                      !/comsur/
  CHARACTER(len=120), DIMENSION(2), SAVE ::                   treeitm     ='euitm'      !/EQDATA/
  REAL(RKIND), SAVE ::                                        triang      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), SAVE ::                                        triplt      = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ttp                       !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            ttpo                      !/comsur/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          valsneo                   !/comneo/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visxz                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visxzm                    !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visyz                     !/flow/
  COMPLEX(CKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::       visyzm                    !/flow/
  REAL(RKIND), SAVE ::                                        volume      = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            vsurf                     !/comsur/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            wallposr                  !/comeqd/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            wallposz                  !/comeqd/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          wgtpsi                    !/comiso/
  REAL(RKIND), SAVE ::                                        wmagp       = 0._RKIND    !/comphy/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            x2srch                    !/combal/
  REAL(RKIND), SAVE ::                                        xi          = 0._RKIND    !/comphy/ /EQDATA/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xlamb                     !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp0                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp1                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp2                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp3                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xp4                       !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xpprdf                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xpprmn                    !/combal/
  REAL(RKIND), DIMENSION( :), ALLOCATABLE, SAVE ::            xpprmx                    !/combal/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          ydrsdt                    !/comint/
  REAL(RKIND), DIMENSION( :,:), ALLOCATABLE, SAVE ::          yrst                      !/comint/
  REAL(RKIND), SAVE ::                                        zboxlen     = 0._RKIND    !/comeqd/ /EQDATA/
  REAL(RKIND), SAVE ::                                        zboxmid     = 0._RKIND    !/comeqd/ /EQDATA/
  REAL(RKIND), SAVE ::                                        zone        = 0._RKIND    !/combnd/
!
  REAL(RKIND), DIMENSION(:,:)  , ALLOCATABLE, SAVE ::             eqchease_out_add_1d
  REAL(RKIND), DIMENSION(:,:,:), ALLOCATABLE, SAVE ::             eqchease_out_add_2d
!
  type(type_equilibrium),pointer,  SAVE ::                         eqchease_in(:)=>null()
  type(type_equilibrium),pointer,  SAVE ::                         eqchease_out(:)=>null()
!
END MODULE globals
