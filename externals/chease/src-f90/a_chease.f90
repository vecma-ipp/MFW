!
! Comments of changes (only started since August 2000)
!
! Most changes now in log of svn check in's
!
! March 2013: Add nsttp=4 option with Iparallel = <mu0 j.B>/B0 (as in ITM, ASTRA). nsttp=3 constructed as nsttp=3.
!
! nfunrho = 1, allows input x-axis mesh to not be sqrt(psi). With nrhomesh=1, uses FCSM input as rho_tor_normalized
! nppfun=8 means input "pprime" array is pressure profile (either in terms of s or rho_tor)
!
! May 2008: Changes for EU-ITM task force, schema v4 format and to allow chease being called as subroutine
!           Introduced NITMOPT, NITMSHOT, NITMRUN, TREEITM as input variables to know how to load/write the data
!           Comments for these variables in COMDAT.inc
!
! Sep 2007: Modifications for nideal=9 ogyropsi started to be incorporated from Xavier Lapillonne changes.
!           Files chipsimetrics.f90, globals.f90, mappin.f90, ogyropsi.f90, ogyropsi_hdf5.f90 (needs futil etc for hdf5)
!
! Sep 2007: Some bugs in outastro and wrtbin for dimensioning corrected
!
! May 2007: added NFUNRHO in namelist and NRHOMESH, read in EXPEQ file, to allow input arrays as function of rho_tor
!           Set NFUNRHO=1, then add value of NRHOMESH(=1) next to NSTTP in EXPEQ file
!           Can just set NFUNRHO=1 and use EXPEQ.OUT.TOR as EXPEQ input file
!           Use rho_tor normalized
!
! April 2007: Several changes to be made: 
!             interface with GENE/ORB5
!             modifications for various signs of B0 and Ip
!             Allow for exp. profiles to be given vs rho_toroidal flux instead of rho_psi
!
! March 2007: saved modifications for interface with EU-ITM TF, made by H. Lutjens (version 4.1)
!
! (02.2003) add NIDEAL=8: interface for ELITE (similar to ERATO (ERDATA), as all required quantities calculated in surface,
!                                              but specific output in outelit. Should use NER=1, NEGP=-1 for equal arc length)
!
! (08.2000) Change definition of NDEQ to be compatible for both nideal=1 and 2, which
! was differentiated before with NDEQ=25 and 29, respectively
! => change: g_0.f90 (ndeq adapted, thus EQ dimension OK in g_2)
!
!
SUBROUTINE a_chease
  ! dummy routine so object not empty
  RETURN
END SUBROUTINE a_chease
