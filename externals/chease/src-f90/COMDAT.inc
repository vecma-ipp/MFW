!*COMDECK COMDAT
!
!**********************************************************************
!                                                                     *
!     C.20   EQUILBRIUM NAMELIST                                      *
!                                                                     *
!**********************************************************************
!
!    ADD COMMENTS ABOUT NAMELIST VARIABLES HERE:
!
!    NIDEAL:  DETERMINES MAPPING OF SPECIFIC CHEASE RESULTS ON TO A SPECIFIC MESH, IN GENERAL FOR A GIVEN CODE
!            =0: FOR MARS
!            =1: FOR ERATO
!            =2: FOR LION
!            =3: FOR NOVA-W (FILE INP1)
!            =4: FOR PENN (FILE NPENN)
!            =5: FOR XTOR (FILES ASTRO, OUTXTOR)
!            =6 (DEFAULT): FOR STANDARD OUTPUT, INCLUDING EQDSK
!            =7: FOR GYROKINETIC CODES (FILE EQCIN)
!            =8: FOR ELITE (FILE NELITE)
!            =9: FOR OGYROPSI (FILE OGYROPSI), REQUIRES STRAIGHT FIELD LINE JACOBIAN
!            =10:FOR GKW AND NEOART WITH HAMADA COORDINATES
!
!     EQDSK and SI units output related:
!
!     R0EXP:  NORMALIZATION OF LENGTH (TYPICALLY GEOMETRICAL AXIS OF LCFS) IN METERS
!     B0EXP:  NORMALIZATION OF MAGNETIC FIELD (VACUUM VALUE AT R=R0EXP) IN TESLA
!     RBOXLFT, RBOXLEN, ZBOXMID, ZBOXLEN: FOR (R,Z) BOX, ALL IN METERS, WILL BE NORMALIZED BY R0EXP
!     ZBOXMID: IF ZBOXMID NOT SET, CHEASE COMPUTES A BOX NEAR THE PLASMA BOUNDARY
!
!     EU-ITM RELATED VARIABLES:
!
!    NITMOPT: DETERMINES TYPE OF INPUT AND POSSIBILITIES. IF SET TO -1 AS DEFAULT VALUE, ASSUME CHEASE NOT COMPILED WITH ITM PACKAGE. IF SET TO 0, THEN CAN BE CHANGED IN NAMELIST SUCH THAT:
!            =01: READ EQUILIBRIUM FROM ITM STRUCTURE, BUT WRITE STANDARD OUTPUT FILES
!            =11: READ AND WRITE EQUIL FROM AND TO ITM DATA STRUCTURE
!            =10: READ EQUIL FROM STANDARD FILES AND WRITE TO ITM DATA STRUCTURE
!            =00: READ FROM FILES AND WRITE ON FILES AS BEFORE
!            =22: READ AND WRITE VIA KEPLER (NOT ACTIVE YET)
!    NITMSHOT(1)/NITMRUN(1): SHOT AND RUN NUMBER TO READ ITM DATA FROM
!    NITMSHOT(2)/NITMRUN(2): SHOT AND RUN NUMBER TO WRITE ITM DATA TO
!    TREEITM: ITM TREE TO USE, TREEITM(1) FOR READING AND TREEITM(2) FOR WRITING THE DATA
!         AT THIS STAGE, IT IS ASSUMED THAT ONLY THE SIGNAL_NAME='EQUILIBRIUM' IS DEALT WITH
!
!    NVERBOSE: -1: No messages printed, no write(0,*), no writing onto files
!               0: write(0,*) messages only + outmksa messages with summary table
!               1: add print *,... and simple messages  + standard output after the convergence + write to files (should be standard minimum except when fully performing)
!               2: print messages during convergence as well (corresponds to previous CHEASE output, thus default value)
!               3: and higher, add debug messages
!
!    COCOS_IN/OUT: COordinate COnventionS, that is R, phi, Z orientations, rho,theta, phi orientations and psi B field definition with respect to G=RB_phi and grad psi
!                             _IN for convention expected in input
!                             _OUT for convention desired in output
!                             = 13: original ITM convention: phi counter-clock from top, (R,phi,Z) theta counter-clockwise (rho,phi,theta); B=G grad phi + grad phi x (-grad psi_13/2pi)
!                             = 11: ITER convention: phi counter-clock from top, (R,phi,Z) theta counter-clockwise (rho,theta,phi); B=G grad phi + grad phi x grad psi_11/2pi
!                             = 2: eqdsk(?),CHEASE(+Ip>0,B0>0) convention: phi clockwise from top, (R,Z,phi) theta counter-clockwise (rho,theta,phi); B=G grad phi + grad phi x grad psi_2
!        Note: COCOS = 2 is the original CHEASE convention (See O. Sauter's paper)
!
Namelist /EQDATA/ &
  &   AFBS,    AFBS2,   AP,      APLACE,  AP2,     ASPCT,   AT,      AT2, &
  &   AT3,     AT4,     AWIDTH,  BEANS,   BENTAXIS,BENTQPROFILE,     BENTRADIUS, &
  &   BPLACE,  BSFRAC,  BWIDTH,  B0EXP,   CETA,    CFBAL,   CFNRESS, CFNRESSO, &
  &   COCOS_IN,COCOS_OUT,CPLACE, CPRESS,  CPRESSO, CQ0,     CSSPEC,  CURRT, &
  &   CWIDTH,  DELTA,   DPLACE,  DWIDTH,  ELONG,   EPLACE, &
  &   ETAEI,   EWIDTH,  EPSLON,  GAMMA,   PANGLE,  PREDGE, &
  &   PSISCL,  QSPEC,   QPLACE,  QWIDTH,  QVALNEO, RBOXLEN, RBOXLFT, &
  &   RC,      RELAX,   REXT,    RNU,     RODABYROD0, RPEOP,   RZION, &
  &   RZ0,     RZ0W,    R0,      R0EXP,   R0W,     SCALNE, &
  &   SCEXP,   SIGNB0XP,SIGNIPXP,SGMA,    SLIMIT,  SNUMBER, &
  &   SOLPDA,  SOLPDB,  SOLPDC,  SOLPDD,  SOLPDE,  SOLPDPOL, SHIFT_P, TENSPROF,TENSBND, &
  &   THETA0,  TREEITM, TRIANG,  TRIPLT,  XI,      ZBOXLEN, ZBOXMID, &
  &   MDT,     MSMAX,   NANAL,   NBAL,    NBLC0,   NBLOPT,  NBPSOUT, NBSEXPQ, &
  &   NBSFUN,  NBSOPT,  NBSTRP,  NCHI,    NCSCAL,  NDIAGOP, NDIFPS, &
  &   NDIFT,   NEGP,    NEONBQS, NEQDSK,  NER,     NITMOPT, NITMRUN, NITMSHOT,  NFFTOPT, &
  &   NFIXWALL,NFUNC,   NFUNRHO, NIDEAL,  NINMAP,  NINSCA,  NIPR,    NISO,    NMESHA, &
  &   NMESHB,  NMESHC,  NMESHD,  NMESHE,  NMESHPOL, NMESHPOLEXP, NMGAUS,  NOPT,    NOUTXTOR, &
  &   NPLOT,   NPOIDA,  NPOIDB,  NPOIDC,  NPOIDD,  NPOIDE,  NPOPULATIONS, &
  &   NPOIDQ,  NPP,     NPPFUN,  NPPR,    NPROF2D, NPROFZ,  NPROPT, &
  &   NPRPSI,  NPSI,    NRBOX,   NRBOX_XTOR,       NRFP,    NRHOMESH, NRSCAL,  NS, &
  &   NSGAUS,  NSMOOTH, NSOUR,   NSTTP,   NSURF,   NSYM, &
  &   NT,      NTCASE,  NTGAUS,  NTEST,   NTMF0,   NTNOVA, &
  &   NTURN,   NV,      NVEXP,   NZBOX,   NZBOX_XTOR, &
  &   NEQDXTPO,NVERBOSE,COMMENTS
!    
!    TENSPROF: Tension for input profile smoothing (used in interpos). Set it to 0. for no smoothing
!    TENSBND: Tension for input plasma boundary smoothing. Set it to 0. for no smoothing
!
