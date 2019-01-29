!
SUBROUTINE NEOART
  !
  !                                        AUTHORS:
  !                                        Y. CAMENEN, November 2009
  !*******************************************************************************************
  !                                                                                          *
  ! Writes some outputs to be used in NEOART for neoclassical transport calculations         *
  ! NER and NEGP have to be 0 to ensure that the jacobian is a flux label                    *
  !                                                                                          *
  ! The output file generation is copied from ogyropsi.f90 and uses the text/hdf5 writting   *
  ! routines defined in write_ogyropsi.f90                                                   *
  !*******************************************************************************************
  !
  USE globals
  USE interpos_module
  IMPLICIT NONE
  INTEGER :: K, J1
  INTEGER :: hdf5_ioutgyro, ioutgyro
  REAL(RKIND)      ::   dfdpsi
  REAL(RKIND)      ::   damindpsi, jac
  REAL(RKIND)      ::   bav, b2av, bi2av, r2av
  REAL(RKIND)      ::   ri2av, gclass, bgradp
  REAL(RKIND)      ::   TENS_DEF

  DIMENSION & 
       &    dfdpsi(NISO1EFF), damindpsi(NISO1EFF), jac(NISO1EFF),&
       &    bav(NISO1EFF), b2av(NISO1EFF), bi2av(NISO1EFF), r2av(NISO1EFF), &
       &    ri2av(NISO1EFF), gclass(NISO1EFF), bgradp(NISO1EFF)

  TENS_DEF = -0.1_RKIND ! tension for interpos

  ! 1) check the NER and NEGP parameters
  IF ((NER.NE.0) .OR. (NEGP.NE.0)) THEN
     PRINT *,' NER=', NER, ' AND NEGP=', NEGP
     PRINT *,' WARNING: HAMADA SHOULD ONLY BE CALLED WITH NER=0 and NEGP=0'
  ENDIF
  !
  ! 2) calculate missing quantities 
  ! from now on, all is in MKSA units 
  ! eqchease_out* arrays are with ITM conventions: no 2pi in psi and signB, signJ included 
  ! outputs in neoart.dat are with the 2pi in psi (as in CHEASE) and signB=signJ=1
  DO K=1,NISO1EFF
     jac(K) = CP(K)*R0EXP/B0EXP
     dfdpsi(K) = (eqchease_out(1)%profiles_1d%ffprime(K+1)*TWOPI*(-signipxp)) / &
          &      (eqchease_out(1)%profiles_1d%F_dia(K+1)*signb0xp)
  END DO
  !
  CALL INTERPOS(eqchease_out(1)%profiles_1d%psi(1:NISO1EFF1)/TWOPI*(-signipxp), &
       & eqchease_out_add_1d(1:NISO1EFF1,iiamin),NIN=NISO1EFF1, NOUT=NISO1EFF, TENSION=TENS_DEF, &
       & XOUT=eqchease_out(1)%profiles_1d%psi(2:NISO1EFF1)/TWOPI*(-signipxp), &
       & YOUTP=damindpsi, option=12, &
       & nbc=(/2, 2/), ybc=(/0._RKIND,eqchease_out_add_1d(NISO1EFF1,iiamin)/))
  !
  DO K=1,NISO1EFF
     bav(K)  = RIB(K) / RIVOL(K) * B0EXP
     b2av(K) = RIB2(K) / RIVOL(K) * B0EXP**2
     bi2av(K) = RIBI2(K) / RIVOL(K) / B0EXP**2
     r2av(K) = RIR2(K) / RIVOL(K) * R0EXP**2
     ri2av(K) = RJ4(K) / RJ5(K) / R0EXP**2
     gclass(K) = r2av(K) - (eqchease_out(1)%profiles_1d%F_dia(K+1)*signb0xp)**2 * bi2av(K)
     bgradp(K) = 1.E0 / jac(K) / bav(K) 
     DO J1=1,6
       FM(J1,K) = FM(J1,K) / R0EXP**2
     END DO
  END DO
  !
  ! 3) write the output file 
  ! Open ASCII and HDF5 files
  CALL INIT_WRITE_OUT(ioutgyro,hdf5_ioutgyro,'neoart')
  !
  ! Saved Scalars
  CALL WRITE_OUT_SCALAR_INT("NPSI",NISO1EFF,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_INT("MMX",6,ioutgyro,hdf5_ioutgyro, "/data")
  CALL WRITE_OUT_SCALAR_RE("R0EXP",R0EXP,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_RE("B0EXP",B0EXP,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_RE("Raxis",RMAG*R0EXP,ioutgyro,hdf5_ioutgyro,"/data")
  !
  ! PSI grid
  CALL WRITE_OUT_1D("PSI",eqchease_out(1)%coord_sys%grid%dim1(2:NISO1EFF1)/TWOPI*(-signipxp),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/grid/PSI")
  !
  ! 1-dim quantities
  CALL WRITE_OUT_1D("Rgeom",eqchease_out_add_1d(2:NISO1EFF1,iirgeo),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/Rgeom")
  CALL WRITE_OUT_1D("amin",eqchease_out_add_1d(2:NISO1EFF1,iiamin),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/amin")
  CALL WRITE_OUT_1D("damindpsi",damindpsi(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/damindpsi")
  CALL WRITE_OUT_1D("Bmax",eqchease_out_add_1d(2:NISO1EFF1,iiBmax)*signb0xp,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/Bmax")
  CALL WRITE_OUT_1D("Bmin",eqchease_out_add_1d(2:NISO1EFF1,iiBmin)*signb0xp,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/Bmin")
  CALL WRITE_OUT_1D("q",eqchease_out(1)%profiles_1d%q(2:NISO1EFF1)*signb0xp*(-signipxp),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/q")
  CALL WRITE_OUT_1D("dqdpsi",eqchease_out_add_1d(2:NISO1EFF1,iidqdpsi)*signb0xp*TWOPI,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/dqdpsi")
  CALL WRITE_OUT_1D("p",eqchease_out(1)%profiles_1d%pressure(2:NISO1EFF1),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/p")
  CALL WRITE_OUT_1D("dpdpsi",eqchease_out(1)%profiles_1d%pprime(2:NISO1EFF1)*(-signipxp)*TWOPI,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/dpdpsi")
  CALL WRITE_OUT_1D("f",eqchease_out(1)%profiles_1d%F_dia(2:NISO1EFF1)*signb0xp,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/f")
  CALL WRITE_OUT_1D("dfdpsi",dfdpsi(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/dfdpsi")
  CALL WRITE_OUT_1D("bav",bav(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/bav")
  CALL WRITE_OUT_1D("b2av",b2av(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/b2av")
  CALL WRITE_OUT_1D("bi2av",bi2av(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/bi2av")
  CALL WRITE_OUT_1D("r2av",r2av(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/r2av")
  CALL WRITE_OUT_1D("ri2av",ri2av(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/ri2av")
  CALL WRITE_OUT_1D("gclass",gclass(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/gclass")
  CALL WRITE_OUT_1D("bgradp",bgradp(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/bgradp")
  CALL WRITE_OUT_1D("fc",rfcirc(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fc")
  CALL WRITE_OUT_1D("fm1",fm(1,1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fm1")
  CALL WRITE_OUT_1D("fm2",fm(2,1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fm2")
  CALL WRITE_OUT_1D("fm3",fm(3,1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fm3")
  CALL WRITE_OUT_1D("fm4",fm(4,1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fm4")
  CALL WRITE_OUT_1D("fm5",fm(5,1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fm5")
  CALL WRITE_OUT_1D("fm6",fm(6,1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/fm6")
 !
  ! CLOSE FILES
  CALL CLOSE_WRITE_OUT(ioutgyro,hdf5_ioutgyro)

END SUBROUTINE NEOART
