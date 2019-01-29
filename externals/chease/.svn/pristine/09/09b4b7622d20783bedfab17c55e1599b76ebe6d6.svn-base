SUBROUTINE HAMADA
  !
  !                                        AUTHORS:
  !                                        Y. CAMENEN, January 2008
  !*******************************************************************************************
  !                                                                                          *
  ! Build field aligned Hamada coordinates (psi,zeta,s) from the (psi,chi,phi) coordinates.  *
  ! Compute the associated metric and equilibrium quantities using the elements calculated   *
  ! for the (psi,chi,phi) coordinates in surface.f90 and chipsimetrics.f90                   *
  ! NER and NEGP have to be 0 to ensure that the jacobian is a flux label                    *
  !
  ! The output file generation is copied from ogyropsi.f90 and uses the text/hdf5 writting   *
  ! routines defined in write_ogyropsi.f90                                                   *
  !*******************************************************************************************
  !
  USE globals
  USE interpos_module
  IMPLICIT NONE
  INTEGER :: K, J1
  INTEGER :: hdf5_ioutgyro, ioutgyro
  REAL(RKIND)      ::   tmp1
  REAL(RKIND)      ::   jac, djacdpsi, dfdpsi, jac_new
  REAL(RKIND)      ::   damindpsi, d2amindpsi2
  REAL(RKIND)      ::   dqdpsi_chk
  REAL(RKIND)      ::   grid_s
  REAL(RKIND)      ::   dBds, dRds, dZds
  REAL(RKIND)      ::   TENS_DEF

  DIMENSION &
       &   djacdpsi(NISO1EFF),   jac(NISO1EFF),    dfdpsi(NISO1EFF), &
       &    tmp1(2), &
       &   jac_new(NISO1EFF),   damindpsi(NISO1EFF), d2amindpsi2(NISO1EFF), &
       &   dBds(NISO1EFF,NCHI),    dRds(NISO1EFF,NCHI), dZds(NISO1EFF,NCHI), &
       &   dqdpsi_chk(NISO1EFF),  grid_s(NCHI)

  REAL(RKIND), DIMENSION(:,:), ALLOCATABLE      :: g11, g12, g13, &
       &                              g22, g23, g33, &
       &                              dzetadpsi, dzetadchi

  allocate(g11(NISO1EFF,NCHI))
  allocate(g12(NISO1EFF,NCHI))
  allocate(g13(NISO1EFF,NCHI))
  allocate(g22(NISO1EFF,NCHI))
  allocate(g23(NISO1EFF,NCHI))
  allocate(g33(NISO1EFF,NCHI))
  allocate(dzetadpsi(NISO1EFF,NCHI))
  allocate(dzetadchi(NISO1EFF,NCHI))

  TENS_DEF = -0.1_RKIND ! tension for interpos

  ! 1) check the NER and NEGP parameters
  IF ((NER.NE.0) .OR. (NEGP.NE.0)) THEN
     PRINT *,' NER=', NER, ' AND NEGP=', NEGP
     PRINT *,' WARNING: HAMADA SHOULD ONLY BE CALLED WITH NER=0 and NEGP=0'
  ENDIF
  !
  ! 2) calculate missing quantities 
  DO K=1,NISO1EFF
     jac(K) = CP(K)*R0EXP/B0EXP
     djacdpsi(K) = CPDP(K)/R0EXP/B0EXP**2
     jac_new(K) = jac(K) * 4._RKIND * CPI**2
     ! from now on, all outputs are in MKSA units 
     ! eqchease_out* arrays are with ITM conventions: no 2pi in psi and signB, signJ included 
     ! outputs in neoart.dat are with the 2pi in psi (as in CHEASE) and signB=signJ=1
     dfdpsi(K) = (eqchease_out(1)%profiles_1d%ffprime(K+1)*TWOPI*(-signipxp)) / &
          &      (eqchease_out(1)%profiles_1d%F_dia(K+1)*signb0xp)
     DO J1=1,NCHI
        dzetadpsi(K,J1) = &
             &      (dfdpsi(K) * jac(K) * eqchease_out_add_2d(K+1,J1,iiAh) + &
             &      (eqchease_out(1)%profiles_1d%F_dia(K+1)*signb0xp)*djacdpsi(K)*eqchease_out_add_2d(K+1,J1,iiAh) + &
             &      (eqchease_out(1)%profiles_1d%F_dia(K+1)*signb0xp)*jac(K)* &
             & (eqchease_out_add_2d(K+1,J1,iidAhdpsi)*TWOPI*(-signipxp)) &
             &      )/CPI/2._RKIND
        dzetadchi(K,J1) = (eqchease_out(1)%profiles_1d%F_dia(K+1)*signb0xp)*jac(K) / &
             &      2._RKIND / CPI / eqchease_out(1)%coord_sys%position%R(K+1,J1)**2
        !
        g11(K,J1) = eqchease_out(1)%coord_sys%g_11(K+1,J1)/TWOPI**2
        g12(K,J1) = eqchease_out(1)%coord_sys%g_11(K+1,J1)/TWOPI**2 * dzetadpsi(K,J1) + &
             &      eqchease_out(1)%coord_sys%g_12(K+1,J1)/TWOPI*(-signipxp) * dzetadchi(K,J1)
        g13(K,J1) = eqchease_out(1)%coord_sys%g_12(K+1,J1)/TWOPI*(-signipxp) / 2._RKIND / CPI
        g22(K,J1) = eqchease_out(1)%coord_sys%g_11(K+1,J1)/TWOPI**2 * dzetadpsi(K,J1)**2 + &
             &      eqchease_out(1)%coord_sys%g_22(K+1,J1) * dzetadchi(K,J1)**2 + &
             &      eqchease_out(1)%coord_sys%g_33(K+1,J1) / 4._RKIND / CPI**2 + &
             &      2 * eqchease_out(1)%coord_sys%g_12(K+1,J1)/TWOPI*(-signipxp) * dzetadpsi(K,J1) * dzetadchi(K,J1)
        g23(K,J1) = (eqchease_out(1)%coord_sys%g_12(K+1,J1)/TWOPI*(-signipxp) * dzetadpsi(K,J1) + &
             &      eqchease_out(1)%coord_sys%g_22(K+1,J1) * dzetadchi(K,J1))/2._RKIND/CPI
        g33(K,J1) = eqchease_out(1)%coord_sys%g_22(K+1,J1) / 4._RKIND / CPI**2
        !
        dBds(K,J1) = 2._RKIND * CPI * eqchease_out_add_2d(K+1,J1,iidBdchi)*signb0xp
        dRds(K,J1) = 2._RKIND * CPI * eqchease_out_add_2d(K+1,J1,iidRdchi)
        dZds(K,J1) = 2._RKIND * CPI * eqchease_out_add_2d(K+1,J1,iidZdchi)
     END DO
  END DO
  !
  DO J1=1,NCHI
     grid_s(J1) = eqchease_out(1)%coord_sys%grid%dim2(J1)/TWOPI
  END DO
  !
  CALL INTERPOS(eqchease_out(1)%profiles_1d%psi(1:NISO1EFF1)/TWOPI*(-signipxp), &
       & eqchease_out_add_1d(1:NISO1EFF1,iiamin),NIN=NISO1EFF1, NOUT=NISO1EFF, TENSION=TENS_DEF, &
       & XOUT=eqchease_out(1)%profiles_1d%psi(2:NISO1EFF1)/TWOPI*(-signipxp), &
       & YOUTP=damindpsi,YOUTPP=d2amindpsi2, option=12, &
       & nbc=(/2, 2/), ybc=(/0._RKIND,eqchease_out_add_1d(NISO1EFF1,iiamin)/))
  !
  ! Calculate dzetapsi(s=1) -> put this value in dqdpsi_chk
  DO K=1,NISO1EFF
     CALL INTERPOS(grid_s(1:NCHI), dzetadpsi(K,1:NCHI), NIN=NCHI, TENSION=TENS_DEF, &
          & XOUT=(/grid_s(NCHI),1._RKIND/), YOUT=tmp1)
     dqdpsi_chk(K)=tmp1(2)   
  END DO
  !
  ! tested in ../WK/TESTCASES/NIDEAL10/matlab/debug_fort41.m
  ! keep write commands commented at this stage
  !OS write(41,*) NCHI, NISO1EFF, NISO1EFF1, NT, NMGAUS, NT2, NT1
  !OS write(41,*) (grid_s(j1),j1=1,nchi)
  !OS write(41,*) ((dzetadpsi(K,j1),K=1,NISO1EFF), j1=1,nchi)
  !OS write(41,*) ((dzetadchi(K,j1),K=1,NISO1EFF), j1=1,nchi)
  !OS write(41,*) ((eqchease_out(1)%coord_sys%g_11(K+1,j1)/TWOPI**2,K=1,NISO1EFF), j1=1,nchi)
  !OS write(41,*) ((eqchease_out(1)%coord_sys%g_12(K+1,j1)/TWOPI,K=1,NISO1EFF), j1=1,nchi)
  !OS write(41,*) ((g11(K,j1),K=1,NISO1EFF), j1=1,nchi)
  !OS write(41,*) ((g12(K,j1),K=1,NISO1EFF), j1=1,nchi)
  !OS write(41,*) (dqdpsi_chk(j1),j1=1,NISO1EFF)
  !OS write(41,*) (eqchease_out(1)%profiles_1d%psi(j1)/TWOPI*(-signipxp),j1=1,NISO1EFF1)
  !OS write(41,*) (eqchease_out_add_1d(j1,iiamin),j1=1,NISO1EFF1)
  !OS write(41,*) (eqchease_out(1)%profiles_1d%psi(j1)/TWOPI*(-signipxp),j1=2,NISO1EFF1)
  !OS write(41,*) (damindpsi(j1),j1=1,NISO1EFF1-1)
  !OS write(41,*) (d2amindpsi2(j1),j1=1,NISO1EFF1-1)
  !OS write(41,*) (djacdpsi(j1),j1=1,NISO1EFF)
  !OS write(41,*) (dfdpsi(j1),j1=1,NISO1EFF)
  ! test spline in surface for periodic
  !OS write(41,*) ((TETMAP(j1,k),j1=1,NT+2),k=1,niso1eff)
  !OS write(41,*) ((BCHIN(j1,k),j1=1,NT+2),k=1,niso1eff)
  !OS write(41,*) ((TETPSI(j1,k),j1=1,Nmgaus*(nt+1)),k=1,niso1eff)
  !
  !
  ! 3) write the output file 
  ! Open ASCII and HDF5 files
  CALL INIT_WRITE_OUT(ioutgyro,hdf5_ioutgyro,'hamada')
  !
  ! Saved Scalars
  CALL WRITE_OUT_SCALAR_INT("NPSI",NISO1EFF,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_INT("NS",NCHI,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_RE("R0EXP",R0EXP,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_RE("B0EXP",B0EXP,ioutgyro,hdf5_ioutgyro,"/data")
  CALL WRITE_OUT_SCALAR_RE("Raxis",RMAG*R0EXP,ioutgyro,hdf5_ioutgyro,"/data")
  !
  ! PSI - CHI grid
  CALL WRITE_OUT_1D("PSI",eqchease_out(1)%coord_sys%grid%dim1(2:NISO1EFF1)/TWOPI*(-signipxp),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/grid/PSI")
  CALL WRITE_OUT_1D("S",grid_s(1:NCHI),NCHI, &
       ioutgyro,hdf5_ioutgyro,"/data/grid/S") 
  !
  ! 1-dim quantities
  CALL WRITE_OUT_1D("Rgeom",eqchease_out_add_1d(2:NISO1EFF1,iirgeo),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/Rgeom")
  CALL WRITE_OUT_1D("amin",eqchease_out_add_1d(2:NISO1EFF1,iiamin),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/amin")
  CALL WRITE_OUT_1D("damindpsi",damindpsi(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/damindpsi")
  CALL WRITE_OUT_1D("d2amindpsi2",d2amindpsi2(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/d2amindpsi2")
  CALL WRITE_OUT_1D("Bmax",eqchease_out_add_1d(2:NISO1EFF1,iiBmax)*signb0xp,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/Bmax")
  CALL WRITE_OUT_1D("Bmin",eqchease_out_add_1d(2:NISO1EFF1,iiBmin)*signb0xp,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/Bmin")
  CALL WRITE_OUT_1D("q",eqchease_out(1)%profiles_1d%q(2:NISO1EFF1)*signb0xp*(-signipxp),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/q")
  CALL WRITE_OUT_1D("dqdpsi",eqchease_out_add_1d(2:NISO1EFF1,iidqdpsi)*signb0xp*TWOPI,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/dqdpsi")
  CALL WRITE_OUT_1D("dqdpsi_chk",dqdpsi_chk(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro,"/data/var1d/dqdpsi_chk")
  CALL WRITE_OUT_1D("p",eqchease_out(1)%profiles_1d%pressure(2:NISO1EFF1),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/p")
  CALL WRITE_OUT_1D("dpdpsi",eqchease_out(1)%profiles_1d%pprime(2:NISO1EFF1)*(-signipxp)*TWOPI,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/dpdpsi")
  CALL WRITE_OUT_1D("jac",jac_new(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/jac")
  CALL WRITE_OUT_1D("djacdpsi",4._RKIND*CPI**2*djacdpsi(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/djacdpsi")
  CALL WRITE_OUT_1D("f",eqchease_out(1)%profiles_1d%F_dia(2:NISO1EFF1)*signb0xp,NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/f")
  CALL WRITE_OUT_1D("dfdpsi",dfdpsi(1:NISO1EFF),NISO1EFF, &
       ioutgyro,hdf5_ioutgyro, "/data/var1d/dfdpsi")
  !
  ! 2-dim quantities  
  CALL WRITE_OUT_2D("g11",g11(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/g11")
  CALL WRITE_OUT_2D("g12",g12(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/g12")
  CALL WRITE_OUT_2D("g13",g13(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/g13")
  CALL WRITE_OUT_2D("g22",g22(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/g22")
  CALL WRITE_OUT_2D("g23",g23(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/g23")
  CALL WRITE_OUT_2D("g33",g33(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/g33")
  CALL WRITE_OUT_2D("B",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iiB)*signb0xp, &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/B")
  CALL WRITE_OUT_2D("dBdpsi",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iidBdpsi)*TWOPI*signb0xp*(-signipxp), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dBdpsi")
  CALL WRITE_OUT_2D("dBds",dBds(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dBds")
  CALL WRITE_OUT_2D("R",eqchease_out(1)%coord_sys%position%R(2:NISO1EFF1,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/R") 
  CALL WRITE_OUT_2D("Z",eqchease_out(1)%coord_sys%position%Z(2:NISO1EFF1,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/Z") 
  CALL WRITE_OUT_2D("dRdpsi",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iidRdpsi)*TWOPI*(-signipxp), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dRdpsi") 
  CALL WRITE_OUT_2D("dRds",dRds(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dRds") 
  CALL WRITE_OUT_2D("dZdpsi",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iidZdpsi)*TWOPI*(-signipxp), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dZdpsi") 
  CALL WRITE_OUT_2D("dZds",dZds(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dZds") 
  CALL WRITE_OUT_2D("theta",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iitheta), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/theta") 
  !
  ! for testing purposes
  CALL WRITE_OUT_2D("Ah",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iiAh), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/Ah") 
  CALL WRITE_OUT_2D("dAhdpsi",eqchease_out_add_2d(2:NISO1EFF1,1:NCHI,iidAhdpsi)*TWOPI*(-signipxp), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dAhdpsi") 
  CALL WRITE_OUT_2D("dzetadpsi",dzetadpsi(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dzetadpsi") 
  CALL WRITE_OUT_2D("dzetadchi",dzetadchi(1:NISO1EFF,1:NCHI), &
       NISO1EFF,NCHI,ioutgyro,hdf5_ioutgyro,"/data/var2d/dzetadchi") 
  !
  ! CLOSE FILES
  CALL CLOSE_WRITE_OUT(ioutgyro,hdf5_ioutgyro)

END SUBROUTINE HAMADA
