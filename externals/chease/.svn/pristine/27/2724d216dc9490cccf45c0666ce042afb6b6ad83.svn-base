SUBROUTINE OGYROPSI

  !        ###################
  !
  !                                        AUTHORS:
  !                                        X. LAPILLONNE, CRPP-EPFL
  !                                        O. SAUTER,  CRPP-EPFL
  !*******************************************************************************************
  !                                                                                          *
  ! CHEASE OUTPUT FOR GYROKINETIC CODES GENE & ORB5                                          *
  ! THEN WRITES DATA, USING VARIABLE CONTAINING METRICS CALCULATED IN CHIPSIMETRIC           *
  ! OUTPUT WRITTEN IN HDF5 FILE OGYROPSI.H5                                                  *
  !*******************************************************************************************
  !
  USE globals
  IMPLICIT NONE
  !
  INTEGER :: noutgyro_hdf5
  !
  ! Open ASCII and HDF5 files
  CALL INIT_WRITE_OUT(NOUTGYRO,noutgyro_hdf5,'ogyropsi')
  !
  ! Saved Scalars
  CALL WRITE_OUT_SCALAR_INT("NPSI",NISO1EFF1,NOUTGYRO,noutgyro_hdf5,"/data")
  CALL WRITE_OUT_SCALAR_INT("NCHI",NCHI,NOUTGYRO,noutgyro_hdf5,"/data")
  CALL WRITE_OUT_SCALAR_RE("R0EXP",R0EXP,NOUTGYRO,noutgyro_hdf5,"/data")
  CALL WRITE_OUT_SCALAR_RE("B0EXP",B0EXP,NOUTGYRO,noutgyro_hdf5,"/data")
  CALL WRITE_OUT_SCALAR_INT("NRBOX",NRBOX,NOUTGYRO,noutgyro_hdf5,"/data")
  CALL WRITE_OUT_SCALAR_INT("NZBOX",NZBOX,NOUTGYRO,noutgyro_hdf5,"/data")
  CALL WRITE_OUT_SCALAR_INT("NBOUND",size(eqchease_out(1)%eqgeometry%boundary(1)%r(:)),NOUTGYRO,noutgyro_hdf5,"/data")
  !
  ! PSI - CHI grid
  CALL WRITE_OUT_1D("PSI",eqchease_out(1)%coord_sys%grid%dim1(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5,"/data/grid/PSI")
  CALL WRITE_OUT_1D("CHI",eqchease_out(1)%coord_sys%grid%dim2(1:NCHI),NCHI, &
       NOUTGYRO,noutgyro_hdf5,"/data/grid/CHI") 
  !
  ! 1-dim quantities
  CALL WRITE_OUT_1D("Rgeom",eqchease_out_add_1d(1:NISO1EFF1,iirgeo),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5,"/data/var1d/Rgeom")
  CALL WRITE_OUT_1D("ageom",eqchease_out_add_1d(1:NISO1EFF1,iiamin),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5,"/data/var1d/ageom")
  CALL WRITE_OUT_1D("q",eqchease_out(1)%profiles_1d%q(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5,"/data/var1d/q")
  CALL WRITE_OUT_1D("dqdpsi",eqchease_out_add_1d(1:NISO1EFF1,iidqdpsi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5,"/data/var1d/dqdpsi")
  CALL WRITE_OUT_1D("d2qdpsi2",eqchease_out_add_1d(1:NISO1EFF1,iid2qdpsi2),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5,"/data/var1d/d2qdpsi2")
  CALL WRITE_OUT_1D("p",eqchease_out(1)%profiles_1d%pressure(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/p")
  CALL WRITE_OUT_1D("dpdpsi",eqchease_out(1)%profiles_1d%pprime(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dpdpsi")
  CALL WRITE_OUT_1D("f",eqchease_out(1)%profiles_1d%F_dia(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/f")
  CALL WRITE_OUT_1D("fdfdpsi",eqchease_out(1)%profiles_1d%ffprime(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/fdfdpsi")
  CALL WRITE_OUT_1D("V",eqchease_out(1)%profiles_1d%volume(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/Volume")
  CALL WRITE_OUT_1D("rho_t",eqchease_out(1)%profiles_1d%rho_tor(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/rho_tor")
  CALL WRITE_OUT_1D("shear",eqchease_out_add_1d(1:NISO1EFF1,iishear),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/shear")
  CALL WRITE_OUT_1D("dsheardpsi",eqchease_out_add_1d(1:NISO1EFF1,iidsheardpsi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dsheardpsi")
  CALL WRITE_OUT_1D("kappa",eqchease_out(1)%profiles_1d%elongation(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/kappa")
  CALL WRITE_OUT_1D("delta_lower",eqchease_out(1)%profiles_1d%tria_lower(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/delta_lower")
  CALL WRITE_OUT_1D("delta_upper",eqchease_out(1)%profiles_1d%tria_upper(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/delta_upper")
  CALL WRITE_OUT_1D("dVdpsi",eqchease_out(1)%profiles_1d%vprime(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dVdpsi")
  CALL WRITE_OUT_1D("dpsidrhotor",eqchease_out(1)%profiles_1d%dpsidrho_tor(1:NISO1EFF1),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dpsidrhotor")
  CALL WRITE_OUT_1D("GDPSI_av",eqchease_out_add_1d(1:NISO1EFF1,iigradpsi_av),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/GDPSI_av")
  CALL WRITE_OUT_1D("radius_av",eqchease_out_add_1d(1:NISO1EFF1,iia_av),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/radius_av")
  CALL WRITE_OUT_1D("R_av",eqchease_out_add_1d(1:NISO1EFF1,iiR_av),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/R_av")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iiTe),eqchease_out_add_1d(1:NISO1EFF1,iiTe),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/Te")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iidTedpsi),eqchease_out_add_1d(1:NISO1EFF1,iidTedpsi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dTedpsi")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iine),eqchease_out_add_1d(1:NISO1EFF1,iine),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/ne")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iidnedpsi),eqchease_out_add_1d(1:NISO1EFF1,iidnedpsi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dnedpsi")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iiTi),eqchease_out_add_1d(1:NISO1EFF1,iiTi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/Ti")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iidTidpsi),eqchease_out_add_1d(1:NISO1EFF1,iidTidpsi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dTidpsi")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iini),eqchease_out_add_1d(1:NISO1EFF1,iini),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/ni")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iidnidpsi),eqchease_out_add_1d(1:NISO1EFF1,iidnidpsi),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/dnidpsi")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iizeff),eqchease_out_add_1d(1:NISO1EFF1,iizeff),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/zeff")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iisigneo),eqchease_out_add_1d(1:NISO1EFF1,iisigneo),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/signeo")
  CALL WRITE_OUT_1D(eqchease_out_add_1d_varnames(iijbsBav),eqchease_out_add_1d(1:NISO1EFF1,iijbsBav),NISO1EFF1, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/jbsBav")

  CALL WRITE_OUT_1D("Rmesh",eqchease_out(1)%profiles_2d(1)%grid%dim1(1:NRBOX),NRBOX, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/rmesh")
  CALL WRITE_OUT_1D("Zmesh",eqchease_out(1)%profiles_2d(1)%grid%dim2(1:NZBOX),NZBOX, &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/zmesh")
  CALL WRITE_OUT_1D("RBOUNDplasma",eqchease_out(1)%eqgeometry%boundary(1)%r(:),size(eqchease_out(1)%eqgeometry%boundary(1)%r(:)), &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/rboundplasma")
  CALL WRITE_OUT_1D("ZBOUNDplasma",eqchease_out(1)%eqgeometry%boundary(1)%z(:),size(eqchease_out(1)%eqgeometry%boundary(1)%z(:)), &
       NOUTGYRO,noutgyro_hdf5, "/data/var1d/zboundplasma")
  !
  !
  ! 2-dim quantities
  CALL WRITE_OUT_2D("g11",eqchease_out(1)%coord_sys%g_11(1:NISO1EFF1, 1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/g11")
  CALL WRITE_OUT_2D("g12",eqchease_out(1)%coord_sys%g_12(1:NISO1EFF1, 1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/g12")
  CALL WRITE_OUT_2D("g22",eqchease_out(1)%coord_sys%g_22(1:NISO1EFF1, 1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/g22")
  CALL WRITE_OUT_2D("g33",eqchease_out(1)%coord_sys%g_33(1:NISO1EFF1, 1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/g33")
  CALL WRITE_OUT_2D("B",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iiB), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/B")
  CALL WRITE_OUT_2D("dBdpsi",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iidBdpsi), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/dBdpsi")
  CALL WRITE_OUT_2D("dBdchi",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iidBdchi), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/dBdchi")
  CALL WRITE_OUT_2D("dPsidR",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iidpsidR), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/dPsidR")
  CALL WRITE_OUT_2D("dPsidZ",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iidpsidZ), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/dPsidZ")
  CALL WRITE_OUT_2D("dChidR",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iidchidR), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/dChidR") 
  CALL WRITE_OUT_2D("dChidZ",eqchease_out_add_2d(1:NISO1EFF1,1:NCHI,iidchidZ), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/dChidZ")

  CALL WRITE_OUT_2D("Jacobian",eqchease_out(1)%coord_sys%jacobian(1:NISO1EFF1,1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/Jacobian")
  CALL WRITE_OUT_2D("R",eqchease_out(1)%coord_sys%position%R(1:NISO1EFF1,1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/R") 
  CALL WRITE_OUT_2D("Z",eqchease_out(1)%coord_sys%position%Z(1:NISO1EFF1,1:NCHI), &
       NISO1EFF1,NCHI,NOUTGYRO,noutgyro_hdf5,"/data/var2d/Z") 
! R, Z
  CALL WRITE_OUT_2D("psiRZ",eqchease_out(1)%profiles_2d(1)%psi(1:NRBOX,1:NZBOX), &
       NRBOX,NZBOX,NOUTGYRO,noutgyro_hdf5,"/data/var2d/psiRZ")
  CALL WRITE_OUT_2D(eqchease_out_add_2d_rz_varnames(iiRZ_chi),eqchease_out_add_2d_rz(1:NRBOX,1:NZBOX,iiRZ_chi), &
       NRBOX,NZBOX,NOUTGYRO,noutgyro_hdf5,"/data/var2d/chiRZ") 
  !
  ! CLOSE FILES
  CALL CLOSE_WRITE_OUT(NOUTGYRO,noutgyro_hdf5)

END SUBROUTINE OGYROPSI
