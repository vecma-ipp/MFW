SUBROUTINE BGB_JETTO(EQUILIBRIUM, COREPROF, CORETRANSP, H_MODE, RHO_PEDESTAL_TOP_NORMALIZED)

 !This is the Bohm/gyro-Bohm model used in JETTO
 !Done during the ISM Working Session at JET (November 2012)
 !By A. Figueiredo, starting from a routine by D. Kalupin with contributions from J. Ferreira

 USE EUITM_SCHEMAS
 USE ITM_TYPES
 USE ALLOCATE_DEALLOCATE

 IMPLICIT NONE

 TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM(:)              !input CPO 
 TYPE (TYPE_COREPROF),    POINTER  :: COREPROF(:)                 !input CPO 
 TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP(:)               !output CPO
 INTEGER                           :: H_MODE                      !input: 0 for L-mode and 1 for H-mode confinement
 REAL(R8)                          :: RHO_PEDESTAL_TOP_NORMALIZED !normalized rho at the top of the pedestral

 REAL(R8)                          :: TIME
 REAL(R8),            ALLOCATABLE  :: RHO(:)
 REAL(R8),            ALLOCATABLE  :: RHO_EQUILIBRIUM(:)
 REAL(R8),            ALLOCATABLE  :: R_EQUILIBRIUM_INBOARD(:)
 REAL(R8),            ALLOCATABLE  :: R_EQUILIBRIUM_OUTBOARD(:)
 REAL(R8),            ALLOCATABLE  :: TE(:)
 REAL(R8),            ALLOCATABLE  :: DTE(:)
 REAL(R8),            ALLOCATABLE  :: NE(:)
 REAL(R8),            ALLOCATABLE  :: PE(:)
 REAL(R8),            ALLOCATABLE  :: DPE(:)
 REAL(R8),            ALLOCATABLE  :: Q(:)
 REAL(R8)                          :: A_0
 REAL(R8)                          :: BTOR

 REAL(R8),            ALLOCATABLE  :: DIFF_TE(:)       !output: electron thermal diffusivity [m^2/s]
 REAL(R8),            ALLOCATABLE  :: DIFF_TI(:,:)     !output: ion thermal diffusivity [m^2/s]

 INTEGER,               PARAMETER  :: NOCUR = 1        !number of CPO occurrences in the workflow
 INTEGER                           :: NRHO, IRHO       !number of radial points (input, determined from COREPROF CPO), radial index
 INTEGER                           :: NION, IION       !number of ion species (input, determined from COREPROF CPO), ion index
 INTEGER                           :: NIMP             !number of impurity species
 INTEGER                           :: MAX_NZIMP        !number of impurity ionization states
 INTEGER                           :: NPSI, IPSI       !number of flux surfaces (radial points in the equilibrium grid), psi/radial index
 REAL(R8)                          :: auxiliary_array_x(1), auxiliary_array_y(1) !these are to call L3interp
 REAL(R8)                          :: coefficient
 INTEGER                           :: index_lower, index_higher
 REAL(R8)                          :: rho_pedestal_top, r_pedestal_top, rho_internal, r_internal !these are for the non-local multiplier in the H-mode BgB model
 REAL(R8)                          :: TE_PEDESTAL_TOP, TE_INTERNAL, BOHM, GYROBOHM
 REAL(R8)                          :: BOHM_CORRECTION_RADIAL_SCALE !AF - this will be an input parameter as well

 NRHO = SIZE(COREPROF(1)%rho_tor, DIM=1)
 NPSI = SIZE(EQUILIBRIUM(1)%profiles_1d%rho_tor, DIM=1)
 NION = SIZE(COREPROF(1)%composition%amn, DIM=1)
 NIMP = 0
 MAX_NZIMP = 0

 CALL ALLOCATE_CORETRANSP_CPO(NOCUR, NRHO, NION, NIMP, MAX_NZIMP, CORETRANSP)

 ALLOCATE(RHO(NRHO))
 ALLOCATE(RHO_EQUILIBRIUM(NPSI))
 ALLOCATE(R_EQUILIBRIUM_INBOARD(NPSI))
 ALLOCATE(R_EQUILIBRIUM_OUTBOARD(NPSI))
 ALLOCATE(TE(NRHO))
 ALLOCATE(DTE(NRHO))
 ALLOCATE(NE(NRHO))
 ALLOCATE(Q(NRHO))
 ALLOCATE(PE(NRHO))
 ALLOCATE(DPE(NRHO))
 ALLOCATE(DIFF_TE(NRHO))
 ALLOCATE(DIFF_TI(NRHO,NION))

 TIME = COREPROF(1)%time
 RHO  = COREPROF(1)%rho_tor
 RHO_EQUILIBRIUM = EQUILIBRIUM(1)%profiles_1d%rho_tor
 !assuming that the equilibrium code provides r_inboard and r_outboard, which is true at least for SPIDER
 R_EQUILIBRIUM_INBOARD = EQUILIBRIUM(1)%profiles_1d%r_inboard !major radius at magnetic axis - inboard [m]
 R_EQUILIBRIUM_OUTBOARD = EQUILIBRIUM(1)%profiles_1d%r_outboard !major radius at magnetic axis - outboard [m]
 TE   = COREPROF(1)%te%value             !temperature [eV]
 NE   = COREPROF(1)%ne%value             !density [1/m^3]
 PE   = NE*TE                            !pressure [eV/m^3]
!PE   = COREPROF(1)%profiles1d%pe%value
 Q    = COREPROF(1)%profiles1d%q%value
 BTOR = EQUILIBRIUM(1)%global_param%mag_axis%bphi
 A_0 = (R_EQUILIBRIUM_OUTBOARD(NPSI) - R_EQUILIBRIUM_INBOARD(NPSI))/2.0_R8 !geometric minor radius of the plasma boundary
 Call L3deriv(TE, RHO, NRHO, DTE, RHO, NRHO) !dTE/dRHO
 Call L3deriv(PE, RHO, NRHO, DPE, RHO, NRHO) !dPE/dRHO

 IF (H_MODE.EQ.1) THEN !H-mode
    rho_pedestal_top = RHO_PEDESTAL_TOP_NORMALIZED*RHO(NRHO) !going from normalized to non-normalized rho
    !the following simple mapping from rho to R and back to rho should be good enough, but it could be refined by using a FLUSH-like library
    IPSI = 1
    DO WHILE ((RHO_EQUILIBRIUM(IPSI).LT.rho_pedestal_top).AND.(IPSI.LE.NPSI)) !(IPSI.LE.NPSI) will lead to IPSI=NPSI+1 if the equilibrium grid does not extend beyond the pedestal top, which is used below to signal a problem
       IPSI = IPSI + 1
    END DO
    IF (IPSI.EQ.1) THEN
       index_lower = 1
       index_higher = 2 !assuming NPSI is at least 2
       print *, 'Problem in the JETTO Bohm/gyro-Bohm actor: could not find the pedestal top in the equilibrium grid!'
    ELSE IF (IPSI.EQ.(NPSI+1)) THEN
       index_lower = NPSI - 1
       index_higher = NPSI
       print *, 'Problem in the JETTO Bohm/gyro-Bohm actor: could not find the pedestal top in the equilibrium grid!'
    ELSE
       index_lower = IPSI - 1
       index_higher = IPSI
    END IF
    coefficient = (rho_pedestal_top - RHO_EQUILIBRIUM(index_lower))/(RHO_EQUILIBRIUM(index_higher) - RHO_EQUILIBRIUM(index_lower))
    r_pedestal_top = R_EQUILIBRIUM_OUTBOARD(index_lower) + coefficient*(R_EQUILIBRIUM_OUTBOARD(index_higher) - R_EQUILIBRIUM_OUTBOARD(index_lower))
    BOHM_CORRECTION_RADIAL_SCALE = 10.85 ![cm] - this could in principle be an input parameter but that does not really make sense as long as this will be the faithful implementation of the JETTO Bohm/gyro-Bohm model in the ETS
    r_internal = r_pedestal_top - BOHM_CORRECTION_RADIAL_SCALE*1.0E-2_R8
    IPSI = 1
    DO WHILE ((R_EQUILIBRIUM_OUTBOARD(IPSI).LT.r_internal).AND.(IPSI.LE.NPSI)) !(IPSI.LE.NPSI) will lead to IPSI=NPSI+1 if the equilibrium grid does not extend beyond the pedestal top, which is used below to signal a problem
       IPSI = IPSI + 1
    END DO
    IF (IPSI.EQ.1) THEN !another option to deal with this problem would be to automatically switch to L-mode BgB, which would have to be clearly communicated to the user via the Kepler workflow
       index_lower = 1
       index_higher = 2 !assuming NPSI is at least 2
       print *, 'Problem in the JETTO Bohm/gyro-Bohm actor: could not find the pedestal top in the equilibrium grid!'
    ELSE IF (IPSI.EQ.(NPSI+1)) THEN
       index_lower = NPSI - 1
       index_higher = NPSI
       print *, 'Problem in the JETTO Bohm/gyro-Bohm actor: could not find the pedestal top in the equilibrium grid!'
    ELSE
       index_lower = IPSI - 1
       index_higher = IPSI
    END IF
    coefficient = (r_internal - R_EQUILIBRIUM_OUTBOARD(index_lower))/(R_EQUILIBRIUM_OUTBOARD(index_higher) - R_EQUILIBRIUM_OUTBOARD(index_lower))
    rho_internal = RHO_EQUILIBRIUM(index_lower) + coefficient*(RHO_EQUILIBRIUM(index_higher) - RHO_EQUILIBRIUM(index_lower))
    auxiliary_array_x(1) = rho_pedestal_top
    Call L3interp(TE, RHO, NRHO, auxiliary_array_y, auxiliary_array_x, 1)
    TE_PEDESTAL_TOP = auxiliary_array_y(1)
    auxiliary_array_x(1) = rho_internal
    Call L3interp(TE, RHO, NRHO, auxiliary_array_y, auxiliary_array_x, 1)
    TE_INTERNAL = auxiliary_array_y(1) 
 END IF

 DO IRHO = 1, NRHO
   BOHM = 2.0E-4_R8*A_0*ABS(DPE(IRHO))*Q(IRHO)**2/(ABS(BTOR)*NE(IRHO)) !Bohm contribution for L-mode - taking the absolute value of BTOR in case it will be negative due to a convention of signs
   IF (H_MODE.EQ.1) THEN !H-mode
      BOHM = BOHM*0.4_R8*ABS(TE_PEDESTAL_TOP-TE_INTERNAL)/TE_PEDESTAL_TOP !non-local correction to Bohm contribution for H-mode plasmas
   END IF
   GYROBOHM = 5.0E-6_R8*SQRT(TE(IRHO))*ABS(DTE(IRHO))/BTOR**2 !gyro-Bohm contribution
   DIFF_TE(IRHO) = BOHM + GYROBOHM 
   DIFF_TI(IRHO, 1:NION) = 2.0_R8*BOHM + 0.5_R8*GYROBOHM !no distinction between ions
 END DO

 CORETRANSP(1)%time                  =  TIME
 CORETRANSP(1)%rho_tor               =  RHO
 CORETRANSP(1)%sigma                 =  0.E0_R8
 CORETRANSP(1)%te_transp%diff_eff    =  DIFF_TE
 CORETRANSP(1)%te_transp%vconv_eff   =  0.E0_R8
 CORETRANSP(1)%ni_transp%diff_eff    =  0.E0_R8
 CORETRANSP(1)%ni_transp%vconv_eff   =  0.E0_R8
 CORETRANSP(1)%ti_transp%diff_eff    =  DIFF_TI
 CORETRANSP(1)%ti_transp%vconv_eff   =  0.E0_R8
 CORETRANSP(1)%vtor_transp%diff_eff  =  0.E0_R8
 CORETRANSP(1)%vtor_transp%vconv_eff =  0.E0_R8
 !the following 3 lines are required otherwise the transport combiner actor will break
 CORETRANSP(1)%composition%amn = COREPROF(1)%composition%amn
 CORETRANSP(1)%composition%zn = COREPROF(1)%composition%zn
 CORETRANSP(1)%composition%zion = COREPROF(1)%composition%zion

 DEALLOCATE(RHO)
 DEALLOCATE(RHO_EQUILIBRIUM)
 DEALLOCATE(R_EQUILIBRIUM_INBOARD)
 DEALLOCATE(R_EQUILIBRIUM_OUTBOARD)
 DEALLOCATE(TE)
 DEALLOCATE(DTE)
 DEALLOCATE(NE)
 DEALLOCATE(PE)
 DEALLOCATE(DPE)
 DEALLOCATE(Q)
 DEALLOCATE(DIFF_TE)
 DEALLOCATE(DIFF_TI)

END SUBROUTINE BGB_JETTO
