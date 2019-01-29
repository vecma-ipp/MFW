! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
SUBROUTINE EQUILIBRIUM_SPIDER  (EQUILIBRIUM_IN, COREPROF_IN, EQUILIBRIUM_OUT, code_parameters)

!-------------------------------------------------------!
!     This routine provides interface between ETS       !
!     and SPIDER equilibrium solver.                    !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin, G.Pereverzev             !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     created for ETS V&V                 !
!                                                       !
!                   for questions on SPIDER contact:    !
!                   Sergei Medvedev                     !
!                   (medvedev@a5.kiam.ru)               !
!                   Andrei Ivanov                       !
!                   (aai@a5.kiam.ru)                    !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES

    USE ITM_CONSTANTS
    USE EUITM_XML_PARSER  
    USE DEALLOCATE_STRUCTURES

    USE MOD_SPIDER
    USE INTEGRATION

    IMPLICIT NONE


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
    TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input CPO with plasma profiles

    TYPE (TYPE_PARAM)                 :: code_parameters



! +++ Dimensions:
    INTEGER                           :: NPSI                !number of PSI points      
    INTEGER                           :: NDIM1, NDIM2        !number of points for 2-D parameters
    INTEGER                           :: IDIM1, IDIM2        !index of points for 2-D parameters
    INTEGER                           :: MAX_NPOINTS         !index of boundary points 

    INTEGER                           :: ICONTROL,IINPUT     !control sitch for SPIDER configuration
    INTEGER                           :: KEY_ITER            !control sitch for SPIDER iterrations
    INTEGER                           :: NRHO,  NTHETA,  NEQ !number of points for SPIDER I/O
    INTEGER                           :: NBND                !number of points for SPIDER I/O
    INTEGER                           :: NTRANSP             !number of points for COREPROF
    INTEGER                           :: IEQ                 !index of points for SPIDER I/O grid
    INTEGER                           :: IBND                !index of boundary points 
    INTEGER                           :: ITIME               !time step number for SPIDER 
    INTEGER                           :: ITER                !iterration number 
    INTEGER                           :: ITHETA, IRHO        !
    INTEGER,                PARAMETER :: SPID_OUT = 0


! +++ Internal parameters(0-D parameters):
    REAL (R8)                         :: R0, Z0              !magnetic axis (on first iteration equal to geom.axis)
    REAL (R8)                         :: B0                  !magnetic field on axis
    REAL (R8)                         :: PC,   PC_INT        !total plasma current
    REAL (R8)                         :: AMIN                !minor radius
    REAL (R8)                         :: RHON                !RHO (boundary value)
    REAL (R8)                         :: RHONNEW             !RHO (new boundary value)
    REAL (R8),            ALLOCATABLE :: BTMIN(:), BTMAX(:)
    REAL (R8)                         :: ELONGATION
    REAL (R8)                         :: TRIA_UPPER
    REAL (R8)                         :: TRIA_LOWER

!   Input:
    REAL (R8),            ALLOCATABLE :: RHO(:)
    REAL (R8),            ALLOCATABLE :: R_INBOARD(:), R_OUTBOARD(:)
    REAL (R8),            ALLOCATABLE :: RR(:), RR_IN(:)
    REAL (R8),            ALLOCATABLE :: Jpar(:), INTJpar(:)
    REAL (R8),            ALLOCATABLE :: J_BS(:), J_CD(:)
    REAL (R8),            ALLOCATABLE :: QSF(:)
    REAL (R8),            ALLOCATABLE :: MU(:)
    REAL (R8),            ALLOCATABLE :: PR(:),   DPR(:)
    REAL (R8),            ALLOCATABLE :: TE(:)
    REAL (R8),            ALLOCATABLE :: PSI(:)
    REAL (R8),            ALLOCATABLE :: SIGMA(:)
    REAL (R8),            ALLOCATABLE :: PPRIME(:)
    REAL (R8),            ALLOCATABLE :: FFPRIME(:)
    REAL (R8),            ALLOCATABLE :: RZBND(:)
    REAL (R8),            ALLOCATABLE :: ROUT(:,:), ZOUT(:,:)

!   Output from SPIDER:
    REAL (R8),            ALLOCATABLE :: RHONEW(:)
    REAL (R8),            ALLOCATABLE :: RHONEW_S(:)
    REAL (R8),            ALLOCATABLE :: GM11(:),  GM22(:), GM33(:)
    REAL (R8),            ALLOCATABLE :: B2B02(:)
    REAL (R8),            ALLOCATABLE :: BB0(:)
    REAL (R8),            ALLOCATABLE :: B02B2(:)
    REAL (R8),            ALLOCATABLE :: FDIA(:)
    REAL (R8),            ALLOCATABLE :: JDIA(:), JDIAS(:)
    REAL (R8),            ALLOCATABLE :: VOL(:),  VPRIME(:),  VPRIMES(:), VPRIME_PSI(:)
    REAL (R8),            ALLOCATABLE :: SURF(:)
    REAL (R8),            ALLOCATABLE :: GRADRHO(:)
    REAL (R8),            ALLOCATABLE :: DRHODA(:)
    REAL (R8),            ALLOCATABLE :: SHAFR_SHIFT(:)
    REAL (R8),            ALLOCATABLE :: ELONG(:)
    REAL (R8),            ALLOCATABLE :: ELONG_U(:)
    REAL (R8),            ALLOCATABLE :: ELONG_L(:)
    REAL (R8),            ALLOCATABLE :: TRIANG_L(:)
    REAL (R8),            ALLOCATABLE :: TRIANG_U(:)
    REAL (R8),            ALLOCATABLE :: FTRAP(:)

!   Output to ETS:
    REAL (R8),            ALLOCATABLE :: G1(:), G2(:), G3(:), G4(:)
    REAL (R8),            ALLOCATABLE :: G5(:), G6(:), G7(:)
    REAL (R8),            ALLOCATABLE :: AREA(:)

!   Convergence parameters:
    REAL (R8)                         :: CONV
    REAL (R8)                         :: TOLERANCE = 1.0E-4_R8
    REAL (R8)                         :: err_VPRIME,    err_G1,     err_FDIA
    REAL (R8),            ALLOCATABLE :: VPRIME_OLD(:), G1_OLD(:),  FDIA_OLD(:)

!   Work:
    REAL (R8)                         :: THETA, TRIA
    REAL (R8),            ALLOCATABLE :: AMID(:)
    REAL (R8),            ALLOCATABLE :: FUN(:), INTFUN(:), FUN_IN(:)

    REAL (R8),            ALLOCATABLE :: AMID_2D(:), RHO_2D(:)
    REAL (R8),            ALLOCATABLE :: SHAFR_SHIFT_2D(:), ELONG_2D(:), ELONG_U2D(:), ELONG_L2D(:), TRIANG_L2D(:), TRIANG_U2D(:)
    REAL (R8),            ALLOCATABLE :: PSI_2D(:), JPAR_2D(:), PR_2D(:), G11_2D(:), G22_2D(:), G33_2D(:)

    INTEGER                           :: return_status
    LOGICAL,              SAVE        :: first=.true.
    INTEGER                           :: i,j

    CHARACTER*18                      :: STR
!-------------------------------------------------------!



! +++ Assign SPIDER parameters:
    CALL ASSIGN_SPIDER_PARAMETERS(code_parameters, return_status)
    
    IF (return_status /= 0) THEN
       WRITE(*,*) 'ERROR: Could not assign SPIDER parametrs.'
    END IF



! +++ Set dimensions and allocate parameters:
    NPSI        =  SIZE (EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,      DIM=1)
    NDIM1       =  SIZE (EQUILIBRIUM_IN(1)%coord_sys%position%R,     DIM=1)
    NDIM2       =  SIZE (EQUILIBRIUM_IN(1)%coord_sys%position%R,     DIM=2)
    MAX_NPOINTS =  SIZE (EQUILIBRIUM_IN(1)%eqgeometry%boundary(1)%r, DIM=1)
    NTRANSP     =  SIZE (COREPROF_IN(1)%rho_tor,                     DIM=1)


    NBND        =  MAX_NPOINTS 




! +++ Set time index for SPIDER:
       ITIME = 0



! +++ Allocate internal parameters:
      ALLOCATE  (RHO(NPSI))
      ALLOCATE  (R_INBOARD(NPSI))
      ALLOCATE  (R_OUTBOARD(NPSI))
      ALLOCATE  (RR(NPSI))
      ALLOCATE  (RR_IN(NPSI))
      ALLOCATE  (Jpar(NPSI))
      ALLOCATE  (INTJpar(NPSI))
      ALLOCATE  (J_BS(NPSI))
      ALLOCATE  (J_CD(NPSI))
      ALLOCATE  (QSF(NPSI))
      ALLOCATE  (MU(NPSI))
      ALLOCATE  (PR(NPSI))
      ALLOCATE  (DPR(NPSI))
      ALLOCATE  (TE(NPSI))
      ALLOCATE  (PSI(NPSI))
      ALLOCATE  (SIGMA(NPSI))
      ALLOCATE  (PPRIME(NPSI))
      ALLOCATE  (FFPRIME(NPSI))
      ALLOCATE  (RZBND(NBND * 2))
      ALLOCATE  (ROUT(NPSI,NTHETA))
      ALLOCATE  (ZOUT(NPSI,NTHETA))

      ALLOCATE  (B2B02(NPSI))
      ALLOCATE  (BB0(NPSI))
      ALLOCATE  (B02B2(NPSI))
      ALLOCATE  (RHONEW(NPSI))
      ALLOCATE  (RHONEW_S(NPSI))
      ALLOCATE  (GM11(NPSI))
      ALLOCATE  (GM22(NPSI))
      ALLOCATE  (GM33(NPSI))
      ALLOCATE  (FDIA(NPSI))
      ALLOCATE  (JDIA(NPSI))
      ALLOCATE  (JDIAS(NPSI))
      ALLOCATE  (BTMIN(NPSI))
      ALLOCATE  (BTMAX(NPSI))
      ALLOCATE  (VOL(NPSI))
      ALLOCATE  (VPRIME(NPSI))
      ALLOCATE  (VPRIMES(NPSI))
      ALLOCATE  (VPRIME_PSI(NPSI))
      ALLOCATE  (SURF(NPSI))
      ALLOCATE  (GRADRHO(NPSI))
      ALLOCATE  (DRHODA(NPSI))
      ALLOCATE  (SHAFR_SHIFT(NPSI))
      ALLOCATE  (ELONG(NPSI))
      ALLOCATE  (ELONG_U(NPSI))
      ALLOCATE  (ELONG_L(NPSI))
      ALLOCATE  (TRIANG_L(NPSI))
      ALLOCATE  (TRIANG_U(NPSI))
      ALLOCATE  (AMID(NPSI))
      ALLOCATE  (FTRAP(NPSI))

      ALLOCATE  (G1(NPSI))
      ALLOCATE  (G2(NPSI))
      ALLOCATE  (G3(NPSI))
      ALLOCATE  (G4(NPSI))
      ALLOCATE  (G5(NPSI))
      ALLOCATE  (G6(NPSI))
      ALLOCATE  (G7(NPSI))
      ALLOCATE  (AREA(NPSI))

      ALLOCATE  (FUN(NPSI))
      ALLOCATE  (FUN_IN(NPSI))
      ALLOCATE  (INTFUN(NPSI))

      ALLOCATE  (VPRIME_OLD(NPSI))
      ALLOCATE  (G1_OLD(NPSI))
      ALLOCATE  (FDIA_OLD(NPSI))

      ALLOCATE  (AMID_2D(NDIM1))
      ALLOCATE  (RHO_2D(NDIM1))
      ALLOCATE  (ELONG_2D(NDIM1))
      ALLOCATE  (ELONG_U2D(NDIM1))
      ALLOCATE  (ELONG_L2D(NDIM1))
      ALLOCATE  (TRIANG_L2D(NDIM1))
      ALLOCATE  (TRIANG_U2D(NDIM1))
      ALLOCATE  (SHAFR_SHIFT_2D(NDIM1))
      ALLOCATE  (PSI_2D(NDIM1))
      ALLOCATE  (JPAR_2D(NDIM1))
      ALLOCATE  (PR_2D(NDIM1))
      ALLOCATE  (G11_2D(NDIM1))
      ALLOCATE  (G22_2D(NDIM1))
      ALLOCATE  (G33_2D(NDIM1))

      RHO           = 0.0_R8
      R_INBOARD     = 0.0_R8
      R_OUTBOARD    = 0.0_R8
      RR            = 0.0_R8
      RR_IN         = 0.0_R8
      Jpar          = 0.0_R8
      INTJpar       = 0.0_R8
      J_BS          = 0.0_R8
      J_CD          = 0.0_R8
      QSF           = 0.0_R8
      MU            = 0.0_R8
      PR            = 0.0_R8
      DPR           = 0.0_R8
      TE            = 0.0_R8
      PSI           = 0.0_R8
      SIGMA         = 0.0_R8
      PPRIME        = 0.0_R8
      FFPRIME       = 0.0_R8
      RZBND         = 0.0_R8
      ROUT          = 0.0_R8
      ZOUT          = 0.0_R8

      B2B02         = 0.0_R8
      BB0           = 0.0_R8
      B02B2         = 0.0_R8
      RHONEW        = 0.0_R8
      RHONEW_S      = 0.0_R8
      GM11          = 0.0_R8
      GM22          = 0.0_R8
      GM33          = 0.0_R8
      FDIA          = 0.0_R8
      JDIA          = 0.0_R8
      BTMIN         = 0.0_R8
      BTMAX         = 0.0_R8
      VOL           = 0.0_R8
      VPRIME        = 0.0_R8
      VPRIMES       = 0.0_R8
      VPRIME_PSI    = 0.0_R8
      SURF          = 0.0_R8
      GRADRHO       = 0.0_R8
      DRHODA        = 0.0_R8
      SHAFR_SHIFT   = 0.0_R8
      ELONG         = 0.0_R8
      ELONG_U       = 0.0_R8
      ELONG_L       = 0.0_R8
      TRIANG_L      = 0.0_R8
      TRIANG_U      = 0.0_R8
      AMID          = 0.0_R8
      FTRAP         = 0.0_R8

      G1            = 0.0_R8
      G2            = 0.0_R8
      G3            = 0.0_R8
      G4            = 0.0_R8
      G5            = 0.0_R8
      G6            = 0.0_R8
      G7            = 0.0_R8
      AREA          = 0.0_R8

      FUN           = 0.0_R8

      VPRIME_OLD    = 1.0_R8
      G1_OLD        = 1.0_R8
      FDIA_OLD      = 1.0_R8


  


! +++ equidistant equilibrium grid:
      RHON   =  EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(NPSI)
      DO IEQ=1,NPSI
         RHO(IEQ)   =  RHON / (NPSI-1) * (IEQ-1)                   
      END DO



! +++ Equilibrium boundary:
      DO IBND=1,NBND*2
         IF (IBND.LE.NBND) THEN
            RZBND(IBND) = EQUILIBRIUM_IN(1)%eqgeometry%boundary(1)%r(IBND)
         ELSE IF (IBND.GT.NBND) THEN
            RZBND(IBND) = EQUILIBRIUM_IN(1)%eqgeometry%boundary(1)%z(IBND-NBND)
         END IF
      END DO
 



! +++ Set parameters:
      R0          =  EQUILIBRIUM_IN(1)%global_param%mag_axis%position%r
      Z0          =  EQUILIBRIUM_IN(1)%global_param%mag_axis%position%z
      B0          =  EQUILIBRIUM_IN(1)%global_param%toroid_field%b0
      AMIN        =  EQUILIBRIUM_IN(1)%eqgeometry%a_minor
      ELONGATION  =  EQUILIBRIUM_IN(1)%eqgeometry%elongation
      TRIA_UPPER  =  EQUILIBRIUM_IN(1)%eqgeometry%tria_upper
      TRIA_LOWER  =  EQUILIBRIUM_IN(1)%eqgeometry%tria_lower



!   From EQUILIBRIUM:
      CALL L3INTERP (-EQUILIBRIUM_IN(1)%profiles_1d%psi,        EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     PSI,                                       RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%jparallel,   EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     Jpar,                                      RHO,                                    NPSI)
      CALL L3INTERP (-EQUILIBRIUM_IN(1)%profiles_1d%q,          EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     QSF,                                       RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%pressure,    EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     PR,                                        RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%gm5 / B0**2, EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     B2B02,                                     RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%F_dia/R0/B0, EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     JDIA,                                      RHO,                                    NPSI)   
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%F_dia,       EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     FDIA,                                      RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%volume,      EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     VOL,                                       RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%gm1,         EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     G1,                                        RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%gm2,         EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     G2,                                        RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%gm3,         EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     G3,                                        RHO,                                    NPSI)

      CALL L3DERIV  (VOL, RHO, NPSI,    VPRIME, RHO, NPSI)




!   From COREPROF:
      CALL L3INTERP (COREPROF_IN(1)%te%value,                   COREPROF_IN(1)%rho_tor,                 NTRANSP, &
                     TE,                                        RHO,                                    NPSI)
      IF (ASSOCIATED(COREPROF_IN(1)%psi%sigma_par%value)) THEN
         CALL L3INTERP (COREPROF_IN(1)%psi%sigma_par%value,     COREPROF_IN(1)%rho_tor,                 NTRANSP, &
                        SIGMA,                                  RHO,                                    NPSI)
      ELSE
         SIGMA    = 1.0E7_R8
      END IF
      IF (ASSOCIATED(COREPROF_IN(1)%psi%jni%value)) THEN
         CALL L3INTERP (COREPROF_IN(1)%psi%jni%value,           COREPROF_IN(1)%rho_tor,                 NTRANSP, &
                        J_CD,                                   RHO,                                    NPSI)
      ELSE
         J_CD    = 0.0E0_R8
      END IF




! +++ Calculate major radius, RR:
      IF(ASSOCIATED(EQUILIBRIUM_IN(1)%profiles_1d%r_inboard).AND.                                                &
         ASSOCIATED(EQUILIBRIUM_IN(1)%profiles_1d%r_outboard)) THEN
         RR_IN   = (EQUILIBRIUM_IN(1)%profiles_1d%r_inboard + EQUILIBRIUM_IN(1)%profiles_1d%r_outboard) / 2.0_R8
         CALL L3INTERP (RR_IN,                                 EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,     &
                        RR,                                    RHO,                                    NPSI)
      ELSE
         RR      = R0
      END IF


! +++ Set parameters in SPIDER units:
      MU       = 1.0_R8 / QSF
      J_BS     = 0.0_R8 *  1.0E-6_R8    ! [MA/m^2]
      J_CD     = J_CD   *  1.0E-6_R8    ! [MA/m^2]
      Jpar     = Jpar   *  1.0E-6_R8    ! [MA/m^2]
      PR       = PR     *  1.0E-6_R8    ! [MPa]
      SIGMA    = SIGMA  *  1.0E-6_R8    ! [1/ (mkOhm*m)]




! +++ Iterrations on equilibrium and current profile:
    ITER       = 0

7   CONTINUE
    
    ITER       = ITER +1

    IF (ITER.GT.100) THEN
       WRITE(*,*) '### ERROR!  Equilibrium loop in EQUILIBRIUM_SPIDER not converging'
       STOP       'Error - no convergence in EQUILIBRIUM_SPIDER'
       RETURN

    END IF

    VPRIME_OLD = VPRIME                               !copy of equilibrium quantities for iterations
    G1_OLD     = G1
    FDIA_OLD   = FDIA




! +++ Calculate P' and FF':
    IF (IINPUT.EQ.0) THEN
      CALL L3DERIV(PR, RHO, NPSI,    DPR, RHO, NPSI)

      PPRIME     = - RR * QSF / B0 / RHO * DPR 

      IF (RHO(1).EQ.0.0_R8) PPRIME(1)  = PPRIME(2) 

      FFPRIME    = JDIA / B2B02 * (Jpar * RR / R0 - PPRIME)

    ELSE IF (IINPUT.EQ.1) THEN

      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%pprime,      EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     PPRIME,                                    RHO,                                    NPSI)
      CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%ffprime,     EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,  NPSI,    &
                     FFPRIME,                                   RHO,                                    NPSI)
    END IF


! +++ Check current:
    FUN        =  0.0E0_R8 
    FUN        =  VPRIME * Jpar / 2.0E0_R8 / ITM_PI * B0 / FDIA**2
    CALL INTEGRAL_VALUE  (NPSI, RHO, FUN, INTJpar)
    PC_INT     =  INTJpar(NPSI) * FDIA(NPSI) / 1.0e6_R8

    IF(EQUILIBRIUM_IN(1)%global_param%i_plasma.GT.-1.0e+40_R8) THEN
       PC     =  EQUILIBRIUM_IN(1)%global_param%i_plasma / 1.0e6_R8

    ELSE
       PC     =  PC_INT

    END IF




! +++ Save 'spidat.out' file:
    IF (SPID_OUT.EQ.1) THEN
      WRITE(*,*) ''
      WRITE(*,*) '-------------------------------------------------------'
      WRITE(*,*) 'Set Ip (MA)        ',PC
      WRITE(*,*) 'Calculated Ip (MA) ',PC_INT
      WRITE(*,*) 'R0, RR, B0 ',        R0, RR(NPSI), B0
      WRITE(*,*) '-------------------------------------------------------'
      WRITE(*,*) ''


     OPEN (2,file='spidat.out')  
       WRITE(2,100) ' jNPSIL,jnteta,NBND'
       WRITE(2,102) NRHO,NTHETA,NBND
       WRITE(2,100) ' (rzbnd(j),j=1,2*NBND)'
       WRITE(2,101) (rzbnd(i),i=1,nbnd*2)
       WRITE(2,100) ' na1'
       WRITE(2,102) NPSI
       WRITE(2,100) ' (eqpf(j),j=1,na1)'
       WRITE(2,101) (PPRIME(i),i=1,NPSI)
       WRITE(2,100) ' (eqff(j),j=1,na1)'
       WRITE(2,101) (FFPRIME(i),i=1,NPSI)
       WRITE(2,100) ' (fp(j),j=1,na1)'
       WRITE(2,101) (PSI(i),i=1,NPSI)
       WRITE(2,100) ' (rho(j),j=1,na1)'
       WRITE(2,101) (RHO(i),i=1,NPSI)
       WRITE(2,100) ' ipl,rtor,btor,roc,jnstep,'
       WRITE(2,103)  PC,R0,B0,RHON,ITIME	 
       WRITE(2,100) ' (fp(j),j=1,na1)'
       WRITE(2,101) (PSI(i),i=1,NPSI)
       WRITE(2,100) ' (g11(j),j=1,na1)'
       WRITE(2,101) (GM11(i),i=1,NPSI)
       WRITE(2,100) ' (g22(j),j=1,na1)'
       WRITE(2,101) (GM22(i),i=1,NPSI)
       WRITE(2,100) ' (g33(j),j=1,na1)'
       WRITE(2,101) (GM33(i),i=1,NPSI)
       WRITE(2,100) ' (vr(j),j=1,na1)'
       WRITE(2,101) (VPRIME(i),i=1,NPSI)
       WRITE(2,100) ' (vrs(j),j=1,na1)'
       WRITE(2,101) (VPRIMES(i),i=1,NPSI)
       WRITE(2,100) ' (slat(j),j=1,na1)'
       WRITE(2,101) (SURF(i),i=1,NPSI)
       WRITE(2,100) ' (gradro(j),j=1,na1)'
       WRITE(2,101) (GRADRHO(i),i=1,NPSI)
       WRITE(2,100) ' (mu(j),j=1,na1)'
       WRITE(2,101) (MU(i),i=1,NPSI)
       WRITE(2,100) ' (ipol(j),j=1,na1)'
       WRITE(2,101) (JDIA(j),j=1,NPSI)
       WRITE(2,100) ' (cc(j),j=1,na1)'
       WRITE(2,101) (SIGMA(j),j=1,NPSI)
       WRITE(2,100) ' (Te(j),j=1,na1)'
       WRITE(2,101) (Te(j),j=1,NPSI)
       WRITE(2,100) ' (cubs(j),j=1,na1)'
       WRITE(2,101) (J_BS(j),j=1,NPSI)
       WRITE(2,100) ' (cd(j),j=1,na1)'
       WRITE(2,101) (J_CD(j),j=1,NPSI)
       WRITE(2,100) ' (pres(j),j=1,na1)'
       WRITE(2,101) (PR(j),j=1,NPSI)
       WRITE(2,100) ' (cu(j),j=1,na1)'
       WRITE(2,101) (JPAR(j),j=1,NPSI)
     CLOSE(2)
! +++
 100  FORMAT(A)
 101  FORMAT(1P,3G24.15E3)
 102  FORMAT(3I12)
 103  FORMAT(1P,3G24.15E3/G24.15E3,I12)
     END IF

 


! +++ Call SPIDER equilibrium solver:
    CALL INT_SPIDER(       &
! Control:
         ICONTROL   &! key_equil in SPIDER (control key for spider input)
!        ICONTROL = 0 - equilibrium with prescribed p' and ff' profiles 
!			+ toroidal plasma current, Ipl;
!              The following inputs are used:
!              fp   - psi 1D  array,fp size is na1,
!              eqpf - p'*rtor 1D array,yeqpf size is na1,
!              eqff - ff'/rtor 1D array,yeqff size is na1,
!              ipl  -  toroidal plasma current. 
!		    rho, cu, pres are not used.
!        ICONTROL =-10 - equilibrium with prescribed p and <(j,B)>;
!              The following inputs are used:
!              rtor, btor - btor is vacuum toroidal field at r=rtor
!              rho - toroidal flux 1D array, sqrt(Phi/btor) rho size is na1
!              cu  - <(j,B)>/btor 1D array, cu size is na1
!              pres - pressure 1D array,pres size is na1
!			fp, eqpf, eqpp, ipl are not used.
! Input:
        ,NRHO	     &! radial grid in SPIDER
        ,NTHETA	     &! poloidal grid in SPIDER
        ,NBND	     &! boundary points
        ,RZBND	     &! array(2*nbnd) of boundary points
        ,NPSI	     &! radial grid for input (dimension of all arrays below)
        ,PPRIME	     &! p'
        ,FFPRIME     &! FF'
        ,RHO	     &! rho
        ,PC	     &! Ip           [MA]
        ,R0	     &! R0           [m] 
        ,Z0	     &! Z0           [m] 
        ,B0	     &! b0           [T]
        ,RHON	     &! rho_bnd      [m]
! Output:
        ,RHONNEW     &! new rho_bnd  [m] 
! Input:
        ,ITIME	     &! time step number
! Input (and output if key_dmf = -2, -3)
        ,PSI	     &! psi          [Wb]                            !defined on the main grid
! Output:
        ,GM11	     &! <|nabla_rho|^2> * V'                         !defined on intermediate grid
        ,GM22	     &! V' R0 / (4*pi^2*Jdia) * <|nabla_rho/r|^2>    !defined on intermediate grid
        ,GM33	     &! <R0^2/r^2>                                   !defined on the main grid
        ,VPRIME	     &! V'=dV/drho on the na1 grid                   !defined on the main grid
        ,VPRIMES     &! V'=dV/drho on the shifted grid               !defined on intermediate grid
        ,SURF	     &! magnetic surface area [m^2]                  !defined on intermediate grid
        ,GRADRHO     &! |nabla_rho|                                  !defined on intermediate grid
        ,MU	     &! mu = 1/q                                     !defined on intermediate grid
        ,JDIA	     &! F_dia/R0/B0                                  !defined on the main grid
        ,BTMAX       &! max of Bt on magn.axis                       !defined on intermediate grid
        ,BTMIN       &! min of Bt on magn.axis                       !defined on intermediate grid
        ,B2B02       &! <B**2/B0**2>                                 !defined on intermediate grid
        ,BB0         &! <B/B0>                                       !defined on intermediate grid
        ,B02B2       &! <B0**2/B**2>                                 !defined on intermediate grid
        ,DRHODA      &! d_rho/d_a - translation from a to rho grid
        ,ROUT	     &! flux surface coordinate [m]                   
        ,ZOUT	     &! flux surface coordinate [m]                  
        ,SHAFR_SHIFT &! 
        ,ELONG       &! 
        ,ELONG_U     &! 
        ,ELONG_L     &! 
        ,TRIANG_L    &!
        ,TRIANG_U    &!
        ,AMID        &!
        ,FTRAP       &!
! Input:        
        ,SIGMA	     &! sigma        [10^6*Ohm^-1*m^-1]              !defined on the main grid
        ,TE	     &! Te           [eV]                            !defined on the main grid
        ,j_BS	     &! j_bootstrap  [MA/m^2]                        !defined on the main grid
        ,J_CD	     &! j_CD         [MA/m^2]                        !defined on the main grid
! Optional input:
        ,PR	     &! pressure     [MJ/m^3]                        !defined on the main grid
        ,Jpar)        ! j_parallel   [MA/m^2]                        !defined on the main grid



    
! +++ Output equilibrium grids from SPIDER output:
      DO IEQ=1,NPSI-1
         RHONEW(IEQ)   =  RHO(IEQ)                                 !main grid               
      END DO
      RHONEW(NPSI)      =  RHONNEW                                  !last point of main and intermediate grids coinside

      DO IEQ=1,NPSI-1
         RHONEW_S(IEQ) =  0.5_R8 * (RHONEW(IEQ) + RHONEW(IEQ+1))   !intermediate grid                 
      END DO
      RHONEW_S(NPSI-1)  =  RHONEW_S(NPSI-2) + RHONEW(2) - RHONEW(1)  !NPSI-1 point of main and intermediate grids coinside
      RHONEW_S(NPSI)    =  RHONNEW                                  !last point of main and intermediate grids coinside




! +++ Calculate quantities not included in SPIDER output:
!     Metric coefficients defined on the main grid:
      G1      =  GM33 / R0**2                                      !<1/R^2>                [-]
      FDIA    =  JDIA * R0 * B0
        
      CALL INTEGRAL_VALUE  (NPSI, RHONEW, VPRIME, VOL)              !volume                 [m^3]




!   Metric coefficients defined on intermediate grid:
      G2      =  GM22 / VPRIMES * 4.0_R8 * ITM_PI**2 / R0          !<(grad_rho^2/R^2>/Jdia [-]
      G3      =  GM11 / VPRIMES                                    !<(grad_rho)^2>         [-]
      G4      =  B02B2 / B0**2                                     !<1/B^2>                [T^-2]
      G5      =  B2B02 * B0**2                                     !<B^2>                  [T^2]
      G6      =  GM11 / B0**2                                      !<(grad_rho^2/B^2>      [m^2/T^2] (approximation)
      G7      =  GRADRHO                                           !<grad_rho>             [-]

      AREA    =  VPRIMES * GRADRHO                                 !poloidal cross section [m^2]

      G4(NPSI) =  G4(NPSI-1) + (G4(NPSI-1)-G4(NPSI-2))              &  !linear interpolation, because SPIDER outputs two
                 / (RHONEW_S(NPSI-1)-RHONEW_S(NPSI-2))            &  !equal last points
                 * (RHONEW_S(NPSI)-RHONEW_S(NPSI-1)) 
      G5(NPSI) =  G5(NPSI-1) + (G5(NEQ-1)-G5(NEQ-2))              &  !linear interpolation, because SPIDER outputs two
                 / (RHONEW_S(NEQ-1)-RHONEW_S(NEQ-2))            &  !equal last points
                 * (RHONEW_S(NEQ)-RHONEW_S(NEQ-1)) 

      CALL L3INTERP (G2,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      G2        =  FUN * JDIA                                      !<(grad_rho^2/R^2>      [-]
      CALL L3INTERP (G3,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      G3        =  FUN
      CALL L3INTERP (G4,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      G4        =  FUN
      CALL L3INTERP (G5,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      G5        =  FUN
      CALL L3INTERP (G6,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      G6        =  FUN
      CALL L3INTERP (G7,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      G7        =  FUN
      CALL L3INTERP (MU,   RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      MU        =  FUN
      CALL L3INTERP (AREA, RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      AREA      =  FUN
      CALL L3INTERP (SURF, RHONEW_S, NEQ,       FUN, RHONEW, NEQ)
      SURF      =  FUN

      RHO       =  RHONEW
      RHON      =  RHONNEW

      AMID(NEQ) = AMIN

! +++ recalculation of the safety factor:
      QSF     =  1.0_R8 / MU






! +++ CONVERGENCY CHECK:
    CONV       = 0.0_R8

    err_VPRIME = MAXVAL (ABS (1.0_R8 - VPRIME_OLD(2:NEQ-1) / VPRIME(2:NEQ-1)))
    err_G1     = MAXVAL (ABS (1.0_R8 - G1_OLD(2:NEQ-1) / G1(2:NEQ-1)))
    err_FDIA   = MAXVAL (ABS (1.0_R8 - FDIA_OLD(2:NEQ-1) / FDIA(2:NEQ-1)))


    CONV       = MAX( err_VPRIME, err_G1, err_FDIA)

    write(*,*) '      CONV SPIDER =', CONV, ITER
    write(*,*) '      err_VPRIME =', err_VPRIME
    write(*,*) '      err_G1 =', err_G1
    write(*,*) '      err_FDIA =', err_FDIA

!    IF(CONV.GT.TOLERANCE.AND.KEY_ITER.NE.0)  GOTO 7




! +++ Save output to EQUILIBRIUM CPO:
! +++ Allocate output CPO:
!      IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT)) THEN
          WRITE(*,*) 'Allocating EQUILIBRIUM_OUT'
          ALLOCATE(EQUILIBRIUM_OUT(1))
!       END IF




! +++ Fill new EQUILIBRIUM CPO with quantities calculated SPIDER solver:




!      1-D profiles:
       CALL L3DERIV  (VOL, PSI, NEQ,    VPRIME_PSI, PSI, NEQ)


!      Constraint:
       call deallocate_cpo(EQUILIBRIUM_OUT(1)%eqconstraint)
       CALL COPY_CPO(EQUILIBRIUM_IN(1)%eqconstraint, EQUILIBRIUM_OUT(1)%eqconstraint)


!      Geometry:
       call deallocate_cpo(EQUILIBRIUM_OUT(1)%eqgeometry)
       CALL COPY_CPO(EQUILIBRIUM_IN(1)%eqgeometry,   EQUILIBRIUM_OUT(1)%eqgeometry)


!      Global parameters:
       call deallocate_cpo(EQUILIBRIUM_OUT(1)%global_param)
       CALL COPY_CPO(EQUILIBRIUM_IN(1)%global_param, EQUILIBRIUM_OUT(1)%global_param)




!      GLOBAL_PARAM

       EQUILIBRIUM_OUT(1)%global_param%area                 =  AREA(NEQ)
       EQUILIBRIUM_OUT(1)%global_param%volume               =  VOL(NEQ)
       EQUILIBRIUM_OUT(1)%global_param%i_plasma             =  PC * 1.0e6_R8
       EQUILIBRIUM_OUT(1)%global_param%psi_ax               =  -PSI(1)
       EQUILIBRIUM_OUT(1)%global_param%psi_bound            =  -PSI(NEQ)



!     PROFILES_1D
!      Allocate 1-D profiles:
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%psi(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%phi(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%pressure(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%F_dia(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%pprime(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%ffprime(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%jphi(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%jparallel(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%q(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor(NPSI)) 
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%beta_pol(NPSI)) 
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%li(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%elongation(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%volume(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%vprime(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%area(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%aprime(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%surface(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%ftrap(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%dpsidrho_tor(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%b_av(NPSI))

       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm1(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm2(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm3(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm4(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm5(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm6(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm7(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm8(NPSI))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%gm9(NPSI))



       DEALLOCATE  (RHONEW)
       DEALLOCATE  (FUN)
       ALLOCATE    (RHONEW(NPSI))
       ALLOCATE    (FUN(NPSI))

       DO I = 1, NPSI
          RHONEW(I)                                          =  RHO(NEQ)*(I-1)/(NPSI-1)
       END DO


       EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor                =  RHONEW
    

       FUN_IN                                                =  SQRT(VOL/VOL(NEQ))
       CALL L3INTERP (FUN_IN,            RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol                =  FUN
       CALL L3INTERP (G1,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm1                    =  FUN
       CALL L3INTERP (G2,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm2                    =  FUN
       CALL L3INTERP (G3,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm3                    =  FUN
       CALL L3INTERP (G4,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm4                    =  FUN
       CALL L3INTERP (G5,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm5                    =  FUN
       CALL L3INTERP (G6,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm6                    =  FUN
       CALL L3INTERP (G7,                RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%gm7                    =  G7
       FUN_IN                                                =  1._R8/G1**0.5
       CALL L3INTERP (FUN_IN,            RHO, NEQ,       FUN, RHONEW, NPSI)!this is approximation
       EQUILIBRIUM_OUT(1)%profiles_1d%gm8                    =  FUN          
       FUN_IN                                                =  G1**0.5
       CALL L3INTERP (FUN_IN,            RHO, NEQ,       FUN, RHONEW, NPSI)!this is approximation
       EQUILIBRIUM_OUT(1)%profiles_1d%gm9                    =  FUN         
       CALL L3INTERP (FDIA,              RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%F_dia                  =  FUN
       CALL L3INTERP (VOL,               RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%volume                 =  FUN
       CALL L3INTERP (VPRIME_PSI,        RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%vprime                 =  -FUN  
       CALL L3INTERP (PSI,               RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%psi                    =  -FUN
       FUN_IN                                                =  PR * 1.0e6_R8
       CALL L3INTERP (FUN_IN,            RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%pressure               =  FUN
       FUN_IN                                                =  Jpar * 1.0e6_R8
       CALL L3INTERP (FUN_IN,            RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%jparallel              =  FUN
       CALL L3INTERP (QSF,               RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%q                      =  -FUN
       CALL L3INTERP (SURF,              RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%surface                =  FUN
       CALL L3INTERP (AREA,              RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%area                   =  FUN
       CALL L3INTERP (ELONG,             RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%elongation             =  FUN
       CALL L3INTERP (TRIANG_U,          RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper             =  FUN
       CALL L3INTERP (TRIANG_L,          RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower             =  FUN
       CALL L3INTERP (AMID,              RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard              =  R0 - FUN 
       EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard             =  R0 + FUN
       CALL L3INTERP (FTRAP,             RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%ftrap                  =  1.0_R8 - FUN
       CALL L3DERIV  (PSI,               RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%dpsidrho_tor           =  -FUN
       CALL L3INTERP (PPRIME,            RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%pprime                 =  -FUN
       CALL L3INTERP (FFPRIME,           RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%ffprime                =  FUN
       FUN_IN                                                =  BB0* B0
       CALL L3INTERP (FUN_IN,            RHO, NEQ,       FUN, RHONEW, NPSI)
       EQUILIBRIUM_OUT(1)%profiles_1d%b_av                   =  FUN


!      Stabilization of current oscillations should be removed later
       CALL L3INTERP (G2,                RHO, NEQ,       FUN, RHONEW, NPSI)
       CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%gm2,                                         &
                      EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,                                     &
                      SIZE (EQUILIBRIUM_IN(1)%profiles_1d%rho_tor),                              &
                      EQUILIBRIUM_OUT(1)%profiles_1d%gm2,                                        &
                      EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,                                    &
                      SIZE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor))
       EQUILIBRIUM_OUT(1)%profiles_1d%gm2                    =  EQUILIBRIUM_OUT(1)%profiles_1d%gm2 * 0.9_R8 + FUN * 0.1_R8
                                                                   





!      COORD_SYS
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1))  DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1)
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2))  DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2)
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%position%R)) DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%R)
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%position%Z)) DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%Z)
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%g_11))       DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%g_11)
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%g_22))       DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%g_22)
       IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%coord_sys%g_33))       DEALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%g_33)
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid_type(4))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1(NDIM1))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2(NDIM2))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%R(NDIM1,NDIM2))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%Z(NDIM1,NDIM2))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%g_11(NDIM1,NDIM2))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%g_22(NDIM1,NDIM2))
       ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%g_33(NDIM1,NDIM2))

       RHO_LOOP2: DO I=1,NDIM1
          AMID_2D(I)                                           = AMIN/(NDIM1-1)*(I-1)  
       END DO RHO_LOOP2
       CALL L3interp (RHO,             AMID, NEQ,     RHO_2D,         AMID_2D, NDIM1) 
       CALL L3interp (ELONG,           AMID, NEQ,     ELONG_2D,       AMID_2D, NDIM1) 
       CALL L3interp (ELONG_U,         AMID, NEQ,     ELONG_U2D,      AMID_2D, NDIM1) 
       CALL L3interp (ELONG_L,         AMID, NEQ,     ELONG_L2D,      AMID_2D, NDIM1) 
       CALL L3interp (TRIANG_L,        AMID, NEQ,     TRIANG_L2D,     AMID_2D, NDIM1) 
       CALL L3interp (TRIANG_U,        AMID, NEQ,     TRIANG_U2D,     AMID_2D, NDIM1) 
       CALL L3interp (SHAFR_SHIFT,     AMID, NEQ,     SHAFR_SHIFT_2D, AMID_2D, NDIM1) 
       CALL L3interp (PSI,             AMID, NEQ,     PSI_2D,         AMID_2D, NDIM1) 
       FUN_IN                                                =  Jpar * 1.0e6_R8
       CALL L3interp (FUN_IN,          AMID, NEQ,     JPAR_2D,        AMID_2D, NDIM1) 
       FUN_IN                                                =  PR * 1.0e6_R8
       CALL L3interp (FUN_IN,          AMID, NEQ,     PR_2D,          AMID_2D, NDIM1) 
       CALL L3interp (GM11,            AMID, NEQ,     G11_2D,         AMID_2D, NDIM1) 
       CALL L3interp (GM22,            AMID, NEQ,     G22_2D,         AMID_2D, NDIM1) 
       CALL L3interp (GM33,            AMID, NEQ,     G33_2D,         AMID_2D, NDIM1) 



       DO ITHETA=1, NDIM2
          THETA                                                    =  REAL(ITHETA-1,R8)/REAL(NDIM2-1,R8)*2.0_R8*itm_pi

          DO IRHO=1, NDIM1
             EQUILIBRIUM_OUT(1)%coord_sys%grid_type(1)             =  '2'
             EQUILIBRIUM_OUT(1)%coord_sys%grid_type(2)             =  'inverse'
             EQUILIBRIUM_OUT(1)%coord_sys%grid_type(3)             =  '3'
             EQUILIBRIUM_OUT(1)%coord_sys%grid_type(4)             =  'polar'
             EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1(IRHO)          =  -PSI_2D(IRHO)
             EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2(ITHETA)        =  THETA
             IF (THETA.LE.ITM_PI)                      TRIA        =  TRIANG_U2D(irho)
             IF (THETA.GT.ITM_PI)                      TRIA        =  TRIANG_L2D(irho)
             IF (THETA.LE.ITM_PI)                ELONGATION        =  ELONG_U2D(irho)
             IF (THETA.GT.ITM_PI)                ELONGATION        =  ELONG_L2D(irho)

             EQUILIBRIUM_OUT(1)%coord_sys%position%R(irho, itheta) =  R0 + SHAFR_SHIFT_2D(irho) + AMID_2D(irho)                   &
                                                                       * (COS(theta)-TRIA*(SIN(theta))**2)
             EQUILIBRIUM_OUT(1)%coord_sys%position%Z(irho, itheta) =  Z0 + AMID_2D(irho) * ELONGATION * SIN(theta)
             EQUILIBRIUM_OUT(1)%coord_sys%g_11(irho, itheta)       =  G11_2D(irho)
             EQUILIBRIUM_OUT(1)%coord_sys%g_22(irho, itheta)       =  G22_2D(irho)
             EQUILIBRIUM_OUT(1)%coord_sys%g_33(irho, itheta)       =  G33_2D(irho)
          END DO
       END DO






!      Allocate 2-D profiles:
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(4))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid%dim1(NDIM1))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid%dim2(NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%R(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%Z(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%psi(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%theta(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%jphi(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%jpar(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%br(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%bz(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%bphi(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%vphi(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%vtheta(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%rho_mass(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%pressure(NDIM1, NDIM2))
       ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_2d(1)%temperature(NDIM1, NDIM2))


       DO ITHETA=1, NDIM2
          THETA                                                       =  REAL(ITHETA-1,R8)/REAL(NDIM2-1,R8)*2.0_R8*itm_pi

          DO IRHO=1, NDIM1
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(1)           =  '2'
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(2)           =  'inverse'
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(3)           =  '3'
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(4)           =  'polar'
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid%dim1(IRHO)        =  -PSI_2D(IRHO)
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid%dim2(ITHETA)      =  THETA
             IF (THETA.LE.ITM_PI)                         TRIA        =  TRIANG_U2D(irho)
             IF (THETA.GT.ITM_PI)                         TRIA        =  TRIANG_L2D(irho)
             IF (THETA.LE.ITM_PI)                   ELONGATION        =  ELONG_U2D(irho)
             IF (THETA.GT.ITM_PI)                   ELONGATION        =  ELONG_L2D(irho)

             EQUILIBRIUM_OUT(1)%profiles_2d(1)%R(irho, itheta)        =  R0 + SHAFR_SHIFT_2D(irho) + AMID_2D(irho)                   &
                                                                       * (COS(theta)-TRIA*(SIN(theta))**2)
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%Z(irho, itheta)        =  Z0 + AMID_2D(irho) * ELONGATION * SIN(theta)
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%theta(irho, itheta)    =  THETA 

             EQUILIBRIUM_OUT(1)%profiles_2d(1)%psi(irho, itheta)      =  -PSI_2D(IRHO) 
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%jpar(irho, itheta)     =  JPAR_2D(IRHO)
             EQUILIBRIUM_OUT(1)%profiles_2d(1)%pressure(irho, itheta) =  PR_2D(IRHO)
          END DO
       END DO






!     CODE_PARAM  
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codename)) THEN
          ALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codename(1))                        ! For a string of 132 characters max.
          EQUILIBRIUM_OUT(1)%codeparam%codename(1)      = 'SPIDER'
       END IF
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%codeversion)) THEN
          ALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%codeversion(1))                     ! For a string of 132 characters max.
          EQUILIBRIUM_OUT(1)%codeparam%codeversion(1)   = '14.12.2012'
       END IF
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%parameters)) THEN
          ALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%parameters(1))                      ! For a string of 132 characters max.
          EQUILIBRIUM_OUT(1)%codeparam%parameters(1)    = 'my_code_specific_parameters'
       END IF
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%codeparam%output_diag)) THEN
          ALLOCATE(EQUILIBRIUM_OUT(1)%codeparam%output_diag(1))                     ! For a string of 132 characters max.
          EQUILIBRIUM_OUT(1)%codeparam%output_diag(1)   = 'my_output_diag'
       END IF
       EQUILIBRIUM_OUT(1)%codeparam%output_flag = 0                                 ! Integer output flag, 0 means the run was successful and can be used 






!     DATA_INFO
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_OUT(1)%datainfo%dataprovider)) THEN
          ALLOCATE(EQUILIBRIUM_OUT(1)%datainfo%dataprovider(1))                     ! For a string of 132 characters max.
       END IF
       EQUILIBRIUM_OUT(1)%datainfo%dataprovider(1) = 'Denis Kalupin' 
       EQUILIBRIUM_OUT(1)%datainfo%cocos           = 13!!!!12





! +++ Deallocate parameters:
      DEALLOCATE  (RHO)
      DEALLOCATE  (R_INBOARD)
      DEALLOCATE  (R_OUTBOARD)
      DEALLOCATE  (RR)
      DEALLOCATE  (RR_IN)
      DEALLOCATE  (Jpar)
      DEALLOCATE  (INTJpar)
      DEALLOCATE  (J_BS)
      DEALLOCATE  (J_CD)
      DEALLOCATE  (QSF)
      DEALLOCATE  (MU)
      DEALLOCATE  (PR)
      DEALLOCATE  (DPR)
      DEALLOCATE  (TE)
      DEALLOCATE  (PSI)
      DEALLOCATE  (SIGMA)
      DEALLOCATE  (PPRIME)
      DEALLOCATE  (FFPRIME)
      DEALLOCATE  (RZBND)
      DEALLOCATE  (ROUT)
      DEALLOCATE  (ZOUT)

      DEALLOCATE  (B2B02)
      DEALLOCATE  (BB0)
      DEALLOCATE  (B02B2)
      DEALLOCATE  (RHONEW)
      DEALLOCATE  (RHONEW_S)
      DEALLOCATE  (GM11)
      DEALLOCATE  (GM22)
      DEALLOCATE  (GM33)
      DEALLOCATE  (FDIA)
      DEALLOCATE  (JDIA)
      DEALLOCATE  (JDIAS)
      DEALLOCATE  (VOL)
      DEALLOCATE  (VPRIME)
      DEALLOCATE  (VPRIMES)
      DEALLOCATE  (VPRIME_PSI)
      DEALLOCATE  (SURF)
      DEALLOCATE  (GRADRHO)
      DEALLOCATE  (DRHODA)
      DEALLOCATE  (SHAFR_SHIFT)
      DEALLOCATE  (ELONG)
      DEALLOCATE  (ELONG_U)
      DEALLOCATE  (ELONG_L)
      DEALLOCATE  (TRIANG_L)
      DEALLOCATE  (TRIANG_U)
      DEALLOCATE  (AMID)
      DEALLOCATE  (FTRAP)

      DEALLOCATE  (G1)
      DEALLOCATE  (G2)
      DEALLOCATE  (G3)
      DEALLOCATE  (G4)
      DEALLOCATE  (G5)
      DEALLOCATE  (G6)
      DEALLOCATE  (G7)
      DEALLOCATE  (AREA)

      DEALLOCATE  (FUN)
      DEALLOCATE  (FUN_IN)
      DEALLOCATE  (INTFUN)
  
      DEALLOCATE  (VPRIME_OLD)
      DEALLOCATE  (G1_OLD)
      DEALLOCATE  (FDIA_OLD)

      DEALLOCATE  (AMID_2D)
      DEALLOCATE  (RHO_2D)
      DEALLOCATE  (ELONG_2D)
      DEALLOCATE  (ELONG_U2D)
      DEALLOCATE  (ELONG_L2D)
      DEALLOCATE  (TRIANG_L2D)
      DEALLOCATE  (TRIANG_U2D)
      DEALLOCATE  (SHAFR_SHIFT_2D)
      DEALLOCATE  (PSI_2D)
      DEALLOCATE  (JPAR_2D)
      DEALLOCATE  (PR_2D)
      DEALLOCATE  (G11_2D)
      DEALLOCATE  (G22_2D)
      DEALLOCATE  (G33_2D)



!+++ Remove SPIDER files from the directory:
    STR         = 'rm -rf *.dat *.wr'
    CALL SYSTEM (STR)

    RETURN


  








    CONTAINS

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
       SUBROUTINE ASSIGN_SPIDER_PARAMETERS(codeparameters, return_status)

       !-------------------------------------------------------!
       !     This subroutine calls the XML parser for          !
       !     the SPIDER parameters and assign the              !
       !     resulting values to the corresponding variables   !
       !-------------------------------------------------------!
       !     Source:       ---                                 !
       !     Developers:   D.Kalupin                           !
       !     Kontacts:     Denis.Kalupin@efda.org              !
       !                                                       !
       !     Comments:     created for V&V between ETS and     !
       !                   ASTRA                               !
       !                                                       !
       !-------------------------------------------------------!
  
       USE ITM_TYPES
       USE EUITM_SCHEMAS
       USE EUITM_XML_PARSER  

       IMPLICIT NONE


       TYPE(type_param)                  :: codeparameters
       TYPE(tree)                        :: parameter_list
       TYPE(element),        POINTER     :: temp_pointer

       INTEGER(ITM_I4)                   :: return_status 
       INTEGER(ITM_I4)                   :: i, nparm, n_values
       INTEGER                           :: n_data

       CHARACTER(len = 132)              :: cname
       !-------------------------------------------------------!



       return_status          = 0      ! no error


! +++ parse xml-string codeparameters%parameters
      WRITE(6,*) 'Calling euitm_xml_parse'
      CALL EUITM_XML_PARSE  (codeparameters, nparm, parameter_list)
      WRITE(6,*) 'Called euitm_xml_parse'


! +++ assign variables
      temp_pointer => parameter_list%first

      outer: do
         cname = char2str(temp_pointer%cname)   ! necessary for AIX
         select case (cname)


! +++ parameters overall
         case ("parameters")
            temp_pointer => temp_pointer%child
            cycle


! +++ SPIDER settings
         case ("SPIDER")
            temp_pointer => temp_pointer%child
            cycle

         case ("icontrol")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, icontrol)
         
         case ("key_iter")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, key_iter)
         
         case ("nrho")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, nrho)
         
         case ("ntheta")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, ntheta)
         
         case ("neq")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, neq)
         
         case ("tolerance")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, tolerance)
         
         case ("iinput")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, iinput)
         
         case default
            write(*, *) 'ERROR: invalid parameter', cname
            return_status = 1
            exit
         end select


         do
            if (associated(temp_pointer%sibling)) then
               temp_pointer => temp_pointer%sibling
               exit
            end if
            if (associated(temp_pointer%parent, parameter_list%first )) &
                 exit outer
            if (associated(temp_pointer%parent)) then
               temp_pointer => temp_pointer%parent
            else
               write(*, *) 'ERROR: broken list.'
               return
            end if
         end do
      end do outer
      
! +++ destroy tree
      CALL DESTROY_XML_TREE(parameter_list)


      RETURN

    END SUBROUTINE ASSIGN_SPIDER_PARAMETERS
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  


END SUBROUTINE EQUILIBRIUM_SPIDER
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





