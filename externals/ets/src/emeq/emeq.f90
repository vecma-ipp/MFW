! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> EMEQ_E3M
!>
!> This routine provides interface between ETS
!> and three moment equilibrium solver
!>
!> \author D.Kalupin, G.Pereverzev 
!>
!> \version "$Id: emeq.f90 1606 2014-10-06 12:34:28Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE EMEQ

CONTAINS

  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   

  SUBROUTINE EMEQ_E3M  (EQUILIBRIUM_IN, EQUILIBRIUM_OUT, code_parameters)

    !-------------------------------------------------------!
    !     This routine provides interface between ETS       !
    !     and three moment equilibrium solver.              !
    !-------------------------------------------------------!
    !     Source:       ---                                 !
    !     Developers:   D.Kalupin, G.Pereverzev             !
    !     Kontacts:     D.Kalupin@fz-juelich.de             !
    !                                                       !
    !     Comments:     created for V&V between ETS and     !
    !                   ASTRA                               !
    !                                                       !
    !-------------------------------------------------------!

    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE EUITM_XML_PARSER
    USE DEALLOCATE_STRUCTURES


    IMPLICIT NONE

    INTEGER                           :: ifail

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
    TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
    TYPE (type_param)                 :: code_parameters
    INTEGER                           :: return_status


! +++ Dimensions:
    INTEGER                           :: NPSI                !number of radial points      (input, determined from EQUILIBRIUM_IN CPO)
    INTEGER                           :: NDIM1, NDIM2        !number of points for 2-D parameters
    INTEGER                           :: IDIM1, IDIM2        !index of points for 2-D parameters
    INTEGER                           :: MAX_NPOINTS         !index of boundary points 
    INTEGER                           :: IPSI, ITHETA

! +++ Internal parameters(0-D parameters):
    REAL (R8)                         :: R0                  !magnetic axis (on first iteration equal to geom.axis)
    REAL (R8)                         :: B0                  !magnetic field on axis
    REAL (R8)                         :: PC                  !total plasma current
    REAL (R8)                         :: DELTA               !Shafranov shift = 0 !!!not defined in EQUILIBRIUM CPO
    REAL (R8)                         :: EL                  !elongation
    REAL (R8)                         :: TR                  !triangularity
    REAL (R8)                         :: AMIN                !minor radius
    REAL (R8)                         :: THETA
    REAL (R8)                         :: RGEO
    REAL (R8)                         :: ZGEO
    REAL (R8)                         :: BGEO 
    REAL (R8)                         :: RMAG 
    REAL (R8)                         :: BMAG 


! +++ 1-D parameters:
    REAL (R8),            ALLOCATABLE :: RHO(:)
    REAL (R8),            ALLOCATABLE :: Jpar(:), INTJpar(:)
    REAL (R8),            ALLOCATABLE :: QSF(:)
    REAL (R8),            ALLOCATABLE :: PR(:)
    REAL (R8),            ALLOCATABLE :: PSI(:)

    REAL (R8),            ALLOCATABLE :: B2B02(:)
    REAL (R8),            ALLOCATABLE :: G1(:), G2(:), G3(:)
    REAL (R8),            ALLOCATABLE :: G4(:), G5(:), G6(:)
    REAL (R8),            ALLOCATABLE :: G7(:)
    REAL (R8),            ALLOCATABLE :: FDIA(:)
    REAL (R8),            ALLOCATABLE :: VOL(:),      VPRIME(:),     VPRIME_PSI(:)
    REAL (R8),            ALLOCATABLE :: TRIANG(:)
    REAL (R8),            ALLOCATABLE :: AREA(:),     AREAPRIME(:)
    REAL (R8),            ALLOCATABLE :: ELONG(:),    SHAFR_SHIFT(:)
    REAL (R8),            ALLOCATABLE :: PPRIME(:),   FFPRIME(:)
    !     Work
    REAL (R8),            ALLOCATABLE :: AMID(:)
    REAL (R8),            ALLOCATABLE :: FUN(:)


! +++ 1-D parameters for 2-D output:
    REAL (R8),            ALLOCATABLE :: AMID_2D(:),  RHO_2D(:)
    REAL (R8),            ALLOCATABLE :: PSI_2D(:),   JPAR_2D(:),    PR_2D(:)
    REAL (R8),            ALLOCATABLE :: SHAFR_SHIFT_2D(:) 
    REAL (R8),            ALLOCATABLE :: ELONG_2D(:), TRIANG_L2D(:), TRIANG_U2D(:)


    REAL (R8),            SAVE        :: ACEQLB
    REAL (R8),            SAVE        :: CONVERGENCE
    INTEGER,              SAVE        :: ITERMAX
    INTEGER,              SAVE        :: NEQ                 !number of equilibrium points (input parameter)

    LOGICAL,              SAVE        :: first=.TRUE.

    REAL (R8)                         :: s_ip, s_bt, s_q, s_psi, s_jpar
    


!-------------------------------------------------------
!-------------------------------------------------------

    IF(first) THEN
       ACEQLB        = 1.0e-3_R8
       CONVERGENCE   = 1.0e-4_R8
       ITERMAX       = 1000


       IF(ASSOCIATED(equilibrium_in)) THEN
          IF(ASSOCIATED(equilibrium_in(1)%profiles_1d%psi)) THEN
             NEQ=SIZE(equilibrium_in(1)%profiles_1d%psi)
             WRITE(*,*) 'Initial NEQ set to ',NEQ,' based on input equilibrium size of PSI'
          ELSEIF(ASSOCIATED(equilibrium_in(1)%profiles_1d%rho_vol)) THEN
             NEQ=SIZE(equilibrium_in(1)%profiles_1d%rho_vol)
             WRITE(*,*) 'Initial NEQ set to ',NEQ,' based on input equilibrium size of RHO_VOL'
          ELSEIF(ASSOCIATED(equilibrium_in(1)%profiles_1d%rho_tor)) THEN
             NEQ=SIZE(equilibrium_in(1)%profiles_1d%rho_tor)
             WRITE(*,*) 'Initial NEQ set to ',NEQ,' based on input equilibrium size of RHO_TOR'
          ELSE
             NEQ=100
             WRITE(*,*) 'Initial NEQ set to ',NEQ
          ENDIF
       ELSE
             NEQ=100
             WRITE(*,*) 'Initial NEQ set to ',NEQ
       ENDIF

       IF (.NOT. ASSOCIATED(code_parameters%parameters)) THEN
          WRITE(6, *) 'ERROR: code parameters not associated!'
          STOP
       ELSE
          IF (.NOT.ASSOCIATED(equilibrium_in(1)%codeparam%parameters)) & 
          ALLOCATE(equilibrium_in(1)%codeparam%parameters(SIZE(code_parameters%parameters)))
          call deallocate_cpo(equilibrium_in(1)%codeparam%parameters)
          CALL COPY_CPO (code_parameters%parameters, equilibrium_in(1)%codeparam%parameters)

          CALL assign_code_parameters(code_parameters, return_status)

          WRITE(*,*) 'Final NEQ set to ',NEQ

          IF (return_status /= 0) THEN
             WRITE(*,*) 'ERROR: Could not assign EMEQ parameters.'
          END IF

       END IF

       first=.FALSE.

    END IF




! +++ Set dimensions and allocate parameters:
    NPSI        =  SIZE (EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,      DIM=1)
    NDIM1       =  SIZE (EQUILIBRIUM_IN(1)%coord_sys%position%R,     DIM=1)
    NDIM2       =  SIZE (EQUILIBRIUM_IN(1)%coord_sys%position%R,     DIM=2)
    MAX_NPOINTS =  SIZE (EQUILIBRIUM_IN(1)%eqgeometry%boundary(1)%r, DIM=1)

    s_ip             = sign(1.0_R8, equilibrium_in(1)%global_param%i_plasma)
    s_bt             = sign(1.0_R8, equilibrium_in(1)%global_param%toroid_field%b0)
    s_q              = - s_ip * s_bt
    s_psi            = - s_ip
    s_jpar           = s_ip

    write(*,*) 's_ip, s_bt, s_q, s_psi, s_jpar = ', s_ip, s_bt, s_q, s_psi, s_jpar

    ALLOCATE  (RHO(NPSI))
    ALLOCATE  (Jpar(NPSI))
    ALLOCATE  (INTJpar(NPSI))
    ALLOCATE  (QSF(NPSI))
    ALLOCATE  (PR(NPSI))
    ALLOCATE  (PSI(NPSI))
    ALLOCATE  (B2B02(NPSI))
    ALLOCATE  (G1(NPSI))
    ALLOCATE  (G2(NPSI))
    ALLOCATE  (G3(NPSI))
    ALLOCATE  (G4(NPSI))
    ALLOCATE  (G5(NPSI))
    ALLOCATE  (G6(NPSI))
    ALLOCATE  (G7(NPSI))
    ALLOCATE  (FDIA(NPSI))
    ALLOCATE  (VOL(NPSI))
    ALLOCATE  (VPRIME(NPSI))
    ALLOCATE  (VPRIME_PSI(NPSI))
    ALLOCATE  (AREA(NPSI))
    ALLOCATE  (AREAPRIME(NPSI))
    ALLOCATE  (AMID(NPSI))
    ALLOCATE  (SHAFR_SHIFT(NPSI))
    ALLOCATE  (ELONG(NPSI))
    ALLOCATE  (TRIANG(NPSI))
    ALLOCATE  (PPRIME(NPSI))
    ALLOCATE  (FFPRIME(NPSI))
    ALLOCATE  (FUN(NPSI))

    ALLOCATE  (AMID_2D(NDIM1))
    ALLOCATE  (RHO_2D(NDIM1))
    ALLOCATE  (ELONG_2D(NDIM1))
    ALLOCATE  (TRIANG_L2D(NDIM1))
    ALLOCATE  (TRIANG_U2D(NDIM1))
    ALLOCATE  (SHAFR_SHIFT_2D(NDIM1))
    ALLOCATE  (PSI_2D(NDIM1))
    ALLOCATE  (JPAR_2D(NDIM1))
    ALLOCATE  (PR_2D(NDIM1))


    ! +++ Set parameters:
    R0         =  EQUILIBRIUM_IN(1)%global_param%toroid_field%r0
    B0         =  EQUILIBRIUM_IN(1)%global_param%toroid_field%b0 * s_bt

    RGEO       =  EQUILIBRIUM_IN(1)%eqgeometry%geom_axis%r
    ZGEO       =  EQUILIBRIUM_IN(1)%eqgeometry%geom_axis%z
    BGEO       =  R0 * B0 / RGEO

    RMAG       =  EQUILIBRIUM_IN(1)%global_param%mag_axis%position%r
    BMAG       =  EQUILIBRIUM_IN(1)%global_param%mag_axis%bphi

    AMIN       =  EQUILIBRIUM_IN(1)%eqgeometry%a_minor

    DELTA      =  R0 - RGEO
    EL         =  EQUILIBRIUM_IN(1)%eqgeometry%elongation
    TR         = (EQUILIBRIUM_IN(1)%eqgeometry%tria_upper + EQUILIBRIUM_IN(1)%eqgeometry%tria_lower) / 2.0_R8  
    AMIN       =  EQUILIBRIUM_IN(1)%eqgeometry%a_minor

    RHO        =  EQUILIBRIUM_IN(1)%profiles_1d%rho_tor
    Jpar       =  EQUILIBRIUM_IN(1)%profiles_1d%jparallel * s_ip
    QSF        =  EQUILIBRIUM_IN(1)%profiles_1d%q * s_q
    PR         =  EQUILIBRIUM_IN(1)%profiles_1d%pressure
    B2B02      =  EQUILIBRIUM_IN(1)%profiles_1d%gm5 / B0**2
    FDIA       =  EQUILIBRIUM_IN(1)%profiles_1d%F_dia * s_bt / R0 / B0
    PSI        =  EQUILIBRIUM_IN(1)%profiles_1d%psi * s_psi

    PC         =  EQUILIBRIUM_IN(1)%global_param%i_plasma / 1.0e6_R8 * s_ip

    DO IPSI=1,NPSI
       IF (Jpar(IPSI).LT.0.0_R8) Jpar(IPSI) = 0.0_R8
    END DO

    IF (ASSOCIATED(EQUILIBRIUM_IN(1)%profiles_1d%r_inboard).AND.    &
        ASSOCIATED(EQUILIBRIUM_IN(1)%profiles_1d%r_outboard)) THEN
       AMID    =  0.5_R8 * (EQUILIBRIUM_IN(1)%profiles_1d%r_outboard - EQUILIBRIUM_IN(1)%profiles_1d%r_inboard)

    ELSE
       AMID    =  RHO / sqrt(EL)         
    ENDIF

    DO IPSI = 2, NPSI
       IF (AMID(IPSI).LT.AMID(IPSI-1)) THEN
          WRITE(*,*) 'ERROR : AMID is non monotonic.'
          AMID =  RHO / sqrt(EL)         
       END IF
    END DO

    IF (AMID(NPSI).NE.AMIN) THEN
       WRITE(*,*) 'ERROR : AMID(NPSI) is diffrent from AMIN.',AMID(NPSI),AMIN
       AMID    =  AMID * AMIN / AMID(NPSI)
    END IF


    DO IPSI = 2, NPSI
       IF (AMID(IPSI).LT.AMID(IPSI-1)) THEN
          WRITE(*,*) 'ERROR : AMID is non monotonic.'
          AMID =  RHO / sqrt(EL)         
       END IF
    END DO

    IF (AMID(NPSI).NE.AMIN) THEN
       WRITE(*,*) 'ERROR : AMID(NPSI) is diffrent from AMIN.',AMID(NPSI),AMIN
       AMID    =  AMID * AMIN / AMID(NPSI)
    END IF







! +++ Call three moment equilibrium solver from ASTRA (EMEQ)
!     (input/output is on the RHO grid)
    CALL EMEQ_INTERFACE( &
       !       
       !     INPUT FROM THE TRANSPORT CODE TO EQUILIBRIUM:
       !     values:
             RGEO,       & ! - Geometrical major radius    [m]  
             BGEO,       & ! - Toroidal magnetic field     [T]  
             PC,         & ! - Plasma current              [A]  
             DELTA,      & ! - Shift of plasma column      [m]  
             EL,         & ! - elongation, boundary value
             TR,         & ! - triangularity, boundary value
             AMIN,       & ! - minor radius                [m]
             NPSI,       & ! - Number of radial points 
             NEQ,        & ! - Number of equilibrium points 
             ACEQLB,     & !
             CONVERGENCE,& ! 
             ITERMAX,    & !
       !     arrays(NPSI):
             RHO,        & ! - Rho                         [m]
             AMID,       & ! - Normalized minor radius,    [m]
             Jpar,       & ! - Parallel current density    [A/m^2]
             QSF,        & ! - safety factor 
             PR,         & ! - pressure profile            [Pa] 
       !
       !     OUTPUT FROM THE EQUILIBRIUM TO THE TRANSPORT CODE:
             G1,         & ! - <1/R^2>                     [m^-2]   
             G2,         & ! - <grad_rho^2/R^2>            [m^-2]   
             G3,         & ! - <grad_rho^2>                [-]
             G4,         & ! - <1/B^2>                     [1/T^2]
             G5,         & ! - <B^2>                       [T^2]
             G6,         & ! - <grad_rho^2/B^2>            [m^2/T^2]  
             G7,         & ! - <grad_rho>                  [-]
             B2B02,      & ! - <B**2/B0**2>                [-]
             VOL,        & ! - volume (V)                  [m^3]
             VPRIME,     & ! - volume derivative (V')      [m^2]
             AREA,       & ! - cross-sectional area        [m^2] 
             AREAPRIME,  & ! - c.-s. area derivative       [m]   
             FDIA,       & ! - diamagnetic function        [-]
             SHAFR_SHIFT,& ! 
             ELONG,      & ! 
             TRIANG)       !




! +++ Save output to EQUILIBRIUM CPO:
! +++ Allocate output CPO:
!    IF(ASSOCIATED(EQUILIBRIUM_OUT)) CALL DEALLOCATE_CPO(EQUILIBRIUM_OUT)
    WRITE(*,*) 'Allocating EQUILIBRIUM_OUT'
    ALLOCATE(EQUILIBRIUM_OUT(1))




! +++ Fill new EQUILIBRIUM CPO with quantities calculated by EMEQ solver:
!   1-D profiles:
    CALL L3DERIV  (VOL, PSI, NEQ,    VPRIME_PSI, PSI, NEQ)
    IF(vol(1).LT.0) THEN
       WRITE(*,*) 'VOL(1) = ',vol(1)
       vol(1)=0.0_R8
    ENDIF


!   Constraint:
    call deallocate_cpo(EQUILIBRIUM_OUT(1)%eqconstraint)
    CALL COPY_CPO(EQUILIBRIUM_IN(1)%eqconstraint, EQUILIBRIUM_OUT(1)%eqconstraint)


!   Eq. Geometry:
    call deallocate_cpo(EQUILIBRIUM_OUT(1)%eqgeometry)
    CALL COPY_CPO(EQUILIBRIUM_IN(1)%eqgeometry,   EQUILIBRIUM_OUT(1)%eqgeometry)


    EQUILIBRIUM_OUT(1)%eqgeometry%tria_upper                    =  TRIANG(NPSI)
    EQUILIBRIUM_OUT(1)%eqgeometry%tria_lower                    =  TRIANG(NPSI)  



!   GLOBAL_PARAM
    call deallocate_cpo(EQUILIBRIUM_OUT(1)%global_param)
    CALL COPY_CPO(EQUILIBRIUM_IN(1)%global_param, EQUILIBRIUM_OUT(1)%global_param)


    EQUILIBRIUM_OUT(1)%global_param%area                        =  AREA(NPSI)
    EQUILIBRIUM_OUT(1)%global_param%volume                      =  VOL(NPSI)
    EQUILIBRIUM_OUT(1)%global_param%i_plasma                    =  PC * 1.0e6_R8 * s_ip
    EQUILIBRIUM_OUT(1)%global_param%psi_ax                      =  PSI(1) * s_psi
    EQUILIBRIUM_OUT(1)%global_param%psi_bound                   =  PSI(NPSI) * s_psi
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%position%R         =  RGEO + SHAFR_SHIFT(1)
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%position%Z         =  0.0_R8
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%bphi               =  FDIA(1) * R0 * B0 * s_bt / (RGEO + SHAFR_SHIFT(1))
    EQUILIBRIUM_OUT(1)%global_param%mag_axis%q                  =  QSF(1) * s_q
    EQUILIBRIUM_OUT(1)%global_param%q_min                       =  MINVAL(QSF) * s_q



!   PROFILES_1D
!   Allocate 1-D profiles:
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
    ALLOCATE (EQUILIBRIUM_OUT(1)%profiles_1d%dvdrho(NPSI))
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


    EQUILIBRIUM_OUT(1)%profiles_1d%psi                          =  PSI * s_psi
    EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor                      =  RHO
    EQUILIBRIUM_OUT(1)%profiles_1d%phi                          =  RHO**2 * itm_pi * B0 * s_bt
    EQUILIBRIUM_OUT(1)%profiles_1d%q                            =  QSF * s_q
    EQUILIBRIUM_OUT(1)%profiles_1d%pressure                     =  PR
    EQUILIBRIUM_OUT(1)%profiles_1d%jparallel                    =  Jpar * s_ip
    EQUILIBRIUM_OUT(1)%profiles_1d%volume                       =  VOL
    EQUILIBRIUM_OUT(1)%profiles_1d%vprime                       =  VPRIME_PSI * s_psi
    EQUILIBRIUM_OUT(1)%profiles_1d%area                         =  AREA
    EQUILIBRIUM_OUT(1)%profiles_1d%aprime                       =  AREAPRIME
    EQUILIBRIUM_OUT(1)%profiles_1d%F_dia                        =  FDIA * R0 * B0 * s_bt
    EQUILIBRIUM_OUT(1)%profiles_1d%elongation                   =  ELONG
    EQUILIBRIUM_OUT(1)%profiles_1d%tria_upper                   =  TRIANG
    EQUILIBRIUM_OUT(1)%profiles_1d%tria_lower                   =  TRIANG
    EQUILIBRIUM_OUT(1)%profiles_1d%r_inboard                    =  RGEO + SHAFR_SHIFT - AMID
    EQUILIBRIUM_OUT(1)%profiles_1d%r_outboard                   =  RGEO + SHAFR_SHIFT + AMID
    EQUILIBRIUM_OUT(1)%profiles_1d%rho_vol                      =  SQRT(vol/vol(NPSI))

    EQUILIBRIUM_OUT(1)%profiles_1d%gm1                          =  G1
    EQUILIBRIUM_OUT(1)%profiles_1d%gm2                          =  G2
    EQUILIBRIUM_OUT(1)%profiles_1d%gm3                          =  G3
    EQUILIBRIUM_OUT(1)%profiles_1d%gm4                          =  G4
    EQUILIBRIUM_OUT(1)%profiles_1d%gm5                          =  G5
    EQUILIBRIUM_OUT(1)%profiles_1d%gm6                          =  G6
    EQUILIBRIUM_OUT(1)%profiles_1d%gm7                          =  G7
    EQUILIBRIUM_OUT(1)%profiles_1d%gm8                          =  1._R8/G1**0.5
    EQUILIBRIUM_OUT(1)%profiles_1d%gm9                          =  G1**0.5

    CALL L3DERIV(PR, RHO, NPSI,    FUN, RHO, NPSI)
    PPRIME                                                      =  (RGEO + SHAFR_SHIFT) * QSF * s_q / B0 * s_bt / RHO * FUN !!!!!- was removed
    IF (RHO(1).EQ.0.0_R8) PPRIME(1)  = PPRIME(2) 
    B2B02                                                       =  G5 / B0**2
    FFPRIME                                                     =  FDIA * s_bt / B2B02 * (Jpar * s_jpar * (RGEO + SHAFR_SHIFT) / R0 - PPRIME)

    EQUILIBRIUM_OUT(1)%profiles_1d%pprime                       =  PPRIME
    EQUILIBRIUM_OUT(1)%profiles_1d%ffprime                      =  FFPRIME
    EQUILIBRIUM_OUT(1)%profiles_1d%dvdrho                       =  VPRIME
    EQUILIBRIUM_OUT(1)%profiles_1d%b_av                         =  G5**0.5



!   Stabilization of current oscillations should be removed later
    CALL L3INTERP (EQUILIBRIUM_IN(1)%profiles_1d%gm2,                                         &
                   EQUILIBRIUM_IN(1)%profiles_1d%rho_tor,                                     &
                   SIZE (EQUILIBRIUM_IN(1)%profiles_1d%rho_tor),                              &
                   EQUILIBRIUM_OUT(1)%profiles_1d%gm2,                                        &
                   EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor,                                    &
                   SIZE (EQUILIBRIUM_OUT(1)%profiles_1d%rho_tor))
    EQUILIBRIUM_OUT(1)%profiles_1d%gm2                    =  EQUILIBRIUM_OUT(1)%profiles_1d%gm2 * 0.9_R8 + G2 * 0.1_R8

!   COORD_SYS
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid_type(4))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1(NDIM1))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2(NDIM2))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%R(NDIM1,NDIM2))
    ALLOCATE(EQUILIBRIUM_OUT(1)%coord_sys%position%Z(NDIM1,NDIM2))

    RHO_LOOP2: DO IDIM1=1,NDIM1
       AMID_2D(IDIM1)                                           =  AMIN/(NDIM1-1)*(IDIM1-1)  
    END DO RHO_LOOP2
    CALL L3interp (RHO,             AMID, NPSI,    RHO_2D,         AMID_2D, NDIM1) 
    CALL L3interp (ELONG,           AMID, NPSI,    ELONG_2D,       AMID_2D, NDIM1) 
    CALL L3interp (TRIANG,          AMID, NPSI,    TRIANG_L2D,     AMID_2D, NDIM1) 
    CALL L3interp (TRIANG,          AMID, NPSI,    TRIANG_U2D,     AMID_2D, NDIM1) 
    CALL L3interp (SHAFR_SHIFT,     AMID, NPSI,    SHAFR_SHIFT_2D, AMID_2D, NDIM1) 
    CALL L3interp (PSI,             AMID, NPSI,    PSI_2D,         AMID_2D, NDIM1)
    FUN                                                         =  JPAR * 1.0e6_R8
    CALL L3interp (FUN,             AMID, NPSI,    JPAR_2D,        AMID_2D, NDIM1) 
    FUN                                                         =  PR * 1.0e6_R8
    CALL L3interp (FUN,             AMID, NPSI,    PR_2D,          AMID_2D, NDIM1) 


    DO ITHETA=1, NDIM2
       THETA                                                    =   REAL(ITHETA-1,R8)/REAL(NDIM2,R8)*2.0_R8*itm_pi
       DO IPSI=1, NDIM1
          EQUILIBRIUM_OUT(1)%coord_sys%grid_type(1)             =  '2'
          EQUILIBRIUM_OUT(1)%coord_sys%grid_type(2)             =  'inverse'
          EQUILIBRIUM_OUT(1)%coord_sys%grid_type(3)             =  '3'
          EQUILIBRIUM_OUT(1)%coord_sys%grid_type(4)             =  'polar'
          EQUILIBRIUM_OUT(1)%coord_sys%grid%dim1(IPSI)          =  PSI_2D(IPSI) * s_psi
          EQUILIBRIUM_OUT(1)%coord_sys%grid%dim2(ITHETA)        =  THETA
          EQUILIBRIUM_OUT(1)%coord_sys%position%R(IPSI, ITHETA) =  R0 + SHAFR_SHIFT_2D(IPSI) + AMID_2D(IPSI)                   &
                                                                       * (COS(THETA)-(TRIANG_U2D(IPSI)+TRIANG_L2D(IPSI))/2._R8*(SIN(THETA))**2)
          EQUILIBRIUM_OUT(1)%coord_sys%position%Z(IPSI, ITHETA) =  ZGEO + AMID_2D(IPSI) * ELONG_2D(IPSI) * SIN(THETA)

       END DO
    END DO



!   EQ_GEOMETRY
    IF(ASSOCIATED(EQUILIBRIUM_OUT(1)%eqgeometry%boundary))         &
    CALL DEALLOCATE_CPO                                            (EQUILIBRIUM_OUT(1)%eqgeometry%boundary)
    ALLOCATE                                                       (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1))
    ALLOCATE                                                       (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(MAX_NPOINTS))
    ALLOCATE                                                       (EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(MAX_NPOINTS))

    DO ITHETA=1, MAX_NPOINTS
       theta                                                    =   REAL(ITHETA-1,R8)/REAL(MAX_NPOINTS-1,R8)*2.0_R8*itm_pi
       EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%r(ITHETA)      =   R0 + SHAFR_SHIFT_2D(NPSI) + AMIN * (COS(theta) - TR*(SIN(theta))**2)
       EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)      =   ZGEO + AMIN * EL * SIN(theta)
       IF (ABS(EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)).LE.1.E-15_R8)                                              &
          EQUILIBRIUM_OUT(1)%eqgeometry%boundary(1)%z(ITHETA)   =   0.0E0_R8
    END DO
    EQUILIBRIUM_OUT(1)%eqgeometry%boundarytype                  =   1


    EQUILIBRIUM_OUT(1)%eqgeometry%geom_axis%r                   =   RGEO
    EQUILIBRIUM_OUT(1)%eqgeometry%geom_axis%z                   =   ZGEO 
    EQUILIBRIUM_OUT(1)%eqgeometry%a_minor                       =   AMIN
    EQUILIBRIUM_OUT(1)%eqgeometry%elong_upper                   =   EL
    EQUILIBRIUM_OUT(1)%eqgeometry%elong_lower                   =   EL





!   Allocate 2-D profiles:
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

       DO IPSI=1, NDIM1
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(1)           =  '2'
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(2)           =  'inverse'
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(3)           =  '3'
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid_type(4)           =  'polar'
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid%dim1(IPSI)        =  PSI_2D(IPSI) * s_psi
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%grid%dim2(ITHETA)      =  THETA
          IF (THETA.LE.ITM_PI)                      TR             =  TRIANG_U2D(IPSI)
          IF (THETA.GT.ITM_PI)                      TR             =  TRIANG_L2D(IPSI)

          EQUILIBRIUM_OUT(1)%profiles_2d(1)%R(IPSI, ITHETA)        =  R0 + SHAFR_SHIFT_2D(IPSI) + AMID_2D(IPSI)                   &
                                                                      * (COS(theta)-TR*(SIN(theta))**2)
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%Z(IPSI, ITHETA)        =  ZGEO + AMID_2D(IPSI) * ELONG_2D(IPSI) * SIN(theta)
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%theta(IPSI, ITHETA)    =  THETA 

          EQUILIBRIUM_OUT(1)%profiles_2d(1)%psi(IPSI, ITHETA)      =  PSI_2D(IPSI) * s_psi
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%jpar(IPSI, ITHETA)     =  JPAR_2D(IPSI) * s_jpar
          EQUILIBRIUM_OUT(1)%profiles_2d(1)%pressure(IPSI, ITHETA) =  PR_2D(IPSI)
       END DO
    END DO





!   CODE_PARAM
    ALLOCATE  (EQUILIBRIUM_OUT(1)%codeparam%codename(1))                  
    ALLOCATE  (EQUILIBRIUM_OUT(1)%codeparam%codeversion(1))               
    ALLOCATE  (EQUILIBRIUM_OUT(1)%codeparam%parameters(SIZE(code_parameters%parameters)))
    ALLOCATE  (EQUILIBRIUM_OUT(1)%codeparam%output_diag(1))               


    EQUILIBRIUM_OUT(1)%codeparam%codename(1)                    = 'emeq'
    EQUILIBRIUM_OUT(1)%codeparam%codeversion(1)                 = '01.03.2013'
    EQUILIBRIUM_OUT(1)%codeparam%output_diag(1)                 = 'my_output_diag'
    EQUILIBRIUM_OUT(1)%codeparam%output_flag                    = 0           
    EQUILIBRIUM_OUT(1)%codeparam%parameters                     = code_parameters%parameters



!   DATA_INFO
    ALLOCATE  (EQUILIBRIUM_OUT(1)%datainfo%dataprovider(1))                     
    EQUILIBRIUM_OUT(1)%datainfo%dataprovider(1)                 = 'Denis Kalupin' 
    EQUILIBRIUM_OUT(1)%datainfo%cocos                           = 13 !!!!12




    ! +++ Deallocate parameters:
    DEALLOCATE  (RHO)
    DEALLOCATE  (Jpar)
    DEALLOCATE  (INTJpar)
    DEALLOCATE  (QSF)
    DEALLOCATE  (PR)
    DEALLOCATE  (PSI)
    DEALLOCATE  (B2B02)
    DEALLOCATE  (G1)
    DEALLOCATE  (G2)
    DEALLOCATE  (G3)
    DEALLOCATE  (G4)
    DEALLOCATE  (G5)
    DEALLOCATE  (G6)
    DEALLOCATE  (G7)
    DEALLOCATE  (FDIA)
    DEALLOCATE  (VOL)
    DEALLOCATE  (VPRIME)
    DEALLOCATE  (VPRIME_PSI)
    DEALLOCATE  (AREA)
    DEALLOCATE  (AREAPRIME)
    DEALLOCATE  (AMID)
    DEALLOCATE  (SHAFR_SHIFT)
    DEALLOCATE  (ELONG)
    DEALLOCATE  (TRIANG)
    DEALLOCATE  (PPRIME)
    DEALLOCATE  (FFPRIME)
    DEALLOCATE  (FUN)

    DEALLOCATE  (AMID_2D)
    DEALLOCATE  (RHO_2D)
    DEALLOCATE  (ELONG_2D)
    DEALLOCATE  (TRIANG_L2D)
    DEALLOCATE  (TRIANG_U2D)
    DEALLOCATE  (SHAFR_SHIFT_2D)
    DEALLOCATE  (PSI_2D)
    DEALLOCATE  (JPAR_2D)
    DEALLOCATE  (PR_2D)



    RETURN



! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + + + +


  CONTAINS

    SUBROUTINE assign_code_parameters(codeparameters, return_status)

      !-----------------------------------------------------------------------
      ! calls the XML parser for the code parameters and assign the
      ! resulting values to the corresponding variables
      !TODO: check an alternative and more elegant solution in Perl
      !-----------------------------------------------------------------------

      USE mod_f90_kind

      IMPLICIT NONE

      TYPE (type_param), INTENT(in) :: codeparameters
      INTEGER(ikind), INTENT(out) :: return_status 

      TYPE(tree) :: parameter_list
      TYPE(element), POINTER :: temp_pointer
      INTEGER(ikind) :: i, nparm, n_values
      CHARACTER(len = 132) :: cname

      return_status = 0      ! no error

      !-- parse xml-string codeparameters

      WRITE(*,*) 'Calling euitm_xml_parse'
      CALL euitm_xml_parse(codeparameters, nparm, parameter_list)
      WRITE(*,*) 'Called euitm_xml_parse'

      !-- assign variables

      temp_pointer => parameter_list%first

      outer: DO
         cname = char2str(temp_pointer%cname)   ! necessary for AIX
         SELECT CASE (cname)
         CASE ("parameters")
            temp_pointer => temp_pointer%child
            CYCLE

!--   dims parameters
         CASE ("dims")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("neq")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, neq)

!--   solver parameters
         CASE ("solver")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("convergence")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, convergence)
         CASE ("aceqlb")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, aceqlb)
         CASE ("itermax")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, itermax)

         CASE default
            WRITE(*, *) 'ERROR: invalid parameter', cname
            return_status = 1
            EXIT
         END SELECT
         DO
            IF (ASSOCIATED(temp_pointer%sibling)) THEN
               temp_pointer => temp_pointer%sibling
               EXIT
            END IF
            IF (ASSOCIATED(temp_pointer%parent, parameter_list%first )) &
                 EXIT outer
            IF (ASSOCIATED(temp_pointer%parent)) THEN
               temp_pointer => temp_pointer%parent
            ELSE
               WRITE(*, *) 'ERROR: broken list.'
               RETURN
            END IF
         END DO
      END DO outer

      !-- destroy tree
      CALL destroy_xml_tree(parameter_list)

      RETURN

    END SUBROUTINE assign_code_parameters

    SUBROUTINE read_codeparam(in_xml, filename, codeparam)

      USE euitm_schemas
      USE ets_version
      IMPLICIT NONE

      INTEGER n_lines, in_xml, ios, i
      CHARACTER (len=*) :: filename
      TYPE (type_codeparam) :: codeparam
      CHARACTER(len = 132) :: xml_line
    
      OPEN (unit = in_xml, file = filename, status = 'old', &
           action = 'read', iostat = ios)
    
      IF (ios /= 0) THEN
         WRITE(*,*) 'Could not open ',TRIM(filename)
         STOP ' ERROR:  XML file does not exist '
      END IF
    
      n_lines = 0
    
      DO
         READ (in_xml, '(a)', iostat = ios) xml_line
         IF (ios == 0) THEN
            n_lines = n_lines + 1
         ELSE
            EXIT
         END IF
      END DO
    
      REWIND in_xml
    
      ALLOCATE(codeparam%codename(1))
      codeparam%codename(1)='EMEQ'
      ALLOCATE(codeparam%codeversion(1))
      codeparam%codeversion(1)=version
      WRITE(*,*) 'Code = ',TRIM(codeparam%codename(1)),' version = ',TRIM(codeparam%codeversion(1))
      ALLOCATE(codeparam%parameters(n_lines))
      DO i = 1, n_lines
         READ (in_xml, '(a)', iostat = ios) codeparam%parameters(i)
      END DO
    
      CLOSE(in_xml)
    
      RETURN
    END SUBROUTINE read_codeparam
  
  END SUBROUTINE EMEQ_E3M
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE EMEQ_INTERFACE(                  &
       ! +++ EQUILIBRIUM:
       ! +++ Interface to three moment solver from ASTRA (G.Pereversev) [EMEQ]
       !       
       !     INPUT FROM THE TRANSPORT CODE TO EQUILIBRIUM:
       !     values:
             RGEO,       & ! - Geometrical major radius    [m]  
             BGEO,       & ! - Toroidal magnetic field     [T]  
             PC,         & ! - Plasma current              [A]  
             DELTA,      & ! - Shift of plasma column      [m]  
             ELN,        & ! - elongation, boundary value
             TRN,        & ! - triangularity, boundary value
             AMIN,       & ! - minor radius                [m]
             NPSI,       & ! - Number of radial points 
             NEQ,        & ! - Number of equilibrium points 
             ACEQLB,     & !
             CONVERGENCE,& ! 
             ITERMAX,    & !
       !     arrays(NPSI):
             RHO,        & ! - Rho                         [m]
             AMID,       & ! - Normalized minor radius,    [m]
             Jpar,       & ! - Parallel current density    [A/m^2]
             QSF,        & ! - safety factor 
             PR,         & ! - pressure profile            [Pa] 
       !
       !     OUTPUT FROM THE EQUILIBRIUM TO THE TRANSPORT CODE:
             G1,         & ! - <1/R^2>                     [m^-2]   
             G2,         & ! - <grad_rho^2/R^2>            [m^-2]   
             G3,         & ! - <grad_rho^2>                [-]
             G4,         & ! - <1/B^2>                     [1/T^2]
             G5,         & ! - <B^2>                       [T^2]
             G6,         & ! - <grad_rho^2/B^2>            [m^2/T^2]  
             G7,         & ! - <grad_rho>                  [-]
             B2B02,      & ! - <B**2/B0**2>                [-]
             VOL,        & ! - volume (V)                  [m^3]
             VPRIME,     & ! - volume derivative (V')      [m^2]
             AREA,       & ! - cross-sectional area        [m^2] 
             AREAPRIME,  & ! - c.-s. area derivative       [m]   
             FDIA,       & ! - diamagnetic function        [-]
             SHAFR_SHIFT,& ! 
             ELONG,      & ! 
             TRIANG)       !



    USE ITM_TYPES
    USE ITM_CONSTANTS
    USE EMEQ_EQUIL



    IMPLICIT NONE

    INTEGER            :: NPSI, NEQ, I, ITER

    ! +++ input (0-D parameters):
    REAL (R8)          :: RGEO, BGEO, PC                           
    REAL (R8)          :: DELTA, ELN, TRN                      
    REAL (R8)          :: ACEQLB, CONVERGENCE                              
    INTEGER            :: ITERMAX

    ! +++ input (1-D arrays):
    REAL (R8)          :: RHO(NPSI),        RHO_NEW(NPSI)
    REAL (R8)          :: AMID(NPSI),       AMID_EQ(NEQ)                            
    REAL (R8)          :: Jpar(NPSI),       QSF(NPSI)
    REAL (R8)          :: PR(NPSI)      

    ! +++ output (1-D arrays):
    REAL (R8)          :: G1(NPSI),         G1_EQ(NEQ)                            
    REAL (R8)          :: G2(NPSI),         G2_EQ(NEQ)                            
    REAL (R8)          :: G3(NPSI),         G3_EQ(NEQ)                            
    REAL (R8)          :: G4(NPSI),         G4_EQ(NEQ)                            
    REAL (R8)          :: G5(NPSI),         G5_EQ(NEQ)                            
    REAL (R8)          :: G6(NPSI),         G6_EQ(NEQ)                            
    REAL (R8)          :: G7(NPSI),         G7_EQ(NEQ)                             
    REAL (R8)          :: AREA(NPSI),       AREA_EQ(NEQ)
    REAL (R8)          :: AREAPRIME(NPSI),  AREAPRIME_EQ(NEQ)
    REAL (R8)          :: VOL(NPSI),        VOL_EQ(NEQ)
    REAL (R8)          :: VPRIME(NPSI),     VPRIME_EQ(NEQ)         
    REAL (R8)          :: FDIA(NPSI),       FDIA_EQ(NEQ)
    REAL (R8)          :: B2B02(NPSI),      B2B02_EQ(NEQ)
    REAL (R8)          :: SHAFR_SHIFT(NPSI),GBD(NEQ)              
    REAL (R8)          :: ELONG(NPSI),      GL(NEQ)               
    REAL (R8)          :: TRIANG(NPSI),     GSD(NEQ)              


    ! +++ internal quantities:
    REAL (R8)          :: JA(NPSI),         JAEQ(NEQ)
    REAL (R8)          :: JB(NPSI),         JBEQ(NEQ)                    

    REAL (R8)          :: TIME
    REAL (R8)          :: R0, B0, AMIN                           
    REAL (R8)          :: DPR(NPSI)                            
    REAL (R8)          :: NABRHO(NPSI),     NABRHO_EQ(NEQ)
    REAL (R8)          :: VPRIME_N(NPSI)
    REAL (R8)          :: NABRHO_N(NPSI)
    REAL (R8)          :: G2_N(NPSI)
    REAL (R8)          :: QSF_N(NPSI)
    REAL (R8)          :: FUN(NPSI),   INTFUN(NPSI)

    REAL (R8)          :: GR(NEQ)
    REAL (R8)          :: GRA(NEQ),    SQGRA(NEQ)
    REAL (R8)          :: GRAR(NEQ),   AVR2(NEQ)               
    REAL (R8)          :: AI0(NEQ),    DGRDA(NEQ),  AVSQG(NEQ)  
    REAL (R8)          :: GRDAEQ(NEQ), B2B0EQ(NEQ)           
    REAL (R8)          :: B0B2EQ(NEQ), B0B0EQ(NEQ), BMAXEQ(NEQ)
    REAL (R8)          :: BMINEQ(NEQ), BMODEQ(NEQ), FOFBEQ(NEQ)

    REAL (R8)          :: CONV, err_VPRIME, err_NABRHO, err_QSF, err_G2




! +++ ALLOCATE WORKING ARRAYS FOR EMEQ:
    CALL ALLOC_EMEQ_EQUIL(NEQ)


! +++ INITIAL SETTINGS:
    R0         = (RGEO+DELTA)                                      ! Major radius
    B0         =  BGEO*RGEO/R0                                     ! Magnetic field at R0

    CALL L3deriv(PR, RHO, NPSI,    DPR, RHO, NPSI)


! +++ this might not be necessary
    DO i=1,NPSI
       DPR(i)   = MIN(0.0_R8,DPR(i))                               ! negative derivative is not acceptable 
    END DO                                                         ! by the equilibrium solver



! +++ equidistant equilibrium grid:
    DO i=1,NEQ
       AMID_EQ(i)   =  AMIN /(NEQ-1)*(i-1)                   
    END DO



    ITER        =  0

    VPRIME      =  0.0_R8
    NABRHO      =  0.0_R8
    G2          =  0.0_R8

! +++ CALCULATION OF GEOMETRY COEFFICIENTS:
1   CONTINUE
    
    ITER       = ITER +1

    IF(ITER.GT.100) THEN
       WRITE(*,*) '### ERROR!  Equilibrium loop in EQUILIBRIUM_INTERFACE not converging'
       STOP       'Error - no convergence in EQUILIBRIUM_INTERFACE'
       CALL        DEALLOC_EMEQ_EQUIL
       RETURN

    ENDIF



! +++ PARAMETERS FOR ITERATIONS:
    VPRIME_N   = VPRIME                                            !copy of equilibrium quantities for iterations
    NABRHO_N   = NABRHO
    QSF_N      = QSF
    G2_N       = G2

    

! +++ RHS of Grad-Shafranov equation:
2   DO i=2,NPSI
       JB(i)   = -R0/B0*QSF(i)/RHO(i)*DPR(i)*1.D-6                 ![MA/m^2]

       JA(i)   =  JB(i)*(1.0_R8-FDIA(i)**2/B2B02(i)*RGEO**2/R0**2)  &
                 + Jpar(i)/1.D6*FDIA(i)/B2B02(i)*RGEO/R0           ![MA/m^2]
    END DO


    CALL AXIS_INT      (NPSI, RHO, JB)
    CALL AXIS_INT      (NPSI, RHO, JA)



! +++ Interpolation of input coefficients on equilibrium grid:
    CALL L3interp      (JA, AMID ,NPSI,    JAEQ, AMID_EQ, NEQ)
    CALL L3interp      (JB, AMID ,NPSI,    JBEQ, AMID_EQ, NEQ)



! +++ CALL TO THE EQUILIBRIUM SOLVER:      
    CALL E3ASTR      &
    !-------------------------------------------------------!
    ! The same equidistant grid with respect to the "radial"! 
    ! variable "A" is assumed to be used both for input and !
    ! output. The radial coordinate x=a/a_edge, is          !
    ! dimensionless, A(1)=0; A(NAEQ1)=1                     !
    !-------------------------------------------------------!
         (JAEQ,JBEQ     &                                          ! j_zeta = BA*(R0/r) + BB*(r/R0-R0/r)[MA/m^2]
         ,R0	        &                                          ! RGEO+DELTA, Major radius of the edge FS    [m]
         ,AMIN          &                                          ! minor radius                               [m]
         ,ELN	        &                                          ! elongation, edge value                     [-]
         ,TRN*AMIN      &                                          ! triangularity, edge value * minor radius   [m]
         ,NEQ           &                                          ! radial grid point No.! NEQL
         ,ACEQLB        & 
         ,ITERMAX       &                                          ! max number of iterations
         ,B0            &                                          ! vacuum B @ R0                              [T]
         ,PC            &                                          ! total plasma current                       [MA]
         ! Output:
         ,GR,GBD,GL,GSD &                                          ! Rho(a),  Delta(a),  ELONG(a),  delta(a)
         ,GRA	        &                                          ! <g^{11}>= <[nabla(a)]^2>
         ,SQGRA	        &                                          ! <\sqrt{|g^{11}|}>= <|nabla(a)|>
         ,GRAR	        &                                          ! <g^{11}g^{33}>	= <[nabla(a)/r]^2>
         ,AVR2	        &                                          ! <g^{33}>= <1/r^2>=G33/R0**2
         ,AI0	        &                                          ! I-> IPOL*R0*BTOR
         ,DGRDA	        &                                          ! \prti{\rho}{a}
         ,AVSQG	        &                                          ! \prti{V}{a}/(4\pi^2)
         ,VOL_EQ        &                                          ! V(a)
         ,B2B0EQ        &                                          ! <B**2/B0**2>
         ,B0B0EQ        &                                          ! <B0**2/B**2>
         ,BMAXEQ        &                                          ! BMAXT
         ,BMINEQ        &                                          ! BMINT
         ,BMODEQ        &                                          ! <B/BTOR>
         ,FOFBEQ        &                                          ! <(BTOR/B)**2*(1.-SQRT(1-B/Bmax)*(1+.5B/Bmax))>
         ,GRDAEQ        &                                          ! <grad a>     
         ,TIME)



! +++ OUTPUT PARAMETERS (on equilibrium grid):


    G1_EQ           =  AVR2                                        !<1/R^2>              [-]
    G2_EQ           =  GRAR * DGRDA**2                             !<(grad_rho^2/R^2>    [-]
    G3_EQ           =  GRA * DGRDA**2                              !<(grad_rho)^2>       [-]
    G4_EQ           =  B0B0EQ / B0**2                              !<1/B^2>              [T^-2]
    G5_EQ           =  B2B0EQ * B0**2                              !<B^2>                [T^2]
    G6_EQ           =  GRA * DGRDA**2 / B0**2                      !<(grad_rho^2/B^2>    [m^2/T^2] (approximation)
    G7_EQ           =  SQGRA * DGRDA                               !<grad_rho>           [-]

    FDIA_EQ         =  AI0 / (B0*R0)                               !diamagnetic function [-]
    B2B02_EQ        =  B2B0EQ                                      !<B^2/B0^2>

    NABRHO_EQ       =  GRA * DGRDA**2                              !<(grad_rho)^2>       [-]
    GSD             =  GSD / AMID_EQ                               !triangularity in ETS units
    CALL AXIS_INT      (NEQ,  AMID_EQ, GSD)

    CALL L3deriv       (VOL_EQ, GR, NEQ,    VPRIME_EQ, GR, NEQ)    !V' on rho coordinate
 
    AREA_EQ         =  VPRIME_EQ * SQGRA * DGRDA                   !V'*<grad_rho>        [m^2]
    CALL L3deriv       (AREA_EQ, GR, NEQ,    AREAPRIME_EQ, GR, NEQ)!AREA' on rho coordinate 




! +++ Interpolation of output quantities on RHO grid:
    DO I =1,NPSI
       RHO_NEW(I)   = GR(NEQ)*(I-1)/(NPSI-1)
    END DO

    FUN             = Jpar
    CALL L3interp    (FUN,          RHO, NPSI,     Jpar,         RHO_NEW, NPSI)
    FUN             = DPR
    CALL L3interp    (FUN,          RHO, NPSI,     DPR,          RHO_NEW, NPSI)

    RHO             = RHO_NEW


    CALL L3interp    (AMID_EQ,      GR, NEQ,     AMID,       RHO, NPSI)
    CALL L3interp    (NABRHO_EQ,    GR, NEQ,     NABRHO,     RHO, NPSI)
    CALL L3interp    (VOL_EQ,       GR, NEQ,     VOL,        RHO, NPSI)
    CALL L3interp    (VPRIME_EQ,    GR, NEQ,     VPRIME,     RHO, NPSI)
    CALL L3interp    (G1_EQ,        GR, NEQ,     G1,         RHO, NPSI)
    CALL L3interp    (G2_EQ,        GR, NEQ,     G2,         RHO, NPSI)
    CALL L3interp    (G3_EQ,        GR, NEQ,     G3,         RHO, NPSI)
    CALL L3interp    (G4_EQ,        GR, NEQ,     G4,         RHO, NPSI)
    CALL L3interp    (G5_EQ,        GR, NEQ,     G5,         RHO, NPSI)
    CALL L3interp    (G6_EQ,        GR, NEQ,     G6,         RHO, NPSI)
    CALL L3interp    (G7_EQ,        GR, NEQ,     G7,         RHO, NPSI)
    CALL L3interp    (FDIA_EQ,      GR, NEQ,     FDIA,       RHO, NPSI)
    CALL L3interp    (B2B02_EQ,     GR, NEQ,     B2B02,      RHO, NPSI)
    CALL L3interp    (AREA_EQ,      GR, NEQ,     AREA,       RHO, NPSI)
    CALL L3interp    (AREAPRIME_EQ, GR, NEQ,     AREAPRIME,  RHO, NPSI)
    CALL L3interp    (GBD,          GR, NEQ,     SHAFR_SHIFT,RHO, NPSI)
    CALL L3interp    (GL,           GR, NEQ,     ELONG ,     RHO, NPSI)
    CALL L3interp    (GSD,          GR, NEQ,     TRIANG,     RHO, NPSI)


! +++ recalculation of the safety factor:
    INTFUN          = 0.0_R8
    DO i=1,NPSI
       FUN(I)       = Jpar(i)*VPRIME(I)*ITM_MU0/(FDIA(I)*B0*R0)**2
       IF (RHO(I).NE.0.0_R8.AND.i.GT.1) THEN                                                      
          INTFUN(I) = INTFUN(I-1) + (FUN(I-1)+FUN(I))*(RHO(i)-RHO(i-1))/2.0_R8
          QSF(I)    = RHO(i)*VPRIME(I)*G2(I)/(FDIA(I)*B0*R0)/INTFUN(I)
       END IF  
    END DO
    CALL AXIS_INT    (NPSI, RHO, QSF)



! +++ CONVERGENCY CHECK:
    conv=0.0_R8

    err_VPRIME      = MAXVAL(ABS(1.0_R8-VPRIME_N(2:)/VPRIME(2:)))
    err_NABRHO      = MAXVAL(ABS(1.0_R8-NABRHO_N/NABRHO))
    err_QSF         = MAXVAL(ABS(1.0_R8-QSF_N/QSF))
    err_G2          = MAXVAL(ABS(1.0_R8-G2_N/G2))

    WRITE(*,*) 'err_VPRIME ', err_VPRIME
    WRITE(*,*) 'err_NABRHO ', err_NABRHO
    WRITE(*,*) '   err_QSF ', err_QSF
    WRITE(*,*) '    err_G2 ', err_G2

    CONV            = MAX( err_VPRIME, err_NABRHO, err_QSF, err_G2)

    WRITE(*,*) '      CONV ', CONV

    IF(CONV.GT.CONVERGENCE)  GOTO 1



! +++ CHECK FOR NEGATIVE VOLUME:
    IF(vol(1).LT.0) THEN
       WRITE(*,*) 'VOL(1) = ',vol(1)
       vol(1)       = 0.0_R8
    ENDIF
    IF(vprime(1).LT.0.0_R8) THEN
       WRITE(*,*) '### ', vprime(1),' changed to 0'
       vprime(1)    = 0.0_R8
    ENDIF


    CALL        DEALLOC_EMEQ_EQUIL


    RETURN



  END SUBROUTINE EMEQ_INTERFACE
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  !  This subroutine finds f(r_1=0) from f(r_2), f(r_3) and f(r_4)
  !
  !  author: M.Tokar (m.tokar@fz-juelich.de)
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE AXIS_INT(n, r, f)
    !-------------------------------------------------------!
    !                                                       !
    !     This subroutine finds                             !
    !     f(r_1=0) from f(r_2), f(r_3) and f(r_4)           !
    !                                                       !
    !-------------------------------------------------------!  

    USE ITM_TYPES
    USE ITM_CONSTANTS

    IMPLICIT NONE

    INTEGER            :: n, i
    REAL (R8)          :: h(n), r(n), f(n)


    DO i=1,3
       h(i)=r(i+1)-r(i)
    END DO

    f(1)     = ((f(2)*r(4)/h(2)+f(4)*r(2)/h(3))*r(3)         &
               -f(3)*(r(2)/h(2)+r(4)/h(3))*r(2)*r(4)/r(3))   &
               /(r(4)-r(2))

    RETURN


  END SUBROUTINE AXIS_INT
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  ! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



END MODULE EMEQ
