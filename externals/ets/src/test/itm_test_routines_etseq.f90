! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module provides routines for testing
!>
!> \author ???
!>
!> \version "$Id: itm_test_routines_etseq.f90 1569 2013-11-20 16:17:36Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE ITM_TEST_ROUTINES_ETSEQ

CONTAINS



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> read in the xml version of the input file
!>
!> \author D. Coster
!>
!> \version "$Id: itm_test_routines_etseq.f90 1569 2013-11-20 16:17:36Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE PROCESS_XML(                                      &
       NPSI, NRHO, NEQ_DIM1, NEQ_DIM2, MAX_NPOINTS,            &
!
       NNUCL, NION, NIMP, NZIMP, NNEUT, NCOMP, NTYPE,          &
!  
       NTIME, NSOL,                                            &
!
       AMN, ZN, ZION,    AMN_IMP, ZN_IMP, MAX_Z_IMP,           &
!
       COLD_NEUTRALS, THERMAL_NEUTRALS,                        &
       FAST_NEUTRALS, NBI_NEUTRALS,                            &

!
       PSI_TYPE, NE_TYPE, NI_TYPE, TI_TYPE, TE_TYPE,           &
       VTOR_TYPE,IMP_TYPE,N0_TYPE, T0_TYPE,                    &
!
       PSI_VALUE,  NE_VALUE,  NI_VALUE,  TI_VALUE, TE_VALUE,   &
       VTOR_VALUE, IMP_VALUE, N0_VALUE, T0_VALUE,              &
!
       SHOT_IN, RUN_IN, INTERPOL, DB_IN,                       &
       SHOT_OUT, RUN_OUT, TAU_OUT, db_out,                     &
       SOLVER_TYPE, SIGMA_SOURCE, TAU, AMIX, CONVREC,          &
       START_TIME,                                             &
!
       IP, GEO_AX, PLASMA_AX, AMIN, ELONG, TRIA_UP, TRIA_LOW,  &
!
       PROF_FLAG, J0_FLAG, Q0_FLAG, EQ_SOURCE,                 &
       time_dep_input, ext_equil, equil_mod,                   &
       ext_source, ext_transport, add_transport, QUASI_NEUT,   &
       TAU_INC, TAU_DEC, ITER_INC, ITER_DEC,                   &
       TAU_MIN, TAU_MAX,                                       &
       exp_option, exp_ncols,                                  &
       evolution_labels, evolution_data,                       &
       augment_equil, rho_f, icoronal,                         &
       CODE_PARAMETERS)

    
    USE itm_types
    USE euitm_schemas
    USE euitm_xml_parser  

    IMPLICIT NONE
    
    INTEGER                :: NPSI                !number of equilibrium      (input)
    INTEGER                :: NEQ_DIM1             
    INTEGER                :: NEQ_DIM2             
    INTEGER                :: NEQ_MAX_NPOINTS           
    INTEGER                :: MAX_NPOINTS

    INTEGER                :: NRHO                !number of radial points    (input)
    INTEGER                :: NNUCL          
    INTEGER                :: NION                !number of ion species      (input)
    INTEGER                :: NIMP                !number of impurity species (input)
    INTEGER,   ALLOCATABLE :: NZIMP(:)            !number of charge states for each impurity (input)
    INTEGER                :: NNEUT               !number of neutrals         (input)
    INTEGER,   ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
    INTEGER,   ALLOCATABLE :: NTYPE(:)            !number of types for each neutral
    INTEGER                :: NTIME               !number of time points      (input)
    
    INTEGER                :: IRHO                !current radial knot
    INTEGER                :: IION                !current ion type
    INTEGER                :: ITIME               !current time step
    INTEGER                :: IIMP              
    INTEGER                :: IZIMP              
    
    INTEGER                :: NSOL                !Number of analytical example
    INTEGER                :: SOLVER_TYPE         !representation of transport equations
    INTEGER                :: SIGMA_SOURCE        !origin of Plasma electrical conductivity
    INTEGER                :: QUASI_NEUT          !Quasi neutrality:0-electrons; 1-ions from BC; 2-ions fron charge
    INTEGER                :: ICORONAL            !Coronal flag: "0"-OFF; "1" - replace boundary conditions by coronal; "2" - replace boundary conditions and profiles by coronal 
    
    REAL (R8)              :: CONVREC             !required convergency 
    REAL (R8)              :: START_TIME               !Start Time
    REAL (R8)              :: TAU                 !time step, and mixing coefficient
    REAL (R8)              :: AMIX                !mixing factor
    
    REAL (R8)              :: TAU_INC             !time step increment factor if ITERATIONS < ITER_INC
    REAL (R8)              :: TAU_DEC             !time step decrement factor if ITERATIONS > ITER_DEC
    INTEGER                :: ITER_INC            !ITERATION limit to cause increase in time-step
    INTEGER                :: ITER_DEC            !ITERATION limit to cause decrease in time-step
    REAL (R8)              :: TAU_MIN             !minimum time step
    REAL (R8)              :: TAU_MAX             !maximim time step
    REAL (R8)              :: RHON

    REAL (R8), ALLOCATABLE :: AMN(:)
    REAL (R8), ALLOCATABLE :: ZN(:)
    REAL (R8), ALLOCATABLE :: ZION(:)
    REAL (R8), ALLOCATABLE :: AMN_IMP(:)
    REAL (R8), ALLOCATABLE :: ZN_IMP(:)
    REAL (R8), ALLOCATABLE :: MAX_Z_IMP(:)

    INTEGER                :: COLD_NEUTRALS
    INTEGER                :: THERMAL_NEUTRALS
    INTEGER                :: FAST_NEUTRALS
    INTEGER                :: NBI_NEUTRALS

    INTEGER                :: PSI_BND_TYPE        !Type of boundary conditions current
    INTEGER                :: NE_BND_TYPE         !Type of boundary conditions electron density 
    INTEGER                :: NI_BND_TYPE         !Type of boundary conditions ion density 
    INTEGER                :: TI_BND_TYPE         !Type of boundary conditions ion temperature
    INTEGER                :: TE_BND_TYPE         !Type of boundary conditions electron temperature
    INTEGER                :: VTOR_BND_TYPE       !Type of boundary conditions toroidal rotation
    INTEGER                :: NIMP_BND_TYPE       !Type of boundary conditions toroidal rotation
    INTEGER                :: N0_BND_TYPE         !Type of boundary conditions toroidal rotation
    INTEGER                :: T0_BND_TYPE         !Type of boundary conditions toroidal rotation

    REAL (R8)              :: PSI_BND_VALUE       !Value of boundary conditions current
    REAL (R8)              :: NE_BND_VALUE        !Value of boundary conditions electron density 
    REAL (R8), ALLOCATABLE :: NI_BND_VALUE(:)     !Value of boundary conditions ion density 
    REAL (R8), ALLOCATABLE :: TI_BND_VALUE(:)     !Value of boundary conditions ion temperature
    REAL (R8)              :: TE_BND_VALUE        !Value of boundary conditions electron temperature
    REAL (R8), ALLOCATABLE :: VTOR_BND_VALUE(:)   !Value of boundary conditions toroidal rotation
    REAL (R8), ALLOCATABLE :: NIMP_BND_VALUE(:,:) !Value of boundary conditions impurity density
    REAL (R8), ALLOCATABLE :: N0_BND_VALUE_COLD(:)    !Value of boundary conditions neutral ion density
    REAL (R8), ALLOCATABLE :: N0_BND_VALUE_THERMAL(:) !Value of boundary conditions neutral impurity density
    REAL (R8), ALLOCATABLE :: T0_BND_VALUE_COLD(:)    !Value of boundary conditions neutral ion density
    REAL (R8), ALLOCATABLE :: T0_BND_VALUE_THERMAL(:) !Value of boundary conditions neutral impurity density

          
    INTEGER                :: PSI_TYPE
    REAL (R8)              :: PSI_VALUE(3)
    INTEGER                :: NE_TYPE
    REAL (R8)              :: NE_VALUE(3)
    INTEGER                :: TE_TYPE
    REAL (R8)              :: TE_VALUE(3)
    INTEGER,   ALLOCATABLE :: NI_TYPE(:)
    REAL (R8), ALLOCATABLE :: NI_VALUE(:,:)
    INTEGER,   ALLOCATABLE :: TI_TYPE(:)
    REAL (R8), ALLOCATABLE :: TI_VALUE(:,:)
    INTEGER,   ALLOCATABLE :: VTOR_TYPE(:)
    REAL (R8), ALLOCATABLE :: VTOR_VALUE(:,:)

    INTEGER,   ALLOCATABLE :: IMP_TYPE(:,:)
    REAL (R8), ALLOCATABLE :: IMP_VALUE(:,:,:)

    INTEGER,   ALLOCATABLE :: N0_TYPE(:,:)
    REAL (R8), ALLOCATABLE :: N0_VALUE(:,:,:)
    INTEGER,   ALLOCATABLE :: T0_TYPE(:,:)
    REAL (R8), ALLOCATABLE :: T0_VALUE(:,:,:)


    REAL (R8)              :: IP
    REAL (R8)              :: GEO_AX(3)
    REAL (R8)              :: PLASMA_AX(3)
    REAL (R8)              :: R_GEO,    Z_GEO,    B_GEO
    REAL (R8)              :: R_PLASMA, z_PLASMA, B_PLASMA
    REAL (R8)              :: AMIN
    REAL (R8)              :: ELONG
    REAL (R8)              :: TRIA_UP
    REAL (R8)              :: TRIA_LOW

    INTEGER                :: PROF_FLAG           !Flag for primary current quantity: 1-PSI, 2-Q, 3-JPAR
    INTEGER                :: J0_FLAG             !Flag for negative current density: 0-allowed, >0-cut off
    INTEGER                :: Q0_FLAG             !Flag for positive dq/drho: 0-allowed, >0-cut off
    INTEGER                :: EQ_SOURCE

    INTEGER                :: SHOT_IN, RUN_IN     !shot and run numbers
    INTEGER                :: INTERPOL            !interpolation index
    INTEGER                :: TIME_DEP_INPUT      !if 1, time dependence in input data
    INTEGER                :: EXT_EQUIL           !0: none, 1: BDSEQ, 2: EMEQ, 3: HELENA
    INTEGER                :: EQUIL_MOD
    INTEGER                :: augment_equil
    INTEGER                :: EXT_SOURCE          !if 2, call combine_source
    INTEGER                :: EXT_TRANSPORT       !if 1, call etaigb and neowes; if 2, call combine_transport
    REAL (R8)              :: ADD_TRANSPORT       !additional diffusive transport
    INTEGER                :: SHOT_OUT, RUN_OUT   !shot and run numbers
    REAL (R8)              :: TAU_OUT             !time step for profiles output into the data base
    INTEGER                :: exp_option
    INTEGER                :: exp_ncols
    character (len=32)     :: db_in, db_out
    INTEGER, PARAMETER     :: BUFLEN = 256
    CHARACTER(len=BUFLEN)  :: RHO_F

    CHARACTER (len=32)     :: tmp_labels(100)
    REAL (R8)              :: tmp_data(100*1000)

    CHARACTER (len=32), POINTER :: evolution_labels(:)
    REAL (R8), POINTER     :: evolution_data(:,:)

    INTEGER                :: return_status, n_labels, n_data, n_rows
    TYPE (type_param)      :: code_parameters



! intial values for parameters
    shot_in             = 4
    shot_out            = 5
    run_in              = 1
    run_out             = 2
    START_TIME          = 0.0_R8
    interpol            = 1
    time_dep_input      = 0
    tau                 = 0.1_R8
    tau_out             = -1.0_R8
    db_in               = 'mdsplus'
    db_out              = 'mdsplus'
    rhon                = 0.95_r8

    nrho                = 50
    npsi                = 100
    neq_dim1            = 100
    neq_dim2            = 100
    neq_max_npoints     = 100

    nion                = 1
    nimp                = 0
    nneut               = 0

    cold_neutrals       = 0
    thermal_neutrals    = 0
    fast_neutrals       = 0
    NBI_neutrals        = 0

    solver_type         = 3
    sigma_source        = 1
    amix                = 1.0_R8
    convrec             = 1.0E-4_R8
    ntime               = 100
    nsol                = 2

    psi_bnd_type        = 2
    ne_bnd_type         = 0
    ni_bnd_type         = 1
    ti_bnd_type         = 1
    te_bnd_type         = 1
    vtor_bnd_type       = 1
    nimp_bnd_type       = 1
    n0_bnd_type         = 1
    t0_bnd_type         = 1

    ext_equil           = 2
    equil_mod           = 0
    augment_equil       = 0
    ext_source          = 0
    ext_transport       = 0
    add_transport       = 0.0_R8
    TAU_INC             = 0.0_R8
    TAU_DEC             = 0.0_R8
    ITER_INC            = 0
    ITER_DEC            = 0
    TAU_MIN             = 0.0_R8
    TAU_MAX             = 0.0_R8
    exp_option          = 0
    PROF_FLAG           = 0
    J0_FLAG             = 1
    RHO_F               = '1.0'
    QUASI_NEUT          = 0
    ICORONAL            = 0




    CALL ASSIGN_CODE_PARAMETERS(code_parameters, return_status,     &
       NZIMP, NCOMP, NTYPE, MAX_NPOINTS,                            &
!  
       AMN, ZN, ZION,    AMN_IMP, ZN_IMP, MAX_Z_IMP,                &
!
       NI_BND_VALUE, TI_BND_VALUE, VTOR_BND_VALUE, NIMP_BND_VALUE,  &
!
       N0_BND_VALUE_COLD,  N0_BND_VALUE_THERMAL,                    &
!
       T0_BND_VALUE_COLD,  T0_BND_VALUE_THERMAL)
    
      if(allocated(nzimp)) then
         write(*,*) 'process_xml: nzimp allocated'
         write(*,*) 'nzimp = ', nzimp
      else
         write(*,*) 'process_xml: nzimp not allocated'
      endif


    ALLOCATE          (NI_TYPE(NION))
    ALLOCATE          (TI_TYPE(NION))
    ALLOCATE          (VTOR_TYPE(NION))

    ALLOCATE          (NI_VALUE(3,NION))
    ALLOCATE          (TI_VALUE(3,NION))
    ALLOCATE          (VTOR_VALUE(3,NION))

    if(nimp.gt.0) then
       ALLOCATE          (IMP_TYPE(NIMP,MAXVAL(NZIMP)))
       ALLOCATE          (IMP_VALUE(NIMP,3,MAXVAL(NZIMP)))
    endif

    write(*,*) 'NNEUT = ', NNEUT
    if(nneut.gt.0) then
       ALLOCATE          (N0_TYPE(NNEUT,MAXVAL(NTYPE)))
       ALLOCATE          (N0_VALUE(NNEUT,3,MAXVAL(NTYPE)))
       ALLOCATE          (T0_TYPE(NNEUT,MAXVAL(NTYPE)))
       ALLOCATE          (T0_VALUE(NNEUT,3,MAXVAL(NTYPE)))
    endif


    PSI_TYPE        = PSI_BND_TYPE
    PSI_VALUE       = 0._R8
    PSI_VALUE(1)    = PSI_BND_VALUE
    NE_TYPE         = NE_BND_TYPE
    NE_VALUE        = 0._R8
    NE_VALUE(1)     = NE_BND_VALUE
    TE_TYPE         = TE_BND_TYPE
    TE_VALUE        = 0._R8
    TE_VALUE(1)     = TE_BND_VALUE
    NI_TYPE(:)      = NI_BND_TYPE
    NI_VALUE        = 0._R8
    NI_VALUE(1,:)   = NI_BND_VALUE(:)
    TI_TYPE(:)      = TI_BND_TYPE
    TI_VALUE        = 0._R8
    TI_VALUE(1,:)   = TE_BND_VALUE
    VTOR_TYPE(:)    = VTOR_BND_TYPE
    VTOR_VALUE      = 0._R8
    VTOR_VALUE(1,:) = VTOR_BND_VALUE(:)

    if(nimp .gt. 0) then
       IMP_TYPE(:,:)   = NIMP_BND_TYPE
       IMP_VALUE       = 0._R8
       DO IIMP =1,NIMP
          DO IZIMP = 1, NZIMP(IIMP)
             IMP_VALUE(IIMP,1,IZIMP)= NIMP_BND_VALUE(IIMP,IZIMP)
          END DO
       END DO
    endif

    if(nneut.gt.0) then
       N0_TYPE(:,:)    = N0_BND_TYPE
       T0_TYPE(:,:)    = T0_BND_TYPE
       N0_VALUE        = 0._R8
       T0_VALUE        = 0._R8
       N0_VALUE(:,1,1) = N0_BND_VALUE_COLD(:)
       N0_VALUE(:,1,2) = N0_BND_VALUE_THERMAL(:)
       T0_VALUE(:,1,1) = T0_BND_VALUE_COLD(:)
       T0_VALUE(:,1,2) = T0_BND_VALUE_THERMAL(:)


    end if

! update parameters
    IF(tau_out .LT. 0.0_R8)     tau_out        = 0.1_R8

    IF (return_status /= 0) THEN
       WRITE(*, *) 'ERROR: Could not assign code parameters.'
       STOP
    END IF

    RETURN

  CONTAINS
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++



!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
!+++++++++++++++++++++++++++++++++++++++++++++++++++++++
    SUBROUTINE assign_code_parameters(codeparameters, return_status,&
       NZIMP, NCOMP, NTYPE, MAX_NPOINTS,                            &
!  
       AMN, ZN, ZION,    AMN_IMP, ZN_IMP, MAX_Z_IMP,                &
!
       NI_BND_VALUE, TI_BND_VALUE, VTOR_BND_VALUE, NIMP_BND_VALUE,  &
!
       N0_BND_VALUE_COLD,  N0_BND_VALUE_THERMAL,                    &
!
       T0_BND_VALUE_COLD,  T0_BND_VALUE_THERMAL)

      !-----------------------------------------------------------------------
      ! calls the XML parser for the code parameters and assign the
      ! resulting values to the corresponding variables
      !TODO: check an alternative and more elegant solution in Perl
      !-----------------------------------------------------------------------

      USE mod_f90_kind

      IMPLICIT NONE

      TYPE (type_param), INTENT(in) :: codeparameters
      INTEGER(ikind),   INTENT(out) :: return_status 

      TYPE(tree)                    :: parameter_list
      TYPE(element),        POINTER :: temp_pointer
      INTEGER(ikind)                :: i, nparm, n_values, n_data
      INTEGER                       :: n_data1, n_data2, n_data3
      CHARACTER(len = 132)          :: cname
      INTEGER                       :: integer_data(1000)
      REAL(R8)                      :: real_data(1000)
      INTEGER                       :: idata, NN, IIMP, IZIMP

      INTEGER,          ALLOCATABLE :: NZIMP(:)            !number of charge states for each impurity (input)
      INTEGER,          ALLOCATABLE :: NCOMP(:)            !number of components for each neutral
      INTEGER,          ALLOCATABLE :: NTYPE(:)            !number of types for each neutral
      INTEGER                       :: MAX_NPOINTS         !number of equilibrium boundary points

      REAL (R8),        ALLOCATABLE :: AMN(:)
      REAL (R8),        ALLOCATABLE :: ZN(:)
      REAL (R8),        ALLOCATABLE :: ZION(:)
      REAL (R8),        ALLOCATABLE :: AMN_IMP(:)
      REAL (R8),        ALLOCATABLE :: ZN_IMP(:)
      REAL (R8),        ALLOCATABLE :: MAX_Z_IMP(:)

      REAL (R8),        ALLOCATABLE :: NI_BND_VALUE(:)     !Value of boundary conditions ion density 
      REAL (R8),        ALLOCATABLE :: TI_BND_VALUE(:)     !Value of boundary conditions ion temperature
      REAL (R8),        ALLOCATABLE :: VTOR_BND_VALUE(:)   !Value of boundary conditions toroidal rotation
      REAL (R8),        ALLOCATABLE :: NIMP_BND_VALUE(:,:) !Value of boundary conditions impurity density
      REAL (R8),        ALLOCATABLE :: N0_BND_VALUE_COLD(:) !Value of boundary conditions neutral ion density
      REAL (R8),        ALLOCATABLE :: N0_BND_VALUE_THERMAL(:) !Value of boundary conditions neutral impurity density
      REAL (R8),        ALLOCATABLE :: T0_BND_VALUE_COLD(:) !Value of boundary conditions neutral ion density
      REAL (R8),        ALLOCATABLE :: T0_BND_VALUE_THERMAL(:) !Value of boundary conditions neutral impurity density

      return_status = 0      ! no error

!--   parse xml-string codeparameters%parameters

      WRITE(*,*) 'Calling euitm_xml_parse'
      CALL euitm_xml_parse(code_parameters, nparm, parameter_list)
      WRITE(*,*) 'Called euitm_xml_parse'

!--   assign variables

      temp_pointer => parameter_list%first

      outer: DO
         cname = char2str(temp_pointer%cname)   ! necessary for AIX
         SELECT CASE (cname)
         CASE ("parameters")
            temp_pointer => temp_pointer%child
            CYCLE

!--   input parameters
         CASE ("input")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("shot_in")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, shot_in)
         CASE ("run_in")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, run_in)
         CASE ("interpol")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, interpol)
         CASE ("time_dep_input")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, time_dep_input)
         case ("db_in")
            if (allocated(temp_pointer%cvalue)) &
                 db_in = char2str(temp_pointer%cvalue)


!--   output parameters
         CASE ("output")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("shot_out")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, shot_out)
         CASE ("run_out")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, run_out)
         CASE ("tau_out")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tau_out)
         case ("db_out")
            if (allocated(temp_pointer%cvalue)) &
                 db_out = char2str(temp_pointer%cvalue)


		 	 
!--   solver parameters
         CASE ("solver")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("rho")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 rho_f = char2str(temp_pointer%cvalue)
         CASE ("solver_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, solver_type)
         CASE ("sigma_source")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, sigma_source)
         CASE ("tau")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tau)
         CASE ("tau_inc")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tau_inc)
         CASE ("tau_dec")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tau_dec)
         CASE ("iter_inc")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, iter_inc)
         CASE ("iter_dec")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, iter_dec)
         CASE ("tau_min")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tau_min)
         CASE ("tau_max")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tau_max)
         CASE ("amix")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, amix)
         CASE ("convrec")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, convrec)
         CASE ("ntime")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ntime)
         CASE ("start_time")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, START_TIME)
         CASE ("nsol")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, nsol)
         CASE ("ext_equil")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ext_equil)
         case ("equil_mod")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, equil_mod)
         CASE ("augment_equil")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, augment_equil)
         CASE ("ext_source")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ext_source)
         CASE ("ext_transport")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ext_transport)
         CASE ("add_transport")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, add_transport)
         CASE ("quasi_neut")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, quasi_neut)
         CASE ("icoronal")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, icoronal)

!--   startup
         CASE ("startup")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("prof_flag")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, prof_flag)
         CASE ("j0_flag")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, j0_flag)
         CASE ("q0_flag")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, q0_flag)


!--   dims parameters
         CASE ("dims")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("npsi")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, npsi)
         CASE ("neq_dim1")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, neq_dim1)
         CASE ("neq_dim2")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, neq_dim2)
         CASE ("neq_max_npoints")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, max_npoints)
         CASE ("nrho")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, nrho)



!--   output parameters
         CASE ("equilibrium")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("ip")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ip)
         CASE ("r_geo")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, r_geo)
         CASE ("z_geo")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, z_geo)
         CASE ("b_geo")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, b_geo)
         CASE ("r_plasma")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, r_plasma)
         CASE ("z_plasma")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, z_plasma)
         CASE ("b_plasma")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, b_plasma)
         CASE ("amin")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, amin)
         CASE ("elong")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, elong)
         CASE ("tria_up")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tria_up)
         CASE ("tria_low")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, tria_low)
         CASE ("eq_source")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, eq_source)
         
         GEO_AX(1)     = R_GEO
         GEO_AX(2)     = Z_GEO
         GEO_AX(3)     = B_GEO

         PLASMA_AX(1)  = R_PLASMA
         PLASMA_AX(2)  = Z_PLASMA
         PLASMA_AX(3)  = B_PLASMA
		 	 
!--   compositions
         CASE ("compositions")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("ions")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("amn")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data1)
            ALLOCATE(amn(n_data1))
            amn  = real_data(1:n_data1)
            
         CASE ("zn")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data2)
            ALLOCATE(zn(n_data2))
            zn   = real_data(1:n_data2)

         CASE ("zion")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data3)
            ALLOCATE(zion(n_data3))
            zion = real_data(1:n_data3)


         NION    =  MIN(n_data1, n_data2, n_data3)
 
         CASE ("impurity")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("amn_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data1)
               ALLOCATE(amn_imp(n_data1))
               amn_imp  = real_data(1:n_data1)
            else
               n_data1 = 0
            endif

         CASE ("zn_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data2)
               ALLOCATE(zn_imp(n_data2))
               zn_imp   = real_data(1:n_data2)
            else
               n_data2 = 0
            endif

         CASE ("max_z_imp")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data3)
               ALLOCATE(max_z_imp(n_data3))
               max_z_imp = real_data(1:n_data3)
            else
               n_data3 = 0
            endif
            
            NIMP    =  MIN(n_data1, n_data2, n_data3)
            if(nimp .gt. 0) then
               ALLOCATE (NZIMP(NIMP))
               NZIMP   =  NINT(max_z_imp)
            endif

         CASE ("neutrals")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("cold_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, cold_neutrals)
            IF (cold_neutrals.gt.0) cold_neutrals = 1
         CASE ("thermal_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, thermal_neutrals)
            IF (thermal_neutrals.gt.0) thermal_neutrals = 1
         CASE ("fast_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, fast_neutrals)
            IF (fast_neutrals.gt.0) fast_neutrals = 1
         CASE ("NBI_neutrals")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, NBI_neutrals)
            IF (NBI_neutrals.gt.0) NBI_neutrals = 1




         NNUCL   = NION + NIMP !assummption of all species being different

         if(cold_neutrals + thermal_neutrals + fast_neutrals + NBI_neutrals.eq.0) then
            nneut=0
         else
            NNEUT   = NION + NIMP !assummption of all species being different
            ALLOCATE (NCOMP(NNEUT))
            ALLOCATE (NTYPE(NNEUT))
            NCOMP   = 1           !assumption: single atoms
            NTYPE   = cold_neutrals + thermal_neutrals + fast_neutrals + NBI_neutrals
         endif
         ALLOCATE (ni_bnd_value(NION))
         ALLOCATE (ti_bnd_value(NION))
         ALLOCATE (vtor_bnd_value(NION))
         if(nimp .gt. 0) ALLOCATE (nimp_bnd_value(NIMP,MAXVAL(NZIMP)))
         ALLOCATE (n0_bnd_value_cold(NNEUT))
         ALLOCATE (n0_bnd_value_thermal(NNEUT))
         ALLOCATE (t0_bnd_value_cold(NNEUT))
         ALLOCATE (t0_bnd_value_thermal(NNEUT))



!--   boundary parameters
         CASE ("boundary")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("type")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("psi_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, psi_bnd_type)
         CASE ("ne_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ne_bnd_type)
         CASE ("ni_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ni_bnd_type)
         CASE ("ti_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ti_bnd_type)
         CASE ("te_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, te_bnd_type)
         CASE ("vtor_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, vtor_bnd_type)
         CASE ("nimp_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, nimp_bnd_type)
         CASE ("n0_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, n0_bnd_type)
         CASE ("t0_bnd_type")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, t0_bnd_type)



         CASE ("value")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("psi_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, psi_bnd_value)
         CASE ("ne_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, ne_bnd_value)
         CASE ("ni_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue),  real_data, n_data)
            ni_bnd_value = real_data(1:n_data)

         CASE ("ti_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
            ti_bnd_value = real_data(1:n_data)

         CASE ("te_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, te_bnd_value)
         CASE ("vtor_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
            vtor_bnd_value = real_data(1:n_data)

         CASE ("nimp_bnd_value")
            IF (ALLOCATED(temp_pointer%cvalue)) then
               CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
               nimp_bnd_value = 0.0_R8
               write(*,*) NIMP
               NN=0
               DO IIMP =1,NIMP
                  DO IZIMP = 1, NZIMP(IIMP)
                     NN = NN + 1
                     IF (NN.LE.n_data)     &
                          nimp_bnd_value(IIMP,IZIMP) = real_data(NN)
                  END DO
               END DO
            endif

         CASE ("n0_bnd_value_cold")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
            n0_bnd_value_cold = real_data(1:n_data)

         CASE ("n0_bnd_value_thermal")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
            n0_bnd_value_thermal = real_data(1:n_data)

         CASE ("t0_bnd_value_cold")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
            t0_bnd_value_cold = real_data(1:n_data)

         CASE ("t0_bnd_value_thermal")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL scan_str2real(char2str(temp_pointer%cvalue), real_data, n_data)
            t0_bnd_value_thermal = real_data(1:n_data)



!--   experimental parameters
         CASE ("experimental")
            temp_pointer => temp_pointer%child
            CYCLE
         CASE ("option")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, exp_option)
         CASE ("ncolumns")
            IF (ALLOCATED(temp_pointer%cvalue)) &
                 CALL char2num(temp_pointer%cvalue, exp_ncols)
         CASE ("evolution_labels")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               CALL scan_str2str(char2str(temp_pointer%cvalue), 32, tmp_labels, n_labels)
               ALLOCATE(evolution_labels(n_labels))
               evolution_labels=tmp_labels(1:n_labels)
            ENDIF
         CASE ("evolution_data")
            IF (ALLOCATED(temp_pointer%cvalue)) THEN
               CALL scan_str2real(char2str(temp_pointer%cvalue), tmp_data, n_data)
               IF(exp_ncols.LE.0) THEN
                  WRITE(*,*) 'exp_ncols = ', exp_ncols, ' invalid'
                  STOP 'Error in exp_ncols'
               ENDIF
               n_rows=n_data/exp_ncols
               IF(n_rows*exp_ncols .NE. n_data) THEN
                  WRITE(*,*) 'Mismatch in "evolution_data" length: ', n_rows, exp_ncols, n_data
                  STOP 'Error in "evolution_data" length'
               ENDIF
               ALLOCATE(evolution_data(exp_ncols, n_rows))
               evolution_data=RESHAPE(tmp_data(1:n_data),(/ exp_ncols, n_rows /))
            ENDIF
            

!--  default
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

      if(allocated(nzimp)) then
         write(*,*) 'assign_code_parameters: nzimp allocated'
         write(*,*) 'nzimp = ', nzimp
      else
         write(*,*) 'assign_code_parameters: nzimp not allocated'
      endif

      RETURN

    END SUBROUTINE assign_code_parameters
    
  END SUBROUTINE process_xml

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
    codeparam%codename(1)='ETS'
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
  



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This subroutine stores the results of computations into files
!>
!> \author ???
!>
!> \version "$Id: itm_test_routines_etseq.f90 1569 2013-11-20 16:17:36Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!DPC 2009-01-29  SUBROUTINE WRITE_OUTPUT &
   SUBROUTINE WRITE_OUT (ITIME, COREPROF)
!     This subroutine stores the results of computations
!     into files

    USE EUITM_SCHEMAS

    IMPLICIT NONE

! +++ Input parameters:
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)      !input CPO with internal ETS parameters profiles 

! +++ Internal parameters:
    INTEGER                          :: IRHO,IION,ITIME
    INTEGER                          :: NRHO,NION

    CHARACTER (33)                      FILENAME

    NRHO             = SIZE(COREPROF(1)%rho_tor)
    NION             = SIZE(COREPROF(1)%compositions%ions)

    WRITE(FILENAME,'(a,i7.7,a)') 'eq_ets_data/OUTPUT/OUT',ITIME,'.DAT'

    OPEN (UNIT=10, FILE=FILENAME)



    DO IRHO = 1, NRHO
!                                                1                                
       WRITE (10,'(10(1x,e16.8))')    COREPROF(1)%rho_tor(IRHO),               &
!                                                2    
                                      COREPROF(1)%ni%value(IRHO,NION),         & 
!                                                3
                                      COREPROF(1)%ne%value(IRHO),              &
!                                                4
                                      COREPROF(1)%ti%value(IRHO,NION),         &
!                                                5
                                      COREPROF(1)%te%value(IRHO),              &
!                                                6
                                      COREPROF(1)%vtor%value(IRHO,NION),       &              
!                                                7
                                      COREPROF(1)%psi%value(IRHO),             &       
!                                                8
                                      COREPROF(1)%profiles1d%jtot%value(IRHO), &
!                                                9
                                      COREPROF(1)%profiles1d%q%value(IRHO),    &   
!                                                10
                                      COREPROF(1)%profiles1d%zeff%value(IRHO) 
    END DO

    CLOSE (10)


    RETURN
  END SUBROUTINE WRITE_OUT
!DPC 2009-01-29  END SUBROUTINE WRITE_OUTPUT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This subroutine stores the results of computations into files
!>
!> author DENIS KALUPIN
!>
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
   SUBROUTINE WRITE_EQUILIBRIUM (ITIME,EQUILIBRIUM)
!     This subroutine stores the results of computations
!     into files

    USE EUITM_SCHEMAS

    IMPLICIT NONE

! +++ Input parameters:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)

! +++ Internal parameters:
    INTEGER                          :: IRHO,ITIME
    INTEGER                          :: NRHO

    CHARACTER (35)                      FILENAME

    NRHO             = SIZE(EQUILIBRIUM(1)%profiles_1d%rho_tor)

    WRITE(FILENAME,'(a,i7.7,a)') 'eq_ets_data/OUTPUT/EQOUT',ITIME,'.DAT'

    OPEN (UNIT=10, FILE=FILENAME)

    WRITE(*,*) ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%rho_tor),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%q),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%pressure),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%jparallel),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm1),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm2),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm3),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm4),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm5),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm6),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%gm7),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%volume),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%vprime),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%area),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%aprime),  &
         ASSOCIATED(EQUILIBRIUM(1)%profiles_1d%F_dia)

    DO IRHO = 1, NRHO
!                                                                     1                                
       WRITE (10,'(16(1x,e16.8))')                       EQUILIBRIUM(1)%profiles_1d%rho_tor(IRHO),  &
!                    2                                                3
            EQUILIBRIUM(1)%profiles_1d%q(IRHO),          EQUILIBRIUM(1)%profiles_1d%pressure(IRHO),     & 
!                    4                                                5
            EQUILIBRIUM(1)%profiles_1d%jparallel(IRHO),  EQUILIBRIUM(1)%profiles_1d%gm1(IRHO),          &
!                    6                                                7
            EQUILIBRIUM(1)%profiles_1d%gm2(IRHO),        EQUILIBRIUM(1)%profiles_1d%gm3(IRHO),          &
!                    8                                                9
            EQUILIBRIUM(1)%profiles_1d%gm4(IRHO),        EQUILIBRIUM(1)%profiles_1d%gm5(IRHO),          &
!                    10                                               11
            EQUILIBRIUM(1)%profiles_1d%gm6(IRHO),        EQUILIBRIUM(1)%profiles_1d%gm7(IRHO),          &
!                    12                                               13
            EQUILIBRIUM(1)%profiles_1d%volume(IRHO),     EQUILIBRIUM(1)%profiles_1d%vprime(IRHO),       &
!                    14                                               15
            EQUILIBRIUM(1)%profiles_1d%area(IRHO),       EQUILIBRIUM(1)%profiles_1d%aprime(IRHO),       &
!                    16                                   
            EQUILIBRIUM(1)%profiles_1d%F_dia(IRHO)
    END DO

    CLOSE (10)


    RETURN
  END SUBROUTINE WRITE_EQUILIBRIUM
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



END MODULE ITM_TEST_ROUTINES_ETSEQ
