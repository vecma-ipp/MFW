! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Framework for testing workflows built around the ETS.
!>
!> Available equilibrium codes
!> * bdseq
!> * emeq
!> * helena
!>
!> Available transport models
!> * neowes
!> * etaigb
!>
!> Availablem source mode
!> * neutrals
!> * dummy ecrh
!> * dummy icrh
!> * dummy NBI
!> 
!> Impurities
!>
!> Limited ability to evolve the plasma shape and key quantities during a run
!>
!> \author D.Kalupin with modifications by D. Coster
!>
!> \version "$Id: ets_workflow.F90 1763 2016-06-22 15:41:53Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
PROGRAM ETS_WORKFLOW

  !-------------------------------------------------------!
  !     This program runs the  ETS with                   !
  !     three moment equilibrium solver, transport        !
  !     coefficients from ETAIGB and NEOWES, souces       !
  !     from NEUTRALS.                                    !
  !-------------------------------------------------------!
  !     Source:       ---                                 !
  !     Developers:   D.Kalupin                           !
  !     Kontacts:     D.Kalupin@fz-juelich.de             !
  !                                                       !
  !     Comments:     created for V&V between ETS and     !
  !                   ASTRA                               !
  !                                                       !
  !-------------------------------------------------------!
  USE ITM_TYPES
  USE ITM_CONSTANTS
  USE EUITM_ROUTINES
  USE EUITM_SCHEMAS
  USE COPY_STRUCTURES
  USE DEALLOCATE_STRUCTURES
  USE XML_FILE_READER
  USE ETS_WRAPPER_BDSEQ
  USE ETS_WRAPPER_EMEQ_E3M
#ifdef GOT_HELENA
  USE ETS_WRAPPER_HELENA
#endif
#ifdef GOT_CHEASE
  USE ETS_WRAPPER_CHEASE
#endif
  USE ETS_WRAPPER_ETAIGB
  USE ETS_WRAPPER_NEOWES
  USE GBTRANSPORT

  USE ETS_VERSION 
  USE ALLOCATE_DEALLOCATE
  USE ITM_TEST_ROUTINES_ETSEQ
  USE ETS
  USE EQUILIBRIUM_INPUT
  USE CONVERGENCE_CHECK
  USE SOURCE_COMBINER   
  USE TRANSPORT_COMBINER
  USE EQUILIBRIUM_START
  USE IMPURITY
  USE NEUTRALS
  USE equilibrium_augmenter
  USE size_of_structures
  USE read_structures
  USE write_structures
  USE ets_species_module
  USE PLASMA_COMPOSITION
  USE ETS_START
  USE FILL_CPOS
  USE CONVERT
  USE GAUSIAN_SRC
  USE CORONAL
  USE SPITZER
  USE SYNCHROTRON
  USE EQUILIBRIUM_TOOLS

!----------------------------------------------------------------------!
!     Declaration of variables:                                        !
!----------------------------------------------------------------------!
  IMPLICIT NONE

  !  +++ Suffixes of CPOs:
  !     index "_OLD"  - previous time step
  !     index "_ITER" - previous iteration
  !     index "_NEW"  - new time step
  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_OLD(:) => NULL(), EQUILIBRIUM_ITER(:) => NULL(), EQUILIBRIUM_NEW(:) => NULL()
  TYPE (TYPE_COREPROF),    POINTER :: COREPROF_OLD(:) => NULL(),    COREPROF_ITER(:) => NULL(),    COREPROF_NEW(:) => NULL()     
  TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_OLD(:) => NULL(),  CORETRANSP_ITER(:) => NULL()
  TYPE (TYPE_CORESOURCE),  POINTER :: CORESOURCE_OLD(:) => NULL(),  CORESOURCE_ITER(:) => NULL()      
  TYPE (TYPE_COREIMPUR),   POINTER :: COREIMPUR_OLD(:) => NULL(),   COREIMPUR_ITER(:) => NULL(),   COREIMPUR_NEW(:) => NULL()      
  TYPE (TYPE_CORENEUTRALS),POINTER :: CORENEUTRALS_OLD(:) => NULL(),CORENEUTRALS_ITER(:) => NULL(),CORENEUTRALS_NEW(:) => NULL() 
  TYPE (TYPE_TOROIDFIELD), POINTER :: TOROIDFIELD_OLD(:) => NULL()  
  TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_OLD(:) => NULL(),  NEOCLASSIC_ITER(:) => NULL()

#ifdef CORE_EDGE
  TYPE (TYPE_EDGE), POINTER        :: EDGE_NEW(:)  
  TYPE (TYPE_LIMITER)              :: LIMITER
  TYPE (TYPE_PARAM)                :: code_parameters_core_edge
#endif

  TYPE (TYPE_COREPROF),    SAVE, POINTER :: COREPROF_DB(:) => NULL()
  TYPE (TYPE_CORETRANSP),  SAVE, POINTER :: CORETRANSP_DB(:) => NULL()
  TYPE (TYPE_CORESOURCE),  SAVE, POINTER :: CORESOURCE_DB(:) => NULL()
  TYPE (TYPE_COREIMPUR),   SAVE, POINTER :: COREIMPUR_DB(:) => NULL()
  TYPE (TYPE_CORENEUTRALS),SAVE, POINTER :: CORENEUTRALS_DB(:) => NULL()
  TYPE (TYPE_EQUILIBRIUM), SAVE, POINTER :: EQUILIBRIUM_DB(:) => NULL()
  TYPE (TYPE_TOROIDFIELD), SAVE, POINTER :: TOROIDFIELD_DB(:) => NULL()
  TYPE (TYPE_NEOCLASSIC),  SAVE, POINTER :: NEOCLASSIC_DB(:) => NULL()

  TYPE (TYPE_CORETRANSP),  SAVE, POINTER :: CORETRANSP1(:) => NULL(),CORETRANSP2(:) => NULL(),CORETRANSP3(:) => NULL(),CORETRANSP4(:) => NULL(),CORETRANSP5(:) => NULL()
  TYPE (TYPE_CORESOURCE),  SAVE, POINTER :: CORESOURCE1(:) => NULL(),CORESOURCE2(:) => NULL(),CORESOURCE3(:) => NULL(),CORESOURCE4(:) => NULL(),CORESOURCE5(:) => NULL(),CORESOURCE6(:) => NULL(),CORESOURCE7(:) => NULL()

  TYPE (TYPE_PARAM)                :: code_parameters_ets, code_parameters_transport_combiner,   &
                                      code_parameters_sources_combiner, code_parameters_ets_workflow,  &
                                      code_parameters_gausian_sources

  REAL(R8)                         :: RHOB                !effective minor radius

  INTEGER                          :: NPSI                !number of equilibrium points (input)
  INTEGER                          :: NEQ_DIM1            !number of equilibrium points (input)
  INTEGER                          :: NEQ_DIM2            !number of equilibrium points (input)
  INTEGER                          :: MAX_NPOINTS         !number of types for each neutral
  INTEGER                          :: NEQ_MAX_NPOINTS      

  INTEGER                          :: NRHO                !number of radial points      (input)
  INTEGER                          :: NNUCL               !number of neutrals species    (input)
  INTEGER                          :: NION                !number of ion species        (input)
  INTEGER                          :: NIMP                !number of impurity species   (input)
  INTEGER,       ALLOCATABLE, SAVE :: NZIMP(:)            !number of charge states for each impurity (input)

  INTEGER                          :: NNEUT               !number of neutrals species                (input)
  INTEGER,       ALLOCATABLE, SAVE :: NCOMP(:)            !number of components for each neutral
  INTEGER,       ALLOCATABLE, SAVE :: NTYPE(:)            !number of types for each neutral

  INTEGER                          :: NTIME               !number of time points        (input)
  INTEGER,               PARAMETER :: NSLICE = 1           !number of CPO ocurancies in the work flow

  INTEGER                          :: IRHO                !current radial knot
  INTEGER                          :: IION                !current ion type
  INTEGER                          :: ITIME               !current time step

  INTEGER                          :: NSOL                !Number of analytical example
  INTEGER                          :: SOLVER_TYPE         !representation of transport equations 
  INTEGER                          :: SIGMA_SOURCE                               !origin of Plasma electrical conductivity

  REAL(R8)                         :: CONVREC             !required convergency 
  REAL(R8)                         :: CONV_NEUT, CONV_IMP
  REAL(R8)                         :: TIME                !time
  REAL(R8)                         :: START_TIME               !starting time
  REAL(R8)                         :: TIME_END            !end time
  REAL(R8)                         :: TAU                 !time step
  REAL(R8)                         :: AMIX                !mixing factor
  INTEGER                          :: ITER                !iteration index
  INTEGER,               PARAMETER :: MAXITER=1000        !maximum number of convergence iterations

  REAL (R8),     ALLOCATABLE, SAVE :: AMN(:)
  REAL (R8),     ALLOCATABLE, SAVE :: ZN(:)
  REAL (R8),     ALLOCATABLE, SAVE :: ZION(:)
  REAL (R8),     ALLOCATABLE, SAVE :: AMN_IMP(:)
  REAL (R8),     ALLOCATABLE, SAVE :: ZN_IMP(:)
  REAL (R8),     ALLOCATABLE, SAVE :: MAX_Z_IMP(:)

  INTEGER                          :: COLD_NEUTRALS
  INTEGER                          :: THERMAL_NEUTRALS
  INTEGER                          :: FAST_NEUTRALS
  INTEGER                          :: NBI_NEUTRALS
          
  INTEGER                          :: PSI_BND_TYPE
  INTEGER                          :: TE_BND_TYPE
  INTEGER                          :: NE_BND_TYPE
  INTEGER,       ALLOCATABLE, SAVE :: NI_BND_TYPE(:)
  INTEGER,       ALLOCATABLE, SAVE :: TI_BND_TYPE(:)
  INTEGER,       ALLOCATABLE, SAVE :: VTOR_BND_TYPE(:)
  INTEGER,       ALLOCATABLE, SAVE :: NIMP_BND_TYPE(:,:)
  INTEGER,       ALLOCATABLE, SAVE :: N0_BND_TYPE(:,:)
  INTEGER,       ALLOCATABLE, SAVE :: T0_BND_TYPE(:,:)

  REAL (R8)                        :: PSI_BND_VALUE(3)
  REAL (R8)                        :: TE_BND_VALUE(3)
  REAL (R8)                        :: NE_BND_VALUE(3)
  REAL (R8),     ALLOCATABLE, SAVE :: NI_BND_VALUE(:,:)
  REAL (R8),     ALLOCATABLE, SAVE :: TI_BND_VALUE(:,:)
  REAL (R8),     ALLOCATABLE, SAVE :: VTOR_BND_VALUE(:,:)

  REAL (R8),     ALLOCATABLE, SAVE :: NIMP_BND_VALUE(:,:,:)

  REAL (R8),     ALLOCATABLE, SAVE :: N0_BND_VALUE(:,:,:)
  REAL (R8),     ALLOCATABLE, SAVE :: T0_BND_VALUE(:,:,:)


  REAL (R8)                        :: IP
  REAL (R8)                        :: GEO_AX(3)
  REAL (R8)                        :: PLASMA_AX(3)
  REAL (R8)                        :: AMIN
  REAL (R8)                        :: ELONG, ELONG_UP, ELONG_LOW
  REAL (R8)                        :: TRIA_UP
  REAL (R8)                        :: TRIA_LOW

  INTEGER                          :: CONTROL_INTEGER(3)  !integer control parameters
  REAL      (R8)                   :: CONTROL_DOUBLE(6)   !real control parameters

!irena
  REAL (R8)                        :: control_double_imp(4) !real control parameters for impurity
  INTEGER                          :: iimp,ISIMP,SIMP
!Irena

  INTEGER                          :: SHOT_IN, RUN_IN     !shot and run numbers
  INTEGER                          :: INTERPOL            !interpolation index
  INTEGER                          :: TIME_DEP_INPUT      !if 1, time dependence in input data
  INTEGER                          :: EXT_EQUIL           !0: none, 1: BDSEQ, 2: EMEQ, 3: HELENA
  INTEGER                          :: EQUIL_MOD           !if not zero, equilibrium will be called whent ITIME mod  EQUIL_MOD == 0
  LOGICAL                          :: do_equil
  INTEGER                          :: EXT_SOURCE          !if 2, call combine_source
  INTEGER                          :: EXT_TRANSPORT       !if 1, call etaigb and neowes; if 2, call combine_transport
  INTEGER                          :: SHOT_OUT, RUN_OUT   !shot and run numbers
  INTEGER                          :: ITIME_OUT           !UAL output time step number 
  INTEGER                          :: ITER_INC            !ITERATION limit to cause increase in time-step     
  INTEGER                          :: ITER_DEC            !ITERATION limit to cause decrease in time-step     
  INTEGER                          :: IDX                 !handle for UAL output
  INTEGER                          :: IDX2

  INTEGER                          :: exp_option          !0 means ignore
  INTEGER                          :: exp_ncols
  INTEGER                          :: PROF_FLAG           !Flag for primary current quantity: 1-PSI, 2-Q, 3-JPAR
  INTEGER                          :: J0_FLAG             !Flag for negative current density: 0-allowed, >0-cut off
  INTEGER                          :: Q0_FLAG             !Flag for positive dq/drho: 0-allowed, >0-cut off
  INTEGER                          :: EQ_SOURCE           !Flag for initial equilibrium: 0-from input CPO, 1-define from XML parameters
  INTEGER                          :: QUASI_NEUT          !Quasi neutrality:0-electrons; 1-ions from BC; 2-ions fron charge
  INTEGER                          :: ICORONAL            !Coronal flag: "0"-OFF; "1" - replace boundary conditions by coronal; "2" - replace boundary conditions and profiles by coronal 


  REAL      (R8)                   :: ADD_TRANSPORT       !additional diffusive transport
  REAL      (R8)                   :: TAU_OUT             !time step for profiles output into the data base
  REAL      (R8)                   :: TIME_OUT            !time for profiles output into the data base
  REAL      (R8)                   :: TAU_INC             !time step increment factor if ITERATIONS < ITER_INC
  REAL      (R8)                   :: TAU_DEC             !time step decrement factor if ITERATIONS > ITER_DEC
  REAL      (R8)                   :: TAU_MIN             !minimum time step
  REAL      (R8)                   :: TAU_MAX             !maximim time step
  INTEGER, PARAMETER               :: BUFLEN = 256
  CHARACTER(len=BUFLEN)            :: RHO_F
  REAL(R8),            ALLOCATABLE :: rho_1(:), rho_2(:), rho_3(:), rho_4(:), rho_5(:)
  REAL(R8)                         :: dummy1, dummy2, x
  REAL      (R8),      ALLOCATABLE :: RHO(:)              !rho grid
  REAL      (R8)                   :: R_in, R_out, R_geo
  REAL      (R8)                   :: rho_tor_rescale

  REAL      (R8)                   :: bc_ip_jrlx, bc_ip_wanted, bc_ip_current, bc_ip_tau

  REAL      (R8),          POINTER :: evolution_data(:,:) => NULL()
  CHARACTER (len=32),      POINTER :: evolution_labels(:) => NULL()
  CHARACTER (len=32)               :: db_in, db_out
  INTEGER                          :: augment_equil

  INTEGER (ITM_I8)                 :: total_size = 0

  INTEGER                          :: i

  CHARACTER (len=256)              :: filename
  LOGICAL, SAVE                    :: use_euitm_get, use_euitm_put

  LOGICAL                          :: quitexist

  INTERFACE
     SUBROUTINE EXTERNAL_TRANSPORT(EQUILIBRIUM_ITER,COREPROF_ITER,CORETRANSP_ITER,add_transport)
       USE EUITM_SCHEMAS
       USE ITM_TYPES
       TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_ITER(:)
       TYPE (TYPE_COREPROF),    POINTER :: COREPROF_ITER(:)  
       TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_ITER(:)
       REAL (R8)                        :: add_transport
    END SUBROUTINE EXTERNAL_TRANSPORT
#ifdef CORE_EDGE
    SUBROUTINE b2mn_ets(coreprof_in, coreimpur_in, coreneutrals_in,  &
         coreprof_out, coreimpur_out, coreneutrals_out, edge_out,  &
         codeparam, end_set)
      USE euitm_schemas    ! IGNORE
      IMPLICIT NONE
      TYPE (type_coreprof), POINTER :: coreprof_in(:), coreprof_out(:)
      TYPE (type_coreimpur), POINTER :: coreimpur_in(:), coreimpur_out(:)
      TYPE (type_coreneutrals), POINTER :: coreneutrals_in(:), coreneutrals_out(:)
      TYPE (type_edge), POINTER :: edge_out(:)
      TYPE (type_param) :: codeparam
      LOGICAL, OPTIONAL :: end_set
    END SUBROUTINE b2mn_ets
#endif
  END INTERFACE


!----------------------------------------------------------------------!
!----------------------------------------------------------------------!
!----------------------------------------------------------------------!



!----------------------------------------------------------------------!
!     Connect xml files                                                !
!----------------------------------------------------------------------!
  CALL FILL_PARAM (code_parameters_ets,    'XML/ets.xml',    '', 'XML/ets.xsd')
  CALL FILL_PARAM (code_parameters_ets_workflow, 'XML/ets_workflow.xml', '', 'XML/ets_workflow.xsd')
#ifdef CORE_EDGE
  CALL FILL_PARAM (code_parameters_core_edge, 'XML/core-edge.xml', '', 'XML/core-edge.xsd')
#endif

!----------------------------------------------------------------------!
!     Read run parameters from xml file                                !
!----------------------------------------------------------------------!
  CALL PROCESS_XML(                                            &
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
       PSI_BND_TYPE,  NE_BND_TYPE,     NI_BND_TYPE,     TI_BND_TYPE,            &
       TE_BND_TYPE,   VTOR_BND_TYPE,   NIMP_BND_TYPE,          &
       N0_BND_TYPE,   T0_BND_TYPE,                             &
!
       PSI_BND_VALUE, NE_BND_VALUE,    NI_BND_VALUE,    TI_BND_VALUE,           &
       TE_BND_VALUE,  VTOR_BND_VALUE,  NIMP_BND_VALUE,         &
       N0_BND_VALUE,  T0_BND_VALUE,                            &
!
       SHOT_IN, RUN_IN, INTERPOL, DB_IN,                       &
       SHOT_OUT, RUN_OUT, TAU_OUT, DB_OUT,                     &
       SOLVER_TYPE, SIGMA_SOURCE, TAU, AMIX, CONVREC,          &
       START_TIME,                                             &
!
       IP, GEO_AX, PLASMA_AX, AMIN, ELONG, TRIA_UP, TRIA_LOW,  &
!
       PROF_FLAG, J0_FLAG, Q0_FLAG,  EQ_SOURCE,                &
       time_dep_input, ext_equil, equil_mod,                   &
       ext_source, ext_transport, add_transport, QUASI_NEUT,   &
       TAU_INC, TAU_DEC, ITER_INC, ITER_DEC,                   &
       TAU_MIN, TAU_MAX,                                       &
       exp_option, exp_ncols,                                  &
       evolution_labels, evolution_data,                       &
       augment_equil, rho_f, icoronal,                         &
       CODE_PARAMETERS_ETS_WORKFLOW)

    ELONG_UP  = ELONG
    ELONG_LOW = ELONG

    WRITE(*,*) ' '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '++++   INPUT FROM XML FILE IS RECEIVED    ++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '



    IF(eq_source.EQ.1) THEN
!----------------------------------------------------------------------!
!     Calculate rho boundary                                           !
!----------------------------------------------------------------------!
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_OLD)) ALLOCATE (EQUILIBRIUM_OLD(1))
       IF(.NOT.ASSOCIATED(EQUILIBRIUM_NEW)) ALLOCATE (EQUILIBRIUM_NEW(1))
       EQUILIBRIUM_OLD(1)%time         =  START_TIME
       CALL GEOMETRY_FROM_WF_PARAMETERS(EQUILIBRIUM_OLD, EQUILIBRIUM_NEW,  &
                                        GEO_AX, PLASMA_AX, PLASMA_AX,      &
                                        IP, AMIN,                          &
                                        ELONG_UP, ELONG_LOW,               &
                                        TRIA_UP, TRIA_LOW,                 &
                                        NPSI, NEQ_DIM1, NEQ_DIM2, MAX_NPOINTS) 

       CALL DEALLOCATE_CPO(EQUILIBRIUM_OLD)

       WRITE(*,*) ' '
       WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
       WRITE(*,*) '++++  INITIAL RHO_BOUNDARY IS CALCULATED  ++++ '
       WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '

    ENDIF


!----------------------------------------------------------------------!
!     Output run settings to the screen                                !
!----------------------------------------------------------------------!
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) 'Input from ',SHOT_IN,  RUN_IN
    WRITE(*,*) 'Output to ', SHOT_OUT, RUN_OUT
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '+++++++++++++  COMPOSITIONS  +++++++++++++++++ '
    WRITE(*,*) 'nrho =',nrho
    WRITE(*,*) 'npsi =',npsi
    WRITE(*,*) 'ndim1=',neq_dim1
    WRITE(*,*) 'ndim2=',neq_dim2
    WRITE(*,*) 'max_npoints=',max_npoints
    WRITE(*,*) 'nnucl=',nnucl
    WRITE(*,*) 'nion= ',nion
    WRITE(*,*) 'nimp= ',nimp
    IF(ALLOCATED(nzimp)) WRITE(*,*) 'nzimp=', nzimp
    WRITE(*,*) 'nneut=',nneut
    IF(ALLOCATED(ncomp)) WRITE(*,*) 'ncomp=',ncomp
    IF(ALLOCATED(ntype)) WRITE(*,*) 'ntype=', ntype
    IF(ALLOCATED(amn)) WRITE(*,'(a,(100f8.2))') 'amn=         ', amn
    IF(ALLOCATED(zn)) WRITE(*,'(a,(100f8.2))') 'zn=          ',zn
    IF(ALLOCATED(zion)) WRITE(*,'(a,(100f8.2))') 'zion=        ', zion
    IF(ALLOCATED(amn_imp)) WRITE(*,'(a,(100f8.2))') 'amn_imp=     ', amn_imp
    IF(ALLOCATED(zn_imp)) WRITE(*,'(a,(100f8.2))') 'zn_imp=      ',zn_imp
    IF(ALLOCATED(max_z_imp)) WRITE(*,'(a,(100f8.2))') 'max_z_imp=   ', max_z_imp
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '++++++++++  BOUNDARY CONDITIONS  +++++++++++++ '
    WRITE(*,*) '++++++++++     TYPE   VALUE      +++++++++++++ '
    WRITE(*,*) 'PSI =', PSI_BND_TYPE,  PSI_BND_VALUE(1)
    WRITE(*,*) 'NE  =', NE_BND_TYPE,   NE_BND_VALUE(1)
    WRITE(*,*) 'NI  =', NI_BND_TYPE,   NI_BND_VALUE(1,:)
    WRITE(*,*) 'TI  =', TI_BND_TYPE,   TI_BND_VALUE(1,:)
    WRITE(*,*) 'TE  =', TE_BND_TYPE,   TE_BND_VALUE(1)
    WRITE(*,*) 'VTOR=', VTOR_BND_TYPE, VTOR_BND_VALUE(1,:)
    IF(nimp.GT.0) WRITE(*,*) 'NIMP=', NIMP_BND_TYPE, NIMP_BND_VALUE(:,1,:)
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '




  !----------------------------------------------------------------------!
  !     Connect other xml files if necessary                             !
  !----------------------------------------------------------------------!
  IF (ext_transport.EQ.2)                                     &
       CALL FILL_PARAM  (code_parameters_transport_combiner,  &
       'XML/transport_combiner.xml', '', 'XML/transport_combiner.xsd')

  IF (ext_source.EQ.2)                                        &
       CALL FILL_PARAM  (code_parameters_sources_combiner,    &
       'XML/source_combiner.xml', '', 'XML/source_combiner.xsd')

  IF (ext_source.EQ.2)                                        &
       CALL FILL_PARAM (code_parameters_gausian_sources,      &
       'XML/source_dummy.xml',  '', 'XML/source_dummy.xsd')





  !----------------------------------------------------------------------!
  !     Allocate run composition                                         !
  !----------------------------------------------------------------------!
    CALL SET_PLASMA_COMPOSITION (COREPROF_NEW,                          &
                                 NION,      NIMP,      NNEUT,           &
                                 AMN,       ZN,        ZION,            &
                                 AMN_IMP,   ZN_IMP,    MAX_Z_IMP,       &
                                 NCOMP,     NTYPE,                      &
                                 COLD_NEUTRALS,        THERMAL_NEUTRALS,&
                                 FAST_NEUTRALS,        NBI_NEUTRALS) 

    DEALLOCATE(AMN, ZN, ZION)
    IF(nimp.GT.0) DEALLOCATE(AMN_IMP, ZN_IMP, MAX_Z_IMP)

    ets_species%nnucl     = nnucl   
    ets_species%nion      = nion
    ets_species%nimp      = nimp
    IF(ALLOCATED(nzimp)) THEN
       ALLOCATE(ets_species%nzimp(SIZE(nzimp)))
       ets_species%nzimp  = nzimp
    ENDIF
    ets_species%nneut     = nneut
    IF(ALLOCATED(ncomp)) THEN
       ALLOCATE(ets_species%ncomp(SIZE(ncomp)))
       ets_species%ncomp  = ncomp
    ENDIF
    IF(ALLOCATED(ntype)) THEN
       ALLOCATE(ets_species%ntype(SIZE(ntype)))
       ets_species%ntype  = ntype
    ENDIF
    WRITE(*,*) ' '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '+++++++   COMPOSITIONS ARE SET UP    +++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '




  ALLOCATE                      (RHO(NRHO))


  CONTROL_INTEGER(1)           = SOLVER_TYPE              !number of numerical solver
  CONTROL_INTEGER(2)           = SIGMA_SOURCE             !number of numerical solver
  CONTROL_INTEGER(3)           = QUASI_NEUT               !
  CONTROL_DOUBLE(1)            = TAU                      !time step
  CONTROL_DOUBLE(2)            = AMIX                     !mixing factor for profiles
  CONTROL_DOUBLE(3)            = AMIX**0.5                !mixing factor for transport coefficients
  CONTROL_DOUBLE(4)            = 1.e0_R8                  !actual convergence
  CONTROL_DOUBLE(5)            = CONVREC                  !required convergence

  CONTROL_DOUBLE_IMP(1)        = 3.                       !number of numerical solver: 1-solver1;2-solver2; 3-solver3
  CONTROL_DOUBLE_IMP(2)        = 1.                       !number of numerical solver_type: 
  CONTROL_DOUBLE_IMP(3)        = TAU                      !time step
  CONTROL_DOUBLE_IMP(4)        = amix                     !mixing factor for profiles
  NNUCL                        = NION + NIMP              !approximation

  ! >>> At this stage we have CONTROL_INTEGER and CONTROL_DOUBLE
  !     filled with parameters from XML





  !----------------------------------------------------------------------!
  !     Generate the RHO grid for ETS, allocate working CPOs             !
  !----------------------------------------------------------------------!
  CALL ETSSTART                                                             &
!PARAMETERS & CPOs_IN:
                      (SOLVER_TYPE,      EQUILIBRIUM_NEW,  COREPROF_NEW,    &
!CPOs_OUT:
                       COREPROF_OLD,     CORETRANSP_OLD,   CORESOURCE_OLD,  &
                       COREIMPUR_OLD,    CORENEUTRALS_OLD, NEOCLASSIC_OLD,  &
                       EQUILIBRIUM_OLD,  TOROIDFIELD_OLD,                   &
!BOUNDARY_CONDITIONS:
                       PSI_BND_TYPE,   NE_BND_TYPE,     NI_BND_TYPE,        &
                       TI_BND_TYPE,    TE_BND_TYPE,     VTOR_BND_TYPE,      &
                       NIMP_BND_TYPE,  N0_BND_TYPE,     T0_BND_TYPE,        &
!
                       PSI_BND_VALUE,  NE_BND_VALUE,    NI_BND_VALUE,       &
                       TI_BND_VALUE,   TE_BND_VALUE,    VTOR_BND_VALUE,     &
                       NIMP_BND_VALUE, N0_BND_VALUE,    T0_BND_VALUE,       &
!SPACE_RESOLUTION:
                       NRHO, NPSI, NEQ_DIM1, NEQ_DIM2,  MAX_NPOINTS) 

  CALL DEALLOCATE_CPO(EQUILIBRIUM_NEW)
  CALL DEALLOCATE_CPO(NEOCLASSIC_OLD)

  CALL COPY_CPO             (COREPROF_OLD,      COREPROF_ITER)         
  CALL COPY_CPO             (EQUILIBRIUM_OLD,   EQUILIBRIUM_NEW)         
  CALL COPY_CPO             (CORETRANSP_OLD,    CORETRANSP_ITER)         
  CALL COPY_CPO             (CORESOURCE_OLD,    CORESOURCE_ITER)         
  IF(NIMP.GT.0) THEN
     CALL COPY_CPO             (COREIMPUR_OLD,     COREIMPUR_ITER)         
     CALL COPY_CPO             (COREIMPUR_OLD,     COREIMPUR_NEW) 
  ENDIF
  IF(NNEUT.GT.0) THEN
     CALL COPY_CPO             (CORENEUTRALS_OLD,  CORENEUTRALS_ITER)         
     CALL COPY_CPO             (CORENEUTRALS_OLD,  CORENEUTRALS_NEW)         
  ENDIF
 
    WRITE(*,*) ' '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '+++++++  WORKING CPOs ARE ALLOCATED  +++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '

    WRITE(*,*) ' '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '+++++++       GRIDS ARE SET UP       +++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '





  !----------------------------------------------------------------------!
  !     Starting time                                                    !
  !----------------------------------------------------------------------!
  ITER     = 0
  TIME     = START_TIME
  TIME_OUT = TIME
  TIME_END = START_TIME + NTIME * TAU




  !----------------------------------------------------------------------!
  !     Code parameters:                                                 !
  !----------------------------------------------------------------------!
  ALLOCATE    (coreprof_old(1)%codeparam%codename(1))
  ALLOCATE    (coreprof_old(1)%codeparam%codeversion(1))
  ALLOCATE    (coreprof_old(1)%codeparam%parameters(SIZE(code_parameters_ets_workflow%parameters)))

  coreprof_old(1)%codeparam%codename(1)       =  'ets_workflow_fortran'
  coreprof_old(1)%codeparam%codeversion(1)    =  version
  coreprof_old(1)%codeparam%parameters        =  code_parameters_ets_workflow%parameters

  CALL DEALLOCATE_CPO(COREPROF_ITER(1)%codeparam)
  CALL COPY_CPO (COREPROF_OLD(1)%codeparam,      COREPROF_ITER(1)%codeparam)





  !----------------------------------------------------------------------!
  !     Upload starting plasms profiles, transport coeffisients, sources !
  !     and equilibrium from the CPO saved to the data base              !
  !----------------------------------------------------------------------!

  ALLOCATE                      (COREPROF_DB(1))
  ALLOCATE                      (CORETRANSP_DB(1))
  ALLOCATE                      (CORESOURCE_DB(1))
  IF(NIMP.GT.0) ALLOCATE        (COREIMPUR_DB(1))
  IF(NNEUT.GT.0) ALLOCATE       (CORENEUTRALS_DB(1))
  ALLOCATE                      (EQUILIBRIUM_DB(1))
  ALLOCATE                      (TOROIDFIELD_DB(1))
  ALLOCATE                      (NEOCLASSIC_DB(1))

#ifdef UAL

  !----------------------------------------------------------------------!
  !     Output settings: shot, run, idx                                  !
  !----------------------------------------------------------------------!
    IF  (SHOT_IN.GT.0.AND.RUN_IN.GE.0) THEN
       SELECT CASE (db_in)
       CASE ("mdsplus")
          WRITE(*,*) 'Opening mdsplus database for ', SHOT_IN, RUN_IN
          CALL EUITM_OPEN('euitm',SHOT_IN, RUN_IN, idx)
          use_euitm_get = .TRUE.
       CASE ("hdf5")
          WRITE(*,*) 'Opening hdf5 database for ', SHOT_IN, RUN_IN
          CALL EUITM_OPEN_HDF5('euitm',SHOT_IN, RUN_IN, idx)
          use_euitm_get = .TRUE.
       CASE ("ascii")
          use_euitm_get = .FALSE.
       CASE default
          WRITE(*,*) 'Unexpected database format choice : ',TRIM(db_in)
          STOP 'Error: unrecognized database format'
       END SELECT
    END IF
#else
    use_euitm_get = .FALSE.
#endif

#ifdef UAL
    IF(use_euitm_get) THEN
       WRITE(*,*) 'reading from the database for time = ', time
       CALL EUITM_GET_SLICE          (IDX, 'coreprof',     COREPROF_DB(1),     TIME, INTERPOL)
       CALL EUITM_GET_SLICE          (IDX, 'coretransp',   CORETRANSP_DB(1),   TIME, INTERPOL)
       CALL EUITM_GET_SLICE          (IDX, 'coresource',   CORESOURCE_DB(1),   TIME, INTERPOL)
       IF(NIMP.GT.0) CALL EUITM_GET_SLICE          (IDX, 'coreimpur',    COREIMPUR_DB(1),    TIME, INTERPOL)
       IF(NNEUT.GT.0) CALL EUITM_GET_SLICE          (IDX, 'coreneutrals', CORENEUTRALS_DB(1), TIME, INTERPOL)
       CALL EUITM_GET_SLICE          (IDX, 'equilibrium',  EQUILIBRIUM_DB(1),  TIME, INTERPOL)
       CALL EUITM_GET_SLICE          (IDX, 'toroidfield',  TOROIDFIELD_DB(1),  TIME, INTERPOL)
       CALL EUITM_GET_SLICE          (IDX, 'neoclassic',   NEOCLASSIC_DB(1),   TIME, INTERPOL)
       CALL EUITM_CLOSE              (IDX)
    ENDIF
#endif

  IF(DB_in .EQ. "ascii") THEN
     WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreprof', shot_in, run_in, 0
     CALL open_read_file(1, TRIM(filename))
     CALL read_cpo(coreprof_db(1), 'coreprof')
     CALL close_read_file
     WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coretransp', shot_in, run_in, 0
     CALL open_read_file(1, TRIM(filename))
     CALL read_cpo(coretransp_db(1), 'coretransp')
     CALL close_read_file
     WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coresource', shot_in, run_in, 0
     CALL open_read_file(1, TRIM(filename))
     CALL read_cpo(coresource_db(1), 'coresource')
     CALL close_read_file
     IF(nimp.GT.0) THEN
        WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreimpur', shot_in, run_in, 0
        CALL open_read_file(1, TRIM(filename))
        CALL read_cpo(coreimpur_db(1), 'coreimpur')
        CALL close_read_file
     ENDIF
     IF(nneut.GT.0) THEN
        WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreneutrals', shot_in, run_in, 0
        CALL open_read_file(1, TRIM(filename))
        CALL read_cpo(coreneutrals_db(1), 'coreneutrals')
        CALL close_read_file
     ENDIF
     WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'equilibrium', shot_in, run_in, 0
     CALL open_read_file(1, TRIM(filename))
     CALL read_cpo(equilibrium_db(1), 'equilibrium')
     CALL close_read_file
     WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'toroidfield', shot_in, run_in, 0
     CALL open_read_file(1, TRIM(filename))
     CALL read_cpo(toroidfield_db(1), 'toroidfield')
     CALL close_read_file
!     write(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'neoclassic', shot_in, run_in, 0
!     call open_read_file(1, trim(filename))
!     call read_cpo(neoclassic_db(1), 'neoclassic')
!     call close_read_file
  ENDIF

  CALL DEALLOCATE_CPO           (COREPROF_ITER)
  CALL DEALLOCATE_CPO           (CORETRANSP_ITER)
  CALL DEALLOCATE_CPO           (CORESOURCE_ITER)

  WRITE(*,*) ' '
  WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
  CALL FILLCOREPROF             (COREPROF_DB,     COREPROF_OLD,      COREPROF_ITER,     INTERPOL)
  WRITE(*,*) '+++++++     COREPROF IS RECEIVED     +++++++++ '
  CALL FILLCORETRANSP           (CORETRANSP_DB,   CORETRANSP_OLD,    CORETRANSP_ITER,   INTERPOL) 
  WRITE(*,*) '+++++++     CORETRANSP IS RECEIVED   +++++++++ '
  CALL FILLCORESOURCE           (CORESOURCE_DB,   CORESOURCE_OLD,    CORESOURCE_ITER,   INTERPOL) 
  WRITE(*,*) '+++++++     CORESOURCE IS RECEIVED   +++++++++ '
  IF(nimp .GT. 0) THEN
     CALL DEALLOCATE_CPO        (COREIMPUR_ITER)
     CALL FILLCOREIMPUR         (COREIMPUR_DB,    COREIMPUR_OLD,     COREIMPUR_ITER,    INTERPOL)
     WRITE(*,*) '+++++++     COREIMPUR IS RECEIVED    +++++++++ '
  ENDIF
  IF(NNEUT > 0) THEN
     CALL DEALLOCATE_CPO        (CORENEUTRALS_ITER)
     CALL FILLCORENEUTRALS      (CORENEUTRALS_DB, CORENEUTRALS_OLD,  CORENEUTRALS_ITER, INTERPOL)
     WRITE(*,*) '+++++++     CORENEUTRALS IS RECEIVED +++++++++ '
  ENDIF
  CALL DEALLOCATE_CPO           (EQUILIBRIUM_ITER)
  CALL FILLEQUILIBRIUM          (EQUILIBRIUM_DB,  EQUILIBRIUM_OLD,   EQUILIBRIUM_ITER,  INTERPOL)

! added by DPC, 2013-05-15, so that the coreprof toroid_field is consistent with that from equilibrium
  COREPROF_ITER%toroid_field%b0 = EQUILIBRIUM_ITER%global_param%toroid_field%b0
  COREPROF_ITER%toroid_field%r0 = EQUILIBRIUM_ITER%global_param%toroid_field%r0

  WRITE(*,*) '+++++++     EQUILIBRIUM  IS RECEIVED +++++++++ '
  WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
  WRITE(*,*) ' '

  CALL DEALLOCATE_CPO(EQUILIBRIUM_OLD)
  CALL COPY_CPO             (EQUILIBRIUM_ITER,   EQUILIBRIUM_OLD)
  CALL DEALLOCATE_CPO(COREPROF_OLD)
  CALL COPY_CPO             (COREPROF_ITER,      COREPROF_OLD)         
  CALL DEALLOCATE_CPO(CORETRANSP_OLD)
!  CALL COPY_CPO             (CORETRANSP_ITER,    CORETRANSP_OLD)         
  CALL DEALLOCATE_CPO(CORESOURCE_OLD)
!  CALL COPY_CPO             (CORESOURCE_ITER,    CORESOURCE_OLD)         
  IF(nimp  .GT. 0) THEN
     CALL DEALLOCATE_CPO(COREIMPUR_OLD)
     IF(icoronal.GT.0) THEN
        WRITE(*,*) 'Calling set_coronal with option ', ICORONAL
        CALL SET_CORONAL(COREIMPUR_ITER, COREPROF_ITER, COREIMPUR_OLD, INTERPOL, ICORONAL)
        CALL DEALLOCATE_CPO(COREIMPUR_ITER)
        CALL COPY_CPO          (COREIMPUR_OLD,     COREIMPUR_ITER)
     ELSE
        CALL COPY_CPO          (COREIMPUR_ITER,     COREIMPUR_OLD)         
     ENDIF
  ENDIF
  IF(NNEUT .GT. 0) THEN
     CALL DEALLOCATE_CPO	(CORENEUTRALS_OLD)
     CALL COPY_CPO          (CORENEUTRALS_ITER,  CORENEUTRALS_OLD)         
  ENDIF

    WRITE(*,*) ' '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '+++++++   CPOs INPUT DATA ARE READ   +++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '



    ! >>> At this stage we have COREPROF_OLD, COREPROF_NEW and EQUILIBRIUM_OLD  
    !     filled with equidistant ETS rho-grid and profiles read from ITM data base

!----------------------------------------------------------------------!
!     make sure we start with a valid equilibrium                      !
!     this also has the advantage of making sure that the same data    ! 
!     is written for slice 0 as for the subsequent slices              !
!----------------------------------------------------------------------!
  IF (NPSI.GT.0) THEN


!----------------------------------------------------------------------!
!    Provide consistent input for equilibrium:                         !
!----------------------------------------------------------------------!
     IF(prof_flag.GT.0) THEN
  !----------------------------------------------------------------------!
  !     Check consistency between current profiles and equilibrium       !
  !----------------------------------------------------------------------!

        CALL START_PROFILES_CONSISTENCY                                 &
             (PROF_FLAG, J0_FLAG, Q0_FLAG, EXT_EQUIL,                   &
             COREPROF_OLD, EQUILIBRIUM_OLD, TOROIDFIELD_OLD, COREPROF_ITER, EQUILIBRIUM_ITER) 
        CALL DEALLOCATE_CPO      (EQUILIBRIUM_OLD)
        CALL copy_cpo (EQUILIBRIUM_ITER, EQUILIBRIUM_OLD)
     ELSE
        IF(exp_option.NE.0) THEN
           ALLOCATE (NEOCLASSIC_ITER(1))
           CALL CHANGERADII        (EQUILIBRIUM_ITER,     &
                                    COREPROF_ITER,        &
                                    CORETRANSP_ITER,      &
                                    CORESOURCE_ITER,      &
                                    COREIMPUR_ITER,       &
                                    CORENEUTRALS_ITER,    & 
                                    NEOCLASSIC_ITER)
           CALL DEALLOCATE_CPO     (NEOCLASSIC_ITER)
        ENDIF
        CALL DEALLOCATE_CPO      (EQUILIBRIUM_OLD)
        CALL EQUIL_INPUT          (COREPROF_ITER, TOROIDFIELD_OLD, EQUILIBRIUM_ITER, EQUILIBRIUM_OLD)
     END IF
     IF      (ext_equil.EQ.1) THEN 
        CALL DEALLOCATE_CPO      (EQUILIBRIUM_ITER)
        CALL BDSEQ_WRAPPER    (EQUILIBRIUM_OLD, EQUILIBRIUM_ITER)

     ELSE IF (ext_equil.EQ.2) THEN
        CALL DEALLOCATE_CPO      (EQUILIBRIUM_ITER)
        CALL EMEQ_E3M_WRAPPER (EQUILIBRIUM_OLD, EQUILIBRIUM_ITER)
         
     ELSE IF (ext_equil.EQ.3) THEN
#ifdef GOT_HELENA
        CALL DEALLOCATE_CPO     (EQUILIBRIUM_ITER)
        CALL HELENA_WRAPPER     (EQUILIBRIUM_OLD, EQUILIBRIUM_ITER)
#else
        WRITE(*,*) 'No HELENA available at compile time'
        STOP 'No HELENA'
#endif
     ELSE IF (ext_equil.EQ.4) THEN
#ifdef GOT_CHEASE
        CALL DEALLOCATE_CPO     (EQUILIBRIUM_ITER)
        CALL CHEASE_WRAPPER     (EQUILIBRIUM_OLD, EQUILIBRIUM_ITER)
#else
        WRITE(*,*) 'No CHEASE available at compile time'
        STOP 'No CHEASE'
#endif
     ELSE
     END IF




     IF(exp_option.NE.0) THEN
        ALLOCATE (NEOCLASSIC_ITER(1))
        CALL CHANGERADII        (EQUILIBRIUM_ITER,     &
                                 COREPROF_ITER,        &
                                 CORETRANSP_ITER,      &
                                 CORESOURCE_ITER,      &
                                 COREIMPUR_ITER,       &
                                 CORENEUTRALS_ITER,    & 
                                 NEOCLASSIC_ITER)
        CALL DEALLOCATE_CPO     (NEOCLASSIC_ITER)
     ENDIF
     CALL DEALLOCATE_CPO        (EQUILIBRIUM_OLD)
     CALL COPY_CPO              (EQUILIBRIUM_ITER, EQUILIBRIUM_OLD)

  END IF

  CALL DEALLOCATE_CPO(COREPROF_OLD)
  CALL COPY_CPO             (COREPROF_ITER,      COREPROF_OLD)         

!irena
  IF(nimp .GT. 0) THEN
     CALL DEALLOCATE_CPO(coreimpur_OLD)
     CALL COPY_CPO              (coreimpur_ITER,coreimpur_OLD)
     CALL DEALLOCATE_CPO(coreimpur_new)
     CALL COPY_CPO              (coreimpur_ITER,coreimpur_new)
  ENDIF

!irena

  ITIME=0
  ITIME_OUT                     = 0        
  CALL WRITE_OUT                (0,  COREPROF_ITER  )
  CALL WRITE_EQUILIBRIUM        (0,  EQUILIBRIUM_ITER)
!irena
  IF(nimp .GT. 0) CALL WRITEOUTIMPUR  (0,COREIMPUR_ITER)
!irena 
!irena neutrals
  IF(NNEUT > 0) CALL WRITEOUTNEUTRALS (0,CORENEUTRALS_ITER) 
!irena neutrals
  TIME_OUT                      = TIME_OUT+TAU_OUT




#ifdef UAL

  !----------------------------------------------------------------------!
  !     Output settings: shot, run, idx                                  !
  !----------------------------------------------------------------------!
    IF  (SHOT_OUT.GT.0.AND.RUN_OUT.GE.0) THEN
       SELECT CASE (db_out)
       CASE ("mdsplus")
          CALL EUITM_CREATE('euitm',SHOT_OUT, RUN_OUT,0,0,idx)
          use_euitm_put = .TRUE.
       CASE ("hdf5")
          CALL EUITM_CREATE_HDF5('euitm',SHOT_OUT,RUN_OUT,0,0,idx)
          use_euitm_put = .TRUE.
       CASE ("ascii")
          use_euitm_put = .FALSE.
       CASE default
          WRITE(*,*) 'Unexpected database format choice : ',TRIM(db_out)
          STOP 'Error: unrecognized database format'
       END SELECT
    END IF
#else
    use_euitm_put = .FALSE.
#endif

  !----------------------------------------------------------------------!
  !    This is the output via the UAL for the initial state before       !
  !    the first time-step                                               !
  !----------------------------------------------------------------------!
    IF  (SHOT_OUT.GT.0.AND.RUN_OUT.GE.0) THEN
 
       IF(augment_equil.EQ.1) THEN
          CALL augment_psi_rz(EQUILIBRIUM_ITER(1))
       ENDIF

#ifdef UAL
       IF(use_euitm_put) THEN

          coreprof_iter(1)%time    = time
          WRITE(*,*) 'euitm_put_non_timed: coreprof', coreprof_iter(1)%time,  &
                   TRIM(coreprof_iter(1)%codeparam%codename(1)),  &
                   '  ',  &
                   TRIM(coreprof_iter(1)%codeparam%codeversion(1))
          CALL euitm_put_non_timed (idx,"coreprof",coreprof_iter(1))
          WRITE(*,*) 'euitm_put_slice: coreprof', COREPROF_ITER(1)%time
          CALL euitm_put_slice     (idx,"coreprof",coreprof_iter(1))

          EQUILIBRIUM_ITER(1)%time = time
          WRITE(*,*) 'euitm_put_non_timed: equilibrium', equilibrium_iter(1)%time
          CALL euitm_put_non_timed (idx,"equilibrium",equilibrium_iter(1))
          WRITE(*,*) 'euitm_put_slice: equilibrium', EQUILIBRIUM_ITER(1)%time
          CALL euitm_put_slice     (idx,"equilibrium",EQUILIBRIUM_ITER(1))

          CORETRANSP_ITER(1)%time  = time
          WRITE(*,*) 'euitm_put_non_timed: coretransp', coretransp_iter(1)%time
          CALL euitm_put_non_timed (idx,"coretransp",coretransp_iter(1))
          WRITE(*,*) 'euitm_put_slice: coretransp', CORETRANSP_ITER(1)%time
          CALL euitm_put_slice     (idx,"coretransp",CORETRANSP_ITER(1))
 
          CORESOURCE_ITER(1)%time  = time
          WRITE(*,*) 'euitm_put_non_timed: coresource', coresource_iter(1)%time
          CALL euitm_put_non_timed (idx,"coresource",coresource_iter(1))
          WRITE(*,*) 'euitm_put_slice: coresource', CORESOURCE_ITER(1)%time
          CALL euitm_put_slice     (idx,"coresource",CORESOURCE_ITER(1))

          IF(nimp .GT. 0) THEN
             COREIMPUR_ITER(1)%time  = time
             WRITE(*,*) 'euitm_put_non_timed: coreimpur', coreimpur_iter(1)%time
             CALL euitm_put_non_timed (idx,"coreimpur",coreimpur_iter(1))
             WRITE(*,*) 'euitm_put_slice: coreimpur', COREIMPUR_ITER(1)%time
             CALL euitm_put_slice     (idx,"coreimpur",COREIMPUR_ITER(1))
          ENDIF
          IF(NNEUT > 0) THEN
             CORENEUTRALS_ITER(1)%time  = time
             WRITE(*,*) 'euitm_put_non_timed: coreneutrals', coreneutrals_iter(1)%time
             CALL euitm_put_non_timed (idx,"coreneutrals",coreneutrals_iter(1))
             WRITE(*,*) 'euitm_put_slice: coreneutrals', CORENEUTRALS_ITER(1)%time
             CALL euitm_put_slice     (idx,"coreneutrals",CORENEUTRALS_ITER(1))
          ENDIF
          TOROIDFIELD_OLD(1)%time  = time
          WRITE(*,*) 'euitm_put_non_timed: toroidfield', toroidfield_old(1)%time
          CALL euitm_put_non_timed (idx,"toroidfield",toroidfield_old(1))
          WRITE(*,*) 'euitm_put_slice: toroidfield', TOROIDFIELD_OLD(1)%time
          CALL euitm_put_slice     (idx,"toroidfield",TOROIDFIELD_OLD(1))
 
       ENDIF
#endif

       IF(DB_out .EQ. "ascii") THEN
          coreprof_iter(1)%time    = time
          WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreprof', shot_out, run_out, itime
          CALL open_write_file(1, TRIM(filename))
          CALL write_cpo(coreprof_iter(1), 'coreprof')
          CALL close_write_file
          equilibrium_iter(1)%time    = time
          WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'equilibrium', shot_out, run_out, itime
          CALL open_write_file(1, TRIM(filename))
          CALL write_cpo(equilibrium_iter(1), 'equilibrium')
          CALL close_write_file
          coretransp_iter(1)%time    = time
          WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coretransp', shot_out, run_out, itime
          CALL open_write_file(1, TRIM(filename))
          CALL write_cpo(coretransp_iter(1), 'coretransp')
          CALL close_write_file
          coresource_iter(1)%time    = time
          WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coresource', shot_out, run_out, itime
          CALL open_write_file(1, TRIM(filename))
          CALL write_cpo(coresource_iter(1), 'coresource')
          CALL close_write_file
          IF(nimp.GT.0) THEN
             coreimpur_iter(1)%time    = time
             WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreimpur', shot_out, run_out, itime
             CALL open_write_file(1, TRIM(filename))
             CALL write_cpo(coreimpur_iter(1), 'coreimpur')
             CALL close_write_file
          ENDIF
          IF(nneut.GT.0) THEN
             coreneutrals_iter(1)%time    = time
             WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreneutrals', shot_out, run_out, itime
             CALL open_write_file(1, TRIM(filename))
             CALL write_cpo(coreneutrals_iter(1), 'coreneutrals')
             CALL close_write_file
          ENDIF
          toroidfield_old(1)%time    = time
          WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'toroidfield', shot_out, run_out, itime
          CALL open_write_file(1, TRIM(filename))
          CALL write_cpo(toroidfield_old(1), 'toroidfield')
          CALL close_write_file
       ENDIF

    WRITE(*,1000) '       AREA ',equilibrium_iter(1)%global_param%area
    WRITE(*,1000) '     VOLUME ',equilibrium_iter(1)%global_param%volume
    WRITE(*,1000) '      Raxis ',equilibrium_iter(1)%global_param%mag_axis%position%r
    WRITE(*,1000) '      Zaxis ',equilibrium_iter(1)%global_param%mag_axis%position%z
    WRITE(*,1000) '      Baxis ',equilibrium_iter(1)%global_param%mag_axis%bphi
    WRITE(*,1000) '       Rgeo ',equilibrium_iter(1)%eqgeometry%geom_axis%r
    WRITE(*,1000) '       Zgeo ',equilibrium_iter(1)%eqgeometry%geom_axis%z
    WRITE(*,1000) '          a ',equilibrium_iter(1)%eqgeometry%a_minor
    WRITE(*,1000) '         R0 ',equilibrium_iter(1)%global_param%toroid_field%b0
    WRITE(*,1000) '         B0 ',equilibrium_iter(1)%global_param%toroid_field%r0

    WRITE(*,*) ' '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '
    WRITE(*,*) '+++++++    INITIAL SLICE IS SAVED    +++++++++ '
    WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++ '

    ENDIF

!
! here we are going to call SOLPS to update the ETS boundary conditions
! note that since this is the first call, the boundary conditions derive
! from b2.boundary.parameters and not from the passed COREPROF
! 
#ifdef CORE_EDGE
#ifdef UAL
    CALL euitm_open('euitm',shot_in,0,idx2)
    CALL euitm_get(idx2,'limiter',limiter)
    IF(use_euitm_put) THEN
       CALL euitm_put(idx,'limiter',limiter)
    ELSE IF(db_out .EQ. 'ascii') THEN
       WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''.cpo'')') 'limiter', shot_out, run_out
       CALL open_write_file(1, TRIM(filename))
       CALL write_cpo(limiter, 'limiter')
       CALL close_write_file
    ENDIF
    CALL euitm_close(idx2)
    CALL DEALLOCATE_CPO(limiter)
#endif
    WRITE(*,*) 'Core-Edge: Initialization call to SOLPS'
    CALL b2mn_ets(COREPROF_ITER, COREIMPUR_ITER, CORENEUTRALS_ITER,  &
         COREPROF_NEW, COREIMPUR_NEW, CORENEUTRALS_NEW, EDGE_NEW,  &
         code_parameters_core_edge)
#ifdef UAL
    IF(use_euitm_put) THEN
       EDGE_NEW(1)%time  = time
       WRITE(*,*) 'euitm_put_non_timed: edge', edge_new(1)%time
       CALL euitm_put_non_timed (idx,"edge",edge_new(1))
       WRITE(*,*) 'euitm_put_slice: edge', EDGE_NEW(1)%time
       CALL euitm_put_slice     (idx,"edge",EDGE_NEW(1))
    ENDIF
#endif
    IF(db_out .EQ. 'ascii') THEN
       edge_new(1)%time    = time
       WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'edge', shot_out, run_out, itime
       CALL open_write_file(1, TRIM(filename))
       CALL write_cpo(edge_new(1), 'edge')
       CALL close_write_file
    ENDIF
    CALL DEALLOCATE_CPO(COREPROF_NEW)
    IF(nimp.GT.0) CALL DEALLOCATE_CPO(COREIMPUR_NEW)
    IF(nneut.GT.0) CALL DEALLOCATE_CPO(CORENEUTRALS_NEW)
    CALL DEALLOCATE_CPO(EDGE_NEW)
#else
#endif

! dpc
    IF (psi_bnd_type .EQ. 2) THEN
       bc_ip_tau = 1.0e-3_R8
       bc_ip_wanted = PSI_BND_VALUE(1)
       bc_ip_current =   &
            - equilibrium_iter(1)%profiles_1d%dpsidrho_tor(npsi) &
            * equilibrium_iter(1)%profiles_1d%vprime(npsi)  &
            * equilibrium_iter(1)%profiles_1d%dpsidrho_tor(npsi)  &
            * equilibrium_iter(1)%profiles_1d%gm2(npsi)  & 
            / 4.e0_R8 / ITM_PI**2 / ITM_MU0
       WRITE(*,*) ' bc_ip_wanted = ', bc_ip_wanted
       WRITE(*,*) 'bc_ip_current = ', bc_ip_current
       WRITE(*,*) '    bc_ip_tau = ', bc_ip_tau
    ENDIF
! cpd

  !----------------------------------------------------------------------!
  !----------------------------------------------------------------------!
  !     START TIME LOOP                                                  !
  !----------------------------------------------------------------------!
  !----------------------------------------------------------------------!

TIME_LOOP: DO WHILE(time + tau*0.1_R8 .LT. TIME_END)
   ITIME = ITIME + 1

   IF(equil_mod.GT.0) THEN
      do_equil=MOD(itime,equil_mod).EQ.0
   ELSE
      do_equil=.TRUE.
   ENDIF

!!  TIME_LOOP1: DO ITIME = 1,NTIME

     TIME = TIME + TAU
     ITER = 0

     WRITE(*,*) ' '
     WRITE(*,*) '!---------------------------------------------!'
     WRITE(*,*) '!---------------------------------------------!'
     WRITE (6,*)'!    TIME=',TIME
     WRITE(*,*) '!---------------------------------------------!'
     WRITE(*,*) '!---------------------------------------------!'
     WRITE(*,*) ' '





  !----------------------------------------------------------------------!
  !----------------------------------------------------------------------!
  !     START CONVERGENCE LOOP                                           !
  !----------------------------------------------------------------------!
  !----------------------------------------------------------------------!
10   CONTINUE

     ITER = ITER + 1
     IF(ITER.GT.MAXITER) THEN
        WRITE(*,'(a,i0,a)') 'Maximum number of iterations ( ',MAXITER,' ) exceeded'
        WRITE(*,'(a,1pg10.3,a,i0)') 'Time = ', TIME, '  Number of time iterations = ', ITIME
        STOP 'Error'
     ENDIF




  !----------------------------------------------------------------------!
  !     Equilibrium                                                      !
  !----------------------------------------------------------------------!
!        Provide consistent input for equilibrium:
     CALL DEALLOCATE_CPO        (EQUILIBRIUM_NEW)
     CALL EQUIL_INPUT           (COREPROF_ITER, TOROIDFIELD_OLD, EQUILIBRIUM_ITER, EQUILIBRIUM_NEW)

! dpc
!     jrlx=0.99_R8
!     write(*,*) 'jpar/jphi relaxation factor = ', jrlx
!     EQUILIBRIUM_NEW(1)%profiles_1d%jparallel = jrlx * EQUILIBRIUM_ITER(1)%profiles_1d%jparallel + &
!          (1 - jrlx) * EQUILIBRIUM_NEW(1)%profiles_1d%jparallel
!     EQUILIBRIUM_NEW(1)%profiles_1d%jphi = jrlx * EQUILIBRIUM_ITER(1)%profiles_1d%jphi + &
!          (1 - jrlx) * EQUILIBRIUM_NEW(1)%profiles_1d%jphi
! cpd
     CALL DEALLOCATE_CPO        (EQUILIBRIUM_ITER)         
     WRITE(*,*) ' '

     ! >>> At this stage we have EQUILIBRIUM_NEW filled with current and pressure profiles

     !        Update equilibrium:
     IF(NPSI.GT.0) THEN
        IF(do_equil) THEN
           IF(ext_equil.EQ.1) THEN
              CALL DEALLOCATE_CPO  (EQUILIBRIUM_ITER)
              CALL bdseq_wrapper   (EQUILIBRIUM_NEW,     EQUILIBRIUM_ITER)
           ELSEIF(ext_equil.EQ.2) THEN
              CALL DEALLOCATE_CPO  (EQUILIBRIUM_ITER)
              CALL EMEQ_E3M_wrapper(EQUILIBRIUM_NEW,     EQUILIBRIUM_ITER)
           ELSEIF(ext_equil.EQ.3) THEN
#ifdef GOT_HELENA
              CALL DEALLOCATE_CPO  (EQUILIBRIUM_ITER)
              CALL helena_wrapper  (equilibrium_new, equilibrium_iter)
#else
              WRITE(*,*) 'No HELENA available at compile time'
              STOP 'No HELENA'
#endif
           ELSEIF(ext_equil.EQ.4) THEN
#ifdef GOT_CHEASE
              CALL DEALLOCATE_CPO  (EQUILIBRIUM_ITER)
              CALL chease_wrapper  (equilibrium_new, equilibrium_iter)
#else
              WRITE(*,*) 'No CHEASE available at compile time'
              STOP 'No CHEASE'
#endif
           ELSEIF(ext_equil.EQ.0) THEN
              CALL DEALLOCATE_CPO(EQUILIBRIUM_ITER)
              CALL COPY_CPO        (EQUILIBRIUM_NEW, EQUILIBRIUM_ITER)
           ELSE
              WRITE(*,*) 'Unknown equilibrium option ', ext_equil
              STOP 'Unknown EQUILIBRIUM'
        
           ENDIF
        ELSE
           WRITE(*,*) 'Re-using equilibrium'
           CALL DEALLOCATE_CPO(EQUILIBRIUM_ITER)
           CALL COPY_CPO              (EQUILIBRIUM_NEW, EQUILIBRIUM_ITER)          
        ENDIF

        IF(exp_option.NE.0) THEN
           IF(.NOT.ASSOCIATED(NEOCLASSIC_ITER))         &
                ALLOCATE     (NEOCLASSIC_ITER(1))
           CALL CHANGERADII     (EQUILIBRIUM_ITER,     &
                                 COREPROF_ITER,        &
                                 CORETRANSP_ITER,      &
                                 CORESOURCE_ITER,      &
                                 COREIMPUR_ITER,       &
                                 CORENEUTRALS_ITER,    & 
                                 NEOCLASSIC_ITER)
           CALL DEALLOCATE_CPO  (NEOCLASSIC_ITER)
        ENDIF
     ELSE
        IF(time_dep_input.EQ.1) THEN
        ELSE
           CALL DEALLOCATE_CPO(EQUILIBRIUM_ITER)
           CALL COPY_CPO        (EQUILIBRIUM_NEW, EQUILIBRIUM_ITER)
        ENDIF
     ENDIF

     CALL DEALLOCATE_CPO        (EQUILIBRIUM_NEW)
     CALL COPY_CPO              (EQUILIBRIUM_ITER, EQUILIBRIUM_NEW)

! dpc
     IF (psi_bnd_type .EQ. 2) THEN
        bc_ip_current =   &
             - equilibrium_iter(1)%profiles_1d%dpsidrho_tor(npsi) &
             * equilibrium_iter(1)%profiles_1d%vprime(npsi)  &
             * equilibrium_iter(1)%profiles_1d%dpsidrho_tor(npsi)  &
             * equilibrium_iter(1)%profiles_1d%gm2(npsi)  & 
             / 4.e0_R8 / ITM_PI**2 / ITM_MU0
        WRITE(*,*) 'bc_ip_current = ', bc_ip_current
        IF (ITER .EQ. 1) THEN
           bc_ip_jrlx = MAX(0.0_R8, MIN(1.0_R8, time / bc_ip_tau))
           WRITE(*,*) '   bc_ip_jrlx = ', bc_ip_jrlx
           coreprof_iter(1)%psi%boundary%value(1) = (1.0_R8 - bc_ip_jrlx) * bc_ip_current + bc_ip_jrlx * bc_ip_wanted
        ENDIF
        WRITE(*,*) ' bc_ip_target = ', coreprof_iter(1)%psi%boundary%value(1)
     ENDIF
! cpd

     ! >>> At this stage we have updated geometry stored in EQUILIBRIUM_ITER

 






  !----------------------------------------------------------------------!
  !     Transport                                                        !
  !----------------------------------------------------------------------!

     IF(ext_transport.EQ.1) THEN
        CALL external_transport(EQUILIBRIUM_ITER, COREPROF_ITER, CORETRANSP_ITER, add_transport)


     ELSEIF(ext_transport.EQ.2) THEN
        !DATABASE TRANSPORT
        CALL COPY_CPO                (CORETRANSP_DB,                               CORETRANSP1)   

        !NEOCLASSICAL TRANSPORT
        CALL NEOWes_wrapper          (EQUILIBRIUM_ITER,    COREPROF_ITER,          NEOCLASSIC_ITER) 
        CALL CONVERT_NEOCLASSIC2CORETRANSP(NEOCLASSIC_ITER,                        CORETRANSP2)

        !ANOMALOUS TRANSPORT
        CALL GB_TRANSPORT            (EQUILIBRIUM_ITER,    COREPROF_ITER,          CORETRANSP3)    

        !NO BACKGROUND TRANSPORT 
        CALL ALLOCATE_CORETRANSP_CPO (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP4)        
        CALL COPY_CPO                (COREPROF_ITER(1)%rho_tor,                    CORETRANSP4(1)%VALUES(1)%rho_tor)     
        CALL COPY_CPO                (COREPROF_ITER(1)%compositions,               CORETRANSP4(1)%compositions)     

        !SPITZER RESISTIVITY
        CALL SPITZER_RESISTIVITY     (COREPROF_ITER,                               CORETRANSP5)



        IF(ASSOCIATED(CORETRANSP_OLD)) STOP 'associated(CORETRANSP_OLD) 1'
        CALL  COPY_CPO               (CORETRANSP_ITER, CORETRANSP_OLD)
        CALL  DEALLOCATE_CPO         (CORETRANSP_ITER)


        !COMBINING INDIVIDUAL TRANSPORT COEFFICIENTS INTO SINGLE CORETRANSP CPO
        CALL  COMBINE_TRANSPORT      (COREPROF_ITER,  CORETRANSP_OLD,                                     &
                                      CORETRANSP1, CORETRANSP2, CORETRANSP3, CORETRANSP4, CORETRANSP5,    &
                                      CORETRANSP_ITER,                                                    &
                                      1._R8, code_parameters_transport_combiner)


        !DEALOCATING INDIVIDUAL CORETRANSP CPOs
        CALL  DEALLOCATE_CPO         (CORETRANSP_OLD)
        CALL  DEALLOCATE_CPO         (CORETRANSP1)            
        CALL  DEALLOCATE_CPO         (CORETRANSP2)
        CALL  DEALLOCATE_CPO         (CORETRANSP3)
        CALL  DEALLOCATE_CPO         (CORETRANSP4)
        CALL  DEALLOCATE_CPO         (CORETRANSP5)



     ELSE
        IF(time_dep_input.EQ.1) THEN
           CALL DEALLOCATE_CPO      (CORETRANSP_OLD)
           CALL copy_cpo            (CORETRANSP_ITER, CORETRANSP_OLD)
           CALL DEALLOCATE_CPO      (CORETRANSP_OLD)
        ENDIF
     ENDIF

     ! >>> At this stage we have updated transport coefficients stored in CORETRANSP_ITER




  !----------------------------------------------------------------------!
  !    Sources                                                           !
  !----------------------------------------------------------------------!
  !        Update sources:

     IF(ext_source.EQ.2) THEN

!!----  DATABASE SOURCE
        CALL COPY_CPO             (CORESOURCE_DB,                               CORESOURCE1)     


!!----  GAUSIAN SOURCES 
        CALL GAUSIAN_SOURCES      (COREPROF_ITER, EQUILIBRIUM_ITER, CORESOURCE2, code_parameters_gausian_sources) 
      

!!----  SYNCHROTRON SOURCES 
        CALL SYNCHROTRON_RADIATION(COREPROF_ITER, CORESOURCE3) 
      

!!----  HCD SOURCES (no HCD in fortran version )
        CALL ALLOCATE_CORESOURCE_CPO                             &     
                                  (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE4)        
        CALL DEALLOCATE_CPO       (CORESOURCE4(1)%VALUES(1)%rho_tor)
        CALL COPY_CPO             (CORESOURCE_ITER(1)%VALUES(1)%rho_tor,         CORESOURCE4(1)%VALUES(1)%rho_tor)     
        CALL DEALLOCATE_CPO       (CORESOURCE4(1)%compositions)
        CALL COPY_CPO             (CORESOURCE_ITER(1)%compositions,              CORESOURCE4(1)%compositions)     


!!----  NEUTRALS
        IF(ASSOCIATED(CORENEUTRALS_NEW)) THEN
           CALL DEALLOCATE_CPO    (CORENEUTRALS_NEW)
           WRITE(*,*) 'Deallocated CORENEUTRALS_NEW just before call to NEUTRALS_ETS'
        ENDIF
  
        IF(NNEUT .GT. 0)  THEN
           CALL NEUTRALS_ETS      (COREIMPUR_ITER,   EQUILIBRIUM_ITER,  COREPROF_ITER,   &
                                   CORENEUTRALS_OLD, CORENEUTRALS_ITER,                  &       !NEUTRAL SOURCE
                                   CORESOURCE5,      CORENEUTRALS_NEW,                   &
		                   CONTROL_INTEGER,  CONTROL_DOUBLE)
           CALL CHECK_CONVERGENCE_NEUTRALS(CORENEUTRALS_ITER, CORENEUTRALS_NEW, CONV_NEUT)
        ELSE 
           CALL ALLOCATE_CORESOURCE_CPO                          &                               !NO NEUTRAL SOURCES
                                  (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE5)        
           CALL DEALLOCATE_CPO    (CORESOURCE5(1)%VALUES(1)%rho_tor)
           CALL COPY_CPO          (CORESOURCE_ITER(1)%VALUES(1)%rho_tor,         CORESOURCE5(1)%VALUES(1)%rho_tor)     
           CALL DEALLOCATE_CPO    (CORESOURCE5(1)%compositions)
           CALL COPY_CPO          (CORESOURCE_ITER(1)%compositions,              CORESOURCE5(1)%compositions)     
        END IF


!!----  IMPURITY
        IF(ASSOCIATED(COREIMPUR_NEW)) THEN
           CALL DEALLOCATE_CPO      (COREIMPUR_NEW)
           WRITE(*,*) 'Deallocated COREIMPUR_NEW just before call to IMPURITY_ETS'
        ENDIF
  
        IF(nimp .GT. 0)    THEN
           CALL IMPURITY_ETS     (EQUILIBRIUM_ITER, COREPROF_ITER,  CORETRANSP_ITER,      &
                                  COREIMPUR_OLD,    COREIMPUR_ITER, CORENEUTRALS_ITER,    &
                                  CORESOURCE5,      CORESOURCE6,    COREIMPUR_NEW,        &       !IMPURITY SOURCE
                                  CONTROL_INTEGER,  CONTROL_DOUBLE)
           CALL CHECK_CONVERGENCE_IMPURITIES (COREIMPUR_ITER, COREIMPUR_NEW, CONV_IMP)
        ELSE
           CALL ALLOCATE_CORESOURCE_CPO                          &                                !NO IMPURITY SOURCES
                                 (NSLICE,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE6)        
           CALL DEALLOCATE_CPO   (CORESOURCE6(1)%VALUES(1)%rho_tor)
           CALL COPY_CPO         (CORESOURCE_ITER(1)%VALUES(1)%rho_tor,         CORESOURCE6(1)%VALUES(1)%rho_tor)     
           CALL DEALLOCATE_CPO   (CORESOURCE6(1)%compositions)
           CALL COPY_CPO         (CORESOURCE_ITER(1)%compositions,              CORESOURCE6(1)%compositions)     
        END IF



!!----  NEOCLASSICAL
        CALL CONVERT_NEOCLASSIC2CORESOURCE   (NEOCLASSIC_ITER,        CORESOURCE7)                !Neoclassical sources




!!----  COMBINE SOURCES
        CALL  COPY_CPO            (CORESOURCE_ITER, CORESOURCE_OLD)
        CALL  DEALLOCATE_CPO      (CORESOURCE_ITER)

        CALL  COMBINE_SOURCES     (COREPROF_ITER,  CORESOURCE_OLD,                   &
                                   CORESOURCE1,    CORESOURCE2,    CORESOURCE3,      &
                                   CORESOURCE4,    CORESOURCE5,    CORESOURCE6,      &
                                   CORESOURCE7,                                      &
                                   CORESOURCE_ITER,1._R8, code_parameters_sources_combiner)

        CALL  DEALLOCATE_CPO      (CORESOURCE_OLD)
        CALL  DEALLOCATE_CPO      (CORESOURCE1)
        CALL  DEALLOCATE_CPO      (CORESOURCE2)
        CALL  DEALLOCATE_CPO      (CORESOURCE3)
        CALL  DEALLOCATE_CPO      (CORESOURCE4)
        CALL  DEALLOCATE_CPO      (CORESOURCE5)
        CALL  DEALLOCATE_CPO      (CORESOURCE6)
        CALL  DEALLOCATE_CPO      (CORESOURCE7)

     ELSE
        IF(time_dep_input.EQ.1) THEN
           CALL DEALLOCATE_CPO      (CORESOURCE_OLD)
           CALL copy_cpo            (CORESOURCE_ITER, CORESOURCE_OLD)
           CALL DEALLOCATE_CPO      (CORESOURCE_OLD)
        ENDIF
     ENDIF

     ! >>> At this stage we have updated profiles of sources stored in CORESOURCE_ITER

     CALL ETS_INPUT           (EQUILIBRIUM_ITER, COREPROF_ITER)


     IF(ASSOCIATED(NEOCLASSIC_ITER)) CALL DEALLOCATE_CPO(NEOCLASSIC_ITER)




  !----------------------------------------------------------------------!
  !    TRANSPORT EQUATIONS                                               !
  !----------------------------------------------------------------------!
     ! >>> At this stage:
     !     CPOs with the index _OLD contain the information from the previous time step
     !     CPOs with the index _ITER contain the information from the previous iteration
     !     CONTROL_INTEGER and CONTROL_DOUBLE types contain the controlling parameters
     WRITE(*,*) ASSOCIATED(COREPROF_OLD)    ,ASSOCIATED(COREPROF_ITER),     &
                ASSOCIATED(EQUILIBRIUM_OLD) ,ASSOCIATED(EQUILIBRIUM_ITER),  &
                ASSOCIATED(CORETRANSP_ITER) ,ASSOCIATED(CORESOURCE_ITER),   &
                ASSOCIATED(COREIMPUR_ITER)

!     CALL size_of_cpo   (COREPROF_OLD(1),total_size,     .FALSE.,'COREPROF_OLD')
!     CALL size_of_cpo   (COREPROF_ITER(1),total_size,    .FALSE.,'COREPROF_ITER')
!     CALL size_of_cpo   (EQUILIBRIUM_OLD(1),total_size,  .FALSE.,'EQUILIBRIUM_OLD')
!     CALL size_of_cpo   (EQUILIBRIUM_ITER(1),total_size, .FALSE.,'EQUILIBRIUM_ITER')
!     CALL size_of_cpo   (CORETRANSP_ITER(1),total_size,  .FALSE.,'CORETRANSP_ITER')
!     CALL size_of_cpo   (CORESOURCE_ITER(1),total_size,  .FALSE.,'CORESOURCE_ITER')


!     IF(nimp .GT. 0)                                                         &
!        CALL size_of_cpo(COREIMPUR_ITER(1),total_size,   .FALSE.,'COREIMPUR_ITER')
!     IF(NNEUT > 0)                                                           &
!        CALL size_of_cpo(CORENEUTRALS_ITER(1),total_size,.FALSE.,'CORENEUTRALS_ITER')
     IF(ASSOCIATED(COREPROF_NEW)) THEN
        CALL DEALLOCATE_CPO      (COREPROF_NEW)
        WRITE(*,*) 'Deallocated COREPROF_NEW just before call to ITM_ETS'
     ENDIF


     CALL ITM_ETS      (COREPROF_OLD,    COREPROF_ITER,    COREPROF_NEW,     &
                        EQUILIBRIUM_OLD, EQUILIBRIUM_ITER,                   &
                        CORETRANSP_ITER, CORESOURCE_ITER,  COREIMPUR_NEW,    &
                        CONTROL_INTEGER, CONTROL_DOUBLE,   code_parameters_ets)


!     CALL size_of_cpo   (COREPROF_NEW(1),total_size,      .FALSE.,'COREPROF_NEW')


  !----------------------------------------------------------------------!
  !    Convergence                                                       !
  !----------------------------------------------------------------------!
     !        Check the actual convergence
     CALL CHECK_CONVERGENCE     (COREPROF_ITER, COREPROF_NEW, CONTROL_DOUBLE)

     ! >>> At this stage we have updated CONTROL_DOUBLE type
     !     COREPROF_ITER contains new plasma profiles copied from COREPROF_NEW 

     CALL DEALLOCATE_CPO        (COREPROF_ITER)
     CALL COPY_CPO              (COREPROF_NEW,    COREPROF_ITER)
     CALL DEALLOCATE_CPO        (COREPROF_NEW    ) 
   
     IF(nimp .GT. 0) THEN
        CALL DEALLOCATE_CPO     (COREIMPUR_ITER)
        CALL COPY_CPO           (COREIMPUR_NEW,   COREIMPUR_ITER)
        CALL DEALLOCATE_CPO     (COREIMPUR_NEW    ) 
     ENDIF
     
     IF(NNEUT > 0) THEN
        CALL DEALLOCATE_CPO     (CORENEUTRALS_ITER)
        CALL COPY_CPO           (CORENEUTRALS_NEW, CORENEUTRALS_ITER)
     ENDIF

     WRITE(*,*) 'CONVERGENCE ',CONTROL_DOUBLE(4)

     !        Compare the actual and required convergence:
     IF (CONTROL_DOUBLE(4).GT.CONTROL_DOUBLE(5)) GOTO 10

  !----------------------------------------------------------------------!
  !----------------------------------------------------------------------!
  !     END TIME LOOP                                                    !
  !----------------------------------------------------------------------!
  !----------------------------------------------------------------------!


     CALL DEALLOCATE_CPO        (COREPROF_OLD)
     CALL COPY_CPO              (COREPROF_ITER,    COREPROF_OLD)
     CALL DEALLOCATE_CPO        (EQUILIBRIUM_OLD)
     CALL COPY_CPO              (EQUILIBRIUM_ITER, EQUILIBRIUM_OLD)

     ! >>> At this stage we have COREPROF_OLD and EQUILIBRIUM_OLD  
     !     updated after iterations on the time step converged

!irena
     IF(nimp .GT. 0) THEN
        CALL DEALLOCATE_CPO      (COREIMPUR_OLD)
        CALL COPY_CPO              (COREIMPUR_ITER,    COREIMPUR_OLD)
     ENDIF
!irena
     IF(NNEUT > 0) THEN
        CALL DEALLOCATE_CPO      (COREneutrals_OLD)
        CALL COPY_CPO              (COREneutrals_iter, COREneutrals_OLD)
     ENDIF
     IF (TIME + TAU*0.1_R8 .GE. TIME_OUT) THEN
        
        !        Writes equilibrium profiles for the comparison with ASTRA:

        ITIME_OUT = ITIME_OUT +1         
        TIME_OUT  = TIME_OUT + TAU_OUT

        CALL WRITE_OUT              (ITIME_OUT,  COREPROF_ITER  )
        CALL WRITE_EQUILIBRIUM      (ITIME_OUT,  EQUILIBRIUM_NEW)
        IF(nimp .GT. 0) CALL WRITEOUTIMPUR (ITIME_OUT, COREIMPUR_ITER)
        IF(NNEUT > 0) CALL WRITEOUTNEUTRALS (ITIME_OUT,CORENEUTRALS_ITER) 

! this is the output via the UAL for the results at the end of the time-steps

        IF(shot_out.GT.0.AND.run_out.GE.0) THEN

           IF(augment_equil.EQ.1) THEN
              CALL augment_psi_rz(EQUILIBRIUM_ITER(1))
           ENDIF

#ifdef UAL
           IF(use_euitm_put) THEN

              coreprof_iter(1)%time=time
              WRITE(*,*) 'euitm_put_slice: coreprof', COREPROF_ITER(1)%time
              CALL euitm_put_slice(idx,"coreprof",coreprof_iter(1))

              IF(do_equil) THEN
                 EQUILIBRIUM_ITER(1)%time=time
                 WRITE(*,*) 'euitm_put_slice: equilibrium', EQUILIBRIUM_ITER(1)%time
                 CALL euitm_put_slice(idx,"equilibrium",EQUILIBRIUM_ITER(1))
              ENDIF
 
              CORETRANSP_ITER(1)%time=time
              WRITE(*,*) 'euitm_put_slice: coretransp', CORETRANSP_ITER(1)%time
              CALL euitm_put_slice(idx,"coretransp",CORETRANSP_ITER(1))

              CORESOURCE_ITER(1)%time=time
              WRITE(*,*) 'euitm_put_slice: coresource', CORESOURCE_ITER(1)%time
              CALL euitm_put_slice(idx,"coresource",CORESOURCE_ITER(1))

              IF(nimp .GT. 0) THEN
                 COREIMPUR_ITER(1)%time=time
                 WRITE(*,*) 'euitm_put_slice: coreimpur', COREIMPUR_ITER(1)%time
                 CALL euitm_put_slice(idx,"coreimpur",COREIMPUR_ITER(1))
              ENDIF

              IF(NNEUT > 0) THEN
                 CORENEUTRALS_ITER(1)%time=time
                 WRITE(*,*) 'euitm_put_slice: coreneutrals', COREneutrals_ITER(1)%time
                 CALL euitm_put_slice(idx,"coreneutrals",COREneutrals_ITER(1))
              ENDIF

              TOROIDFIELD_OLD(1)%time  = time
              WRITE(*,*) 'euitm_put_slice: toroidfield', TOROIDFIELD_OLD(1)%time
              CALL euitm_put_slice     (idx,"toroidfield",TOROIDFIELD_OLD(1))
 
           ENDIF
#endif

           IF(DB_out .EQ. "ascii") THEN
              coreprof_iter(1)%time    = time
              WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreprof', shot_out, run_out, itime
              CALL open_write_file(1, TRIM(filename))
              CALL write_cpo(coreprof_iter(1), 'coreprof')
              CALL close_write_file
              IF(do_equil) THEN
                 equilibrium_iter(1)%time    = time
                 WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'equilibrium', shot_out, run_out, itime
                 CALL open_write_file(1, TRIM(filename))
                 CALL write_cpo(equilibrium_iter(1), 'equilibrium')
                 CALL close_write_file
              ENDIF
              coretransp_iter(1)%time    = time
              WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coretransp', shot_out, run_out, itime
              CALL open_write_file(1, TRIM(filename))
              CALL write_cpo(coretransp_iter(1), 'coretransp')
              CALL close_write_file
              coresource_iter(1)%time    = time
              WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coresource', shot_out, run_out, itime
              CALL open_write_file(1, TRIM(filename))
              CALL write_cpo(coresource_iter(1), 'coresource')
              CALL close_write_file
              IF(nimp.GT.0) THEN
                 coreimpur_iter(1)%time    = time
                 WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreimpur', shot_out, run_out, itime
                 CALL open_write_file(1, TRIM(filename))
                 CALL write_cpo(coreimpur_iter(1), 'coreimpur')
                 CALL close_write_file
              ENDIF
              IF(nneut.GT.0) THEN
                 coreneutrals_iter(1)%time    = time
                 WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'coreneutrals', shot_out, run_out, itime
                 CALL open_write_file(1, TRIM(filename))
                 CALL write_cpo(coreneutrals_iter(1), 'coreneutrals')
                 CALL close_write_file
              ENDIF
              toroidfield_old(1)%time    = time
              WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'toroidfield', shot_out, run_out, itime
              CALL open_write_file(1, TRIM(filename))
              CALL write_cpo(toroidfield_old(1), 'toroidfield')
              CALL close_write_file
           ENDIF

        ENDIF

        WRITE(*,1000) '       AREA ',equilibrium_iter(1)%global_param%area
        WRITE(*,1000) '     VOLUME ',equilibrium_iter(1)%global_param%volume
        WRITE(*,1000) '      Raxis ',equilibrium_iter(1)%global_param%mag_axis%position%r
        WRITE(*,1000) '      Zaxis ',equilibrium_iter(1)%global_param%mag_axis%position%z
        WRITE(*,1000) '      Baxis ',equilibrium_iter(1)%global_param%mag_axis%bphi
        WRITE(*,1000) '       Rgeo ',equilibrium_iter(1)%eqgeometry%geom_axis%r
        WRITE(*,1000) '       Zgeo ',equilibrium_iter(1)%eqgeometry%geom_axis%z
        WRITE(*,1000) '          a ',equilibrium_iter(1)%eqgeometry%a_minor
        WRITE(*,1000) '         R0 ',equilibrium_iter(1)%global_param%toroid_field%b0
        WRITE(*,1000) '         B0 ',equilibrium_iter(1)%global_param%toroid_field%r0

     END IF

     IF(ITER_INC.GT.0 .AND. ITER.LT.ITER_INC) THEN
        IF(TAU_INC.GT.1.0_R8) THEN
           tau=tau*tau_inc
           WRITE(*,*) 'TAU, TAU_MAX ',tau,tau_max
           IF(tau_max.GT.0.0_R8) tau=MIN(tau,tau_max)
           WRITE(*,*) 'TAU increased to ',tau
        ENDIF
     ELSEIF(ITER_DEC.GT.0 .AND. ITER.GT.ITER_DEC) THEN
        IF(TAU_DEC.GT.0.0_R8.AND.TAU_DEC.LT.1.0_R8) THEN
           tau=tau*tau_dec
           WRITE(*,*) 'TAU, TAU_MIN ',tau,tau_min
           IF(tau_min.GT.0.0_R8) tau=MAX(tau,tau_min)
           WRITE(*,*) 'TAU decreased to ',tau
        ENDIF
     ENDIF
     
     TAU=MIN(TAU,TIME_END-TIME)
     IF(TAU.LT.0) THEN
        WRITE(*,*) 'ERROR: TAU < 0'
        TIME=NTIME*TAU-TIME
     ENDIF

     WRITE(*,'(a)')                   'Global Diagnostics Start'
     WRITE(*,'(a,1p,I10)')            'ITERATIONS ',ITER
     WRITE(*,'(a,1p,1024(1x,g15.6))') '        Ip ',EQUILIBRIUM_NEW(1)%global_param%i_plasma 
     WRITE(*,'(a,1p,1024(1x,g15.6))') '       Vol ',EQUILIBRIUM_NEW(1)%global_param%volume   
     WRITE(*,'(a,1p,1024(1x,g15.6))') '  Q_e(bnd) ',COREPROF_ITER(1)%te%flux%flux_dv(nrho)
     WRITE(*,'(a,1p,1024(1x,g15.6))') '  Q_i(bnd) ',COREPROF_ITER(1)%ti%flux%flux_dv(nrho,:)
     WRITE(*,'(a,1p,1024(1x,g15.6))') '  G_e(bnd) ',COREPROF_ITER(1)%ne%flux%flux_dv(nrho)
     WRITE(*,'(a,1p,1024(1x,g15.6))') '  G_i(bnd) ',COREPROF_ITER(1)%ni%flux%flux_dv(nrho,:)
     WRITE(*,'(a)')                   'Global Diagnostics End'

!
! here we are going to call SOLPS to update the ETS boundary conditions
! 
#ifdef CORE_EDGE
     WRITE(*,*) 'Core-Edge: Continuation call to SOLPS'
     CALL b2mn_ets(COREPROF_OLD, COREIMPUR_OLD, CORENEUTRALS_OLD,  &
          COREPROF_ITER, COREIMPUR_ITER, CORENEUTRALS_ITER, EDGE_NEW,  &
          code_parameters_core_edge)
#ifdef UAL
     IF(use_euitm_put) THEN
        EDGE_NEW(1)%time  = time
        WRITE(*,*) 'euitm_put_slice: edge', EDGE_NEW(1)%time
        CALL euitm_put_slice     (idx,"edge",EDGE_NEW(1))
     ENDIF
#endif
     IF(db_out .EQ. 'ascii') THEN
        edge_new(1)%time    = time
        WRITE(filename,'(a,''_'',I6.6,''_'',I6.6,''_'',I6.6,''.cpo'')') 'edge', shot_out, run_out, itime
        CALL open_write_file(1, TRIM(filename))
        CALL write_cpo(edge_new(1), 'edge')
        CALL close_write_file
     ENDIF
!     call size_of_cpo(EDGE_NEW(1),total_size,.false.,'EDGE_NEW')
     CALL DEALLOCATE_CPO(EDGE_NEW)
#endif 

     INQUIRE(file='.quit',exist=quitexist)
     IF(quitexist) EXIT TIME_LOOP

!!  END DO TIME_LOOP1
  END DO TIME_LOOP

#ifdef CORE_EDGE
     WRITE(*,*) 'Core-Edge: Finalization call to SOLPS'
     CALL b2mn_ets(COREPROF_OLD, COREIMPUR_OLD, CORENEUTRALS_OLD,  &
          COREPROF_ITER, COREIMPUR_ITER, CORENEUTRALS_ITER, EDGE_NEW,  &
          code_parameters_core_edge, .TRUE.)
#endif

#ifdef UAL
  IF(shot_out.GT.0.AND.run_out.GE.0.AND.db_out.NE.'ascii') THEN
     CALL euitm_close(idx)
  ENDIF
#endif


     WRITE(*,*) ASSOCIATED(COREPROF_OLD),ASSOCIATED(COREPROF_ITER),ASSOCIATED(COREPROF_NEW),  &
          ASSOCIATED(EQUILIBRIUM_OLD),ASSOCIATED(EQUILIBRIUM_ITER),ASSOCIATED(EQUILIBRIUM_NEW),  &
          ASSOCIATED(CORETRANSP_OLD),ASSOCIATED(CORETRANSP_ITER),  &
          ASSOCIATED(CORESOURCE_OLD),ASSOCIATED(CORESOURCE_ITER),   &
          ASSOCIATED(COREIMPUR_OLD),ASSOCIATED(COREIMPUR_ITER),&
	  ASSOCIATED(CORENEUTRALS_OLD),ASSOCIATED(CORENEUTRALS_ITER)

  CALL DEALLOCATE_CPO          (COREPROF_OLD )        
  CALL DEALLOCATE_CPO          (COREPROF_ITER)        
!  CALL DEALLOCATE_CPO          (COREPROF_NEW ) 

  CALL DEALLOCATE_CPO       (EQUILIBRIUM_OLD )         
  CALL DEALLOCATE_CPO       (EQUILIBRIUM_ITER)         
  CALL DEALLOCATE_CPO       (EQUILIBRIUM_NEW )         

!  CALL DEALLOCATE_CPO        (CORETRANSP_OLD )        
  CALL DEALLOCATE_CPO        (CORETRANSP_ITER)

!  CALL DEALLOCATE_CPO        (CORESOURCE_OLD )
  CALL DEALLOCATE_CPO        (CORESOURCE_ITER)

  IF(nimp .GT. 0) CALL DEALLOCATE_CPO         (COREIMPUR_OLD )        
  IF(nimp .GT. 0) CALL DEALLOCATE_CPO         (COREIMPUR_ITER)
!  CALL DEALLOCATE_CPO         (COREIMPUR_NEW)

  IF(NNEUT > 0) CALL DEALLOCATE_CPO         (CORENEUTRALS_OLD )        
  IF(NNEUT > 0) CALL DEALLOCATE_CPO         (CORENEUTRALS_ITER)
  IF(NNEUT > 0) CALL DEALLOCATE_CPO         (CORENEUTRALS_NEW)
  
  CALL DEALLOCATE_CPO       (TOROIDFIELD_OLD )         

  IF(ASSOCIATED(evolution_labels)) DEALLOCATE(evolution_labels)
  IF(ASSOCIATED(evolution_data)) DEALLOCATE(evolution_data)
  CALL DEALLOCATE_CPO      (code_parameters_ets)
  CALL DEALLOCATE_CPO      (code_parameters_ets_workflow)
  CALL DEALLOCATE_CPO      (code_parameters_transport_combiner)
  CALL DEALLOCATE_CPO      (code_parameters_sources_combiner)
#ifdef CORE_EDGE
  CALL DEALLOCATE_CPO(code_parameters_core_edge)
#endif

  CALL DEALLOCATE_CPO      (COREPROF_DB)
  CALL DEALLOCATE_CPO      (CORETRANSP_DB)
  CALL DEALLOCATE_CPO      (CORESOURCE_DB)
  IF(NIMP.GT.0) CALL DEALLOCATE_CPO      (COREIMPUR_DB)
  IF(NNEUT.GT.0) CALL DEALLOCATE_CPO      (CORENEUTRALS_DB)
  CALL DEALLOCATE_CPO      (EQUILIBRIUM_DB)
  CALL DEALLOCATE_CPO      (TOROIDFIELD_DB)
  CALL DEALLOCATE_CPO      (NEOCLASSIC_DB)

  IF(ALLOCATED(ets_species%nzimp)) DEALLOCATE(ets_species%nzimp)
  IF(ALLOCATED(ets_species%ncomp)) DEALLOCATE(ets_species%ncomp)
  IF(ALLOCATED(ets_species%ntype)) DEALLOCATE(ets_species%ntype)
  IF(ALLOCATED(NZIMP)) DEALLOCATE(NZIMP)
  IF(ALLOCATED(NCOMP)) DEALLOCATE(NCOMP)
  IF(ALLOCATED(NTYPE)) DEALLOCATE(NTYPE)


  IF(nimp.GT.0) CALL impurity_finish


  WRITE(*,*) 'Total_size = ', total_size


1000 FORMAT(a,1pg20.10)

CONTAINS

  SUBROUTINE evolution(T, R_in, R_out, El, Tr_l, Tr_U, Ip)

    USE itm_types

    IMPLICIT NONE

! input 
    REAL(R8) :: T

! output
    REAL(R8) :: R_in, R_out, El, Tr_l, Tr_U, Ip

! local
    INTEGER, SAVE :: npts = 0, ncol, isrch=1
    INTEGER :: ipts, icol
    REAL(R8) :: tf

    IF(npts.EQ.0) THEN
       ncol=SIZE(evolution_data,1)
       npts=SIZE(evolution_data,2)
       WRITE(*,*) 'evolution: ncol, npts = ', ncol, npts
    ENDIF
    
    DO WHILE (isrch.LT.npts .AND. T.GT.evolution_data(1,isrch+1))
       isrch=isrch+1
    ENDDO
    
    IF(ncol.LT.7) THEN
       WRITE(*,*) 'Not enough data in "evolution.exp"'
       STOP
    ENDIF
    IF(T.LT.evolution_data(1,isrch).OR.isrch.EQ.npts) THEN
       IF(isrch.NE.1) THEN
          WRITE(*,*) 'Coding error in evolution, isrch <> 1'
          STOP
       ENDIF
       R_in  = evolution_data(2,isrch)
       R_out = evolution_data(3,isrch)
       El    = evolution_data(4,isrch)
       Tr_L  = evolution_data(5,isrch)
       Tr_U  = evolution_data(6,isrch)
       Ip    = evolution_data(7,isrch)
    ELSE
       tf=(T-evolution_data(1,isrch))/(evolution_data(1,isrch+1)-evolution_data(1,isrch))
       IF(tf.LT.0.0_R8 .OR. tf.GT.1.0_R8) THEN
          WRITE(*,*) 'Coding error in evolution, tf not in [0,1], tf = ',tf
       ENDIF
       R_in  = (1.0_R8-tf)*evolution_data(2,isrch)+tf*evolution_data(2,isrch+1)
       R_out = (1.0_R8-tf)*evolution_data(3,isrch)+tf*evolution_data(3,isrch+1)
       El    = (1.0_R8-tf)*evolution_data(4,isrch)+tf*evolution_data(4,isrch+1)
       Tr_L  = (1.0_R8-tf)*evolution_data(5,isrch)+tf*evolution_data(5,isrch+1)
       Tr_U  = (1.0_R8-tf)*evolution_data(6,isrch)+tf*evolution_data(6,isrch+1)
       Ip    = (1.0_R8-tf)*evolution_data(7,isrch)+tf*evolution_data(7,isrch+1)
    ENDIF
    WRITE(*,*) evolution_labels(1), T
    WRITE(*,*) evolution_labels(2), R_in
    WRITE(*,*) evolution_labels(3), R_out
    WRITE(*,*) evolution_labels(4), El
    WRITE(*,*) evolution_labels(5), Tr_L
    WRITE(*,*) evolution_labels(6), Tr_U
    WRITE(*,*) evolution_labels(7), Ip
  END SUBROUTINE evolution

  FUNCTION profile(function_string, x)

    IMPLICIT NONE

    REAL(R8)               :: x(:), profile(1:SIZE(x))
    CHARACTER (len=BUFLEN) :: function_string

    INTEGER*8              :: evaluator_create, function_descriptor
    DOUBLE PRECISION       :: evaluator_evaluate_x
    EXTERNAL                  evaluator_destroy

    INTEGER                :: i

    function_descriptor = evaluator_create (TRIM(function_string))
    IF(function_descriptor == 0) THEN
       WRITE(*,*) 'Invalid function ', TRIM(function_string)
       STOP
    ENDIF

    DO i = 1, SIZE(x)
       profile(i) = evaluator_evaluate_x (function_descriptor, x(i))
    ENDDO

    CALL evaluator_destroy(function_descriptor)

  END FUNCTION profile

END PROGRAM ETS_WORKFLOW
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

SUBROUTINE external_transport(EQUILIBRIUM_ITER,COREPROF_ITER,CORETRANSP_ITER,add_transport)

  USE ITM_CONSTANTS
  USE EUITM_ROUTINES
  USE EUITM_SCHEMAS
  USE ALLOCATE_DEALLOCATE
  USE ITM_TEST_ROUTINES_ETSEQ
  USE ets_wrapper_etaigb
  USE ets_wrapper_neowes
  USE ets_species_module

  IMPLICIT NONE

  TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM_ITER(:)
  TYPE (TYPE_COREPROF),    POINTER :: COREPROF_ITER(:)  
  TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP_ITER(:),  CORETRANSP_etaigb(:), CORETRANSP_neowes(:)
  TYPE (TYPE_NEOCLASSIC),  POINTER :: NEOCLASSIC_neowes(:)
  INTEGER,               PARAMETER :: NSLICE = 1           !number of CPO ocurancies in the work flow
  INTEGER :: ITR, IION, NRHO, NION
  REAL(R8) :: add_transport

  NRHO          = SIZE (COREPROF_ITER(1)%rho_tor, DIM=1)

  CALL ALLOCATE_CORETRANSP_CPO  (NSLICE, NRHO, ETS_SPECIES%NATM, ETS_SPECIES%NION,  ETS_SPECIES%NIMP,  ETS_SPECIES%NZIMP, ETS_SPECIES%NNEUT, ETS_SPECIES%NTYPE, ETS_SPECIES%NCOMP, CORETRANSP_NEOWES  )
  CORETRANSP_NEOWES(1)%VALUES(1)%rho_tor(:) = CORETRANSP_ITER(1)%VALUES(1)%rho_tor(:)

  CALL EtaiGB_wrapper(EQUILIBRIUM_ITER,COREPROF_ITER,CORETRANSP_etaigb)
  CALL NEOWes_wrapper(EQUILIBRIUM_ITER,COREPROF_ITER,NEOCLASSIC_neowes)

! map the electron particle transport --- don't do since we don't use
!        do itr=1,3
!           call L3interp(  &
!                coretransp_etaigb(1)%values(1)%ne_transp%diff_eff(:,itr),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
!                coretransp_ITER(1)%values(1)%ne_transp%diff_eff(:,itr),coretransp_ITER(1)%values(1)%rho_tor,nrho)
!           call L3interp(  &
!                coretransp_etaigb(1)%values(1)%ne_transp%vconv_eff(:,itr),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
!                coretransp_ITER(1)%values(1)%ne_transp%vconv_eff(:,itr),coretransp_ITER(1)%values(1)%rho_tor,nrho)
!        enddo
!        coretransp_neowes(1)%values(1)%ne_transp%diff_eff=0.0_R8
!        coretransp_neowes(1)%values(1)%ne_transp%vconv_eff=0.0_R8
!        call L3interp(  &
!             neoclassic_neowes(1)%ne_neo%diff_eff(:),neoclassic_neowes(1)%rho_tor,nrho,  &
!             coretransp_NEOWES(1)%values(1)%ne_transp%diff_eff(:,3),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)
!        call L3interp(  &
!             neoclassic_neowes(1)%ne_neo%vconv_eff(:),neoclassic_neowes(1)%rho_tor,nrho,  &
!             coretransp_NEOWES(1)%values(1)%ne_transp%vconv_eff(:,3),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)
!        coretransp_ITER(1)%values(1)%ne_transp%diff_eff = coretransp_ITER(1)%values(1)%ne_transp%diff_eff + coretransp_NEOWES(1)%values(1)%ne_transp%diff_eff
!        coretransp_ITER(1)%values(1)%ne_transp%vconv_eff = coretransp_ITER(1)%values(1)%ne_transp%vconv_eff + coretransp_NEOWES(1)%values(1)%ne_transp%vconv_eff

! map the electron heat transport        
  CALL L3interp(  &
       coretransp_etaigb(1)%values(1)%te_transp%diff_eff(:),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
       coretransp_ITER(1)%values(1)%te_transp%diff_eff(:),coretransp_ITER(1)%values(1)%rho_tor,nrho)
  CALL L3interp(  &
       coretransp_etaigb(1)%values(1)%te_transp%vconv_eff(:),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
       coretransp_ITER(1)%values(1)%te_transp%vconv_eff(:),coretransp_ITER(1)%values(1)%rho_tor,nrho)
  CALL L3interp(  &
       neoclassic_neowes(1)%te_neo%diff_eff(:),neoclassic_neowes(1)%rho_tor,nrho,  &
       coretransp_NEOWES(1)%values(1)%te_transp%diff_eff(:),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)
  CALL L3interp(  &
       neoclassic_neowes(1)%te_neo%vconv_eff(:),neoclassic_neowes(1)%rho_tor,nrho,  &
       coretransp_NEOWES(1)%values(1)%te_transp%vconv_eff(:),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)
  coretransp_ITER(1)%values(1)%te_transp%diff_eff = coretransp_ITER(1)%values(1)%te_transp%diff_eff +   &
       coretransp_NEOWES(1)%values(1)%te_transp%diff_eff + add_transport
  coretransp_ITER(1)%values(1)%te_transp%vconv_eff = coretransp_ITER(1)%values(1)%te_transp%vconv_eff +  &
       coretransp_NEOWES(1)%values(1)%te_transp%vconv_eff

! map the ion particle transport
  coretransp_neowes(1)%values(1)%ni_transp%diff_eff=0.0_R8
  coretransp_neowes(1)%values(1)%ni_transp%vconv_eff=0.0_R8
  DO iion=1,ets_species%nion
     DO itr=1,3
        CALL L3interp(  &
             coretransp_etaigb(1)%values(1)%ni_transp%diff_eff(:,iion,itr),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
             coretransp_ITER(1)%values(1)%ni_transp%diff_eff(:,iion,itr),coretransp_ITER(1)%values(1)%rho_tor,nrho)
        CALL L3interp(  &
             coretransp_etaigb(1)%values(1)%ni_transp%vconv_eff(:,iion,itr),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
             coretransp_ITER(1)%values(1)%ni_transp%vconv_eff(:,iion,itr),coretransp_ITER(1)%values(1)%rho_tor,nrho)
     ENDDO
     CALL L3interp(  &
          neoclassic_neowes(1)%ni_neo%diff_eff(:,iion),neoclassic_neowes(1)%rho_tor,nrho,  &
          coretransp_NEOWES(1)%values(1)%ni_transp%diff_eff(:,iion,3),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)
     CALL L3interp(  &
          neoclassic_neowes(1)%ni_neo%vconv_eff(:,iion),neoclassic_neowes(1)%rho_tor,nrho,  &
          coretransp_NEOWES(1)%values(1)%ni_transp%vconv_eff(:,iion,3),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)

! map the ion heat transport
     CALL L3interp(  &
          coretransp_etaigb(1)%values(1)%ti_transp%diff_eff(:,iion),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
          coretransp_ITER(1)%values(1)%ti_transp%diff_eff(:,iion),coretransp_ITER(1)%values(1)%rho_tor,nrho)
     CALL L3interp(  &
          coretransp_etaigb(1)%values(1)%ti_transp%vconv_eff(:,iion),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
          coretransp_ITER(1)%values(1)%ti_transp%vconv_eff(:,iion),coretransp_ITER(1)%values(1)%rho_tor,nrho)
     CALL L3interp(  &
          neoclassic_neowes(1)%ti_neo%diff_eff(:,iion),neoclassic_neowes(1)%rho_tor,nrho,  &
          coretransp_NEOWES(1)%values(1)%ti_transp%diff_eff(:,iion),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)
     CALL L3interp(  &
          neoclassic_neowes(1)%ti_neo%vconv_eff(:,iion),neoclassic_neowes(1)%rho_tor,nrho,  &
          coretransp_NEOWES(1)%values(1)%ti_transp%vconv_eff(:,iion),coretransp_NEOWES(1)%values(1)%rho_tor,nrho)

! map the vtor transport --- not stored yet
!           call L3interp(  &
!                coretransp_etaigb(1)%values(1)%vtor_transp%diff_eff(:,iion),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
!                coretransp_ITER(1)%values(1)%vtor_transp%diff_eff(:,iion),coretransp_ITER(1)%values(1)%rho_tor,nrho)
!           call L3interp(  &
!                coretransp_etaigb(1)%values(1)%vtor_transp%vconv_eff(:,iion),coretransp_etaigb(1)%values(1)%rho_tor,nrho,  &
!                coretransp_ITER(1)%values(1)%vtor_transp%vconv_eff(:,iion),coretransp_ITER(1)%values(1)%rho_tor,nrho)
  ENDDO
  coretransp_ITER(1)%values(1)%ni_transp%diff_eff = coretransp_ITER(1)%values(1)%ni_transp%diff_eff +   &
       coretransp_NEOWES(1)%values(1)%ni_transp%diff_eff + add_transport
  coretransp_ITER(1)%values(1)%ni_transp%vconv_eff = coretransp_ITER(1)%values(1)%ni_transp%vconv_eff +   &
       coretransp_NEOWES(1)%values(1)%ni_transp%vconv_eff
  coretransp_ITER(1)%values(1)%ti_transp%diff_eff = coretransp_ITER(1)%values(1)%ti_transp%diff_eff +   &
       coretransp_NEOWES(1)%values(1)%ti_transp%diff_eff + add_transport
  coretransp_ITER(1)%values(1)%ti_transp%vconv_eff = coretransp_ITER(1)%values(1)%ti_transp%vconv_eff +   &
       coretransp_NEOWES(1)%values(1)%ti_transp%vconv_eff

! map the vtor transport --- for the moment use 2/3 ion heat contributions
  coretransp_ITER(1)%values(1)%vtor_transp%diff_eff=coretransp_ITER(1)%values(1)%ti_transp%diff_eff * 2.0_R8/3.0_R8
  coretransp_ITER(1)%values(1)%vtor_transp%vconv_eff=coretransp_ITER(1)%values(1)%ti_transp%vconv_eff * 2.0_R8/3.0_R8

!  write(*,*) CORETRANSP_etaigb(1)%values(1)%rho_tor
!  write(*,*) NEOCLASSIC_neowes(1)%rho_tor
!  write(*,*) CORETRANSP_ITER(1)%VALUES(1)%rho_tor

!  write(*,*) coretransp_etaigb(1)%values(1)%te_transp%diff_eff
!  write(*,*) neoclassic_neowes(1)%te_neo%diff_eff
!  write(*,*) coretransp_ITER(1)%values(1)%te_transp%diff_eff

  WRITE(*,*) 'coretransp_etaigb(1)%values(1)%te_transp%diff_eff ',  &
       MINVAL(coretransp_etaigb(1)%values(1)%te_transp%diff_eff),  &
       MAXVAL(coretransp_etaigb(1)%values(1)%te_transp%diff_eff)
  WRITE(*,*) 'coretransp_etaigb(1)%values(1)%te_transp%vconv_eff '  &
       ,MINVAL( coretransp_etaigb(1)%values(1)%te_transp%vconv_eff),  &
       MAXVAL( coretransp_etaigb(1)%values(1)%te_transp%vconv_eff)
  WRITE(*,*) 'neoclassic_neowes(1)%te_neo%diff_eff ',  &
       MINVAL(neoclassic_neowes(1)%te_neo%diff_eff),  &
       MAXVAL(neoclassic_neowes(1)%te_neo%diff_eff)
  WRITE(*,*) 'neoclassic_neowes(1)%te_neo%vconv_eff ',  &
       MINVAL(neoclassic_neowes(1)%te_neo%vconv_eff),  &
       MAXVAL(neoclassic_neowes(1)%te_neo%vconv_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%te_transp%diff_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%te_transp%diff_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%te_transp%diff_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%te_transp%vconv_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%te_transp%vconv_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%te_transp%vconv_eff)
  WRITE(*,*) 'coretransp_etaigb(1)%values(1)%ti_transp%diff_eff ',  &
       MINVAL(coretransp_etaigb(1)%values(1)%ti_transp%diff_eff),  &
       MAXVAL(coretransp_etaigb(1)%values(1)%ti_transp%diff_eff)
  WRITE(*,*) 'coretransp_etaigb(1)%values(1)%ti_transp%vconv_eff ',  &
       MINVAL( coretransp_etaigb(1)%values(1)%ti_transp%vconv_eff),  &
       MAXVAL( coretransp_etaigb(1)%values(1)%ti_transp%vconv_eff)
  WRITE(*,*) 'neoclassic_neowes(1)%ti_neo%diff_eff ',  &
       MINVAL(neoclassic_neowes(1)%ti_neo%diff_eff),  &
       MAXVAL(neoclassic_neowes(1)%ti_neo%diff_eff)
  WRITE(*,*) 'neoclassic_neowes(1)%ti_neo%vconv_eff ',  &
       MINVAL(neoclassic_neowes(1)%ti_neo%vconv_eff),  &
       MAXVAL(neoclassic_neowes(1)%ti_neo%vconv_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%ti_transp%diff_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%ti_transp%diff_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%ti_transp%diff_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%ti_transp%vconv_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%ti_transp%vconv_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%ti_transp%vconv_eff)
  WRITE(*,*) 'coretransp_etaigb(1)%values(1)%ni_transp%diff_eff ',  &
       MINVAL(coretransp_etaigb(1)%values(1)%ni_transp%diff_eff),  &
       MAXVAL(coretransp_etaigb(1)%values(1)%ni_transp%diff_eff)
  WRITE(*,*) 'coretransp_etaigb(1)%values(1)%ni_transp%vconv_eff ',  &
       MINVAL( coretransp_etaigb(1)%values(1)%ni_transp%vconv_eff),  &
       MAXVAL( coretransp_etaigb(1)%values(1)%ni_transp%vconv_eff)
  WRITE(*,*) 'neoclassic_neowes(1)%ni_neo%diff_eff ',  &
       MINVAL(neoclassic_neowes(1)%ni_neo%diff_eff),  &
       MAXVAL(neoclassic_neowes(1)%ni_neo%diff_eff)
  WRITE(*,*) 'neoclassic_neowes(1)%ni_neo%vconv_eff ',  &
       MINVAL(neoclassic_neowes(1)%ni_neo%vconv_eff),  &
       MAXVAL(neoclassic_neowes(1)%ni_neo%vconv_eff)
  WRITE(*,*) 'coretransp_NEOWES(1)%values(1)%ni_transp%diff_eff ',  &
       MINVAL(coretransp_NEOWES(1)%values(1)%ni_transp%diff_eff),  &
       MAXVAL(coretransp_NEOWES(1)%values(1)%ni_transp%diff_eff)
  WRITE(*,*) 'coretransp_NEOWES(1)%values(1)%ni_transp%vconv_eff ',  &
       MINVAL(coretransp_NEOWES(1)%values(1)%ni_transp%vconv_eff),  &
       MAXVAL(coretransp_NEOWES(1)%values(1)%ni_transp%vconv_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%ni_transp%diff_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%ni_transp%diff_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%ni_transp%diff_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%ni_transp%vconv_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%ni_transp%vconv_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%ni_transp%vconv_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%vtor_transp%diff_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%vtor_transp%diff_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%vtor_transp%diff_eff)
  WRITE(*,*) 'coretransp_ITER(1)%values(1)%vtor_transp%vconv_eff ',  &
       MINVAL(coretransp_ITER(1)%values(1)%vtor_transp%vconv_eff),  &
       MAXVAL(coretransp_ITER(1)%values(1)%vtor_transp%vconv_eff)

  CALL euitm_deallocate(CORETRANSP_etaigb)
  CALL euitm_deallocate(CORETRANSP_neowes)
  CALL euitm_deallocate(NEOCLASSIC_neowes)

  RETURN

END SUBROUTINE external_transport
