! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module for manufacture of a test case for the ETS
!>
!> \author R.Stankiewicz, D.Kalupin
!>
!> \version "$Id: analytics1.f90 1627 2014-11-18 13:11:37Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE ANALYTICS1

CONTAINS




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine manufactures the solution for the set of 
!> transport equations describing the main plasma. It 
!> provides sources and transport coefficients for all 
!> transport equations.
!>
!> \author R.Stankiewicz, D.Kalupin
!>
!> \version "$Id: analytics1.f90 1627 2014-11-18 13:11:37Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ANALYTICAL_PLASMA (                                            &
       TIME,COREPROF_in,                                                    &
       EQUILIBRIUM,COREPROF_ANALYTIC,CORETRANSP,CORESOURCE,COREIMPUR, code_parameters)       


!-------------------------------------------------------!
!     This routine manufactures the solution for the    !
!     set of transport equations describing the main    !
!     plasma. It provides sources and transport         !
!     coefficients for all transport equations.         !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   R.Stankiewicz, D.Kalupin            !
!     Kontacts:     Roman.Stankiewich@gmail.com         !
!                   D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


    USE itm_types
    USE EUITM_SCHEMAS
    USE ETS_PLASMA
    USE ITM_TEST_ROUTINES
    USE ALLOCATE_DEALLOCATE
    USE COPY_CPO_ETS
    USE is_set_structures
    USE COPY_STRUCTURES
!!    use euitm_printcpo

    IMPLICIT NONE


    INTEGER, SAVE                    :: SHOT_NO             !shot mumber
    INTEGER, SAVE                    :: RUN_NO              !run mumber
    
    INTEGER, SAVE                    :: NRHO                !number of radial points    (input)
    INTEGER, SAVE                    :: NNUCL               !number of ion species      (input)
    INTEGER, SAVE                    :: NNEUT               !number of ion species      (input)
    INTEGER, SAVE                    :: NION                !number of ion species      (input)
    INTEGER, SAVE                    :: NIMP                !number of impurity species (input)
    INTEGER, ALLOCATABLE, SAVE       :: NZIMP(:)            !number of charge states for each impurity (input)
    INTEGER, ALLOCATABLE, SAVE       :: NCOMP(:)            !number of charge states for each impurity (input)
    INTEGER, ALLOCATABLE, SAVE       :: NTYPE(:)            !number of charge states for each impurity (input)
    INTEGER, SAVE                    :: NPOINTS             !number of charge states for each impurity (input)
    INTEGER, SAVE                    :: NTIME               !number of time points      (input)
    INTEGER,               PARAMETER :: NOCUR = 1           !number of CPO ocurancies in the work flow
    
    INTEGER                          :: IRHO                !current radial knot
    INTEGER                          :: IION                !current ion type
    INTEGER                          :: ITIME               !current time step
    
    INTEGER, SAVE                    :: NSOL                !Number of analytical example
    INTEGER, SAVE                    :: SOLVER_TYPE         !representation of transport equations 
    !1-"standard"; 2-"integral"(default)
    INTEGER, SAVE                    :: SIGMA_SOURCE         !origin of Plasma electrical conductivity
    !0: plasma collisions;1: transport module;2: source module
    
    REAL (R8), SAVE                  :: CONVREC             !required convergency 
    REAL (R8)                        :: TIME                !Time
    REAL (R8), SAVE                  :: TAU                 !time step, and mixing coefficient
    REAL (R8), SAVE                  :: AMIX                !mixing factor
    INTEGER                          :: ITER                !iteration index
    INTEGER, PARAMETER               :: MAXITER=1000        !maximum number of convergence iterations
    
    INTEGER, SAVE                    :: PSI_BND_TYPE        !Type of boundary conditions current
    INTEGER, SAVE                    :: NI_BND_TYPE         !Type of boundary conditions ion density 
    INTEGER, SAVE                    :: TI_BND_TYPE         !Type of boundary conditions ion temperature
    INTEGER, SAVE                    :: TE_BND_TYPE         !Type of boundary conditions electron temperature
    INTEGER, SAVE                    :: VTOR_BND_TYPE       !Type of boundary conditions toroidal rotation
    
    TYPE (RUN_CONTROL)               :: CONTROL             !contains all parameters required by run

    INTEGER                          :: ifail


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),POINTER        :: EQUILIBRIUM(:)       !input CPO with geometry
    TYPE (TYPE_COREPROF),   POINTER        :: COREPROF_in(:)       !CPO with internal ETS parameters profiles from previous time step
    TYPE (TYPE_COREPROF),   POINTER        :: COREPROF_ANALYTIC(:) !CPO with profiles
    TYPE (TYPE_CORETRANSP), POINTER        :: CORETRANSP(:)        !CPO with transport coefficients
    TYPE (TYPE_CORESOURCE), POINTER        :: CORESOURCE(:)        !CPO with sources
    TYPE (TYPE_COREIMPUR),  POINTER        :: COREIMPUR(:)         !CPO with impurities
    TYPE (type_param) :: code_parameters


! +++ Local derived types:
    TYPE (MAGNETIC_GEOMETRY)               :: GEOMETRY1         !contains all geometry quantities
    TYPE (PLASMA_PROFILES)                 :: PROFILES1         !contains profiles of plasma parameters
    TYPE (TRANSPORT_COEFFICIENTS)          :: TRANSPORT1        !contains profiles of trasport coefficients
    TYPE (SOURCES_AND_SINKS)               :: SOURCES1          !contains profiles of sources

    CHARACTER (len=32)               :: database_format

    LOGICAL, SAVE :: first=.TRUE.

! +++ Set up dimensions:

    WRITE(6,*) time
!!    call is_set_cpo(coreprof_in(1),'coreprof_in')
!!!    flush(6)

    IF(first) THEN
       IF (.NOT. ASSOCIATED(code_parameters%parameters)) THEN
          WRITE(6, *) 'ERROR: code parameters not associated!'
          STOP
       END IF
       CALL PROCESS_XML(SOLVER_TYPE,SIGMA_SOURCE,TAU,AMIX,CONVREC,NRHO,NION,NIMP,NZIMP,NTIME,NSOL,      &
            PSI_BND_TYPE,NI_BND_TYPE,TI_BND_TYPE,TE_BND_TYPE,VTOR_BND_TYPE,                &
            shot_no, run_no, code_parameters, database_format)
       first=.FALSE.
    ENDIF
    
    NPOINTS = 101
    NNUCL = NION + NIMP
    NNEUT = NION + NIMP
    ALLOCATE (NTYPE(NNEUT)) 
    ALLOCATE (NCOMP(NNEUT))
    NTYPE = 1
    NCOMP = 1

    CALL ALLOCATE_EQUILIBRIUM_CPO  (NOCUR,  NRHO,  NRHO,  101,   NPOINTS,                           EQUILIBRIUM      )         
        
    CALL ALLOCATE_COREPROF_CPO     (NOCUR,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREPROF_ANALYTIC    )
    CALL ALLOCATE_CORETRANSP_CPO   (NOCUR,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORETRANSP  )        
    CALL ALLOCATE_CORESOURCE_CPO   (NOCUR,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, CORESOURCE  )

    if(nimp .GT. 0) &
    CALL ALLOCATE_COREIMPUR_CPO    (NOCUR,  NRHO,  NNUCL, NION,  NIMP,  NZIMP, NNEUT, NTYPE, NCOMP, COREIMPUR   )        


    CALL copy_codeparam            (COREPROF_IN(1)%codeparam, COREPROF_ANALYTIC(1)%codeparam)
    call deallocate_cpo(COREPROF_ANALYTIC(1)%COMPOSITIONS)
    CALL COPY_CPO                  (COREPROF_IN(1)%COMPOSITIONS,        COREPROF_ANALYTIC(1)%COMPOSITIONS)
    call deallocate_cpo(CORETRANSP(1)%COMPOSITIONS)
    CALL COPY_CPO                  (COREPROF_IN(1)%COMPOSITIONS,        CORETRANSP(1)%COMPOSITIONS)
    call deallocate_cpo(CORESOURCE(1)%COMPOSITIONS)
    CALL COPY_CPO                  (COREPROF_IN(1)%COMPOSITIONS,        CORESOURCE(1)%COMPOSITIONS)

! +++ Set up parameters for the test work flow:
    CALL perfon ('setcpo')
    CALL SET_CPO  (NRHO,NION,NIMP,NZIMP,NTIME,NSOL,                               &
         PSI_BND_TYPE,NI_BND_TYPE,TI_BND_TYPE,TE_BND_TYPE,VTOR_BND_TYPE,          &
         EQUILIBRIUM, COREPROF_IN, CORETRANSP)
    CALL perfoff
    

! +++ Allocation of local derived types:
    CALL ALLOCATE_MAGNETIC_GEOMETRY        (NRHO,       GEOMETRY1,  ifail)
    CALL ALLOCATE_PLASMA_PROFILES          (NRHO,NION,  PROFILES1,  ifail)
    CALL ALLOCATE_TRANSPORT_COEFFICIENTS   (NRHO,NION,  TRANSPORT1, ifail)
    CALL ALLOCATE_SOURCES_AND_SINKS        (NRHO,NION,  SOURCES1,   ifail)



! +++ Copy CPOs in local derived types:
    CALL CONVERT_CPO_TO_LOCAL_TYPES        (COREPROF_in,  PROFILES1, TRANSPORT1)



! +++ Call MAIN_PLASMA with internal ETS derived types:
    CALL ANALYTICS_FULL                    (TIME, GEOMETRY1, PROFILES1, TRANSPORT1, SOURCES1) 



! +++ Copy local derived types in CPOs:
    CALL CONVERT_LOCAL_TO_CPO_TYPES        (GEOMETRY1,   PROFILES1, TRANSPORT1, SOURCES1,    &
         EQUILIBRIUM, COREPROF_ANALYTIC,  CORETRANSP, CORESOURCE)


! +++ Deallocation of ETS derived types:
    CALL DEALLOCATE_MAGNETIC_GEOMETRY      (GEOMETRY1,  ifail)
    CALL DEALLOCATE_PLASMA_PROFILES        (PROFILES1,  ifail)
    CALL DEALLOCATE_TRANSPORT_COEFFICIENTS (TRANSPORT1, ifail)
    CALL DEALLOCATE_SOURCES_AND_SINKS      (SOURCES1,   ifail)


    WRITE(6,*) 'EQUILIBRIUM'
    IF(.NOT.ASSOCIATED(EQUILIBRIUM(1)%codeparam%codename)) THEN
       ALLOCATE(EQUILIBRIUM(1)%codeparam%codename(1))   ! For a string of 132 characters max.
       EQUILIBRIUM(1)%codeparam%codename(1)   = 'ANALYTIC_SOLVER_TEST'
    ENDIF
    IF(.NOT.ASSOCIATED(EQUILIBRIUM(1)%codeparam%codeversion)) THEN
       ALLOCATE(EQUILIBRIUM(1)%codeparam%codeversion(1))   ! For a string of 132 characters max.
       EQUILIBRIUM(1)%codeparam%codeversion(1)   = '?'
    ENDIF
    IF(.NOT.ASSOCIATED(EQUILIBRIUM(1)%codeparam%parameters)) THEN
       ALLOCATE(EQUILIBRIUM(1)%codeparam%parameters(1))   ! For a string of 132 characters max.
       EQUILIBRIUM(1)%codeparam%parameters(1) = 'my_code_specific_parameters'
    ENDIF
    IF(.NOT.ASSOCIATED(EQUILIBRIUM(1)%codeparam%output_diag)) THEN
       ALLOCATE(EQUILIBRIUM(1)%codeparam%output_diag(1))   ! For a string of 132 characters max.
       EQUILIBRIUM(1)%codeparam%output_diag(1) = 'my_output_diag'
    ENDIF
    EQUILIBRIUM(1)%codeparam%output_flag = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure
    EQUILIBRIUM(1)%time=TIME
!!    call printcpo(EQUILIBRIUM(1))
    WRITE(6,*) 'COREPROF_ANALYTIC'
    IF(.NOT.ASSOCIATED(COREPROF_ANALYTIC(1)%codeparam%codename)) THEN
       ALLOCATE(COREPROF_ANALYTIC(1)%codeparam%codename(1))   ! For a string of 132 characters max.
       COREPROF_ANALYTIC(1)%codeparam%codename(1)   = 'ANALYTIC_SOLVER_TEST'
    ENDIF
    IF(.NOT.ASSOCIATED(COREPROF_ANALYTIC(1)%codeparam%codeversion)) THEN
       ALLOCATE(COREPROF_ANALYTIC(1)%codeparam%codeversion(1))   ! For a string of 132 characters max.
       COREPROF_ANALYTIC(1)%codeparam%codeversion(1)   = '?'
    ENDIF
    IF(.NOT.ASSOCIATED(COREPROF_ANALYTIC(1)%codeparam%parameters)) THEN
       ALLOCATE(COREPROF_ANALYTIC(1)%codeparam%parameters(1))   ! For a string of 132 characters max.
       COREPROF_ANALYTIC(1)%codeparam%parameters(1) = 'my_code_specific_parameters'
    ENDIF
    IF(.NOT.ASSOCIATED(COREPROF_ANALYTIC(1)%codeparam%output_diag)) THEN
       ALLOCATE(COREPROF_ANALYTIC(1)%codeparam%output_diag(1))   ! For a string of 132 characters max.
       COREPROF_ANALYTIC(1)%codeparam%output_diag(1) = 'my_output_diag'
    ENDIF
    COREPROF_ANALYTIC(1)%codeparam%output_flag = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure
    COREPROF_ANALYTIC(1)%time=TIME
!!    call printcpo(COREPROF_ANALYTIC(1))
    WRITE(6,*) 'CORETRANSP'
    IF(.NOT.ASSOCIATED(CORETRANSP(1)%codeparam%codename)) THEN
       ALLOCATE(CORETRANSP(1)%codeparam%codename(1))   ! For a string of 132 characters max.
       CORETRANSP(1)%codeparam%codename(1)   = 'ANALYTIC_SOLVER_TEST'
    ENDIF
    IF(.NOT.ASSOCIATED(CORETRANSP(1)%codeparam%codeversion)) THEN
       ALLOCATE(CORETRANSP(1)%codeparam%codeversion(1))   ! For a string of 132 characters max.
       CORETRANSP(1)%codeparam%codeversion(1)   = '?'
    ENDIF
    IF(.NOT.ASSOCIATED(CORETRANSP(1)%codeparam%parameters)) THEN
       ALLOCATE(CORETRANSP(1)%codeparam%parameters(1))   ! For a string of 132 characters max.
       CORETRANSP(1)%codeparam%parameters(1) = 'my_code_specific_parameters'
    ENDIF
    IF(.NOT.ASSOCIATED(CORETRANSP(1)%codeparam%output_diag)) THEN
       ALLOCATE(CORETRANSP(1)%codeparam%output_diag(1))   ! For a string of 132 characters max.
       CORETRANSP(1)%codeparam%output_diag(1) = 'my_output_diag'
    ENDIF
    CORETRANSP(1)%codeparam%output_flag = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure
    CORETRANSP(1)%time=TIME
!!    call printcpo(CORETRANSP(1))
    WRITE(6,*) 'CORESOURCE'
    IF(.NOT.ASSOCIATED(CORESOURCE(1)%codeparam%codename)) THEN
       ALLOCATE(CORESOURCE(1)%codeparam%codename(1))   ! For a string of 132 characters max.
       CORESOURCE(1)%codeparam%codename(1)   = 'ANALYTIC_SOLVER_TEST'
    ENDIF
    IF(.NOT.ASSOCIATED(CORESOURCE(1)%codeparam%codeversion)) THEN
       ALLOCATE(CORESOURCE(1)%codeparam%codeversion(1))   ! For a string of 132 characters max.
       CORESOURCE(1)%codeparam%codeversion(1)   = '?'
    ENDIF
    IF(.NOT.ASSOCIATED(CORESOURCE(1)%codeparam%parameters)) THEN
       ALLOCATE(CORESOURCE(1)%codeparam%parameters(1))   ! For a string of 132 characters max.
       CORESOURCE(1)%codeparam%parameters(1) = 'my_code_specific_parameters'
    ENDIF
    IF(.NOT.ASSOCIATED(CORESOURCE(1)%codeparam%output_diag)) THEN
       ALLOCATE(CORESOURCE(1)%codeparam%output_diag(1))   ! For a string of 132 characters max.
       CORESOURCE(1)%codeparam%output_diag(1) = 'my_output_diag'
    ENDIF
    CORESOURCE(1)%codeparam%output_flag = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure
    CORESOURCE(1)%time=TIME
!!    call printcpo(CORESOURCE(1))
    if(nimp.gt.0) then
       WRITE(6,*) 'COREIMPUR'
       IF(.NOT.ASSOCIATED(COREIMPUR(1)%codeparam%codename)) THEN
          ALLOCATE(COREIMPUR(1)%codeparam%codename(1))   ! For a string of 132 characters max.
          COREIMPUR(1)%codeparam%codename(1)   = 'ANALYTIC_SOLVER_TEST'
       ENDIF
       IF(.NOT.ASSOCIATED(COREIMPUR(1)%codeparam%codeversion)) THEN
          ALLOCATE(COREIMPUR(1)%codeparam%codeversion(1))   ! For a string of 132 characters max.
          COREIMPUR(1)%codeparam%codeversion(1)   = '?'
       ENDIF
       IF(.NOT.ASSOCIATED(COREIMPUR(1)%codeparam%parameters)) THEN
          ALLOCATE(COREIMPUR(1)%codeparam%parameters(1))   ! For a string of 132 characters max.
          COREIMPUR(1)%codeparam%parameters(1) = 'my_code_specific_parameters'
       ENDIF
       IF(.NOT.ASSOCIATED(COREIMPUR(1)%codeparam%output_diag)) THEN
          ALLOCATE(COREIMPUR(1)%codeparam%output_diag(1))   ! For a string of 132 characters max.
          COREIMPUR(1)%codeparam%output_diag(1) = 'my_output_diag'
       ENDIF
       COREIMPUR(1)%codeparam%output_flag = 0   ! Integer output flag, 0 means the run was successful and can be used in the rest of the workflow, <0 means failure
       COREIMPUR(1)%time=TIME
!!    call printcpo(COREIMPUR(1))
    endif
    WRITE(6,*) 'Finished in ANALYTICAL_PLASMA'
!!!    flush(6)

    RETURN



  END SUBROUTINE ANALYTICAL_PLASMA

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine converts CPOs into the local derived types.  
!>
!> \author D.Kalupin
!>
!> \version "$Id: analytics1.f90 1627 2014-11-18 13:11:37Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE CONVERT_CPO_TO_LOCAL_TYPES  (COREPROF,  PROFILES1, TRANSPORT1)

!-------------------------------------------------------!
!     This routine converts CPOs into the local         !
!     derived types.                                    !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS

    USE ETS_PLASMA

    IMPLICIT NONE

! +++ CPO derived types:
    TYPE (TYPE_COREPROF),          POINTER     :: COREPROF(:)      !input CPO with plasma profiles
! +++ Local derived types:
    TYPE (PLASMA_PROFILES),        INTENT(INOUT) :: PROFILES1        !contains profiles of plasma parameters
    TYPE (TRANSPORT_COEFFICIENTS), INTENT(INOUT) :: TRANSPORT1       !contains profiles of trasport coefficients



! +++ Convert profiles:
    PROFILES1%ZION            = COREPROF(1)%composition%zion 
    PROFILES1%MION            = COREPROF(1)%composition%amn

    PROFILES1%PSI_BND_TYPE    = COREPROF(1)%psi%boundary%type

    PROFILES1%NI_BND_TYPE     = COREPROF(1)%ni%boundary%type

    PROFILES1%TI_BND_TYPE     = COREPROF(1)%ti%boundary%type

    PROFILES1%TE_BND_TYPE     = COREPROF(1)%te%boundary%type

    PROFILES1%VTOR_BND_TYPE   = COREPROF(1)%vtor%boundary%type



! +++ Convert transport:
    TRANSPORT1%C1(1)          = 0.0E0_R8                               
    TRANSPORT1%C1(2)          = 1.5E0_R8                               
    TRANSPORT1%C1(3)          = 2.5E0_R8                               




    RETURN


  END SUBROUTINE CONVERT_CPO_TO_LOCAL_TYPES


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  




! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine converts local into the CPOs derived types.
!>
!> \author D.Kalupin
!>
!> \version "$Id: analytics1.f90 1627 2014-11-18 13:11:37Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE CONVERT_LOCAL_TO_CPO_TYPES  (GEOMETRY1,   PROFILES1, TRANSPORT1, SOURCES1,    &
       EQUILIBRIUM, COREPROF,  CORETRANSP, CORESOURCE)

!-------------------------------------------------------!
!     This routine converts local into the CPOs         !
!     derived types.                                    !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   D.Kalupin                           !
!     Kontacts:     D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


    USE EUITM_SCHEMAS

    USE ETS_PLASMA

    USE itm_constants

    IMPLICIT NONE

! +++ Local derived types:
    TYPE (MAGNETIC_GEOMETRY),      INTENT(IN)  :: GEOMETRY1        !contains all geometry quantities
    TYPE (PLASMA_PROFILES),        INTENT(IN)  :: PROFILES1        !contains profiles of plasma parameters
    TYPE (TRANSPORT_COEFFICIENTS), INTENT(IN)  :: TRANSPORT1       !contains profiles of trasport coefficients
    TYPE (SOURCES_AND_SINKS),      INTENT(IN)  :: SOURCES1         !contains profiles of sources

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),       POINTER     :: EQUILIBRIUM(:)   !output CPO with geometry
    TYPE (TYPE_COREPROF),          POINTER     :: COREPROF(:)      !output CPO with plasma profiles
    TYPE (TYPE_CORETRANSP),        POINTER     :: CORETRANSP(:)    !output CPO with transport coefficients
    TYPE (TYPE_CORESOURCE),        POINTER     :: CORESOURCE(:)    !output CPO with sources



! +++ Convert geometry:
    EQUILIBRIUM(1)%profiles_1d%rho_tor     = GEOMETRY1%RHO
!!!DPC-EQ-4.09a
    EQUILIBRIUM(1)%profiles_1d%vprime      = GEOMETRY1%VPR
    EQUILIBRIUM(1)%profiles_1d%gm3         = GEOMETRY1%G1
    EQUILIBRIUM(1)%profiles_1d%gm8         = GEOMETRY1%G2     ! added DPC 2012-04-27
    EQUILIBRIUM(1)%profiles_1d%gm2         = GEOMETRY1%G3
    EQUILIBRIUM(1)%profiles_1d%F_dia       = GEOMETRY1%FDIA

    COREPROF(1)%rho_tor                    = GEOMETRY1%RHO
    COREPROF(1)%toroid_field%r0            = GEOMETRY1%RGEO 
    COREPROF(1)%toroid_field%b0            = GEOMETRY1%BGEO 

    CORETRANSP(1)%values(1)%rho_tor                  = GEOMETRY1%RHO
    CORESOURCE(1)%values(1)%rho_tor                  = GEOMETRY1%RHO



! +++ Convert profiles:
    COREPROF(1)%composition%zion           = PROFILES1%ZION   
    COREPROF(1)%composition%amn            = PROFILES1%MION 

    COREPROF(1)%psi%value                  = PROFILES1%PSI 
    COREPROF(1)%profiles1d%q%value         = PROFILES1%QSF
    COREPROF(1)%psi%boundary%value         = PROFILES1%PSI_BND
    COREPROF(1)%psi%boundary%type          = PROFILES1%PSI_BND_TYPE

    COREPROF(1)%ni%value                   = PROFILES1%NI
    COREPROF(1)%ni%boundary%value          = PROFILES1%NI_BND
    COREPROF(1)%ni%boundary%type           = PROFILES1%NI_BND_TYPE
!DPC
!    COREPROF(1)%ni%transp_coef%diff        = sum(TRANSPORT1%DIFF_NI,dim=3)
!    COREPROF(1)%ni%transp_coef%vconv       = sum(TRANSPORT1%VCONV_NI,dim=3)
!DPC.end

    COREPROF(1)%ne%value                   = PROFILES1%NE
!DPC
!    COREPROF(1)%ne%transp_coef%diff        = TRANSPORT1%DIFF_NI
!    COREPROF(1)%ne%transp_coef%vconv       = TRANSPORT1%VCONV_NI
!DPC.end

    COREPROF(1)%ti%value                   = PROFILES1%TI
    COREPROF(1)%ti%boundary%value          = PROFILES1%TI_BND
    COREPROF(1)%ti%boundary%type           = PROFILES1%TI_BND_TYPE
!DPC
    COREPROF(1)%ti%transp_coef%diff        = TRANSPORT1%DIFF_TI
    COREPROF(1)%ti%transp_coef%vconv       = TRANSPORT1%VCONV_TI
!DPC.end

    COREPROF(1)%te%value                   = PROFILES1%TE
    COREPROF(1)%te%boundary%value          = PROFILES1%TE_BND
    COREPROF(1)%te%boundary%type           = PROFILES1%TE_BND_TYPE
!DPC
    COREPROF(1)%te%transp_coef%diff        = TRANSPORT1%DIFF_TE
    COREPROF(1)%te%transp_coef%vconv       = TRANSPORT1%VCONV_TE
!DPC.end

    COREPROF(1)%vtor%value                 = PROFILES1%VTOR
    COREPROF(1)%vtor%boundary%value        = PROFILES1%VTOR_BND
    COREPROF(1)%vtor%boundary%type         = PROFILES1%VTOR_BND_TYPE
!DPC
    COREPROF(1)%vtor%transp_coef%diff      = TRANSPORT1%DIFF_VTOR
    COREPROF(1)%vtor%transp_coef%vconv     = TRANSPORT1%VCONV_VTOR
!DPC.end

!DPC set fluxes to 0 (otherwise we write garbage)
    COREPROF(1)%ni%flux%flux_dv            = 0.0_R8
    COREPROF(1)%ne%flux%flux_dv            = 0.0_R8
    COREPROF(1)%ti%flux%flux_dv            = 0.0_R8
    COREPROF(1)%te%flux%flux_dv            = 0.0_R8
    COREPROF(1)%vtor%flux%flux_dv          = 0.0_R8
!DPC.end

! +++ Convert transport:
    CORETRANSP(1)%values(1)%sigma                    = TRANSPORT1%SIGMA

    CORETRANSP(1)%values(1)%te_transp%diff_eff       = TRANSPORT1%DIFF_TE
    CORETRANSP(1)%values(1)%te_transp%vconv_eff      = TRANSPORT1%VCONV_TE

    CORETRANSP(1)%values(1)%ni_transp%diff_eff       = TRANSPORT1%DIFF_NI 
    CORETRANSP(1)%values(1)%ni_transp%vconv_eff      = TRANSPORT1%VCONV_NI

    CORETRANSP(1)%values(1)%ti_transp%diff_eff       = TRANSPORT1%DIFF_TI 
    CORETRANSP(1)%values(1)%ti_transp%vconv_eff      = TRANSPORT1%VCONV_TI 

    CORETRANSP(1)%values(1)%vtor_transp%diff_eff     = TRANSPORT1%DIFF_VTOR 
    CORETRANSP(1)%values(1)%vtor_transp%vconv_eff    = TRANSPORT1%VCONV_VTOR



! +++ Convert sources:
    CORESOURCE(1)%VALUES(1)%j                        = SOURCES1%CURR_EXP

    CORESOURCE(1)%VALUES(1)%qe%exp                   = SOURCES1%QE_EXP * itm_ev    !! DPC 2009-07-02
    CORESOURCE(1)%VALUES(1)%qe%imp                   = SOURCES1%QE_IMP

    CORESOURCE(1)%VALUES(1)%si%exp                   = SOURCES1%SI_EXP
    CORESOURCE(1)%VALUES(1)%si%imp                   = SOURCES1%SI_IMP

    CORESOURCE(1)%VALUES(1)%qi%exp                   = SOURCES1%QI_EXP * itm_ev    !! DPC 2009-07-02
    CORESOURCE(1)%VALUES(1)%qi%imp                   = SOURCES1%QI_IMP

    CORESOURCE(1)%VALUES(1)%ui%exp                   = SOURCES1%UI_EXP
    CORESOURCE(1)%VALUES(1)%ui%imp                   = SOURCES1%UI_IMP



    RETURN


  END SUBROUTINE CONVERT_LOCAL_TO_CPO_TYPES


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This routine manufactures the solution for the
!> set of transport equations describing the main
!> plasma. It provides sources and transport
!> coefficients for all transport equations. 
!>
!> \author R.Stankiewicz, D.Kalupin
!>
!> \version "$Id: analytics1.f90 1627 2014-11-18 13:11:37Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE ANALYTICS_FULL  (TIME, GEOMETRY1, PROFILES1, TRANSPORT1, SOURCES1)   


!-------------------------------------------------------!
!     This routine manufactures the solution for the    !
!     set of transport equations describing the main    !
!     plasma. It provides sources and transport         !
!     coefficients for all transport equations.         !
!-------------------------------------------------------!
!     Source:       ---                                 !
!     Developers:   R.Stankiewicz, D.Kalupin            !
!     Kontacts:     Roman.Stankiewich@gmail.com         !
!                   D.Kalupin@fz-juelich.de             !
!                                                       !
!     Comments:     ---                                 !
!                                                       !
!-------------------------------------------------------!


    USE itm_types
    USE ETS_PLASMA
    USE ANALYTICAL_FUNCTIONS

    IMPLICIT NONE

    REAL (R8)           :: TIME                                  !Time


    INTEGER             :: NRHO, IRHO                            !number of radial points (input) and current radial knot
    INTEGER             :: NION, IION                            !number of ion species   (input) and current ion type
    INTEGER             :: SIGMA_SOURCE
    INTEGER             :: ifail


! +++ Local derived types:
    TYPE (MAGNETIC_GEOMETRY),      INTENT(INOUT)    :: GEOMETRY1   !contains all geometry quantities
    TYPE (PLASMA_PROFILES),        INTENT(INOUT)    :: PROFILES1   !contains profiles of plasma parameters
    TYPE (TRANSPORT_COEFFICIENTS), INTENT(INOUT)    :: TRANSPORT1  !contains profiles of trasport coefficients
    TYPE (SOURCES_AND_SINKS),      INTENT(INOUT)    :: SOURCES1    !contains profiles of sources
    TYPE (COLLISIONALITY)                           :: COLLISIONS1 !contains all terms determined by plasma collisions


! +++ Local variables:
!     Geometry:
    REAL (R8)           :: RHO(PROFILES1%NRHO),       HRHO(PROFILES1%NRHO)
    REAL (R8)           :: FDIA(PROFILES1%NRHO),      VPR(PROFILES1%NRHO)
    REAL (R8)           :: G1(PROFILES1%NRHO),        G2(PROFILES1%NRHO)
    REAL (R8)           :: G3(PROFILES1%NRHO)

!     Plasma composition:
    REAL (R8)           :: MION(PROFILES1%NION),      ZION(PROFILES1%NION)

!     Plasma profiles:
    REAL (R8)           :: PSI(PROFILES1%NRHO) 
    REAL (R8)           :: NE(PROFILES1%NRHO)
    REAL (R8)           :: NI(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: TE(PROFILES1%NRHO) 
    REAL (R8)           :: TI(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: VTOR(PROFILES1%NRHO,PROFILES1%NION)

!     Transport coefficients:
    REAL (R8)           :: SIGMA(PROFILES1%NRHO)
    REAL (R8)           :: DIFF_NI(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: VCONV_NI(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: DIFF_TI(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: VCONV_TI(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: DIFF_TE(PROFILES1%NRHO)
    REAL (R8)           :: VCONV_TE(PROFILES1%NRHO)
    REAL (R8)           :: DIFF_VTOR(PROFILES1%NRHO,PROFILES1%NION)
    REAL (R8)           :: VCONV_VTOR(PROFILES1%NRHO,PROFILES1%NION)

!     Sources:
    REAL (R8)           :: VIE(PROFILES1%NRHO),       VEI(PROFILES1%NRHO)
    REAL (R8)           :: QIE(PROFILES1%NRHO),       QEI(PROFILES1%NRHO)
    REAL (R8)           :: VZI(PROFILES1%NRHO),       UZI(PROFILES1%NRHO)
    REAL (R8)           :: QZI(PROFILES1%NRHO),       WZI(PROFILES1%NRHO)

!     Other quantities:
    REAL (R8)           :: DNE(PROFILES1%NRHO), DTNE(PROFILES1%NRHO)
    REAL (R8)           :: C1
    REAL (R8)           :: BT, R                                  


! +++ Functions:
    REAL (R8)           :: X, T
    REAL (R8)           :: AVAL1, AVAL2, AVAL3, AVAL4, AVAL5, AVAL6, AVAL7
    REAL (R8)           :: GAMMA(PROFILES1%NION,PROFILES1%NRHO)
    REAL (R8)           :: DGAMMA(PROFILES1%NION,PROFILES1%NRHO)
    REAL (R8)           :: GAMMAE(PROFILES1%NRHO)
    REAL (R8)           :: DGAMMAE(PROFILES1%NRHO)


! +++ Set up dimensions:
    NRHO         = SIZE(PROFILES1%NI)
    NION         = SIZE(PROFILES1%MION)
    C1           = TRANSPORT1%C1(1)
    SIGMA_SOURCE = 0


    CALL ALLOCATE_COLLISIONALITY  (NRHO, NION, COLLISIONS1, ifail)




!=================================================================================      
! +++ MAGNETIC GEOMETRY:       

!--------------------------------------------------------------------------------!
!     This part sets up the geometry used by analytical solution.                !
!--------------------------------------------------------------------------------!

    T                       = TIME 
    R                       = 1.0E0_R8
    BT                      = ABT(T)


    DO IRHO=1,NRHO
       X                     = ARHO(IRHO,NRHO)
       RHO(IRHO)             = X
       VPR(IRHO)             = AVPR(X,T)
       G1(IRHO)              = AG1(X,T)
       G2(IRHO)              = AG2(X,T)
       G3(IRHO)              = AG3(X,T)
       FDIA(IRHO)            = AFDIA(X,T)

       GEOMETRY1%RHO(IRHO)   = X
       GEOMETRY1%VPR(IRHO)   = VPR(IRHO)  
       GEOMETRY1%G1(IRHO)    = G1(IRHO)  
       GEOMETRY1%G2(IRHO)    = G2(IRHO)  
       GEOMETRY1%G3(IRHO)    = G3(IRHO)  
       GEOMETRY1%FDIA(IRHO)  = FDIA (IRHO)
    ENDDO

    GEOMETRY1%RGEO           = R
    GEOMETRY1%BGEO           = BT






!=================================================================================      
! +++ PROFILES OF PLASMA PARAMETERS:

!--------------------------------------------------------------------------------!
!     This part describes analytical profiles for all plasma parameters.         !
!--------------------------------------------------------------------------------!

    T                             = TIME

    DO IION=1,NION
       ZION(IION)                  = PROFILES1%ZION(IION)
       MION(IION)                  = PROFILES1%MION(IION)!*MP
    ENDDO


    DO IRHO=1,NRHO
       X                           = RHO(IRHO)
       TE(IRHO)                    = ATE(X,T)
       PSI(IRHO)                   = APSI(X,T)

       PROFILES1%TE(IRHO)          = TE(IRHO)
       PROFILES1%PSI(IRHO)         = PSI(IRHO)
       PROFILES1%NE(IRHO)          = 0.e0_R8

       DO IION=1,NION
          TI(IRHO,IION)             = ATI(IION,X,T)
          NI(IRHO,IION)             = ANI(IION,X,T)
          VTOR(IRHO,IION)           = AVTOR(IION,X,T)

          PROFILES1%NE(IRHO)        = PROFILES1%NE(IRHO)+PROFILES1%ZION(IION)*NI(IRHO,IION)
          PROFILES1%NI(IRHO,IION)   = NI(IRHO,IION)
          PROFILES1%TI(IRHO,IION)   = TI(IRHO,IION)
          PROFILES1%VTOR(IRHO,IION) = VTOR(IRHO,IION)
       ENDDO

    ENDDO






!=================================================================================       
! +++ CALCULATING COLLISIONAL TERMS:
    CALL PLASMA_COLLISIONS (GEOMETRY1,PROFILES1,COLLISIONS1,ifail) 






!=================================================================================       
! +++ TRANSPORT_COEFFICIENT

!--------------------------------------------------------------------------------!
!     This part describes analytical profiles of transport coefficients.         !
!--------------------------------------------------------------------------------!

    DO IRHO=1,NRHO
       X                                   = RHO(IRHO)


       IF(SIGMA_SOURCE.EQ.1) THEN
          SIGMA(IRHO)                       = ASIGMA(X,T)

          TRANSPORT1%SIGMA(IRHO)            = SIGMA(IRHO)
       ENDIF


       IF(SIGMA_SOURCE.EQ.0) THEN
          SIGMA(IRHO)                       = COLLISIONS1%SIGMA(IRHO)
          TRANSPORT1%SIGMA(IRHO)            = SIGMA(IRHO)
       ENDIF

    ENDDO




    DO IRHO=1,NRHO
       X                                   = RHO(IRHO)
       DIFF_TE(IRHO)                       = AKATE(X,T)
       VCONV_TE(IRHO)                      = AVTE(X,T)

       TRANSPORT1%DIFF_TE(IRHO)            = DIFF_TE(IRHO)
       TRANSPORT1%VCONV_TE(IRHO)           = VCONV_TE(IRHO)


       DO IION=1,NION
          DIFF_NI(IRHO,IION)                = AD(IION,X,T)
          VCONV_NI(IRHO,IION)               = AV(IION,X,T)
          DIFF_TI(IRHO,IION)                = AKAPPAE(IION,X,T)
          VCONV_TI(IRHO,IION)               = AVTI(IION,X,T)
          DIFF_VTOR(IRHO,IION)              = ADVTOR(IION,X,T)
          VCONV_VTOR(IRHO,IION)             = ACONVTOR(IION,X,T)


          IF(C1.EQ.0.0E0_R8) THEN
             TRANSPORT1%DIFF_NI(IRHO,IION,1) = DIFF_NI(IRHO,IION)
             TRANSPORT1%VCONV_NI(IRHO,IION,1)= VCONV_NI(IRHO,IION)
          ELSE IF(C1.EQ.1.5E0_R8) THEN
             TRANSPORT1%DIFF_NI(IRHO,IION,2) = DIFF_NI(IRHO,IION)
             TRANSPORT1%VCONV_NI(IRHO,IION,2)= VCONV_NI(IRHO,IION)
          ELSE IF(C1.EQ.2.5E0_R8) THEN
             TRANSPORT1%DIFF_NI(IRHO,IION,3) = DIFF_NI(IRHO,IION)
             TRANSPORT1%VCONV_NI(IRHO,IION,3)= VCONV_NI(IRHO,IION)
          END IF
          TRANSPORT1%DIFF_TI(IRHO,IION)     = DIFF_TI(IRHO,IION)
          TRANSPORT1%VCONV_TI(IRHO,IION)    = VCONV_TI(IRHO,IION)
          TRANSPORT1%DIFF_VTOR(IRHO,IION)   = DIFF_VTOR(IRHO,IION)        
          TRANSPORT1%VCONV_VTOR(IRHO,IION)  = VCONV_VTOR(IRHO,IION)
       ENDDO

    ENDDO



!=================================================================================        
! +++ SOURCES_AND_SINKS 
!--------------------------------------------------------------------------------!
!     CALCULATION OF EXPLICIT SOURCES FROM TRANSPORT EQUATION USING THE          !
!     ANALATICAL FORMULAE FOR SOLUTIONS AND TRANSPORT COEFFICIENTS               !
!--------------------------------------------------------------------------------!





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ CURRENT TRANSPORT:
    T                        = TIME

    DO IRHO=2,NRHO
       X                      = RHO(IRHO)

       AVAL1                  = SIGMA(IRHO)*DTAPSI(X,T)

       AVAL2                  = -X*DTABT(T)/2.0E0_R8/ABT(T)*DAPSI(X,T)*SIGMA(IRHO)
       AVAL3                  = FDIA(IRHO)**2/ITM_MU0/ABT(T)/X

       AVAL4                  = VPR(IRHO)/(4.0E0_R8*ITM_PI**2)*G3(IRHO)/FDIA(IRHO)
       AVAL5                  = DAVPR(X,T)/(4.0E0_R8*ITM_PI**2)*G3(IRHO)/FDIA(IRHO)      
       AVAL5                  = AVAL5+AVPR(X,T)/(4.0E0_R8*ITM_PI**2)*DAG3(X,T)/FDIA(IRHO)
       AVAL5                  = AVAL5-AVPR(X,T)/(4.0E0_R8*ITM_PI**2)*G3(IRHO)/FDIA(IRHO)**2*DAFDIA(X,T)
       AVAL1                  = AVAL1+AVAL2-AVAL3*AVAL4*DDAPSI(X,T)-AVAL3*AVAL5*DAPSI(X,T)


       AVAL1                  = AVAL1*(2.e0_R8*ITM_PI*X)/VPR(IRHO)+ACURR_IMP(X,T)*PSI(IRHO)


       SOURCES1%CURR_EXP(IRHO)= -AVAL1
       SOURCES1%CURR_IMP(IRHO)= ACURR_IMP(X,T)

    ENDDO



    SOURCES1%CURR_EXP(1)     = SOURCES1%CURR_EXP(2)
    SOURCES1%CURR_IMP(1)     = ACURR_IMP(0.e0_R8,T)



! +++ BOUNDARY CONDITION
    IF(PROFILES1%PSI_BND_TYPE.EQ.0) THEN
       DO IRHO=2,NRHO
          PROFILES1%QSF(IRHO)  = 2.e0_R8*ITM_PI*RHO(IRHO)*ABT(T)/DAPSI(RHO(IRHO),T)
       ENDDO

       PROFILES1%QSF(1)       = 2.e0_R8*ITM_PI*ABT(T)/DDAPSI(RHO(1),T)                
    ENDIF


    IF(PROFILES1%PSI_BND_TYPE.EQ.1) THEN
       PROFILES1%PSI_BND(1)    = PSI(NRHO)
    ENDIF

    IF(PROFILES1%PSI_BND_TYPE.EQ.2) THEN
       PROFILES1%PSI_BND(1)    = VPR(NRHO)*G3(NRHO)*DAPSI(RHO(NRHO),T)/4.e0_R8/ITM_PI**2/ITM_MU0
    ENDIF

    IF(PROFILES1%PSI_BND_TYPE.EQ.3) THEN
       PROFILES1%PSI_BND(1)    = DTAPSI(RHO(NRHO),T)
    ENDIF





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ PARTICLE TRANSPORT

    DO IION=1,NION
       T                             = TIME
       GAMMA(IION,1)                 = 0.0e0_R8               !DPC 2009-01-19
       DGAMMA(IION,1)                = 0.0e0_R8               !DPC 2009-01-19
       DO IRHO=2,NRHO
          X                           = RHO(IRHO)
          AVAL1                       = DTAVPR(X,T)*ANI(IION,X,T)+AVPR(X,T)*DTANI(IION,X,T)
          AVAL2                       = -DTABT(T)/2.e0_R8/ABT(T)*(AVPR(X,T)* ANI(IION,X,T)+ &
               X *(DAVPR(X,T)* ANI(IION,X,T)+AVPR(X,T)* DANI(IION,X,T)))
          GAMMA(IION,IRHO)            = AVPR(X,T)* AG1(X,T)*(-AD(IION,X,T)* DANI(IION,X,T)+ANI(IION,X,T)*AV(IION,X,T))
          DGAMMA(IION,IRHO)           = AVPR(X,T)* AG1(X,T)*(-AD(IION,X,T)* DDANI(IION,X,T)+DANI(IION,X,T)*AV(IION,X,T))  

          DGAMMA(IION,IRHO)           = DGAMMA(IION,IRHO)+ &
               DAVPR(X,T)* AG1(X,T)*(-AD(IION,X,T)* DANI(IION,X,T)+AV(IION,X,T)*ANI(IION,X,T))       
          DGAMMA(IION,IRHO)           = DGAMMA(IION,IRHO)+ &
               AVPR(X,T)* DAG1(X,T)*(-AD(IION,X,T)* DANI(IION,X,T)+AV(IION,X,T)*ANI(IION,X,T))         
          DGAMMA(IION,IRHO)           = DGAMMA(IION,IRHO)+ &
               AVPR(X,T)* AG1(X,T)*(-DAD(IION,X,T)* DANI(IION,X,T)+DAV(IION,X,T)*ANI(IION,X,T))           

          AVAL1                       = AVAL1+AVAL2+DGAMMA(IION,IRHO)+AVPR(X,T)*ANI(IION,X,T)*ASI_IMP(IION,X,T)
          SOURCES1%SI_EXP(IRHO,IION)  = AVAL1/AVPR(X,T)
          SOURCES1%SI_IMP(IRHO,IION)  = ASI_IMP(IION,X,T)
       ENDDO


       SOURCES1%SI_EXP(1,IION)       = SOURCES1%SI_EXP(2,IION)
       SOURCES1%SI_IMP(1,IION)       = ASI_IMP(IION,0.e0_R8,T)


! +++ BOUNDARY CONDITION
       IF (PROFILES1%NI_BND_TYPE(IION).EQ.1) THEN
          PROFILES1%NI_BND(1,IION)   = ANI(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%NI_BND_TYPE(IION).EQ.2) THEN
          PROFILES1%NI_BND(1,IION)   = -DANI(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%NI_BND_TYPE(IION).EQ.3) THEN
          PROFILES1%NI_BND(1,IION)   = -ANI(IION,RHO(NRHO),T)/DANI(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%NI_BND_TYPE(IION).EQ.4) THEN
          PROFILES1%NI_BND(1,IION)   = GAMMA(IION,NRHO)   
       ENDIF

    ENDDO





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ ELECTRON DENSISTY AND FLUXES
    DO IRHO =1,NRHO
       X                           = RHO(IRHO)
       NE(IRHO)                    = 0.e0_R8
       DNE(IRHO)                   = 0.e0_R8
       DTNE(IRHO)                  = 0.e0_R8
       GAMMAE(IRHO)                = 0.e0_R8
       DGAMMAE(IRHO)               = 0.e0_R8

       DO IION=1,NION 
          NE(IRHO)                  = NE(IRHO) + ZION(IION)*NI(IRHO,IION)
          DTNE(IRHO)                = DTNE(IRHO) + ZION(IION)*DTANI(IION,X,T)
          DNE(IRHO)                 = DNE(IRHO) + ZION(IION)*DANI(IION,X,T)
          GAMMAE(IRHO)              = GAMMAE(IRHO) + GAMMA(IION,IRHO)*ZION(IION)
          DGAMMAE(IRHO)             = DGAMMAE(IRHO) + DGAMMA(IION,IRHO)*ZION(IION)
       ENDDO

    ENDDO





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ ION ENERGY TRANSPORT
    DO IION=1,NION
       T                            = TIME

       DO IRHO=2,NRHO
          X                          = RHO(IRHO)
          AVAL1                      = DTAVPR(X,T)*AVPR(X,T)**(2.e0_R8/3.e0_R8)*ANI(IION,X,T)*ATI(IION,X,T)*5.e0_R8/3.e0_R8
          AVAL1                      = AVAL1+AVPR(X,T)**(5.e0_R8/3.e0_R8)*DTANI(IION,X,T)*ATI(IION,X,T)
          AVAL1                      = AVAL1+AVPR(X,T)**(5.e0_R8/3.e0_R8)*ANI(IION,X,T)*DTATI(IION,X,T)

          AVAL2                      = -DTABT(T)/2.e0_R8/ABT(T)

          AVAL3                      = ANI(IION,X,T)*ATI(IION,X,T)*AVPR(X,T)**(5.e0_R8/3.e0_R8)
          AVAL3                      = AVAL3+ X*DANI(IION,X,T)*ATI(IION,X,T)*AVPR(X,T)**(5.e0_R8/3.e0_R8)
          AVAL3                      = AVAL3+ X*ANI(IION,X,T)*DATI(IION,X,T)*AVPR(X,T)**(5.e0_R8/3.e0_R8)
          AVAL3                      = AVAL3+ X*ANI(IION,X,T)*ATI(IION,X,T)*AVPR(X,T)**(2.e0_R8/3.e0_R8)*DAVPR(X,T)*5.e0_R8/3.e0_R8

          AVAL1                      = AVAL1+AVAL2*AVAL3
          AVAL1                      = AVAL1*3.e0_R8/2.e0_R8

          AVAL2                      = AVPR(X,T)*AG1(X,T)*ANI(IION,X,T)                                    

          AVAL3                      = -AKAPPAE(IION,X,T)*DATI(IION,X,T)+ATI(IION,X,T)*AVTI(IION,X,T)

          AVAL4                      = DAVPR(X,T)*AG1(X,T)*ANI(IION,X,T)+ &
               AVPR(X,T)*DAG1(X,T)*ANI(IION,X,T)+AVPR(X,T)*AG1(X,T)*DANI(IION,X,T)

          AVAL5                      = -DAKAPPAE(IION,X,T)*DATI(IION,X,T)+ATI(IION,X,T)*DAVTI(IION,X,T)
          AVAL5                      = AVAL5-AKAPPAE(IION,X,T)*DDATI(IION,X,T)+DATI(IION,X,T)*AVTI(IION,X,T)

          AVAL6                      = AVAL2*AVAL5+AVAL4*AVAL3

          AVAL1                      = AVAL1+AVAL6*AVPR(X,T)**(2.e0_R8/3.e0_R8)


          AVAL2                      = DATI(IION,X,T)*GAMMA(IION,IRHO)+ATI(IION,X,T)*DGAMMA(IION,IRHO)

          AVAL1                      = AVAL1+C1*AVAL2*AVPR(X,T)**(2.e0_R8/3.e0_R8)
          AVAL1                      = AVAL1/AVPR(X,T)**(5.e0_R8/3.e0_R8)

          VEI(IRHO)                  = COLLISIONS1%VEI(IRHO,IION)
          QEI(IRHO)                  = COLLISIONS1%QEI(IRHO,IION)
          VZI(IRHO)                  = COLLISIONS1%VZI(IRHO,IION)
          QZI(IRHO)                  = COLLISIONS1%QZI(IRHO,IION)

          AVAL2                      = (AQI_IMP(IION,X,T) + VEI(IRHO) + VZI(IRHO) )                                         

          SOURCES1%QI_EXP(IRHO,IION) = AVAL1- QEI(IRHO) - QZI(IRHO) +AVAL2*ATI(IION,X,T)
          SOURCES1%QI_IMP(IRHO,IION) = AQI_IMP(IION,X,T)
       ENDDO

       SOURCES1%QI_EXP(1,IION)      = SOURCES1%QI_EXP(2,IION)
       SOURCES1%QI_IMP(1,IION)      = AQI_IMP(IION,0.e0_R8,T)
    ENDDO




! +++ BOUNDARY CONDITION
    DO IION=1,NION
       IF (PROFILES1%TI_BND_TYPE(IION).EQ.1)THEN
          PROFILES1%TI_BND(1,IION)  = ATI(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%TI_BND_TYPE(IION).EQ.2)THEN
          PROFILES1%TI_BND(1,IION)  = -DATI(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%TI_BND_TYPE(IION).EQ.3)THEN
          PROFILES1%TI_BND(1,IION)  = -ATI(IION,RHO(NRHO),T)/DATI(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%TI_BND_TYPE(IION).EQ.4)THEN
          X                         = RHO(NRHO)
          AVAL2                     = AVPR(X,T)*AG1(X,T)*ANI(IION,X,T) 
          AVAL3                     = (-AKAPPAE(IION,X,T)*DATI(IION,X,T)+ATI(IION,X,T)*AVTI(IION,X,T))* &
               AVAL2+C1*ATI(IION,X,T)*GAMMA(IION,NRHO)
          PROFILES1%TI_BND(1,IION)  = AVAL3   
       ENDIF

    ENDDO






! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ ELECTRON ENERGY

    T                              = TIME

    DO IRHO=2,NRHO
       X                             = RHO(IRHO)
       AVAL1                         = DTAVPR(X,T)*AVPR(X,T)**(2.e0_R8/3.e0_R8)*NE(IRHO)*ATE(X,T)*5.e0_R8/3.e0_R8
       AVAL1                         = AVAL1+AVPR(X,T)**(5.e0_R8/3.e0_R8)*DTNE(IRHO)*ATE(X,T)
       AVAL1                         = AVAL1+AVPR(X,T)**(5.e0_R8/3.e0_R8)*NE(IRHO)*DTATE(X,T)
       AVAL2                         = -DTABT(T)/2.e0_R8/ABT(T)
       AVAL3                         = NE(IRHO)*ATE(X,T)*AVPR(X,T)**(5.e0_R8/3.e0_R8)
       AVAL3                         = AVAL3+ X*DNE(IRHO)*ATE(X,T)*AVPR(X,T)**(5.e0_R8/3.e0_R8)
       AVAL3                         = AVAL3+ X*NE(IRHO)*DATE(X,T)*AVPR(X,T)**(5.e0_R8/3.e0_R8)
       AVAL3                         = AVAL3+ X*NE(IRHO)*ATE(X,T)*AVPR(X,T)**(2.e0_R8/3.e0_R8)*DAVPR(X,T)*5.e0_R8/3.e0_R8
       AVAL1                         = AVAL1+AVAL2*AVAL3
       AVAL1                         = AVAL1*3.e0_R8/2.e0_R8
       AVAL2                         = AVPR(X,T)*AG1(X,T)*NE(IRHO)                        
       AVAL3                         = -AKATE(X,T)*DATE(X,T)+ATE(X,T)*AVTE(X,T)
       AVAL4                         = DAVPR(X,T)*AG1(X,T)*NE(IRHO)+AVPR(X,T)*DAG1(X,T)*NE(IRHO)+AVPR(X,T)*AG1(X,T)*DNE(IRHO)
       AVAL5                         = -DAKATE(X,T)*DATE(X,T)+ATE(X,T)*DAVTE(X,T)
       AVAL5                         = AVAL5-AKATE(X,T)*DDATE(X,T)+DATE(X,T)*AVTE(X,T)
       AVAL6                         = AVAL2*AVAL5+AVAL4*AVAL3
       AVAL1                         = AVAL1+AVAL6*AVPR(X,T)**(2.e0_R8/3.e0_R8)
       C1                            = 0.e0_R8
       AVAL2                         = C1*(AVPR(X,T)**(2.e0_R8/3.e0_R8)*(DATE(X,T)*GAMMAE(IRHO)+ ATE(X,T)*DGAMMAE(IRHO)))
       AVAL1                         = AVAL1+AVAL2
       AVAL1                         = AVAL1/AVPR(X,T)**(5.e0_R8/3.e0_R8)
       VIE(IRHO)                     = COLLISIONS1%VIE(IRHO)
       QIE(IRHO)                     = COLLISIONS1%QIE(IRHO)
       AVAL2                         = (AQE_IMP(X,T) + VIE(IRHO)  )                                         
       SOURCES1%QE_EXP(IRHO)         = AVAL1 +AVAL2*ATE(X,T)-QIE(IRHO)
       SOURCES1%QE_IMP(IRHO)         = AQE_IMP(X,T)
    ENDDO

    SOURCES1%QE_EXP(1)              = SOURCES1%QE_EXP(2)
    SOURCES1%QE_IMP(1)              = SOURCES1%QE_IMP(2)



! +++ BOUNDARY CONDITION
    IF (PROFILES1%TE_BND_TYPE.EQ.1)THEN
       PROFILES1%TE_BND(1)          = TE(NRHO)    
    ENDIF

    IF (PROFILES1%TE_BND_TYPE.EQ.2)THEN
       PROFILES1%TE_BND(1)          = -DATE(RHO(NRHO),T)   
    ENDIF

    IF (PROFILES1%TE_BND_TYPE.EQ.3)THEN
       PROFILES1%TE_BND(1)          = -TE(NRHO)/DATE(RHO(NRHO),T)    
    ENDIF

    IF (PROFILES1%TE_BND_TYPE.EQ.4)THEN
       X                            = RHO(NRHO)
       AVAL2                        = AVPR(X,T)*AG1(X,T)*NE(NRHO) 
       AVAL3                        = -AKATE(X,T)*DATE(X,T)+ATE(RHO(NRHO),T)*AVTE(X,T)
       PROFILES1%TE_BND(1)          = AVAL2*AVAL3+C1*ATE(RHO(NRHO),T)*GAMMAE(NRHO)
    ENDIF





! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! +++ ROTATION TRANSPORT
    DO IION=1,NION
       T                            = TIME

       DO IRHO=2,NRHO
          X                          = RHO(IRHO)
          AVAL1                      = DTAVPR(X,T)*ANI(IION,X,T)*AVTOR(IION,X,T)*AG2(X,T)
          AVAL1                      = AVAL1+AVPR(X,T)*DTANI(IION,X,T)*AVTOR(IION,X,T)*AG2(X,T)
          AVAL1                      = AVAL1+AVPR(X,T)*ANI(IION,X,T)*DTAVTOR(IION,X,T)*AG2(X,T)
          AVAL1                      = AVAL1+AVPR(X,T)*ANI(IION,X,T)*AVTOR(IION,X,T)*DTAG2(X,T)

          AVAL2                      = -DTABT(T)/2.e0_R8/ABT(T)

          AVAL3                      = ANI(IION,X,T)*AVTOR(IION,X,T)*AVPR(X,T)*AG2(X,T)
          AVAL3                      = AVAL3+ X*DANI(IION,X,T)*AVTOR(IION,X,T)*AVPR(X,T)*AG2(X,T)
          AVAL3                      = AVAL3+ X*ANI(IION,X,T)*DAVTOR(IION,X,T)*AVPR(X,T)*AG2(X,T)
          AVAL3                      = AVAL3+ X*ANI(IION,X,T)*AVTOR(IION,X,T)*DAVPR(X,T)*AG2(X,T)
          AVAL3                      = AVAL3+ X*ANI(IION,X,T)*AVTOR(IION,X,T)*DAVPR(X,T)*DAG2(X,T)

          AVAL1                      = AVAL1+AVAL2*AVAL3

          AVAL2                      = AVPR(X,T)*AG2(X,T)*ANI(IION,X,T)*AG1(X,T)                                    

          AVAL3                      = -ADVTOR(IION,X,T)*DAVTOR(IION,X,T)+AVTOR(IION,X,T)*ACONVTOR(IION,X,T)

          AVAL4                      = DAVPR(X,T)*AG2(X,T)*ANI(IION,X,T)+ &
               AVPR(X,T)*DAG2(X,T)*ANI(IION,X,T)+AVPR(X,T)*AG2(X,T)*DANI(IION,X,T)
          AVAL4                      = AVAL4*AG1(X,T)+AVPR(X,T)*AG2(X,T)*ANI(IION,X,T)*DAG1(X,T)

          AVAL5                      = -DADVTOR(IION,X,T)*DAVTOR(IION,X,T)+AVTOR(IION,X,T)*DACONVTOR(IION,X,T)
          AVAL5                      = AVAL5-ADVTOR(IION,X,T)*DDAVTOR(IION,X,T)+DAVTOR(IION,X,T)*ACONVTOR(IION,X,T)

          AVAL6                      = AVAL2*AVAL5+AVAL4*AVAL3 +DAG2(X,T)*GAMMA(IION,IRHO)*AVTOR(IION,X,T)!*AVAL4
          AVAL6                      = AVAL6+(AG2(X,T)*DGAMMA(IION,IRHO)*AVTOR(IION,X,T)+ &
               AG2(X,T)*GAMMA(IION,IRHO)*DAVTOR(IION,X,T))!*AVAL4

          AVAL1                      = AVAL1+AVAL6
          AVAL1                      = AVAL1*PROFILES1%MION(IION)/AVPR(X,T)



          UZI(IRHO)                  = COLLISIONS1%UZI(IRHO,IION)
          WZI(IRHO)                  = COLLISIONS1%WZI(IRHO,IION)
          AVAL2                      = AUI_IMP(IION,X,T)  + WZI(IRHO)                                        

          SOURCES1%UI_EXP(IRHO,IION) = AVAL1 +AVAL2*AVTOR(IION,X,T) - UZI(IRHO)
          SOURCES1%UI_IMP(IRHO,IION) = AUI_IMP(IION,X,T)
       ENDDO

       SOURCES1%UI_EXP(1,IION)      = SOURCES1%UI_EXP(2,IION)
       SOURCES1%UI_EXP(1,IION)      = AUI_IMP(IION,0.e0_R8,T)
       SOURCES1%UI_IMP(1,IION)      = AUI_IMP(IION,0.e0_R8,T)
    ENDDO




! +++ BOUNDARY CONDITION
    DO IION=1,NION
       IF (PROFILES1%VTOR_BND_TYPE(IION).EQ.1)THEN
          PROFILES1%VTOR_BND(1,IION) = AVTOR(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%VTOR_BND_TYPE(IION).EQ.2)THEN
          PROFILES1%VTOR_BND(1,IION) = -DAVTOR(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%VTOR_BND_TYPE(IION).EQ.3)THEN
          PROFILES1%VTOR_BND(1,IION) = -AVTOR(IION,RHO(NRHO),T)/DAVTOR(IION,RHO(NRHO),T)    
       ENDIF

       IF (PROFILES1%VTOR_BND_TYPE(IION).EQ.4)THEN
          X                          = RHO(NRHO)
          AVAL2                      = AVPR(X,T)*AG2(X,T)*ANI(IION,X,T)*AG1(X,T) 
          AVAL3                      = -ADVTOR(IION,X,T)*DAVTOR(IION,X,T)+AVTOR(IION,RHO(NRHO),T)*ACONVTOR(IION,X,T)
          PROFILES1%VTOR_BND(1,IION) = AVAL2*AVAL3 +AVTOR(IION,RHO(NRHO),T)*AG2(X,T)*GAMMA(IION,NRHO)
          PROFILES1%VTOR_BND(1,IION) = PROFILES1%VTOR_BND(1,IION)*MION(IION)  
       ENDIF

    ENDDO



    CALL DEALLOCATE_COLLISIONALITY  (COLLISIONS1, ifail) 


    RETURN



  END SUBROUTINE ANALYTICS_FULL

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  





END MODULE ANALYTICS1
