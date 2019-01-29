! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FC2K_CHANGERADII (EQUILIBRIUM_IN,                      &
                               COREPROF_IN,     COREPROF_OUT,       &
                               CORETRANSP_IN,   CORETRANSP_OUT,     &
                               CORESOURCE_IN,   CORESOURCE_OUT,     &
                               COREIMPUR_IN,    COREIMPUR_OUT,      &
                               CORENEUTRALS_IN, CORENEUTRALS_OUT,   & 
                               NEOCLASSIC_IN,   NEOCLASSIC_OUT) 

! +++ Declaration of variables: 
    USE ITM_TYPES
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE CONVERT


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),  POINTER  :: EQUILIBRIUM_IN(:)      !input CPO

    TYPE (TYPE_COREPROF),     POINTER  :: COREPROF_IN(:)         !input CPO  
    TYPE (TYPE_COREPROF),     POINTER  :: COREPROF_OUT(:)        !output CPO  
    TYPE (TYPE_CORETRANSP),   POINTER  :: CORETRANSP_IN(:)       !input CPO  
    TYPE (TYPE_CORETRANSP),   POINTER  :: CORETRANSP_OUT(:)      !output CPO  
    TYPE (TYPE_CORESOURCE),   POINTER  :: CORESOURCE_IN(:)       !input CPO  
    TYPE (TYPE_CORESOURCE),   POINTER  :: CORESOURCE_OUT(:)      !output CPO  
    TYPE (TYPE_COREIMPUR),    POINTER  :: COREIMPUR_IN(:)        !input CPO  
    TYPE (TYPE_COREIMPUR),    POINTER  :: COREIMPUR_OUT(:)       !output CPO  
    TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS_IN(:)     !input CPO
    TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS_OUT(:)    !output CPO
    TYPE (TYPE_NEOCLASSIC),   POINTER  :: NEOCLASSIC_IN(:)       !input CPO
    TYPE (TYPE_NEOCLASSIC),   POINTER  :: NEOCLASSIC_OUT(:)      !output CPO


! +++ Allocate output CPOs:
    ALLOCATE(COREPROF_OUT(1))
    ALLOCATE(CORETRANSP_OUT(1))
    ALLOCATE(CORESOURCE_OUT(1))
    ALLOCATE(COREIMPUR_OUT(1))
    ALLOCATE(CORENEUTRALS_OUT(1))
    ALLOCATE(NEOCLASSIC_OUT(1))


! +++ Copy input CPOs to output CPOs:
    CALL COPY_CPO      (COREPROF_IN(1),     COREPROF_OUT(1))
    CALL COPY_CPO      (CORETRANSP_IN(1),   CORETRANSP_OUT(1))
    CALL COPY_CPO      (CORESOURCE_IN(1),   CORESOURCE_OUT(1))
    CALL COPY_CPO      (COREIMPUR_IN(1),    COREIMPUR_OUT(1))
    CALL COPY_CPO      (CORENEUTRALS_IN(1), CORENEUTRALS_OUT(1)) 
    CALL COPY_CPO      (NEOCLASSIC_IN(1),   NEOCLASSIC_OUT(1)) 


! +++ Modify radii:
    CALL CHANGERADII   (EQUILIBRIUM_IN,     &
                        COREPROF_OUT,       &
                        CORETRANSP_OUT,     &
                        CORESOURCE_OUT,     &
                        COREIMPUR_OUT,      &
                        CORENEUTRALS_OUT,   & 
                        NEOCLASSIC_OUT) 

    RETURN 

    END SUBROUTINE FC2K_CHANGERADII 
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







