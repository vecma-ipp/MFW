! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE FC2K_CHANGEPSI   (EQUILIBRIUM_IN,                      &
                               COREPROF_IN,     COREPROF_OUT) 

! +++ Declaration of variables: 
    USE ITM_TYPES
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES
    USE EUITM_ROUTINES


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),  POINTER  :: EQUILIBRIUM_IN(:)      !input CPO

    TYPE (TYPE_COREPROF),     POINTER  :: COREPROF_IN(:)         !input CPO  
    TYPE (TYPE_COREPROF),     POINTER  :: COREPROF_OUT(:)        !output CPO  
    REAL(R8)                           :: RHO_TOR_RESCALE


! +++ Allocate output CPOs:
    ALLOCATE(COREPROF_OUT(1))


! +++ Copy input CPOs to output CPOs:
    CALL COPY_CPO      (COREPROF_IN(1),     COREPROF_OUT(1))


! +++ Copy input CPOs to output CPOs:
    RHO_TOR_RESCALE             = EQUILIBRIUM_IN(1)%profiles_1d%rho_tor(SIZE(EQUILIBRIUM_IN(1)%profiles_1d%rho_tor)) / &
                                  COREPROF_OUT(1)%rho_tor(SIZE(COREPROF_OUT(1)%rho_tor))
    WRITE(*,*) 'RHO_TOR COREPROF rescale factor = ', RHO_TOR_RESCALE
    COREPROF_OUT(1)%rho_tor     = COREPROF_OUT(1)%rho_tor * RHO_TOR_RESCALE

! +++ Modify psi:
    CALL L3INTERP      (EQUILIBRIUM_IN(1)%profiles_1d%psi, EQUILIBRIUM_IN(1)%profiles_1d%rho_tor, SIZE(EQUILIBRIUM_IN(1)%profiles_1d%rho_tor),&
                        COREPROF_OUT(1)%psi%value,         COREPROF_OUT(1)%rho_tor,               SIZE(COREPROF_OUT(1)%rho_tor)) 

    RETURN 

    END SUBROUTINE FC2K_CHANGEPSI
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







