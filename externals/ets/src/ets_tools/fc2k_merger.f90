! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +   
  SUBROUTINE MERGER   (EQUILIBRIUM,                     &
                       TOROIDFIELD,                     &
                       MHD,                             &
                       SAWTEETH,                        &
                       COREPROF,                        &
                       CORETRANSP,                      &
                       CORESOURCE,                      &
                       COREIMPUR,                       &
                       CORENEUTRALS,                    &
                       COREFAST,                        &
                       COREDELTA,                       &
                       COMPOSITIONC,                    &
                       NEOCLASSIC,                      &
                       WAVES,                           &
                       DISTSOURCE,                      &
                       DISTRIBUTION,                    &
                       WALL,                            &
                       NBI,                             &
                       ANTENNAS,                        &
                       IRONMODEL,                       &
                       PFSYSTEMS,                       &
                       FUSIONDIAG,                      &
                       SCENARIO,                        &
                       PELLETS ) 

! +++ Declaration of variables: 
    USE ITM_TYPES
    USE EUITM_SCHEMAS
    USE COPY_STRUCTURES


! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM),  POINTER  :: EQUILIBRIUM(:)    
    TYPE (TYPE_TOROIDFIELD),  POINTER  :: TOROIDFIELD(:)  
    TYPE (TYPE_MHD),          POINTER  :: MHD(:)        
    TYPE (TYPE_SAWTEETH),     POINTER  :: SAWTEETH(:)     

    TYPE (TYPE_COREPROF),     POINTER  :: COREPROF(:)       
    TYPE (TYPE_CORETRANSP),   POINTER  :: CORETRANSP(:)     
    TYPE (TYPE_CORESOURCE),   POINTER  :: CORESOURCE(:)     
    TYPE (TYPE_COREIMPUR),    POINTER  :: COREIMPUR(:)      
    TYPE (TYPE_CORENEUTRALS), POINTER  :: CORENEUTRALS(:)   
    TYPE (TYPE_COREFAST),     POINTER  :: COREFAST(:)
    TYPE (TYPE_COREDELTA),    POINTER  :: COREDELTA(:)
    TYPE (TYPE_COMPOSITIONC), POINTER  :: COMPOSITIONC(:)
    TYPE (TYPE_NEOCLASSIC),   POINTER  :: NEOCLASSIC(:)   

    TYPE (TYPE_WAVES),        POINTER  :: WAVES(:)          
    TYPE (TYPE_DISTSOURCE),   POINTER  :: DISTSOURCE(:)   
    TYPE (TYPE_DISTRIBUTION), POINTER  :: DISTRIBUTION(:) 

    TYPE (TYPE_WALL),         POINTER  :: WALL(:)         
    TYPE (TYPE_NBI),          POINTER  :: NBI(:)          
    TYPE (TYPE_ANTENNAS),     POINTER  :: ANTENNAS(:)
    TYPE (TYPE_IRONMODEL),    POINTER  :: IRONMODEL(:)
    TYPE (TYPE_PFSYSTEMS),    POINTER  :: PFSYSTEMS(:)

    TYPE (TYPE_FUSIONDIAG),   POINTER  :: FUSIONDIAG(:)
    TYPE (TYPE_SCENARIO),     POINTER  :: SCENARIO(:)
    TYPE (TYPE_PELLETS),      POINTER  :: PELLETS(:)

    RETURN 

    END SUBROUTINE MERGER
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







