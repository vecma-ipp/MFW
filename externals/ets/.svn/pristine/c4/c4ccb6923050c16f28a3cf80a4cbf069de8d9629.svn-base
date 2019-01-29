MODULE PARAMETERS       ! declaration of SPIDER code parameters


    USE ITM_TYPES


    TYPE TYPE_PARAMETERS  
    INTEGER       :: nstep      = 0      ! nstep=0 - initial eq., nstep>0 using computed eq.
    INTEGER       :: neql       = 100    ! number of nodes in radial
    INTEGER       :: nteta      = 100    ! number of intervals in poloidal + 2
    
    INTEGER       :: kpr        = 1      ! print in spider (0 - no print, -1 - no write)

    INTEGER       :: k_auto     = 1      ! k_auto= 1->   full initialization
    INTEGER       :: k_fixfree  = 0      ! =0->only fixed boundary spider 
    INTEGER       :: k_grid     = 0      ! k_grid= 0   rect. grid
                                         ! k_grid= 1   adap. grid

    INTEGER       :: key_dmf    = -10    ! =1->diff.mag.field, =0->without
    INTEGER       :: key_ini    = 1      ! =1 astra profiles, =0 start from EQDSK and SPIDER profiles
    INTEGER       :: key_start  = 1      ! =1 astra profiles, =0 start from EQDSK and SPIDER profiles
    INTEGER       :: key_0stp   = 1      ! initial eq. only =0 - p',ff'; =1 - p,cu
    INTEGER       :: key_pres   = 1      ! =1 - pressure profile, =0 - p' profile  
        
    INTEGER       :: kname      = 1      ! path name length >=1
    
    REAL(R8)      :: dt         = 1.d-3
    REAL(R8)      :: time       = 0.d0
    REAL(R8)      :: dpsdt      = 0.d0
    
    CHARACTER*40  :: prename    = ''     ! working directory path
    CHARACTER*40  :: eqdfn      = 'g900004.00230_ITER_09MA_eqdsk14MR.txt'
    

    END TYPE TYPE_PARAMETERS


END MODULE PARAMETERS
