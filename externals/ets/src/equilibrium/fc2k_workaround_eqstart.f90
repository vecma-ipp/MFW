! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module to cope with problems in fc2k
!>
!> Provides EQUIL_INPUT for FC2K with arguments:
!> in COREPROF: TYPE_COREPROF
!> \author D.Kalupin
!>
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

     SUBROUTINE eqstart(PROF_FLAG,    J0_FLAG,         Q0_FLAG,        &
                        COREPROF_IN,  EQUILIBRIUM_IN,  TOROIDFIELD_IN, &
                        COREPROF_OUT, EQUILIBRIUM_OUT) 

     USE EUITM_SCHEMAS

     USE EQUILIBRIUM_START

     IMPLICIT NONE

! +++ CPO derived types:
     TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_IN(:)   !input CPO with geometry quantities from previous time
     TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OUT(:)  !output CPO with geometry quantities from previous iteration
     TYPE (TYPE_TOROIDFIELD), POINTER  :: TOROIDFIELD_IN(:)   !toroidal field, major radius, total current
     TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input CPO with plasma profiles   
     TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_ITER(:)    !iterration CPO with plasma profiles   
     TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OUT(:)     !output CPO with plasma profiles   
     TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_NEW(:)     !output CPO with plasma profiles   


! +++ Dimensions:
     INTEGER                           :: PROF_FLAG, FLAG     !determines primary profile
     INTEGER                           :: J0_FLAG             !flag for negative current, if J0_FLAG>0 negative current will be cut off
     INTEGER                           :: Q0_FLAG             !Flag for positive dq/drho: 0-allowed, >0-cut off
     INTEGER,               PARAMETER  :: EXT_EQUIL = 2


     CALL START_PROFILES_CONSISTENCY  (PROF_FLAG,    J0_FLAG,        Q0_FLAG,        EXT_EQUIL,  &
                                       COREPROF_IN,  EQUILIBRIUM_IN, TOROIDFIELD_IN,             &
                                       COREPROF_OUT, EQUILIBRIUM_OUT) 

     RETURN


     END SUBROUTINE eqstart
