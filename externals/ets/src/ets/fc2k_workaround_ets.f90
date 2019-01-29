! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module to cope with problems in fc2k 
!>
!> Provides itmets for FC2K with arguments:
!> in COREPROF_OLD: TYPE_COREPROF
!> in COREPROF_ITER: TYPE_COREPROF
!> out COREPROF_NEW: TYPE_COREPROF
!> in EQUILIBRIUM_OLD: TYPE_EQUILIBRIUM
!> in EQUILIBRIUM_ITER: TYPE_EQUILIBRIUM
!> in CORETRANSP: TYPE_CORETRANSP
!> in CORESOURCE: TYPE_CORESOURCE
!> in COREIMPUR: TYPE_COREIMPUR
!> in CONTROL_INTEGER(2): INTEGER
!> in CONTROL_DOUBLE(5): REAL(R8)
!>
!> \author D. Coster
!>
!> \version "$Id: fc2k_workaround_ets.f90.not_yet_OK 1340 2012-06-29 11:24:40Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

SUBROUTINE ITMETS                                      &
     (COREPROF_OLD, COREPROF_ITER, COREPROF_NEW,       &
      EQUILIBRIUM_OLD, EQUILIBRIUM_ITER,               &
      CORETRANSP, CORESOURCE, COREIMPUR,               &
!      CONTROL_INTEGER, CONTROL_DOUBLE) 
      CONTROL_INTEGER, CONTROL_DOUBLE, HYPER_DIFF_RATIO) !AF 25.Apr.2016
  
  USE EUITM_SCHEMAS
  USE ETS_PLASMA
  USE ETS
!  USE ISO_C_BINDING 
 
  IMPLICIT NONE
  
  TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQUILIBRIUM_OLD(:), EQUILIBRIUM_ITER(:)
  TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_OLD(:), COREPROF_NEW(:), COREPROF_ITER(:)
  TYPE (TYPE_CORETRANSP),  POINTER  :: CORETRANSP(:)
  TYPE (TYPE_CORESOURCE),  POINTER  :: CORESOURCE(:)
  TYPE (TYPE_COREIMPUR),   POINTER  :: COREIMPUR(:)
  TYPE (RUN_CONTROL)                :: CONTROL
  TYPE (type_param)                 :: code_parameters

  INTEGER,               INTENT(IN) :: CONTROL_INTEGER(3)  !integer control parameters
  REAL (R8),             INTENT(IN) :: CONTROL_DOUBLE(6)   !real control parameters

  INTEGER                           :: ifail, i
  CHARACTER(LEN=500)                :: failstring

! +++ Stabilization scheme !AF 25.Apr.2016
  REAL (R8)                         :: HYPER_DIFF_RATIO    !ratio of hyper diffusivity to maximum diffusivity !AF 25.Apr.2016

! Clean initial error statement
  ifail      = 0
  failstring = 'No errors have been found'


! Set run control:
  CALL ALLOCATE_RUN_CONTROL (CONTROL, ifail)

  CONTROL%SOLVER_TYPE                        = CONTROL_INTEGER(1)
  CONTROL%SIGMA_SOURCE                       = CONTROL_INTEGER(2)
  CONTROL%QUASI_NEUT                         = CONTROL_INTEGER(3)
  CONTROL%TAU                                = CONTROL_DOUBLE(1)
  CONTROL%AMIX                               = CONTROL_DOUBLE(2)
  CONTROL%AMIXTR                             = CONTROL_DOUBLE(3)
  CONTROL%CONV                               = CONTROL_DOUBLE(4)
  CONTROL%CONVREC                            = CONTROL_DOUBLE(5)
  CONTROL%OHMIC_HEATING_MULTIPLIER           = CONTROL_DOUBLE(6)
  
! Call the actual code:
  CALL ITM_ETS_KEPLER                                     &
       (COREPROF_OLD,    COREPROF_ITER,    COREPROF_NEW,  &
        EQUILIBRIUM_OLD, EQUILIBRIUM_ITER,                &
        CORETRANSP,      CORESOURCE,       COREIMPUR,     &
!        CONTROL_INTEGER, CONTROL_DOUBLE,   ifail,         & 
        CONTROL_INTEGER, CONTROL_DOUBLE, HYPER_DIFF_RATIO,  ifail,         & !AF 25.Apr.2016
        code_parameters) 
  
  CALL DEALLOCATE_RUN_CONTROL (CONTROL, ifail)


  IF(ifail.EQ.0) failstring                = 'No errors have been found'

!  COREPROF_NEW(1)%codeparam%codename       = 'European Transport Solver'
!  COREPROF_NEW(1)%codeparam%codeversion    = '01.June.2013'
!  COREPROF_NEW(1)%codeparam%output_diag    = failstring
!  COREPROF_NEW(1)%codeparam%output_flag    = ifail


  RETURN
  
END SUBROUTINE ITMETS
  
