! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module to cope with problems in fc2k
!>
!> Provides analyticalplasma for FC2K with arguments:
!> in TIME: REAL(R8)
!> in COREPROF_in: TYPE_COREPROF
!> out EQUILIBRIUM: TYPE_EQUILIBRIUM
!> out COREPROF: TYPE_COREPROF
!> out CORETRANSP:TYPE_CORETRANSP
!> out CORESOURCE:TYPE_CORESOURCE
!>
!> \author D. Coster
!>
!> \version "$Id: fc2k_workaround_analytics.f90 458 2009-10-22 17:37:08Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  

subroutine analyticalplasma(TIME,COREPROF_in,EQUILIBRIUM,COREPROF,CORETRANSP,CORESOURCE,COREIMPUR,code_parameters)

  use itm_types
  USE EUITM_SCHEMAS
  use ANALYTICS1

  implicit none

!input arguments
  REAL (R8)                              :: TIME
  TYPE (TYPE_COREPROF),   POINTER        :: COREPROF_in(:)
!output arguments
  TYPE (TYPE_EQUILIBRIUM),POINTER        :: EQUILIBRIUM(:)
  TYPE (TYPE_COREPROF),   POINTER        :: COREPROF(:)
  TYPE (TYPE_CORETRANSP), POINTER        :: CORETRANSP(:)
  TYPE (TYPE_CORESOURCE), POINTER        :: CORESOURCE(:)
  TYPE (TYPE_COREIMPUR),  POINTER        :: COREIMPUR(:)         !CPO with impurities
  type (type_param) :: code_parameters

  call ANALYTICAL_PLASMA (TIME,COREPROF_in,EQUILIBRIUM,COREPROF,CORETRANSP,CORESOURCE,COREIMPUR,code_parameters)

  return

end subroutine analyticalplasma
