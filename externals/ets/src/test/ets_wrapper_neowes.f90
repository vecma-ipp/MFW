! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> wrapper for NEOWES
!>
!> \author D. Coster
!>
!> \version "$Id: ets_wrapper_neowes.f90 1763 2016-06-22 15:41:53Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
module ets_wrapper_neowes

  use euitm_schemas
  implicit none
  
contains

  subroutine neowes_wrapper(eq, coreprof, neoclassic)

    use xml_file_reader

    implicit none

    TYPE (type_equilibrium), pointer :: eq(:)
    TYPE (type_coreprof), pointer :: coreprof(:)
    TYPE (type_neoclassic), pointer :: neoclassic(:)
!!    type (type_param), save  :: code_parameters
    logical, save :: first = .true.

    interface
       subroutine neowes(eq, coreprof, neoclassic)      !! , code_parameters
         use euitm_schemas
         type (type_equilibrium), pointer :: eq(:)
         type (type_coreprof), pointer :: coreprof(:)
         type (type_neoclassic), pointer :: neoclassic(:)
!!         type (type_param) :: code_parameters
       end subroutine neowes
    end interface
    
!!    if(first) then
!!       call fill_param(code_parameters, 'XML/neowes.xml', '', 'XML/neowes.xsd')
!!       first=.false.
!!    endif

    CALL NEOWES(eq, coreprof, neoclassic)      !! , code_parameters

    return

  end subroutine neowes_wrapper

end module ets_wrapper_neowes
