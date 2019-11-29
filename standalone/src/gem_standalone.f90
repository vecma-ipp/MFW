module gem_standalone
  use euitm_schemas
  implicit none

  integer, save :: init_step             !initial step count


  interface
     subroutine gem(equil, corep, coret, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  equil(:)
       type (type_coreprof), pointer :: corep(:)
       type (type_coretransp), pointer :: coret(:)
       type (type_param) :: code_parameters
     end subroutine gem
  end interface

contains

  subroutine gem_cpo(equil, corep, coret) 
    use xml_file_reader
    use deallocate_structures
    implicit none

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_param) :: code_parameters

    !print *,"fortran GEM0 wrapper"

    !print *,"get code params"
    call fill_param(code_parameters, 'gem.xml', '', 'gem.xsd')

    !print *,"run gem0 routine"
    call gem(equil, corep, coret, code_parameters)

  end subroutine gem_cpo


end module gem_standalone
