module sources_standalone
  use euitm_schemas
  use itm_types
  use string_binding
  implicit none

  integer, save :: init_step = 0    !initial step count

  interface
     ! GAUSIAN with one 's' is not a typo...
     subroutine gausian_sources(corep, equil, cores, code_parameters)
       use euitm_schemas
       type (type_coreprof), pointer ::  corep(:)
       type (type_equilibrium), pointer ::  equil(:)
       type (type_coresource), pointer ::  cores(:)
       type (type_param) :: code_parameters
     end subroutine gausian_sources
  end interface

contains
  
  ! ... fortran wrapper
  subroutine gaussian_source_cpo(corep_in, equil_in, params_in, cores_out) 
    use iso_c_binding
    use string_binding
    use xml_file_reader
    use deallocate_structures
    use gausian_src
    implicit none

    type (type_coreprof), pointer :: corep_in(:)
    type (type_equilibrium), pointer :: equil_in(:)
    ! TODO find a way to write in code params like cops 
    real(8):: params_in(3) ! temporary params 
    type (type_coresource), pointer :: cores_out(:)
    type (type_param) :: code_parameters

    ! Path to the workflows directory
    character(len=10) :: WTOT_el, RHEAT_el, FWHEAT_el
    
    ! Get code params
    call fill_param(code_parameters, '../../workflows/source_dummy.xml', '', '../../workflows/source_dummy.xsd')
    
    !i_JNITOT = INDEX(code_parameters%parameters(9), "1.E5")
    
    write(WTOT_el,'(ES10.3)') params_in(1)
    write(RHEAT_el,'(ES10.3)') params_in(2)
    write(FWHEAT_el,'(ES10.3)') params_in(3)
    
    code_parameters%parameters(19)(26:35) = WTOT_el   ! Amplitude
    code_parameters%parameters(20)(26:35) = RHEAT_el  ! Mean
    code_parameters%parameters(21)(26:35) = FWHEAT_el ! STD
    
    ! Run gausian_sources
    call gausian_sources(corep_in, equil_in, cores_out, code_parameters)

    ! Deallocations
    if (associated(code_parameters%schema)) then
       deallocate(code_parameters%schema)
    endif
    if (associated(code_parameters%parameters)) then
       deallocate(code_parameters%parameters)
    endif
    if (associated(code_parameters%default_param)) then
       deallocate(code_parameters%default_param)
    endif

    print *,"return from fortran wrapper"
    
  end subroutine gaussian_source_cpo
  ! ...

end module sources_standalone
