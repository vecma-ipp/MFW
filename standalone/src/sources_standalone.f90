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
    
    real(8) :: params_in(6) 
    type (type_coresource), pointer :: cores_out(:)
    type (type_param) :: code_parameters
    character(len=10) :: amp_e, pos_e, width_e, amp_i, pos_i, width_i


    ! Get code params
    call fill_param(code_parameters, '../../workflows/source_dummy.xml', '', '../../workflows/source_dummy.xsd')
    
    ! New paramas for electrons and ions heating in source_dummy.xml: Amplitude, position ans width
    write(amp_e,'(ES10.3)') params_in(1)
    write(pos_e,'(ES10.3)') params_in(2)
    write(width_e,'(ES10.3)') params_in(3)

    write(amp_i,'(ES10.3)') params_in(4)
    write(pos_i,'(ES10.3)') params_in(5)
    write(width_i,'(ES10.3)') params_in(6)
    
    ! Electrons
    code_parameters%parameters(19)(26:35) = amp_e ! WTOT_el  
    code_parameters%parameters(20)(26:35) = pos_e ! RHEAT_el  
    code_parameters%parameters(21)(26:35) = width_e ! FWHEAT_el 
    ! Ions
    code_parameters%parameters(43)(26:35) = amp_i ! WTOT   
    code_parameters%parameters(44)(26:35) = pos_i ! RHEAT  
    code_parameters%parameters(45)(26:35) = width_i ! FWHEAT 
    
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
