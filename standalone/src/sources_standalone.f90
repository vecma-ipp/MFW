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
    
    real(8) :: params_in(3) 
    type (type_coresource), pointer :: cores_out(:)
    type (type_param) :: code_parameters
    character(len=10) :: S1, S2, S3

    ! Get code params
    call fill_param(code_parameters, '../../workflows/source_dummy.xml', '', '../../workflows/source_dummy.xsd')
    
    ! Uncertrainties in Sources: S1: amplitude, S2i: position (mean) and width (std)
    write(S1,'(ES10.3)') params_in(1)
    write(S2,'(ES10.3)') params_in(2)
    write(S3,'(ES10.3)') params_in(3)
    
    ! Fill new paramas for heating in source_dummy.xml
    ! Electrons
!    code_parameters%parameters(19)(26:35) = S1 ! WTOT_el  
!    code_parameters%parameters(20)(26:35) = S2 ! RHEAT_el  
!    code_parameters%parameters(21)(26:35) = S3 ! FWHEAT_el 
    ! Ions
    code_parameters%parameters(43)(26:35) = S1 ! WTOT   
    code_parameters%parameters(44)(26:35) = S2 ! RHEAT  
    code_parameters%parameters(45)(26:35) = S3 ! FWHEAT 
    
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
