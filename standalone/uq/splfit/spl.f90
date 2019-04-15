module splfit
  use euitm_schemas
  use itm_types
  use string_binding
  implicit none


  interface
     subroutine chease(eq_in, eq, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq_in(:), eq(:)
       type (type_param) :: code_parameters
     end subroutine chease
  end interface

contains
  
  ! ... fortran FITPACK wrapper
  subroutine chease_cpo(equil_in, equil_out) 
    use iso_c_binding
    use string_binding
    use xml_file_reader
    use deallocate_structures
    !use ifport, only: FULLPATHQQ

    implicit none
    type (type_equilibrium), pointer :: equil_in(:)
    type (type_equilibrium), pointer :: equil_out(:)
    
    type (type_param) :: code_parameters

    ! Path to the workflows directory
    character(len=128) :: workflows_dir  
    integer :: len
    
    ! Get code params
    call fill_param(code_parameters, '../../workflows/chease.xml', '', '../../workflows/chease.xsd')

    ! FULLPATHQQ is just available with intel compiler
!    len = FULLPATHQQ('../../workflows/', workflows_dir)
!    call fill_param(code_parameters, workflows_dir(:len)// 'chease.xml', '', &
!                                     workflows_dir(:len)// 'chease.xsd')
!    !...  run CHEASE
    call chease(equil_in, equil_out, code_parameters)

    ! deallocations
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
    
  end subroutine chease_cpo
  ! ...

end module splfit
