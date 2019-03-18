module chease_standalone
  use euitm_schemas
  use itm_types
  use string_binding
  implicit none

  integer, save :: init_step = 0    !initial step count

  interface
     subroutine chease(eq_in, eq, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq_in(:), eq(:)
       type (type_param) :: code_parameters
     end subroutine chease
  end interface

contains
  
  ! ... fortran CHEASE wrapper
  subroutine chease_cpo(equil_in, equil_out) 
    use iso_c_binding
    use string_binding
    use xml_file_reader
    use deallocate_structures
    use ifport, only: FULLPATHQQ

    implicit none
    type (type_equilibrium), pointer :: equil_in(:)
    type (type_equilibrium), pointer :: equil_out(:)
    
    type (type_param) :: code_parameters

    ! Path to the workflows directory
    character(len=128) :: workflows_dir  
    integer :: len
    
    ! Get code params
    len = FULLPATHQQ('../../workflows/', workflows_dir)
    call fill_param(code_parameters, workflows_dir(:len)// 'chease.xml', '', &
                                     workflows_dir(:len)// 'chease.xsd')

    !...  run CHEASE
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

  ! ...
  subroutine chease2file(equil_old, aoutput) 
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use xml_file_reader
    use deallocate_structures
    use ifport, only: FULLPATHQQ
    implicit none
    type (type_equilibrium), pointer :: equil_old(:)
    real(R8) :: aoutput(:)
    
    integer :: ios, i, npar, len
    integer, save :: cpt = 0
    character(4)  :: cptstr

    type (type_equilibrium), pointer ::  equil_new(:) => NULL()
    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_param) :: code_parameters

    character(len=26) :: equil_file_out

    character(F_STR_SIZE) :: username, tmpdir
    integer :: tmpsize

    character(len=128) :: workflows_dir  ! Path to the workflows directory
    
    print *,"fortran CHEASE wrapper"

    
    print *,"get code params"
    !...  run CHEASE
    len = FULLPATHQQ('../../workflows/', workflows_dir)
    call fill_param(code_parameters, workflows_dir(:len)// 'chease.xml', '', &
                                     workflows_dir(:len)// 'chease.xsd')

    print *,"run chease routine"
    call chease(equil_old, equil_new, code_parameters)
    
    ! Safety factor
    aoutput(:)=equil_new(1)%profiles_1d%q

    ! transfer CPO to buf
    !...  write the results
    write(cptstr,'(I4.4)') init_step+cpt
    cpt = cpt+1
    equil_file_out = 'chease_equilibrium_'//cptstr//'.cpo'
    call open_write_file(18,equil_file_out)
    call write_cpo(equil_new(1),'equilibrium')
    call close_write_file
    call deallocate_cpo(equil_old)
    call deallocate_cpo(equil_new)


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

  end subroutine chease2file
  ! ...

end module chease_standalone
