module chease_standalone
  use euitm_schemas
  use itm_types
  use string_binding
  implicit none

  integer, save :: init_step = 0    !initial step count

  interface
     subroutine chease(eq_in, eq, code_parameters, output_flag, output_message)
       use euitm_schemas
       type (type_equilibrium), pointer ::  eq_in(:), eq(:)
       type (type_param) :: code_parameters
       integer, intent(out) :: output_flag
       character(len=:), pointer :: output_message
     end subroutine chease
  end interface

contains
  
  ! ... fortran CHEASE wrapper
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
    integer :: output_flag
    character(len=:), pointer :: output_message
    
    ! Get code params
    call fill_param(code_parameters, 'chease.xml', '', 'chease.xsd')

    !...  run CHEASE
    call chease(equil_in, equil_out, code_parameters, output_flag, output_message)

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
  subroutine chease2buf(equil_in_buf, equil_out_buf) 
    use iso_c_binding
    use string_binding
    use deallocate_structures
    use read_structures
    use write_structures
    use c_tools
    implicit none

    integer(kind=c_signed_char), pointer :: equil_in_buf(:)
    integer(kind=c_signed_char), pointer :: equil_out_buf(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil_in(:)
    type (type_equilibrium), pointer :: equil_out(:)
    
    character(F_STR_SIZE) :: equil_in_file, equil_out_file, username, tmpdir
    integer :: tmpsize, ios

    allocate(equil_in(1))

    call getenv("USER",username)
    call getenv("CPO_SERIALIZATION_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_chease_equilibrium_in.cpo'
    call byte2file(equil_in_file, equil_in_buf, size(equil_in_buf))

    open (unit = 10, file = equil_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, equil_in_file )
       call read_cpo(equil_in(1), 'equilibrium')
       call close_read_file
    else
       print *,"ERROR: no input equilibrium"
       STOP
    end if

    call chease_cpo(equil_in, equil_out)

    ! transfer CPO to buf
    !...  write the results
    equil_out_file = 'chease_equilibrium_out.cpo'
    call open_write_file(11,equil_out_file)
    call write_cpo(equil_out(1),'equilibrium')
    call close_write_file

    call file2byte(equil_out_file, tmpbuf, tmpsize)
    allocate(equil_out_buf(tmpsize))
    equil_out_buf(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call deallocate_cpo(equil_in)
    call deallocate_cpo(equil_out)

  end subroutine chease2buf
  ! ...


  ! ...
  subroutine chease2file(equil_old, aoutput) 
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use xml_file_reader
    use deallocate_structures
    !use ifport, only: FULLPATHQQ
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

    integer :: output_flag
    character(len=:), pointer :: output_message
    
    print *,"fortran CHEASE wrapper"

    
    print *,"get code params"
    !...  run CHEASE
    call fill_param(code_parameters, '../../workflows/chease.xml', '', '../../workflows/chease.xsd')
    
    ! FULLPATHQQ is just available with intel compiler
!    len = FULLPATHQQ('../../workflows/', workflows_dir)
!    call fill_param(code_parameters, workflows_dir(:len)// 'chease.xml', '', &
!                                     workflows_dir(:len)// 'chease.xsd')
!
    
    print *,"run chease routine"
    call chease(equil_old, equil_new, code_parameters, output_flag, output_message)
    
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
