module sources_standalone
  use euitm_schemas
  implicit none

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
  subroutine gaussian_source_cpo(corep_in, equil_in, cores_out) 
    use iso_c_binding
    use string_binding
    use xml_file_reader
    use deallocate_structures
    use gausian_src
    implicit none

    type (type_coreprof), pointer :: corep_in(:)
    type (type_equilibrium), pointer :: equil_in(:)
    
    type (type_coresource), pointer :: cores_out(:)
    type (type_param) :: code_parameters

    ! Get code params
    call fill_param(code_parameters, 'source_dummy.xml', '', 'source_dummy.xsd')
    
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

  ! ... 
  subroutine gaussian_source2buf(corep_in_buf, equil_in_buf, cores_out_buf)
    use iso_c_binding
    use string_binding
    use deallocate_structures
    use read_structures
    use write_structures
    use c_tools
    implicit none

    integer(kind=c_signed_char), pointer :: equil_in_buf(:)
    integer(kind=c_signed_char), pointer :: corep_in_buf(:)
    integer(kind=c_signed_char), pointer :: cores_out_buf(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil_in(:)
    type (type_coreprof), pointer :: corep_in(:)
    type (type_coresource), pointer :: cores_out(:)

    character(F_STR_SIZE) :: equil_in_file, corep_in_file, cores_out_file
    character(F_STR_SIZE) :: username, tmpdir
    integer :: tmpsize, ios

    allocate(equil_in(1))
    allocate(corep_in(1))

    call getenv("USER",username)
    call getenv("CPO_SERIALIZATION_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if

    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_gaussian_source_equilibrium_in.cpo'
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

    corep_in_file = TRIM(tmpdir)//TRIM(username)//'_gaussian_source_coreprof_in.cpo'
    call byte2file(corep_in_file, corep_in_buf, size(corep_in_buf))
    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_in(1), 'coreprof')
       call close_read_file
    else
       print *,"ERROR: no input coreprof"
       STOP
    end if

    call gaussian_source_cpo(corep_in, equil_in, cores_out)

    ! transfer CPO to buf
    !...  write the results
    cores_out_file = 'gaussian_source_coresource_out.cpo'
    call open_write_file(11,cores_out_file)
    call write_cpo(cores_out(1),'coresource')
    call close_write_file

    call file2byte(cores_out_file, tmpbuf, tmpsize)
    allocate(cores_out_buf(tmpsize))
    cores_out_buf(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call deallocate_cpo(equil_in)
    call deallocate_cpo(corep_in)
    call deallocate_cpo(cores_out)
    
  end subroutine gaussian_source2buf


end module sources_standalone
