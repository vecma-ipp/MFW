module chease_wrapper
  use euitm_schemas
  use c_tools
#ifdef PERF
  use c_perf
#endif
  implicit none

  integer, save :: init_step = 0    !initial step count
#ifdef PERF
  integer(kind=c_long_long), save :: t0,t1,tread,texec,twrite
#endif

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



  subroutine chease2buf(equil_in, equil_out) 
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use xml_file_reader
    use deallocate_structures
    implicit none

    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: equil_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

    type (type_equilibrium), pointer :: equil_old(:) => NULL(), equil_new(:) => NULL()
    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_param) :: code_parameters

    character(F_STR_SIZE) :: equil_file_in, equil_file_out, username, tmpdir
    integer :: tmpsize
    integer :: output_flag
    character(len=:), pointer :: output_message

    print *,"fortran CHEASE wrapper"

    allocate(equil_old(1))

#ifdef PERF
    call c_getMillis(t0)
#endif
    ! transfer buf to CPO file
    call getenv("USER",username)
    call getenv("MUSCLE_TMP_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    equil_file_in = TRIM(tmpdir)//TRIM(username)//'_chease_equilibrium_in.cpo'
    call byte2file(equil_file_in, equil_in, size(equil_in))

    open (unit = 10, file = equil_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, equil_file_in )
       call read_cpo(equil_old(1), 'equilibrium' )
       call close_read_file
    else
       print *,"ERROR: no init equil" 
       STOP
    end if
#ifdef PERF
    call c_getMillis(t1)
    tread = t1 - t0
#endif
    

#ifdef PERF
    call c_getMillis(t0)
#endif
    print *,"get code params"
    !...  run CHEASE
    call fill_param(code_parameters, 'chease.xml', '', 'chease.xsd')

    print *,"run chease routine"
    call chease(equil_old, equil_new, code_parameters, output_flag, output_message)

#ifdef PERF
    call c_getMillis(t1)
    texec = t1 - t0
#endif


#ifdef PERF
    call c_getMillis(t0)
#endif
    ! transfer CPO to buf
    !...  write the results
    write(cptstr,'(I4.4)') init_step+cpt
    cpt = cpt+1
    equil_file_out = 'chease_equilibrium_'//cptstr//'.cpo'
    call open_write_file(11,equil_file_out)
    call write_cpo(equil_new(1),'equilibrium')
    call close_write_file

    call file2byte(equil_file_out, tmpbuf, tmpsize)
    allocate(equil_out(tmpsize))
    equil_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

#ifdef PERF
    call c_getMillis(t1)
    twrite = t1 - t0
#endif

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
    

  end subroutine chease2buf


end module chease_wrapper
