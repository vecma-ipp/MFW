module equilupdate_standalone
  use euitm_schemas
  implicit none

  interface 
     subroutine equil_input(corep, toroidf, equil, equil_out)
       use euitm_schemas
       type(type_coreprof), pointer :: corep_in(:)
       type(type_toroidfield), pointer :: toroidf_in(:)
       type(type_equilibrium), pointer :: equil_in(:), equil_out(:)
     end subroutine equil_input
  end interface

contains

  subroutine equilupdate2cpo(corep_in, toroidf_in, equil_in, equil_out)
    use euitm_schemas, only: type_coreprof, type_toroidfield, type_equilibrium
    use equilibrium_input, only: equil_input
    implicit none

    type(type_coreprof), pointer :: corep_in(:)
    type(type_toroidfield), pointer :: toroidf_in(:)
    type(type_equilibrium), pointer :: equil_in(:), equil_out(:)

    call equil_input(corep_in, toroidf_in, equil_in, equil_out)

  end subroutine equilupdate2cpo




  subroutine equilupdate2buf(corep_in_buf, toroidf_in_buf, equil_in_buf, equil_out_buf)
    use iso_c_binding
    use string_binding
    use deallocate_structures
    use read_structures
    use write_structures
    use c_tools
    implicit none

    character(kind=c_char), pointer :: equil_in_buf(:)
    character(kind=c_char), pointer :: corep_in_buf(:)
    character(kind=c_char), pointer :: toroidf_in_buf(:)
    character(kind=c_char), pointer :: equil_out_buf(:)
    character(kind=c_char), pointer :: tmpbuf(:)

    type(type_coreprof), pointer :: corep_in(:)
    type(type_toroidfield), pointer :: toroidf_in(:)
    type(type_equilibrium), pointer :: equil_in(:), equil_out(:)

    character(F_STR_SIZE) :: equil_in_file, corep_in_file, toroidf_in_file, equil_out_file
    character(F_STR_SIZE) :: username, tmpdir
    integer :: tmpsize, ios

    allocate(equil_in(1))
    allocate(corep_in(1))
    allocate(toroidf_in(1))

    call getenv("USER",username)
    call getenv("CPO_SERIALIZATION_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if

    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_equilupdate_equilibrium_in.cpo'
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

    corep_in_file = TRIM(tmpdir)//TRIM(username)//'_equilupdate_coreprof_in.cpo'
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

    toroidf_in_file = TRIM(tmpdir)//TRIM(username)//'_equilupdate_toroidfield_in.cpo'
    call byte2file(toroidf_in_file, toroidf_in_buf, size(toroidf_in_buf))
    open (unit = 10, file = toroidf_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, toroidf_in_file )
       call read_cpo(toroidf_in(1), 'toroidfield')
       call close_read_file
    else
       print *,"ERROR: no input toroidfield"
       STOP
    end if

    nullify(equil_out)
    call equilupdate2cpo(corep_in, toroidf_in, equil_in, equil_out)

    ! transfer CPO to buf
    !...  write the results
    equil_out_file = 'equilupdate_equilibrium_out.cpo'
    call open_write_file(11,equil_out_file)
    call write_cpo(equil_out(1),'equilibrium')
    call close_write_file

    call file2byte(equil_out_file, tmpbuf, tmpsize)
    allocate(equil_out_buf(tmpsize))
    equil_out_buf(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call deallocate_cpo(equil_in)
    call deallocate_cpo(corep_in)
    call deallocate_cpo(toroidf_in)
    call deallocate_cpo(equil_out)

  end subroutine equilupdate2buf





  subroutine equilupdate2file(corep_in_file, toroidf_in_file, equil_in_file, equil_out_file)
    use euitm_schemas, only: type_coreprof, type_toroidfield, type_equilibrium
    use equilibrium_input, only: equil_input
    use read_structures
    use write_structures
    use deallocate_structures
    implicit none

    character(len=*), intent(in) :: corep_in_file
    character(len=*), intent(in) :: toroidf_in_file
    character(len=*), intent(in) :: equil_in_file
    character(256), intent(out) :: equil_out_file

    type(type_coreprof), pointer :: corep_in(:)
    type(type_toroidfield), pointer :: toroidf_in(:)
    type(type_equilibrium), pointer :: equil_in(:), equil_out(:)

    integer :: ios

    allocate(corep_in(1))
    allocate(toroidf_in(1))
    allocate(equil_in(1))

    open (unit = 10, file = corep_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_in_file )
       call read_cpo(corep_in(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found:",corep_in_file
       STOP
    end if
    open (unit = 10, file = toroidf_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, toroidf_in_file )
       call read_cpo(toroidf_in(1), 'toroidfield' )
       call close_read_file
    else
       print *,"CPO file not found:",toroidf_in_file
       STOP
    end if
    open (unit = 10, file = equil_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, equil_in_file )
       call read_cpo(equil_in(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found:",equil_in_file
       STOP
    end if

    call equil_input(corep_in, toroidf_in, equil_in, equil_out)

    equil_out_file = 'ets_equilibrium_out.cpo'
    !...  write the results
    call open_write_file(11,equil_out_file)
    call write_cpo(equil_out(1),'equilibrium')
    call close_write_file

    call deallocate_cpo(corep_in)
    call deallocate_cpo(toroidf_in)
    call deallocate_cpo(equil_in)
    call deallocate_cpo(equil_out)

  end subroutine equilupdate2file


end module equilupdate_standalone
