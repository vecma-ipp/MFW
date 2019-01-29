module continue_wrapper
  use euitm_schemas
  implicit none


contains


  subroutine continue2buf(path, &
       corep_out, &
       coret_out, &
       cores_out, &
       corei_out, &
       equil_out, &
       toroidf_out) 
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use deallocate_structures
    use c_tools
    implicit none

    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(in) :: path

    integer(kind=c_signed_char), pointer :: corep_out(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: cores_out(:)
    integer(kind=c_signed_char), pointer :: corei_out(:)
    integer(kind=c_signed_char), pointer :: equil_out(:)
    integer(kind=c_signed_char), pointer :: toroidf_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)
    integer :: tmpsize = 0

    character(F_STR_SIZE) :: init_path
    character(F_STR_SIZE) :: corep_file, coret_file, cores_file, corei_file
    character(F_STR_SIZE) :: equil_file, toroidf_file

    integer :: ios

    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_coretransp), pointer :: coret(:) => NULL()
    type (type_coresource), pointer :: cores(:) => NULL()
    type (type_coreimpur), pointer :: corei(:) => NULL()
    type (type_equilibrium), pointer :: equil(:) => NULL()
    type (type_toroidfield), pointer :: toroidf(:) => NULL()


    print *,'in continue_wrapper'

    !... allocate CPOs
    allocate(corep(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(equil(1))
    allocate(toroidf(1))

    !... convert C strings into F strings
    call str_c2f(path, init_path)

    if (init_path(len_trim(init_path):len_trim(init_path)) .ne. '/') then
       print *,"Add '/' to path=",trim(init_path)
       init_path = trim(init_path)//'/'
    end if
    print *,"Path=",trim(init_path)


!!$    !... read CPOs from shared file
!!$    open (unit = 10, file = cpo_file, &
!!$         status = 'old', form = 'formatted', &
!!$         action = 'read', iostat = ios)
!!$
!!$    if (ios == 0) then
!!$       close (10)
!!$       call open_read_file(10, cpo_file )
!!$       call read_cpo(corep(1), 'coreprof' )
!!$       call read_cpo(coret(1), 'coretransp' )
!!$       call read_cpo(cores(1), 'coresource' )
!!$       call read_cpo(corei(1), 'coreimpur' )
!!$       call read_cpo(equil(1), 'equilibrium' )
!!$       call read_cpo(toroidf(1), 'toroidfield' )
!!$       call close_read_file
!!$    else
!!$       print *,'Cannot find a CPO file named ',cpo_file
!!$       STOP
!!$    end if


    !... init names for each CPO ascii file
    corep_file = trim(init_path)//"ets_coreprof_in.cpo"
    coret_file = trim(init_path)//"ets_coretransp_in.cpo"
    cores_file = trim(init_path)//"ets_coresource_in.cpo"
    corei_file = trim(init_path)//"ets_coreimpur_in.cpo"
    equil_file = trim(init_path)//"ets_equilibrium_in.cpo"
    toroidf_file = trim(init_path)//"ets_toroidfield_in.cpo"


    !...  write each CPO in its own file
    call open_read_file(11,corep_file)
    call read_cpo(corep(1),'coreprof')
    call close_read_file

    call open_read_file(12,coret_file)
    call read_cpo(coret(1),'coretransp')
    call close_read_file

    call open_read_file(13,cores_file)
    call read_cpo(cores(1),'coresource')
    call close_read_file

    call open_read_file(14,corei_file)
    call read_cpo(corei(1),'coreimpur')
    call close_read_file

    call open_read_file(16,equil_file)
    call read_cpo(equil(1),'equilibrium')
    call close_read_file

    call open_read_file(17,toroidf_file)
    call read_cpo(toroidf(1),'toroidfield')
    call close_read_file


    print *,'transfer CPO to buf'
    call file2byte(corep_file, tmpbuf, tmpsize)
    allocate(corep_out(tmpsize))
    corep_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(coret_file, tmpbuf, tmpsize)
    allocate(coret_out(tmpsize))
    coret_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(cores_file, tmpbuf, tmpsize)
    allocate(cores_out(tmpsize))
    cores_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(corei_file, tmpbuf, tmpsize)
    allocate(corei_out(tmpsize))
    corei_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(equil_file, tmpbuf, tmpsize)
    allocate(equil_out(tmpsize))
    equil_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(toroidf_file, tmpbuf, tmpsize)
    allocate(toroidf_out(tmpsize))
    toroidf_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)


    call deallocate_cpo(corep)
    call deallocate_cpo(coret)
    call deallocate_cpo(cores)
    call deallocate_cpo(corei)
    call deallocate_cpo(equil)
    call deallocate_cpo(toroidf)

  end subroutine continue2buf



end module continue_wrapper
