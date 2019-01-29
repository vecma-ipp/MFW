module write_ual_wrapper
  use euitm_schemas
  implicit none

  logical, save :: first = .true.
  integer, save :: shot = 0
  integer, save :: run = 0
  integer, save :: idx = -1
  character(255), save :: machine

contains

#ifdef HAS_UAL
  subroutine write_buf2ual(corep_in, &
       coret_in, &
       equil_in)
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use deallocate_structures
    use c_tools
    use euitm_routines
    implicit none

    
    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: coret_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)

    character(F_STR_SIZE) :: corep_file, coret_file, equil_file, username, tmpdir

    integer :: ios

    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_coretransp), pointer :: coret(:) => NULL()
    type (type_equilibrium), pointer :: equil(:) => NULL()

    integer :: tmpsize

    print *,'in write_ual_wrapper'

    !... allocate CPOs
    allocate(corep(1))
    allocate(coret(1))
    allocate(equil(1))



    ! transfer buf to CPO
    call getenv("USER",username)
    call getenv("MUSCLE_TMP_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    corep_file = TRIM(tmpdir)//TRIM(username)//'_write_ual_coreprof_in.cpo'
    equil_file = TRIM(tmpdir)//TRIM(username)//'_write_ual_equilibrium_in.cpo'
    coret_file = TRIM(tmpdir)//TRIM(username)//'_write_ual_coretransp_in.cpo'
    call byte2file(corep_file, corep_in, size(corep_in))
    call byte2file(equil_file, equil_in, size(equil_in))
    call byte2file(coret_file, coret_in, size(coret_in))

    open (unit = 10, file = corep_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_file )
       call read_cpo(corep(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found ", corep_file
       STOP
    end if
    open (unit = 11, file = equil_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (11)
       call open_read_file(11, equil_file )
       call read_cpo(equil(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found ", coret_file
       STOP
    end if
    open (unit = 12, file = coret_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (12)
       call open_read_file(12, coret_file )
       call read_cpo(coret(1), 'coretransp' )
       call close_read_file
    else
       print *,"CPO file not found ", coret_file
       STOP
    end if


    if (first) then
       print *,'first create a new entry in local DB shot=',shot,', run=',run
       call euitm_create_env('euitm',shot,run,0,0,idx,TRIM(username),machine,"4.10b")
       print *,'now put non timed fields'
       call euitm_put_non_timed(idx,'coreprof',corep(1))
       call euitm_put_non_timed(idx,'equilibrium',equil(1))
       call euitm_put_non_timed(idx,'coretransp',coret(1))
       first = .false.
    endif

    print *,'then put timed fields'
    call euitm_put_slice(idx,'coreprof',corep(1))
    call euitm_put_slice(idx,'equilibrium',equil(1))
    call euitm_put_slice(idx,'coretransp',coret(1))


    print *,'deallocate cpos'
    call deallocate_cpo(corep)
    call deallocate_cpo(coret)
    call deallocate_cpo(equil)

  end subroutine write_buf2ual

  subroutine close_buf2ual()
    use euitm_routines
    implicit none

    print *,'CLOSE ual'
    call euitm_close(idx)
  end subroutine close_buf2ual

#endif

end module write_ual_wrapper
