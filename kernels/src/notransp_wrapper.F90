module notransp_wrapper
  use euitm_schemas
  implicit none

  integer, save :: init_step = 0     !initial step count

contains

  subroutine notransp2buf(equil_in, corep_in, coret_out) 
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use xml_file_reader
    use deallocate_structures
    use c_tools
    use mod_turb
    implicit none

    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_param) :: code_parameters

    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_out, username, tmpdir
    integer :: tmpsize

    integer :: ios,i
    integer, save :: cpt = 0
    character(4)  :: cptstr

    ! hack => check test in ets wrapper
    character(len = 132), target :: codename(1) = 'GEM'
    character(len = 132), target :: codeversion(1) = 'Dec 2012'

    print *,"fortran NOTRANSP wrapper"


    !...  set initial state
    allocate(corep(1))
    allocate(equil(1))    


    ! transfer buf to CPO
    ! transfer buf to CPO
    call getenv("USER",username)
    call getenv("MUSCLE_TMP_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    corep_file_in = TRIM(tmpdir)//TRIM(username)//'_notransp_coreprof_in.cpo'
    equil_file_in = TRIM(tmpdir)//TRIM(username)//'_notransp_equilibrium_in.cpo'
    call byte2file(corep_file_in, corep_in, size(corep_in))
    call byte2file(equil_file_in, equil_in, size(equil_in))

    open (unit = 10, file = corep_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, corep_file_in )
       call read_cpo(corep(1), 'coreprof' )
       call close_read_file
    else
       print *,"CPO file not found ", corep_file_in
       STOP
    end if
    open (unit = 11, file = equil_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (11)
       call open_read_file(11, equil_file_in )
       call read_cpo(equil(1), 'equilibrium' )
       call close_read_file
    else
       print *,"CPO file not found ", equil_file_in
       STOP
    end if

    print *,"fill empty coretransp with 8 fluxtubes"
    allocate(coret(1))
    call turb_constructor(coret(1), 1, 8, 1)
    
    do i=0,7
       coret(1)%values(1)%rho_tor_norm(i+1) = &
            (REAL(2*i+1)/REAL(2*8))**0.7
    end do
    coret(1)%values(1)%rho_tor = coret(1)%values(1)%rho_tor_norm * &
         MAXVAL(equil(1)%profiles_1d%rho_tor)

    allocate(coret(1)%codeparam%codename(1))
    allocate(coret(1)%codeparam%codeversion(1))
    coret(1)%codeparam%codename = codename
    coret(1)%codeparam%codeversion = codeversion


    ! transfer CPO to buf
    !print *,"write coretransp CPO"
    write(cptstr,'(I4.4)') init_step+cpt
    cpt = cpt+1
    coret_file_out = 'notransp_coretransp_'//cptstr//'.cpo'
    !coret_file_out = 'notransp_coretransp_out.cpo'
    call open_write_file(12,coret_file_out)
    call write_cpo(coret(1),'coretransp')
    call close_write_file

    call file2byte(coret_file_out, tmpbuf, tmpsize)
    allocate(coret_out(tmpsize))
    coret_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call deallocate_cpo(corep)
    call deallocate_cpo(equil)
    call deallocate_cpo(coret)

  end subroutine notransp2buf


end module notransp_wrapper
