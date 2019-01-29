module init_cpo_wrapper
  use euitm_schemas
  implicit none

  real(8), save :: n_addcoef, n_multcoef

contains


  subroutine init_cpo2buf(cpos_in, &
       corep_out, &
       coret_out, &
       cores_out, &
       corei_out, &
       coren_out, &
       equil_out, &
       toroidf_out) 
    use iso_c_binding
    use string_binding
!    use itm_constants
    use read_structures
    use write_structures
    use deallocate_structures
    use c_tools
    implicit none

    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(in) :: cpos_in

    integer(kind=c_signed_char), pointer :: corep_out(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: cores_out(:)
    integer(kind=c_signed_char), pointer :: corei_out(:)
    integer(kind=c_signed_char), pointer :: coren_out(:)
    integer(kind=c_signed_char), pointer :: equil_out(:)
    integer(kind=c_signed_char), pointer :: toroidf_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)
    integer :: tmpsize = 0

    character(F_STR_SIZE) :: cpo_file
    character(F_STR_SIZE) :: corep_file, coret_file, cores_file, corei_file
    character(F_STR_SIZE) :: coren_file, equil_file, toroidf_file

    integer :: ios  

    type (type_coreprof), pointer :: corep(:) => NULL()
    type (type_coretransp), pointer :: coret(:) => NULL()
    type (type_coresource), pointer :: cores(:) => NULL()
    type (type_coreimpur), pointer :: corei(:) => NULL()
    type (type_coreneutrals), pointer :: coren(:) => NULL()
    type (type_equilibrium), pointer :: equil(:) => NULL()
    type (type_toroidfield), pointer :: toroidf(:) => NULL()


    print *,'in init_cpo_wrapper'

    !... allocate CPOs
    allocate(corep(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(coren(1))
    allocate(equil(1))
    allocate(toroidf(1))

    !... convert C strings into F strings
    call str_c2f(cpos_in, cpo_file)

    !... read CPOs from shared file
    open (unit = 10, file = cpo_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)

    if (ios == 0) then
       close (10)
       call open_read_file(10, cpo_file )
       call read_cpo(corep(1), 'coreprof' )
       call read_cpo(coret(1), 'coretransp' )
       call read_cpo(cores(1), 'coresource' )
       call read_cpo(corei(1), 'coreimpur' )
       call read_cpo(coren(1), 'coreneutrals' )
       call read_cpo(equil(1), 'equilibrium' )
       call read_cpo(toroidf(1), 'toroidfield' )
       call close_read_file
    else
       print *,'Cannot find a CPO file named ',cpo_file
       STOP
    end if


    ! modify n profile
    corep(1)%ne%value = corep(1)%ne%value * n_multcoef + n_addcoef
    corep(1)%ni%value = corep(1)%ni%value * n_multcoef + n_addcoef


    !... init names for each CPO ascii file
    corep_file = "init_coreprof.cpo"
    coret_file = "init_coretransp.cpo"
    cores_file = "init_coresource.cpo"
    corei_file = "init_coreimpur.cpo"
    coren_file = "init_coreneutrals.cpo"
    equil_file = "init_equilibrium.cpo"
    toroidf_file = "init_toroidfield.cpo"


    !...  write each CPO in its own file
    call open_write_file(11,corep_file)
    call write_cpo(corep(1),'coreprof')
    call close_write_file

    call open_write_file(12,coret_file)
    call write_cpo(coret(1),'coretransp')
    call close_write_file

    call open_write_file(13,cores_file)
    call write_cpo(cores(1),'coresource')
    call close_write_file

    call open_write_file(14,corei_file)
    call write_cpo(corei(1),'coreimpur')
    call close_write_file

    call open_write_file(15,coren_file)
    call write_cpo(coren(1),'coreneutrals')
    call close_write_file

    call open_write_file(16,equil_file)
    call write_cpo(equil(1),'equilibrium')
    call close_write_file

    call open_write_file(17,toroidf_file)
    call write_cpo(toroidf(1),'toroidfield')
    call close_write_file


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

    call file2byte(coren_file, tmpbuf, tmpsize)
    allocate(coren_out(tmpsize))
    coren_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(equil_file, tmpbuf, tmpsize)
    allocate(equil_out(tmpsize))
    equil_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call file2byte(toroidf_file, tmpbuf, tmpsize)
    allocate(toroidf_out(tmpsize))
    toroidf_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)


    print *,'deallocate cpos'
    call deallocate_cpo(corep)
    call deallocate_cpo(coret)
    call deallocate_cpo(cores)
    call deallocate_cpo(corei)
    call deallocate_cpo(coren)
    call deallocate_cpo(equil)
    call deallocate_cpo(toroidf)

  end subroutine init_cpo2buf



  subroutine init_cpo2file(cpos_in, &
       corep_out, &
       coret_out, &
       cores_out, &
       corei_out, &
       coren_out, &
       equil_out, &
       toroidf_out) 
    use iso_c_binding
    use string_binding
!    use itm_constants
    use read_structures
    use write_structures
    use deallocate_structures
    implicit none

    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(in) :: cpos_in
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: corep_out
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: coret_out
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: cores_out
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: corei_out
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: coren_out
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: equil_out
    character(kind=c_char,len=1), dimension(F_STR_SIZE), intent(out) :: toroidf_out
    character(F_STR_SIZE) :: cpo_file, corep_file, coret_file, cores_file, corei_file
    character(F_STR_SIZE) :: coren_file, equil_file, toroidf_file

    integer :: ios

    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_coresource), pointer :: cores(:)
    type (type_coreimpur), pointer :: corei(:)
    type (type_coreneutrals), pointer :: coren(:)
    type (type_equilibrium), pointer :: equil(:)
    type (type_toroidfield), pointer :: toroidf(:)


    !print *,"fortran init_cpo wrapper"

    !... init names for each CPO ascii file
    corep_file = "init_coreprof.cpo"
    coret_file = "init_coretransp.cpo"
    cores_file = "init_coresource.cpo"
    corei_file = "init_coreimpur.cpo"
    coren_file = "init_coreneutrals.cpo"
    equil_file = "init_equilibrium.cpo"
    toroidf_file = "init_toroidfield.cpo"

    !... convert names into C strings
    call str_f2c(corep_file,corep_out)
    call str_f2c(coret_file,coret_out)
    call str_f2c(cores_file,cores_out)
    call str_f2c(corei_file,corei_out)
    call str_f2c(coren_file,coren_out)
    call str_f2c(equil_file,equil_out)
    call str_f2c(toroidf_file,toroidf_out)

    !... convert C strings into F strings
    call str_c2f(cpos_in, cpo_file)


    !... allocate CPOs
    allocate(corep(1))
    allocate(coret(1))
    allocate(cores(1))
    allocate(corei(1))
    allocate(coren(1))
    allocate(equil(1))
    allocate(toroidf(1))


    !... read CPOs from shared file
    open (unit = 10, file = cpo_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)

    if (ios == 0) then
       close (10)
       call open_read_file(10, cpo_file )
       call read_cpo(corep(1), 'coreprof' )
       call read_cpo(coret(1), 'coretransp' )
       call read_cpo(cores(1), 'coresource' )
       call read_cpo(corei(1), 'coreimpur' )
       call read_cpo(coren(1), 'coreneutrals' )
       call read_cpo(equil(1), 'equilibrium' )
       call read_cpo(toroidf(1), 'toroidfield' )
       call close_read_file
    else
       print *,'Cannot find a CPO file named ',cpo_file
       STOP
    end if


    !...  write each CPO in its own file
    call open_write_file(11,corep_file)
    call write_cpo(corep(1),'coreprof')
    call close_write_file

    call open_write_file(12,coret_file)
    call write_cpo(coret(1),'coretransp')
    call close_write_file

    call open_write_file(13,cores_file)
    call write_cpo(cores(1),'coresource')
    call close_write_file

    call open_write_file(14,corei_file)
    call write_cpo(corei(1),'coreimpur')
    call close_write_file

    call open_write_file(15,coren_file)
    call write_cpo(coren(1),'coreneutrals')
    call close_write_file

    call open_write_file(16,equil_file)
    call write_cpo(equil(1),'equilibrium')
    call close_write_file

    call open_write_file(17,toroidf_file)
    call write_cpo(toroidf(1),'toroidfield')
    call close_write_file

    !print *,"return from fortran wrapper"

    deallocate(corep)
    deallocate(coret)
    deallocate(cores)
    deallocate(corei)
    deallocate(coren)
    deallocate(equil)
    deallocate(toroidf)


  end subroutine init_cpo2file

end module init_cpo_wrapper
