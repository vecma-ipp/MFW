module imp4dv_standalone
  use euitm_schemas
  implicit none

  integer, save :: init_step     !initial step count

  logical, save :: f_limit = .false.    ! option to set a heat flux limit
  real(8), save :: f_floor = 0._8       ! lower limit on heat flux

  interface
     subroutine imp4dv(equil, corep, coret, coret_dv)
       use euitm_schemas
       type (type_equilibrium), pointer ::  equil(:)
       type (type_coreprof), pointer :: corep(:)
       type (type_coretransp), pointer :: coret(:)
       type (type_coretransp), pointer :: coret_dv(:)
     end subroutine imp4dv
  end interface

contains

  subroutine imp4dv_cpo(equil, corep, coret, coret_dv) 
    implicit none

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_coretransp), pointer :: coret_dv(:)

    ! to limit heat flux values
    integer :: nft       ! number of flux tubes in coret
    integer :: ionn      ! same as 'nion' from coretransp cpo
    integer :: j,k       ! dummy variables

    print *,"fortran IMP4DV wrapper"

    ! setting heat flux to floor value
    nft = SIZE(coret(1)%values(1)%rho_tor)
    ionn = SIZE(coret(1)%values(1)%ni_transp%diff_eff, DIM=2)
    if (f_limit) then !if decide to apply floor to flux
       print *, "limit the following heat flux values to ",f_floor,":"
       do j = 1,nft

          if (coret(1)%values(1)%te_transp%flux(j) .lt. f_floor) then
             print *, "Te_transp%flux at fluxtube #",j,"=",coret(1)%values(1)%te_transp%flux(j)
             coret(1)%values(1)%te_transp%flux(j) = f_floor
          endif

          do k = 1,ionn

             if (coret(1)%values(1)%ti_transp%flux(j,k) .lt. f_floor) then
                print *, "Ti_transp%flux at fluxtube #",j,"=",coret(1)%values(1)%ti_transp%flux(j,k)
                coret(1)%values(1)%ti_transp%flux(j,k) = f_floor
             endif

          enddo

       enddo
    endif


    print *,"run imp4dv routine"
    call imp4dv(equil, corep, coret, coret_dv)

  end subroutine imp4dv_cpo


  subroutine imp4dv2buf(equil_in_buf, corep_in_buf, coret_in_buf, coret_out_buf)
    use iso_c_binding
    use string_binding
    use deallocate_structures
    use read_structures
    use write_structures
    use c_tools
    implicit none

    integer(kind=c_signed_char), pointer :: equil_in_buf(:)
    integer(kind=c_signed_char), pointer :: corep_in_buf(:)
    integer(kind=c_signed_char), pointer :: coret_in_buf(:)
    integer(kind=c_signed_char), pointer :: coret_out_buf(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil_in(:)
    type (type_coreprof), pointer :: corep_in(:)
    type (type_coretransp), pointer :: coret_in(:)
    type (type_coretransp), pointer :: coret_out(:)

    character(F_STR_SIZE) :: equil_in_file, corep_in_file
    character(F_STR_SIZE) :: coret_in_file, coret_out_file
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

    equil_in_file = TRIM(tmpdir)//TRIM(username)//'_imp4dv_equilibrium_in.cpo'
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

    corep_in_file = TRIM(tmpdir)//TRIM(username)//'_imp4dv_coreprof_in.cpo'
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

    coret_in_file = TRIM(tmpdir)//TRIM(username)//'_imp4dv_coretransp_in.cpo'
    call byte2file(coret_in_file, coret_in_buf, size(coret_in_buf))
    open (unit = 10, file = coret_in_file, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (10)
       call open_read_file(10, coret_in_file )
       call read_cpo(coret_in(1), 'coretransp')
       call close_read_file
    else
       print *,"ERROR: no input coretransp"
       STOP
    end if

    call imp4dv_cpo(equil_in, corep_in, coret_in, coret_out)

    ! transfer CPO to buf
    !...  write the results
    coret_out_file = 'imp4dv_coretransp_out.cpo'
    call open_write_file(11,coret_out_file)
    call write_cpo(coret_out(1),'coretransp')
    call close_write_file

    call file2byte(coret_out_file, tmpbuf, tmpsize)
    allocate(coret_out_buf(tmpsize))
    coret_out_buf(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

    call deallocate_cpo(equil_in)
    call deallocate_cpo(corep_in)
    call deallocate_cpo(coret_in)
    call deallocate_cpo(coret_out)

  end subroutine imp4dv2buf



end module imp4dv_standalone
