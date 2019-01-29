module dv2flux_wrapper
  use euitm_schemas
  use c_tools
#ifdef PERF
  use c_perf
#endif
  implicit none

  integer, save :: init_step     !initial step count

#ifdef PERF
  integer(kind=c_long_long), save :: t0,t1,tread,texec,twrite
#endif


contains


  subroutine dv2flux2buf(equil_in, corep_in, coret_in, coret_out) 
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use deallocate_structures
    implicit none

    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: coret_in(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:)
    type (type_coretransp), pointer :: coret_fl(:)

    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_in, coret_file_out, username, tmpdir
    integer :: tmpsize

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

    print *,"fortran DV2FLUX wrapper"

    allocate(corep(1))
    allocate(equil(1))  
    allocate(coret(1))


#ifdef PERF
    call c_getMillis(t0)
#endif
    ! transfer buf to CPO
    call getenv("USER",username)
    call getenv("MUSCLE_TMP_DIR",tmpdir)
    tmpsize = len_trim(tmpdir)
    if (tmpsize.ne.0) then
       if (tmpdir(tmpsize:tmpsize) .ne. '/') then
          tmpdir = trim(tmpdir)//'/'
       end if
    end if
    corep_file_in = TRIM(tmpdir)//TRIM(username)//'_dv2flux_coreprof_in.cpo'
    equil_file_in = TRIM(tmpdir)//TRIM(username)//'_dv2flux_equilibrium_in.cpo'
    coret_file_in = TRIM(tmpdir)//TRIM(username)//'_dv2flux_coretransp_in.cpo'

    call byte2file(corep_file_in, corep_in, size(corep_in))
    call byte2file(equil_file_in, equil_in, size(equil_in))
    call byte2file(coret_file_in, coret_in, size(coret_in))

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
    open (unit = 12, file = coret_file_in, &
         status = 'old', form = 'formatted', &
         action = 'read', iostat = ios)
    if (ios == 0) then
       close (12)
       call open_read_file(12, coret_file_in )
       call read_cpo(coret(1), 'coretransp' )
       call close_read_file
    else
       print *,"CPO file not found ", coret_file_in
       STOP
    end if
#ifdef PERF
    call c_getMillis(t1)
    tread = t1 - t0
#endif


#ifdef PERF
    call c_getMillis(t0)
#endif
    print *,"run dv2flux routine"
    call dv2flux(coret, equil, corep, coret_fl)
#ifdef PERF
    call c_getMillis(t1)
    texec = t1 - t0
#endif

#ifdef PERF
    call c_getMillis(t0)
#endif
    ! transfer CPO to buf
    write(cptstr,'(I4.4)') init_step+cpt
    cpt = cpt+1
    coret_file_out = 'dv2flux_coretransp_'//cptstr//'.cpo'
    call open_write_file(12,coret_file_out)
    call write_cpo(coret_fl(1),'coretransp')
    call close_write_file

    call file2byte(coret_file_out, tmpbuf, tmpsize)
    allocate(coret_out(tmpsize))
    coret_out(1:tmpsize) = tmpbuf(1:tmpsize)
    call dealloc_cbytebuf(tmpbuf)

#ifdef PERF
    call c_getMillis(t1)
    twrite = t1 - t0
#endif


    call deallocate_cpo(corep)
    call deallocate_cpo(equil)
    call deallocate_cpo(coret)
    call deallocate_cpo(coret_fl)

  end subroutine dv2flux2buf

  subroutine dv2flux(coret_old, equil, corep, coret_new)
    ! check which modules	to use !
    use euitm_schemas
    use copy_structures
    use deallocate_structures
    implicit none

    type (type_coretransp), pointer  :: coret_old(:), coret_new(:)
    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer    :: corep(:), corep_int(:)

    !add other integer and real arrays!

    integer :: i
    integer :: nrho_corep, nrho_coret, npsi
    real    :: equil_prof

    real,allocatable :: vprime(:), gm3(:)
    real,allocatable :: ddrho(:), ne_value(:)
    real,allocatable :: diff_eff(:), vconv_eff(:), ne_flux(:)

    print *,"make sure number of rho_tor grids in coreprof matches with the one in coretransp"

    nrho_corep = size(corep(1)%rho_tor)
    nrho_coret = size(coret_old(1)%values(1)%rho_tor)
    npsi       = size(equil(1)%profiles_1d%psi)


    allocate(corep_int(1))
    call copy_cpo(corep(1),corep_int(1))

    if (nrho_corep /= nrho_coret) then

       if (associated(corep(1)%ne%ddrho)) then
          deallocate(corep_int(1)%ne%ddrho)
          allocate(corep_int(1)%ne%ddrho(nrho_coret))
          call Linterp(corep(1)%ne%ddrho, corep(1)%rho_tor, nrho_corep, &
               corep_int(1)%ne%ddrho, corep_int(1)%rho_tor, nrho_coret)
       endif

       if (associated(corep(1)%ne%value)) then
          deallocate(corep_int(1)%ne%value)
          allocate(corep_int(1)%ne%value(nrho_coret))
          call Linterp(corep(1)%ne%value, corep(1)%rho_tor, nrho_corep, &
               corep_int(1)%ne%value, corep_int(1)%rho_tor, nrho_coret)
       endif

    endif



    print *,"calculate electron flux"

    call copy_cpo(coret_old(1),coret_new(1))

    allocate(vprime(1:npsi))
    allocate(gm3(1:npsi))
    allocate(ddrho(1:nrho_coret))
    allocate(ne_value(1:nrho_coret))
    allocate(diff_eff(1:nrho_coret))
    allocate(vconv_eff(1:nrho_coret))
    allocate(ne_flux(1:nrho_coret))


    vprime    = equil(1)%profiles_1d%vprime
    gm3       = equil(1)%profiles_1d%gm3

    ddrho     = corep_int(1)%ne%ddrho
    ne_value  = corep_int(1)%ne%value

    diff_eff  = coret_old(1)%values(1)%Te_transp%diff_eff 
    vconv_eff = coret_old(1)%values(1)%Te_transp%vconv_eff

    equil_prof = 0.0
    do i = 1,npsi
       equil_prof = equil_prof + vprime(i) * gm3(i)
    enddo

    ne_flux = equil_prof * (-1.*diff_eff*ddrho + ne_value*vconv_eff)

    coret_new(1)%values(1)%ne_transp%flux = ne_flux

    deallocate(vprime,gm3,ddrho,ne_value,diff_eff,vconv_eff,ne_flux)

  end subroutine dv2flux

end module dv2flux_wrapper

