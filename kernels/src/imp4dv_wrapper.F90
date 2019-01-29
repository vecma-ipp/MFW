module imp4dv_wrapper
  use euitm_schemas
  use c_tools
#ifdef PERF
  use c_perf
#endif
  implicit none

  integer, save :: init_step     !initial step count

  logical, save :: f_limit = .false.    ! option to set a heat flux limit
  real(8), save :: f_floor = 0._8       ! lower limit on heat flux

#ifdef PERF
  integer(kind=c_long_long), save :: t0,t1,tread,texec,twrite
#endif

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



  subroutine imp4dv2buf(equil_in, corep_in, coret_in, coret_out) 
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
    type (type_coretransp), pointer :: coret_dv(:)

    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_in, coret_file_out, username, tmpdir
    integer :: tmpsize

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

   ! to limit heat flux values
   integer :: nft       ! number of flux tubes in coret
   integer :: ionn      ! same as 'nion' from coretransp cpo
   integer :: j,k       ! dummy variables

    print *,"fortran IMP4DV wrapper"

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
    corep_file_in = TRIM(tmpdir)//TRIM(username)//'_imp4dv_coreprof_in.cpo'
    equil_file_in = TRIM(tmpdir)//TRIM(username)//'_imp4dv_equilibrium_in.cpo'
    coret_file_in = TRIM(tmpdir)//TRIM(username)//'_imp4dv_coretransp_in.cpo'

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


#ifdef PERF
    call c_getMillis(t0)
#endif
    print *,"run imp4dv routine"
    call imp4dv(equil, corep, coret, coret_dv)
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
    coret_file_out = 'imp4dv_coretransp_'//cptstr//'.cpo'
    call open_write_file(12,coret_file_out)
    call write_cpo(coret_dv(1),'coretransp')
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
    call deallocate_cpo(coret_dv)

  end subroutine imp4dv2buf


end module imp4dv_wrapper
