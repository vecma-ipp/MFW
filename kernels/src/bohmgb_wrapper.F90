module bohmgb_wrapper
  use euitm_schemas
  use c_tools
#ifdef PERF
  use c_perf
#endif
  implicit none

  integer, save :: init_step      !initial step count
  logical, save :: cus_ftubes     !Y/N on f.tubes customisa
  integer, save :: nftubes        !# of fluxtubes (customise)

#ifdef PERF
  integer(kind=c_long_long), save :: t0,t1,tread,texec,twrite
#endif

  interface
     subroutine bohmgb(equil, corep, coret, code_parameters)
       use euitm_schemas
       type (type_equilibrium), pointer ::  equil(:)
       type (type_coreprof), pointer :: corep(:)
       type (type_coretransp), pointer :: coret(:)
       type (type_param) :: code_parameters
     end subroutine bohmgb
  end interface

contains



  subroutine bohmgb2buf(equil_in, corep_in, coret_out)
    use iso_c_binding
    use string_binding
    use read_structures
    use write_structures
    use xml_file_reader
    use deallocate_structures
    use copy_structures
    implicit none

    integer(kind=c_signed_char), pointer :: corep_in(:)
    integer(kind=c_signed_char), pointer :: equil_in(:)
    integer(kind=c_signed_char), pointer :: coret_out(:)
    integer(kind=c_signed_char), pointer :: tmpbuf(:)

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:), coret_fluxtube(:)
    type (type_param) :: code_parameters

    character(F_STR_SIZE) :: corep_file_in, equil_file_in, coret_file_out, username, tmpdir
    integer :: tmpsize

    integer :: ios
    integer, save :: cpt = 0
    character(4)  :: cptstr

    integer :: nrhotor                                 !# of fluxtubes (original)
    integer :: nion                                    !# of elements in diff_eff
    integer :: i,iion,icon                             !dummy variables
    integer,dimension(:),allocatable :: myftube        !fluxtube ID (customise)
    real,dimension(:),allocatable :: rho_tor_norm_out  !normalised rho_tor (customise)

    print *,"fortran BOHMGB wrapper"

    allocate(corep(1))
    allocate(equil(1))    


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
    corep_file_in = TRIM(tmpdir)//TRIM(username)//'_bohmgb_coreprof_in.cpo'
    equil_file_in = TRIM(tmpdir)//TRIM(username)//'_bohmgb_equilibrium_in.cpo'

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
#ifdef PERF
    call c_getMillis(t1)
    tread = t1 - t0
#endif


#ifdef PERF
    call c_getMillis(t0)
#endif
    print *,"get code params"
    call fill_param(code_parameters, 'bohmgb.xml', '', 'bohmgb.xsd')

    print *,"run bohmgb routine"
    call bohmgb(equil, corep, coret, code_parameters)
#ifdef PERF
    call c_getMillis(t1)
    texec = t1 - t0
#endif

!**********   customisation on flux tube locations   **********
!**********    and CPO field values interpolation    **********



    if (cus_ftubes == .true.) then

       print *,"customise the placement of the flux tubes"

       nrhotor = size(coret(1)%values(1)%rho_tor)
       nion = size(coret(1)%values(1)%ni_transp%diff_eff, DIM=2)

       allocate(myftube(1:nftubes))
       allocate(rho_tor_norm_out(1:nftubes))

       do i=1,nftubes
          myftube(i) = i-1
       enddo

       rho_tor_norm_out = (REAL(2*myftube+1)/REAL(2*nftubes))**0.7

! allocate the new output coretransp CPO
       allocate(coret_fluxtube(1))
       call copy_cpo(coret(1),coret_fluxtube(1))

       deallocate(coret_fluxtube(1)%values(1)%rho_tor_norm)
       allocate(coret_fluxtube(1)%values(1)%rho_tor_norm(nftubes))
       coret_fluxtube(1)%values(1)%rho_tor_norm = rho_tor_norm_out

       deallocate(coret_fluxtube(1)%values(1)%rho_tor)
       allocate(coret_fluxtube(1)%values(1)%rho_tor(nftubes))
       coret_fluxtube(1)%values(1)%rho_tor = coret_fluxtube(1)%values(1)%rho_tor_norm * &
          MAXVAL(equil(1)%profiles_1d%rho_tor)

! resize coret_fluxtube CPO fields and interpolate

! sigma
       if(associated(coret(1)%values(1)%sigma)) then
          deallocate(coret_fluxtube(1)%values(1)%sigma)
          allocate(coret_fluxtube(1)%values(1)%sigma(nftubes))
          call Linterp(coret(1)%values(1)%sigma, coret(1)%values(1)%rho_tor_norm, nrhotor, &
               coret_fluxtube(1)%values(1)%sigma, coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
       endif

! ni
       if(associated(coret(1)%values(1)%ni_transp%diff_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%ni_transp%diff_eff)
          allocate(coret_fluxtube(1)%values(1)%ni_transp%diff_eff(nftubes,nion,3))
          do iion=1,nion
             do icon=1,3
                call Linterp(coret(1)%values(1)%ni_transp%diff_eff(:,iion,icon), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                     coret_fluxtube(1)%values(1)%ni_transp%diff_eff(:,iion,icon), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
             enddo
          enddo
       endif
       if(associated(coret(1)%values(1)%ni_transp%vconv_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%ni_transp%vconv_eff)
          allocate(coret_fluxtube(1)%values(1)%ni_transp%vconv_eff(nftubes,nion,3))
          do iion=1,nion
             do icon=1,3
                call Linterp(coret(1)%values(1)%ni_transp%vconv_eff(:,iion,icon), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                     coret_fluxtube(1)%values(1)%ni_transp%vconv_eff(:,iion,icon), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
             enddo
          enddo
       endif
       if(associated(coret(1)%values(1)%ni_transp%flux)) then
          deallocate(coret_fluxtube(1)%values(1)%ni_transp%flux)
          allocate(coret_fluxtube(1)%values(1)%ni_transp%flux(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%ni_transp%flux(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%ni_transp%flux(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif

! ne
       if(associated(coret(1)%values(1)%ne_transp%diff_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%ne_transp%diff_eff)
          allocate(coret_fluxtube(1)%values(1)%ne_transp%diff_eff(nftubes,3))
          do icon=1,3
             call Linterp(coret(1)%values(1)%ne_transp%diff_eff(:,icon), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%ne_transp%diff_eff(:,icon), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif
       if(associated(coret(1)%values(1)%ne_transp%vconv_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%ne_transp%vconv_eff)
          allocate(coret_fluxtube(1)%values(1)%ne_transp%vconv_eff(nftubes,3))
          do icon=1,3
             call Linterp(coret(1)%values(1)%ne_transp%vconv_eff(:,icon), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%ne_transp%vconv_eff(:,icon), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif
       if(associated(coret(1)%values(1)%ne_transp%flux)) then
          deallocate(coret_fluxtube(1)%values(1)%ne_transp%flux)
          allocate(coret_fluxtube(1)%values(1)%ne_transp%flux(nftubes))
          call Linterp(coret(1)%values(1)%ne_transp%flux, coret(1)%values(1)%rho_tor_norm, nrhotor, &
               coret_fluxtube(1)%values(1)%ne_transp%flux, coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
       endif

! Ti
       if(associated(coret(1)%values(1)%Ti_transp%diff_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%Ti_transp%diff_eff)
          allocate(coret_fluxtube(1)%values(1)%Ti_transp%diff_eff(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%Ti_transp%diff_eff(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%Ti_transp%diff_eff(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif
       if(associated(coret(1)%values(1)%Ti_transp%vconv_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%Ti_transp%vconv_eff)
          allocate(coret_fluxtube(1)%values(1)%Ti_transp%vconv_eff(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%Ti_transp%vconv_eff(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%Ti_transp%vconv_eff(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif
       if(associated(coret(1)%values(1)%Ti_transp%flux)) then
          deallocate(coret_fluxtube(1)%values(1)%Ti_transp%flux)
          allocate(coret_fluxtube(1)%values(1)%Ti_transp%flux(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%Ti_transp%flux(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%Ti_transp%flux(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif

! Te
       if(associated(coret(1)%values(1)%Te_transp%diff_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%Te_transp%diff_eff)
          allocate(coret_fluxtube(1)%values(1)%Te_transp%diff_eff(nftubes))
          call Linterp(coret(1)%values(1)%Te_transp%diff_eff, coret(1)%values(1)%rho_tor_norm, nrhotor, &
               coret_fluxtube(1)%values(1)%Te_transp%diff_eff, coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
       endif
       if(associated(coret(1)%values(1)%Te_transp%vconv_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%Te_transp%vconv_eff)
          allocate(coret_fluxtube(1)%values(1)%Te_transp%vconv_eff(nftubes))
          call Linterp(coret(1)%values(1)%Te_transp%vconv_eff, coret(1)%values(1)%rho_tor_norm, nrhotor, &
               coret_fluxtube(1)%values(1)%Te_transp%vconv_eff, coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
       endif
       if(associated(coret(1)%values(1)%Te_transp%flux)) then
          deallocate(coret_fluxtube(1)%values(1)%Te_transp%flux)
          allocate(coret_fluxtube(1)%values(1)%Te_transp%flux(nftubes))
          call Linterp(coret(1)%values(1)%Te_transp%flux, coret(1)%values(1)%rho_tor_norm, nrhotor, &
               coret_fluxtube(1)%values(1)%Te_transp%flux, coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
       endif

! vtor
       if(associated(coret(1)%values(1)%vtor_transp%diff_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%vtor_transp%diff_eff)
          allocate(coret_fluxtube(1)%values(1)%vtor_transp%diff_eff(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%vtor_transp%diff_eff(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%vtor_transp%diff_eff(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif
       if(associated(coret(1)%values(1)%vtor_transp%vconv_eff)) then
          deallocate(coret_fluxtube(1)%values(1)%vtor_transp%vconv_eff)
          allocate(coret_fluxtube(1)%values(1)%vtor_transp%vconv_eff(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%vtor_transp%vconv_eff(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%vtor_transp%vconv_eff(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif
       if(associated(coret(1)%values(1)%vtor_transp%flux)) then
          deallocate(coret_fluxtube(1)%values(1)%vtor_transp%flux)
          allocate(coret_fluxtube(1)%values(1)%vtor_transp%flux(nftubes,nion))
          do iion=1,nion
             call Linterp(coret(1)%values(1)%vtor_transp%flux(:,iion), coret(1)%values(1)%rho_tor_norm, nrhotor, &
                  coret_fluxtube(1)%values(1)%vtor_transp%flux(:,iion), coret_fluxtube(1)%values(1)%rho_tor_norm, nftubes)
          enddo
       endif


       deallocate(myftube)
       deallocate(rho_tor_norm_out)

    endif



!**************************************************************

#ifdef PERF
    call c_getMillis(t0)
#endif
    ! transfer CPO to buf
    write(cptstr,'(I4.4)') init_step+cpt
    cpt = cpt+1
    coret_file_out = 'bohmgb_coretransp_'//cptstr//'.cpo'
    call open_write_file(12,coret_file_out)
    if (cus_ftubes == .true.) then
       call write_cpo(coret_fluxtube(1),'coretransp')
    else
       call write_cpo(coret(1),'coretransp')
    endif
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
    call deallocate_cpo(coret_fluxtube)

    if (associated(code_parameters%schema)) then
       deallocate(code_parameters%schema)
    endif
    if (associated(code_parameters%parameters)) then
       deallocate(code_parameters%parameters)
    endif
    if (associated(code_parameters%default_param)) then
       deallocate(code_parameters%default_param)
    endif

  end subroutine bohmgb2buf




end module bohmgb_wrapper
