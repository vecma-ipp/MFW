module bohmgb_standalone
  use euitm_schemas
  implicit none

  integer, save :: init_step             !initial step count
  logical, save :: cus_ftubes = .true.  !Y/N on f.tubes customisa
  integer, save :: nftubes = 4           !# of fluxtubes (customise)


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

  subroutine bohmgb_cpo(equil, corep, coret) 
    use xml_file_reader
    use deallocate_structures
    use copy_structures
    implicit none

    type (type_equilibrium), pointer :: equil(:)
    type (type_coreprof), pointer :: corep(:)
    type (type_coretransp), pointer :: coret(:), coret_fluxtube(:)
    type (type_param) :: code_parameters

    integer :: nrhotor                                 !# of fluxtubes (original)
    integer :: nion                                    !# of elements in diff_eff
    integer :: i,iion,icon                             !dummy variables
    integer,dimension(:),allocatable :: myftube        !fluxtube ID (customise)
    real,dimension(:),allocatable :: rho_tor_norm_out  !normalised rho_tor (customise)

    print *,"fortran BOHMGB wrapper"

    print *,"get code params"
    call fill_param(code_parameters, 'bohmgb.xml', '', 'bohmgb.xsd')

    print *,"run bohmgb routine"
    call bohmgb(equil, corep, coret, code_parameters)


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

       coret => coret_fluxtube
       !call deallocate_cpo(coret)
       !allocate(coret(1))
       !call copy_cpo(coret_fluxtube(1),coret(1))
    endif

  end subroutine bohmgb_cpo


end module bohmgb_standalone
