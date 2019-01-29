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


end module imp4dv_standalone
