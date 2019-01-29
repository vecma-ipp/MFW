! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> wrapper for HELENA
!>
!> \author D. Coster
!>
!> \version "$Id: ets_wrapper_helena.f90 1763 2016-06-22 15:41:53Z dpc $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
module ets_wrapper_helena

  use euitm_schemas
  implicit none
  
contains

  subroutine helena_wrapper(euitm_equilibrium_in, euitm_equilibrium_out)

    use xml_file_reader
    use write_structures
    use itm_types
    implicit none

    type (type_equilibrium), pointer :: euitm_equilibrium_in(:)
    type (type_equilibrium), pointer :: euitm_equilibrium_out(:)
    type (type_param), save  :: code_parameters
    REAL (R8) :: maxabs
    logical, save :: first = .true.
    integer, save :: ncall = 0
    character*32 :: filename = 'EQ_######'

    interface
       subroutine helena (euitm_equilibrium_in, euitm_equilibrium_out, in_path, code_parameters)
         use euitm_schemas
         IMPLICIT NONE
         type (type_equilibrium), pointer :: euitm_equilibrium_in(:)
         type (type_equilibrium), pointer :: euitm_equilibrium_out(:)
         character(len = 132), optional :: in_path
         type (type_param) :: code_parameters
       end SUBROUTINE Helena
    end interface
    
    if(first) then
       call fill_param(code_parameters, 'XML/helena.xml', '', 'XML/helena.xsd')
       first=.false.
    endif

    if(associated(euitm_equilibrium_in(1)%profiles_1d%jparallel)) then
       maxabs = maxval(abs(euitm_equilibrium_in(1)%profiles_1d%jphi))
       write(*,*) 'max( | jphi | ) = ', maxabs
       if(maxabs .EQ. 0.0_R8) then
          write(*,*) 'helena_wrapper copied jparallel to jphi [ERROR]'
          if(.not.associated(euitm_equilibrium_in(1)%profiles_1d%jphi)) &
               allocate(euitm_equilibrium_in(1)%profiles_1d%jphi(size(euitm_equilibrium_in(1)%profiles_1d%jparallel)))
          euitm_equilibrium_in(1)%profiles_1d%jphi = euitm_equilibrium_in(1)%profiles_1d%jparallel  ! -? ToDo
       endif
    endif
    if(associated(euitm_equilibrium_in(1)%profiles_1d%psi)) then
       write(*,*) 'helena_wrapper set psi_bound and psi_ax [ERROR]'
       write(*,*) euitm_equilibrium_in(1)%global_param%psi_ax, euitm_equilibrium_in(1)%profiles_1d%psi(1)
       write(*,*) euitm_equilibrium_in(1)%global_param%psi_bound, euitm_equilibrium_in(1)%profiles_1d%psi(size(euitm_equilibrium_in(1)%profiles_1d%psi))
       euitm_equilibrium_in(1)%global_param%psi_ax = euitm_equilibrium_in(1)%profiles_1d%psi(1)
       euitm_equilibrium_in(1)%global_param%psi_bound =   &
            euitm_equilibrium_in(1)%profiles_1d%psi(size(euitm_equilibrium_in(1)%profiles_1d%psi))
    endif

! the following is experimental
    call F_par_AXIS(size(euitm_equilibrium_in(1)%profiles_1d%rho_tor),  &
         euitm_equilibrium_in(1)%profiles_1d%rho_tor,euitm_equilibrium_in(1)%profiles_1d%pressure)
! end of the experiment


    write(filename(4:9),'(I6.6)') ncall
    write(*,*) 'Writing EQUILIBRIUM ', trim(filename)//'.IN'
    call open_write_file(1, trim(filename)//'.IN')
    call write_cpo(euitm_equilibrium_in(1), 'equilibrium')
    call close_write_file
    write(*,*) 'associated(euitm_equilibrium_out) ', associated(euitm_equilibrium_out)
    CALL HELENA(euitm_equilibrium_in, euitm_equilibrium_out, code_parameters=code_parameters)
    write(*,*) 'Writing EQUILIBRIUM ', trim(filename)//'.OUT'
    call open_write_file(1, trim(filename)//'.OUT')
    call write_cpo(euitm_equilibrium_out(1), 'equilibrium')
    call close_write_file
    if(.not.associated(euitm_equilibrium_out(1)%profiles_1d%jparallel)) then
       write(*,*) 'helena_wrapper copied jphi to jparallel [ERROR]'
       allocate(euitm_equilibrium_out(1)%profiles_1d%jparallel(size(euitm_equilibrium_out(1)%profiles_1d%jphi)))
       euitm_equilibrium_out(1)%profiles_1d%jparallel = euitm_equilibrium_out(1)%profiles_1d%jphi
    else
       maxabs = maxval(abs(euitm_equilibrium_out(1)%profiles_1d%jparallel))
       write(*,*) 'max( | jparallel | ) = ', maxabs
       if(maxabs .EQ. 0.0_R8) then
          write(*,*) 'helena_wrapper copied jphi to jparallel [ERROR]'
          euitm_equilibrium_out(1)%profiles_1d%jparallel = euitm_equilibrium_out(1)%profiles_1d%jphi
       endif
    endif

    ncall=ncall+1

    return

  end subroutine helena_wrapper

end module ets_wrapper_helena
