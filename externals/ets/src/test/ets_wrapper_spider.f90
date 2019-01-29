! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> wrapper for SPIDER
!>
!> \author D. Coster
!>
!> \version "$Id: ets_wrapper_spider.f90 805 2010-08-28 12:21:07Z coster $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
module ets_wrapper_spider

  use euitm_schemas
  implicit none
  
contains

  subroutine spider_wrapper(euitm_equilibrium_in, coreprof, euitm_equilibrium_out)

    use xml_file_reader
    use write_structures

    implicit none

    type (type_equilibrium), pointer :: euitm_equilibrium_in(:)
    type (type_equilibrium), pointer :: euitm_equilibrium_out(:)
    TYPE (TYPE_COREPROF), POINTER :: COREPROF(:)
    type (type_param), save  :: code_parameters
    logical, save :: first = .true.
    integer, save :: ncall = 0
    character*32 :: filename = 'EQ_######'

    interface
       SUBROUTINE EQUILIBRIUM_SPIDER  (EQ_IN, COREPROF_IN, EQ_OUT, code_parameters)
         USE EUITM_SCHEMAS
         IMPLICIT NONE
         TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQ_IN(:)   !input CPO with geometry quantities from previous time
         TYPE (TYPE_EQUILIBRIUM), POINTER  :: EQ_OUT(:)  !output CPO with geometry quantities from previous iteration
         TYPE (TYPE_COREPROF),    POINTER  :: COREPROF_IN(:)      !input CPO with plasma profiles
         TYPE (TYPE_PARAM)                 :: code_parameters
       end SUBROUTINE EQUILIBRIUM_SPIDER
    end interface
    
    if(first) then
       call fill_param(code_parameters, 'XML/SPIDER.xml', '', 'XML/SPIDER.xsd')
       first=.false.
    endif

    if(associated(euitm_equilibrium_in(1)%profiles_1d%jparallel)) then
       write(*,*) 'spider_wrapper copied jparallel to jphi [ERROR]'
       if(.not.associated(euitm_equilibrium_in(1)%profiles_1d%jphi)) &
            allocate(euitm_equilibrium_in(1)%profiles_1d%jphi(size(euitm_equilibrium_in(1)%profiles_1d%jparallel)))
       euitm_equilibrium_in(1)%profiles_1d%jphi=euitm_equilibrium_in(1)%profiles_1d%jparallel  ! -? ToDo
    endif
    if(associated(euitm_equilibrium_in(1)%profiles_1d%psi)) then
       write(*,*) 'spider_wrapper set psi_bound and psi_ax [ERROR]'
       euitm_equilibrium_in(1)%global_param%psi_ax = euitm_equilibrium_in(1)%profiles_1d%psi(1)
       euitm_equilibrium_in(1)%global_param%psi_bound =   &
            euitm_equilibrium_in(1)%profiles_1d%psi(size(euitm_equilibrium_in(1)%profiles_1d%psi))
    endif

! the following is experimental
    call F_par_AXIS(size(euitm_equilibrium_in(1)%profiles_1d%rho_tor),  &
         euitm_equilibrium_in(1)%profiles_1d%rho_tor,euitm_equilibrium_in(1)%profiles_1d%pressure)
! end of the experiment


!    write(filename(4:9),'(I6.6)') ncall
!    call open_write_file(1, trim(filename)//'.IN')
!    call write_cpo(euitm_equilibrium_in(1), 'equilibrium')
!    call close_write_file
!    write(*,*) 'associated(euitm_equilibrium_out) ', associated(euitm_equilibrium_out)

    CALL EQUILIBRIUM_SPIDER(euitm_equilibrium_in, coreprof, euitm_equilibrium_out, code_parameters)

!    if(.not.associated(euitm_equilibrium_out(1)%profiles_1d%jparallel)) then
!       write(*,*) 'spider_wrapper copied jphi to jparallel [ERROR]'
!       allocate(euitm_equilibrium_out(1)%profiles_1d%jparallel(size(euitm_equilibrium_out(1)%profiles_1d%jphi)))
!       euitm_equilibrium_out(1)%profiles_1d%jparallel=euitm_equilibrium_out(1)%profiles_1d%jphi
!    endif
!    call open_write_file(1, trim(filename)//'.OUT')
!    call write_cpo(euitm_equilibrium_out(1), 'equilibrium')
!    call close_write_file
    ncall=ncall+1

    return

  end subroutine spider_wrapper

end module ets_wrapper_spider
