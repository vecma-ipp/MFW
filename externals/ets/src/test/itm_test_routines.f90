! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> Module provides routines for testing
!>
!> \author ???
!>
!> \version "$Id: itm_test_routines.f90 1609 2014-10-06 13:54:25Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
MODULE ITM_TEST_ROUTINES

CONTAINS



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> process the xml version of the input file from codeparam
!>
!> \author D. Coster
!>
!> \version "$Id: itm_test_routines.f90 1609 2014-10-06 13:54:25Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  subroutine process_xml &
       (SOLVER_TYPE,SIGMA_SOURCE,TAU,AMIX,CONVREC,                           &
       NRHO,NION,NIMP,NZIMP,NTIME,NSOL,                                      &
       PSI_BND_TYPE,NI_BND_TYPE,TI_BND_TYPE,TE_BND_TYPE,VTOR_BND_TYPE,       &
       shot_no, run_no, codeparam, database_format)
    
    use itm_types
    use euitm_schemas
    use euitm_xml_parser  

    implicit none

    INTEGER     :: SHOT_NO             !shot number
    INTEGER     :: RUN_NO              !run number

    INTEGER     :: NRHO                !number of radial points    (input)
    INTEGER     :: NION                !number of ion species      (input)
    INTEGER     :: NIMP                !number of impurity species (input)
    INTEGER, ALLOCATABLE :: NZIMP(:)   !number of charge states for each impurity (input)
    INTEGER     :: NTIME               !number of time points      (input)
    
    INTEGER     :: IRHO                !current radial knot
    INTEGER     :: IION                !current ion type
    INTEGER     :: ITIME               !current time step
    
    INTEGER     :: NSOL                !Number of analytical example
    INTEGER     :: SOLVER_TYPE         !representation of transport equations
    INTEGER     :: SIGMA_SOURCE        !origin of Plasma electrical conductivity
    
    REAL (R8)   :: CONVREC             !required convergency 
    REAL (R8)   :: TIME                !Time
    REAL (R8)   :: TAU                 !time step, and mixing coefficient
    REAL (R8)   :: AMIX                !mixing factor
    
    INTEGER     :: PSI_BND_TYPE        !Type of boundary conditions current
    INTEGER     :: NI_BND_TYPE         !Type of boundary conditions ion density 
    INTEGER     :: TI_BND_TYPE         !Type of boundary conditions ion temperature
    INTEGER     :: TE_BND_TYPE         !Type of boundary conditions electron temperature
    INTEGER     :: VTOR_BND_TYPE       !Type of boundary conditions toroidal rotation

    character (len=32)               :: database_format

    REAL (R8)   :: tmp_data(100)
    integer return_status, n_data
    type (type_param) :: codeparam

! intial values for parameters
    database_format     = 'mdsplus'

    call assign_code_parameters(codeparam, return_status)
    
    if (return_status /= 0) then
       write(*, *) 'ERROR: Could not assign code parameters.'
       stop
    end if

!    deallocate(codeparam%codename, codeparam%codeversion, codeparam%parameters)
    return

  contains

    subroutine assign_code_parameters(codeparameters, return_status)

      !-----------------------------------------------------------------------
      ! calls the XML parser for the code parameters and assign the
      ! resulting values to the corresponding variables
      !TODO: check an alternative and more elegant solution in Perl
      !-----------------------------------------------------------------------

      use mod_f90_kind

      implicit none

      type (type_param), intent(in) :: codeparameters
      integer(ikind), intent(out) :: return_status 

      type(tree) :: parameter_list
      type(element), pointer :: temp_pointer
      integer(ikind) :: i, nparm, n_values
      character(len = 132) :: cname

      return_status = 0      ! no error

!-- parse xml-string codeparameters%parameters

      call euitm_xml_parse(codeparameters, nparm, parameter_list)

!-- assign variables

      temp_pointer => parameter_list%first

      outer: do
         cname = char2str(temp_pointer%cname)   ! necessary for AIX
         select case (cname)
         case ("parameters")
            temp_pointer => temp_pointer%child
            cycle
!--   output parameters
         case ("output")
            temp_pointer => temp_pointer%child
            cycle
         case ("shot")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, shot_no)
         case ("run")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, run_no)
         case ("db")
            if (allocated(temp_pointer%cvalue)) &
                 database_format = char2str(temp_pointer%cvalue)
!--   dims parameters
         case ("dims")
            temp_pointer => temp_pointer%child
            cycle
         case ("nrho")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, nrho)
         case ("nion")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, nion)
         case ("nimp")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, nimp)
         case ("nzimp")
            if (allocated(temp_pointer%cvalue)) then
               call scan_str2real(char2str(temp_pointer%cvalue), tmp_data, n_data)
               allocate(nzimp(n_data))
               nzimp=tmp_data(1:n_data)
            endif
!--   solver parameters
         case ("solver")
            temp_pointer => temp_pointer%child
            cycle
         case ("solver_type")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, solver_type)
         case ("sigma_source")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, sigma_source)
         case ("tau")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, tau)
         case ("amix")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, amix)
         case ("convrec")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, convrec)
         case ("ntime")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, ntime)
         case ("nsol")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, nsol)
!--   boundary parameters
         case ("boundary")
            temp_pointer => temp_pointer%child
            cycle
         case ("psi_bnd_type")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, psi_bnd_type)
         case ("ni_bnd_type")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, ni_bnd_type)
         case ("ti_bnd_type")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, ti_bnd_type)
         case ("te_bnd_type")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, te_bnd_type)
         case ("vtor_bnd_type")
            if (allocated(temp_pointer%cvalue)) &
                 call char2num(temp_pointer%cvalue, vtor_bnd_type)
         case default
            write(*, *) 'ERROR: invalid parameter', cname
            return_status = 1
            exit
         end select
         do
            if (associated(temp_pointer%sibling)) then
               temp_pointer => temp_pointer%sibling
               exit
            end if
            if (associated(temp_pointer%parent, parameter_list%first )) &
                 exit outer
            if (associated(temp_pointer%parent)) then
               temp_pointer => temp_pointer%parent
            else
               write(*, *) 'ERROR: broken list.'
               return
            end if
         end do
      end do outer

!-- destroy tree
      call destroy_xml_tree(parameter_list)

      return

    end subroutine assign_code_parameters

end subroutine process_xml

subroutine read_codeparam(in_xml, filename, codeparam)

  use euitm_schemas
  use ets_version
  implicit none

  integer n_lines, in_xml, ios, i
  character (len=*) :: filename
  type (type_codeparam) :: codeparam
  character(len = 132) :: xml_line

  open (unit = in_xml, file = filename, status = 'old', &
       action = 'read', iostat = ios)

  if (ios /= 0) then
     write(*,*) 'Could not open ',trim(filename)
     stop ' ERROR:  XML file does not exist '
  end if

  n_lines = 0

  do
     read (in_xml, '(a)', iostat = ios) xml_line
     if (ios == 0) then
        n_lines = n_lines + 1
     else
        exit
     end if
  end do

  rewind in_xml

  allocate(codeparam%codename(1))
  codeparam%codename(1)='ETS'
  allocate(codeparam%codeversion(1))
  codeparam%codeversion(1)=version
  write(*,*) 'Code = ',trim(codeparam%codename(1)),' version = ',trim(codeparam%codeversion(1))
  allocate(codeparam%parameters(n_lines))
  do i = 1, n_lines
     read (in_xml, '(a)', iostat = ios) codeparam%parameters(i)
  end do

  close(in_xml)

  return
end subroutine read_codeparam



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> 
!>
!> \author ???
!>
!> \version "$Id: itm_test_routines.f90 1609 2014-10-06 13:54:25Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE SET_CPO  &
       (NRHO,NION,NIMP,NZIMP,NTIME,NSOL,                                        &
       PSI_BND_TYPE,NI_BND_TYPE,TI_BND_TYPE,TE_BND_TYPE,VTOR_BND_TYPE,          &
       EQUILIBRIUM,COREPROF,CORETRANSP)

    use itm_types
    USE EUITM_SCHEMAS
    USE ETS_PLASMA

    IMPLICIT NONE

    INTEGER                          :: ifail

    INTEGER                          :: NRHO                !number of radial points    (input)
    INTEGER                          :: NION                !number of ion species      (input)
    INTEGER                          :: NIMP                !number of impurity species (input)
    INTEGER, ALLOCATABLE             :: NZIMP(:)            !number of charge states for each impurity (input)
    INTEGER                          :: NTIME               !number of time points      (input)

    INTEGER                          :: IRHO                !current radial knot
    INTEGER                          :: IION                !current ion type
    INTEGER                          :: ITIME               !current time step

    INTEGER                          :: NSOL                !Number of analytical example

    INTEGER                          :: PSI_BND_TYPE        !Type of boundary conditions current
    INTEGER                          :: NI_BND_TYPE         !Type of boundary conditions ion density 
    INTEGER                          :: TI_BND_TYPE         !Type of boundary conditions ion temperature
    INTEGER                          :: TE_BND_TYPE         !Type of boundary conditions electron temperature
    INTEGER                          :: VTOR_BND_TYPE       !Type of boundary conditions toroidal rotation

! +++ CPO derived types:
    TYPE (TYPE_EQUILIBRIUM), POINTER :: EQUILIBRIUM(:)      !input CPO with geometry quantities
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF(:)         !input/output CPO with plasma profiles 
    TYPE (TYPE_CORETRANSP),  POINTER :: CORETRANSP(:)       !input CPO with transport coefficients


! +++ Plasma composition: 
    IF (.not.associated(COREPROF(1)%composition%zion)) ALLOCATE(COREPROF(1)%composition%zion(nion))
    IF (.not.associated(COREPROF(1)%composition%amn))  ALLOCATE(COREPROF(1)%composition%amn(nion))
    IF (.not.associated(COREPROF(1)%composition%zn))   ALLOCATE(COREPROF(1)%composition%zn(nion))

    COREPROF(1)%composition%zion(:)     = 1._r8
    COREPROF(1)%composition%amn(:)      = 2._r8


! +++ Generation of grid   
    RHO_LOOP1: DO IRHO=1,NRHO
       COREPROF(1)%rho_tor(IRHO)               = 1.e0_R8/(NRHO-1)*(IRHO-1)  !rho in [m]
!!!DPC-EQ-4.08b-problem
       EQUILIBRIUM(1)%profiles_1d%vprime(IRHO) = COREPROF(1)%rho_tor(IRHO)
       EQUILIBRIUM(1)%profiles_1d%volume(IRHO) = COREPROF(1)%rho_tor(IRHO)**2/2.0_R8
       EQUILIBRIUM(1)%profiles_1d%gm3(IRHO)    = 1.e0_R8
       EQUILIBRIUM(1)%profiles_1d%gm8(IRHO)    = 1.e0_R8     ! added DPC 2012-04-27 to fix a divide by 0 in the analytic solver test
       COREPROF(1)%toroid_field%r0             = 1.e0_R8
       COREPROF(1)%toroid_field%b0             = 1.e0_R8
       EQUILIBRIUM(1)%profiles_1d%F_dia(IRHO)  = COREPROF(1)%toroid_field%r0*COREPROF(1)%toroid_field%b0
    END DO RHO_LOOP1



! +++ Definition of boundary conditions type 

    COREPROF(1)%psi%boundary%type             = PSI_BND_TYPE
    COREPROF(1)%te%boundary%type              = TE_BND_TYPE

    ION_LOOP1: DO IION=1,NION         
       COREPROF(1)%ni%boundary%type(IION)      = NI_BND_TYPE
       COREPROF(1)%ti%boundary%type(IION)      = TI_BND_TYPE
       COREPROF(1)%vtor%boundary%type(IION)    = VTOR_BND_TYPE

! +++ Plasma composition: 
       COREPROF(1)%composition%zion(IION)      = 1.e0_R8
       COREPROF(1)%composition%amn(IION)       = 1.e0_R8

       IF (COREPROF(1)%ni%boundary%type(IION).EQ.0) THEN
          COREPROF(1)%ni%value(:,:)              = 1.e0_R8
          COREPROF(1)%ne%value(:)                = 1.e0_R8
       END IF

    END DO ION_LOOP1



    IF (COREPROF(1)%psi%boundary%type.EQ.0) THEN
       CORETRANSP(1)%values(1)%sigma(:)                   = 1.e0_R8
    END IF

    RETURN

  END SUBROUTINE SET_CPO



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> setup control structure
!>
!> \author ???
!>
!> \version "$Id: itm_test_routines.f90 1609 2014-10-06 13:54:25Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
  SUBROUTINE SET_CONTROL  &
       (SOLVER_TYPE,SIGMA_SOURCE,TAU,AMIX,CONVREC,CONTROL)

    use itm_types
    USE EUITM_SCHEMAS
    USE ETS_PLASMA

    IMPLICIT NONE

    INTEGER                          :: SOLVER_TYPE         !representation of transport equations 
!1-"standard"; 2-"integral"(default)
  INTEGER                            :: SIGMA_SOURCE        !source for plasma conductivity

    REAL (R8)                        :: CONVREC             !required convergency 
    REAL (R8)                        :: TIME                !Time
    REAL (R8)                        :: TAU                 !time step, and mixing coefficient
    REAL (R8)                        :: AMIX                !mixing factor

    TYPE (RUN_CONTROL)               :: CONTROL             !contains all parameters required by run

    CONTROL%SOLVER_TYPE                      = SOLVER_TYPE  !defines the number of numerical solver
    CONTROL%SIGMA_SOURCE                     = SIGMA_SOURCE !source for plasma conductivity
    CONTROL%TAU                              = TAU          !defines the time step
    CONTROL%AMIX                             = AMIX         !defines the time step
    CONTROL%AMIXTR                           = AMIX**0.5    !defines the time step
    CONTROL%CONV                             = 1.e0_R8         !defines the time step
    CONTROL%CONVREC                          = CONVREC      !defines the time step

    RETURN

  END SUBROUTINE SET_CONTROL



! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  







! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
!> This subroutine stores the results of computations into files
!>
!> \author ???
!>
!> \version "$Id: itm_test_routines.f90 1609 2014-10-06 13:54:25Z denka $"
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
   SUBROUTINE WRITE_OUT &
      (ITIME,COREPROF_NEW, COREPROF_ANALYTIC)
!     This subroutine stores the results of computations
!     into files

    USE EUITM_SCHEMAS

    USE TYPE_ANALYTICS

    IMPLICIT NONE

! +++ Input parameters:
    TYPE (ANALYTICAL_SOLUTION)       :: ASOLUTION           !contains all quantities from analytical solution
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_NEW(:)     !input CPO with internal ETS parameters profiles 
    TYPE (TYPE_COREPROF),    POINTER :: COREPROF_ANALYTIC(:)!input CPO with internal analytical parameters profiles 

! +++ Internal parameters:
    INTEGER                          :: IRHO,IION,ITIME
    INTEGER                          :: NRHO,NION

    CHARACTER (26)                      FILENAME

    NRHO             = SIZE(COREPROF_NEW(1)%rho_tor)
    NION             = SIZE(COREPROF_NEW(1)%composition%amn)

    write(FILENAME,'(a,i7.7,a)') 'data/OUTPUT/OUT',ITIME,'.DAT'

    OPEN (UNIT=10, FILE=FILENAME)



    DO IRHO = 1, NRHO
!                                                          1                                
       WRITE (10,'(13(1x,e14.7))')            COREPROF_NEW(1)%rho_tor(IRHO),              &
!                    2                                     3
            COREPROF_NEW(1)%ni%value(IRHO,NION),   COREPROF_ANALYTIC(1)%ni%value(IRHO,NION),   & 
!                    4                                     5
            COREPROF_NEW(1)%ne%value(IRHO),        COREPROF_ANALYTIC(1)%ne%value(IRHO),        &
!                    6                                     7
            COREPROF_NEW(1)%ti%value(IRHO,NION),   COREPROF_ANALYTIC(1)%ti%value(IRHO,NION),   &
!                    8                                     9
            COREPROF_NEW(1)%te%value(IRHO),        COREPROF_ANALYTIC(1)%te%value(IRHO),        &
!                    10                                    11
            COREPROF_NEW(1)%vtor%value(IRHO,NION), COREPROF_ANALYTIC(1)%vtor%value(IRHO,NION), &              
!                    12                                    13
            COREPROF_NEW(1)%psi%value(IRHO),       COREPROF_ANALYTIC(1)%psi%value(IRHO)       
    END DO

    CLOSE (10)


    RETURN
  END SUBROUTINE WRITE_OUT
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +  



END MODULE ITM_TEST_ROUTINES
