program test_functions

  USE ITM_CONSTANTS
  USE EUITM_SCHEMAS
  USE XML_FILE_READER
  use euitm_xml_parser  

  implicit none

! Declarations of GNU libmatheval procedures used.
  integer*8 evaluator_create
  integer*8 evaluator_derivative_x
  double precision evaluator_evaluate_x
  external evaluator_destroy

! Size of input buffer.
  integer :: BUFLEN
  parameter(BUFLEN = 256)

  integer*8, allocatable             :: f(:)         ! Evaluators for function.
  double precision                   :: x                  ! Variable x value.
  integer                            :: i, j

  TYPE (TYPE_PARAM)                  :: code_parameters
  character(len=BUFLEN), allocatable :: functions(:)
  integer :: return_status, n_functions

  integer                            :: iion

  real(R8)                           :: rho_1(0:1000), rho_2(0:1000), rho_3(0:1000), rho_4(0:1000), dummy


  CALL FILL_PARAM (code_parameters, 'XML/test_functions.xml', '', 'XML/test_functions.xsd')
  call assign_code_parameters(code_parameters, return_status)

  write(*,*) '# Number of input functions = ', n_functions
  do i=1, n_functions
     write(*,*) '# ', i,' : ',trim(functions(i))
  enddo

  allocate(f(n_functions))

  do i=1, n_functions
     f(i) = evaluator_create (trim(functions(i)))
     if (f(i) == 0) stop
  enddo

  do i=0,1000
     x=1.0d0/1000*i
     rho_1(i) = x
     rho_2(i) = evaluator_evaluate_x (f(1), x)
  enddo
  rho_3(0)=0
  do i=1,1000
     call cubint(1001, rho_1, rho_2, 1, i+1, rho_3(i), dummy)
  enddo
  rho_3=rho_3/rho_3(1000)
  call l3interp(rho_1, rho_3, 1001, rho_4, rho_1, 1001)
  do i=0,1000
     write(*,'(4(f15.6))') rho_1(i), rho_2(i), rho_3(i), rho_4(i)
  enddo
  
  stop
  

! Calculate and print values of function for given value of x.
  do i=0,100
     x=1.0d0/100*i
     write(*,'(f10.5,1p,100(1x,g15.6))') x, (evaluator_evaluate_x (f(j), x), j=1, n_functions)
  enddo

! Destroy evaluators.
  do i=1, n_functions
     call evaluator_destroy (f(i))
  enddo

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
    character (len=256) :: tmp_functions(100)
    character (len=256), allocatable :: tmp_string(:)

    return_status = 0      ! no error

      !-- parse xml-string codeparameters%parameters

    write(*,*) 'Calling euitm_xml_parse'
    call euitm_xml_parse(code_parameters, nparm, parameter_list)
    write(*,*) 'Called euitm_xml_parse'

      !-- assign variables

    temp_pointer => parameter_list%first

    outer: do
       cname = char2str(temp_pointer%cname)   ! necessary for AIX
       select case (cname)
       case ("parameters")
          temp_pointer => temp_pointer%child
          cycle

!--   input parameters
       case ("input")
          temp_pointer => temp_pointer%child
          cycle
       case ("functions")
          if (allocated(temp_pointer%cvalue)) then
             call scan_str2str(char2str(temp_pointer%cvalue), 256, tmp_functions, n_functions)
             allocate(functions(n_functions))
             functions=tmp_functions(1:n_functions)
          endif

!--  default
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
    

end program test_functions
