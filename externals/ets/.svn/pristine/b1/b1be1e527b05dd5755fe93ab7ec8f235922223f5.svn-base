subroutine assign_equil_parameters(code_parameters, return_status)

!-----------------------------------------------------------------------
! calls the XML parser for the code parameters and assign the
! resulting values to the corresponding variables
!-----------------------------------------------------------------------
  
  USE BDSEQ_Coeff

  use euitm_schemas
  use euitm_xml_parser  

  implicit none

  type (type_param), intent(in) :: code_parameters
  integer(itm_i4), intent(out) :: return_status 

  type(tree) :: parameter_list
  type(element), pointer :: temp_pointer
  integer(itm_i4) :: i, nparm, n_values
  character(len = 132) :: cname

!...  initialisations

  nparm = 0
  n_values = 0
  return_status = 0      ! no error

!-- parse xml-string code_parameters%parameters using W3C XML schema in
!   code_parameters%schema

  call euitm_xml_parse(code_parameters, nparm, parameter_list)

!-- assign variables

  temp_pointer => parameter_list%first

  outer: do
    cname = char2str(temp_pointer%cname)   ! necessary for AIX
    select case (cname)
!--   parameters overall
      case ("parameters")
        temp_pointer => temp_pointer%child
        cycle
!--   parameter classes
!--   individual parameters
      case ("symmetry_coords")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, symmetry_coords)
      case ("write_cpos")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, write_cpos)
      case ("write_diags")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, write_diags)
      case ("nr_eq")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, nr_eq)
      case ("neta_eq")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, neta_eq)
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

end subroutine assign_equil_parameters
