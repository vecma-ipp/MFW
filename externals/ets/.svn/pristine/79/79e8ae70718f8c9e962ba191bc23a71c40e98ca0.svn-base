subroutine assign_etaigb_parameters(codeparameters, return_status)

!-----------------------------------------------------------------------
! calls the XML parser for the code parameters and assign the
! resulting values to the corresponding variables
!-----------------------------------------------------------------------
  
  USE ETAIGB_Coeff

  use euitm_schemas
  use euitm_xml_parser  

  implicit none

  type (type_param), intent(in) :: codeparameters
  integer(ITM_I4), intent(out) :: return_status 

  type(tree) :: parameter_list
  type(element), pointer :: temp_pointer
  integer(ITM_I4) :: i, nparm, n_values
  character(len = 132) :: cname

  return_status = 0      ! no error

!-- parse xml-string codeparameters%parameters

  call euitm_xml_parse(codeparameters, nparm, parameter_list)

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
      case ("physical")
        temp_pointer => temp_pointer%child
        cycle
      case ("grid")
        temp_pointer => temp_pointer%child
        cycle
!--   individual parameters
      case ("thresh")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, thresh)
      case ("tfloor")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, tfloor)
      case ("beta_reduction")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, beta_reduction)
      case ("etae_pinch")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, etae_pinch)
      case ("chi_d")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, chi_d)
      case ("nrho_transp")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, nrho_transp)
      case ("nion")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, nion)
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

end subroutine assign_etaigb_parameters
