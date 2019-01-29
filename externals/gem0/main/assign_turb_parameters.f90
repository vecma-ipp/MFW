subroutine assign_turb_parameters(code_parameters, return_status)

!-----------------------------------------------------------------------
! calls the XML parser for the code parameters and assign the
! resulting values to the corresponding variables
!-----------------------------------------------------------------------
  
  USE Turb_Coeff

!  use euitm_schemas
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
      case ("flags")
        temp_pointer => temp_pointer%child
        cycle
      case ("physical")
        temp_pointer => temp_pointer%child
        cycle
      case ("grid")
        temp_pointer => temp_pointer%child
        cycle
!--   individual parameters
      case ("write_cpos")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, write_cpos)
      case ("write_diags")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, write_diags)
      case ("hmode")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, hmode)
      case ("q_choice")
        if (allocated(temp_pointer%cvalue)) &
         q_choice = char2str(temp_pointer%cvalue)
      case ("thresh")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, thresh)
      case ("beta_reduction")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, beta_reduction)
      case ("etae_pinch")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, etae_pinch)
      case ("chi_d")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, chi_d)
      case ("chiratio_phi")
        if (allocated(temp_pointer%cvalue)) &
         call char2num(temp_pointer%cvalue, chiratio_phi)
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

end subroutine assign_turb_parameters
