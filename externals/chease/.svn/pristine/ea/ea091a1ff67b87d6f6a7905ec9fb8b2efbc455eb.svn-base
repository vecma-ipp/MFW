subroutine assign_code_parameters(codeparameters, return_status)

!-----------------------------------------------------------------------
! calls the XML parser for the code parameters and assign the
! resulting values to the corresponding variables
!TODO: check an alternative and more elegant solution in Perl
!-----------------------------------------------------------------------
  
  use prec_const
  
  use euitm_schemas
  use euitm_xml_parser  
  use globals

  implicit none

  type (type_codeparam), intent(in) :: codeparameters
  integer(ikind), intent(out) :: return_status 

  type(tree) :: parameter_list
  type(element), pointer :: temp_pointer
  integer(ikind) :: i, nparm, n_values
  character(len = 132) :: cname
!  integer(ikind) :: ns, NEQDXTPO

!-- set path to XML schema
  file_xml_schema = 'EQDATA_schema.xml'

  return_status = 0      ! no error

!-- parse xml-string codeparameters%parameters

  call euitm_xml_parse(codeparameters%parameters, nparm, parameter_list)
  print *,'codeparameters%parameters= ',codeparameters%parameters
  print *,' nparm= ',nparm
!  print *,'parameter_list= ',parameter_list
  print *,'salut 1'

!-- assign variables

  temp_pointer => parameter_list%first

  outer: do
    cname = char2str(temp_pointer%cname)   ! necessary for AIX
    print *,'cname = ', cname
    select case (cname)
      case ("parameters")
        temp_pointer => temp_pointer%child
        cycle
!--   profile_parameters
      case ("profile_parameters")
        temp_pointer => temp_pointer%child
        cycle
      case ("NS")
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, ns)
        print *,' ns= ',ns
      case ("NEQDXTPO")
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, neqdxtpo)
        print *,' neqdxtpo= ',neqdxtpo
      case ("ELONG")
         ! integer/real constant
        if (allocated(temp_pointer%cvalue)) &
             call char2num(temp_pointer%cvalue, elong)
        print *,' elong= ',elong
      case ("APLACE")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), aplace, n_values)
        print *,' aplace= ',aplace
        print *,' n_values= ',n_values
      case ("NITMSHOT")
         ! array
        if (allocated(temp_pointer%cvalue)) &
             call scan_str2num(char2str(temp_pointer%cvalue), nitmshot, n_values)
        print *,' nitmshot= ',nitmshot
        print *,' n_values= ',n_values
!!$      case ("XXXX")
!!$         ! integer/real constant
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, xxxx)
!!$        print *,' xxxx= ',xxxx
!!$      case ("XXXX")
!!$         ! array
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call scan_str2num(char2str(temp_pointer%cvalue), xxxx, n_values)
!!$        print *,' xxxx= ',xxxx
!!$      case ("XXXX")
!!$         ! integer/real constant
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call char2num(temp_pointer%cvalue, xxxx)
!!$        print *,' xxxx= ',xxxx
!!$      case ("XXXX")
!!$         ! array
!!$        if (allocated(temp_pointer%cvalue)) &
!!$             call scan_str2num(char2str(temp_pointer%cvalue), xxxx, n_values)
!!$        print *,' xxxx= ',xxxx
      case default
        write(*, *) 'ERROR: invalid parameter', cname
        return_status = 1
 !       exit
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
