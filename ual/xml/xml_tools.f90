module xml_tools

!-----------------------------------------------------------------------
! Set of useful tools for the use of XML within FORTRAN
!-----------------------------------------------------------------------

  use itm_types
  use euitm_xml_parser

contains

  subroutine write_xml_tree(out_xml, parameter_list)
!-----------------------------------------------------------------------
! This subroutine writes out the entire XML tree parameter_list to
! the file handle out_xml following the XML 1.0 standard.
! As it is currently configured, it separates first level elements by
! XML comments containing the element name and newlines.
!-----------------------------------------------------------------------
    implicit none

    integer(itm_i4), intent(in) :: out_xml
    type(tree), intent(in) :: parameter_list

    type(element), pointer :: temp_pointer
    integer(itm_i4) :: level

!-- write header
    write(out_xml, *) '<?xml version="1.0"?>'
    write(out_xml, *) ''
    write(out_xml, *) '<?xml-stylesheet type="text/xsl"' &
     // ' href="./input_helena.xsl"'
    write(out_xml, *) 'charset="ISO-8859-1"?>'
    write(out_xml, *) ''

!-- write XML tree
    level = 0
    temp_pointer => parameter_list%first

    call write_xml_element(out_xml, temp_pointer, level)

  end subroutine write_xml_tree

  recursive subroutine write_xml_element(out_xml, temp_pointer, level)
!-------------------------------------------------------------------------
! This subroutine writes out an XML element and is called recursively.
!-------------------------------------------------------------------------
    implicit none

    integer(itm_i4), parameter :: data_level = 2
    integer(itm_i4), parameter :: indent = 2

    integer(itm_i4), intent(in) :: out_xml
    type(element), pointer :: temp_pointer
    integer(itm_i4) :: level, i

    do
      if (associated(temp_pointer%child)) then    ! is parent element
        if (.not. empty_element(temp_pointer%child)) then
          if (level == data_level - 1) &
            write(out_xml, *) '<!-- ', temp_pointer%cname, ' -->'
          if (level < data_level) write(out_xml, *) ''
          write(out_xml, *) (' ', i = 1, indent * level), '<', &
           temp_pointer%cname, '>'
          if (level < data_level - 1) write(out_xml, *) ''
          level = level + 1
          temp_pointer => temp_pointer%child
          call write_xml_element(out_xml, temp_pointer, level)
        end if
      else
        if (allocated(temp_pointer%cvalue)) then
          write(out_xml, *) (' ', i = 1, indent * level), '<',  &
           temp_pointer%cname, '> ', temp_pointer%cvalue, &
           ' </', temp_pointer%cname, '>'
!TODO: improve this to take care of long cvalues (linebreaks)
        end if
      end if
      if (associated(temp_pointer%sibling)) then
        temp_pointer => temp_pointer%sibling
      else if (associated(temp_pointer%parent)) then
        level = level - 1
        temp_pointer => temp_pointer%parent
        write(out_xml, *) (' ', i = 1, indent * level), '</', &
         temp_pointer%cname, '>'
        if (level < data_level) write(out_xml, *) ''
        exit
      else
        if (level > 0) write(*, *) 'ERROR: broken tree.'
        exit
      end if
    end do

  end subroutine write_xml_element

  recursive function empty_element(temp_pointer) result(is_empty)
!------------------------------------------------------------------------
!  This function determines whether an element is empty or has any
!  content (even at a deeper level).
!  returns .true. if empty
!------------------------------------------------------------------------
    implicit none

    type(element), pointer :: temp_pointer
    logical :: is_empty

    is_empty = .true.

    if (.not. associated(temp_pointer%child) &
     .and. allocated(temp_pointer%cvalue)) then
      is_empty = .false.
      return
    end if

    if (associated(temp_pointer%child)) then
      is_empty = empty_element(temp_pointer%child)
    end if
    if (.not. is_empty) return

    if (associated(temp_pointer%sibling)) then
      is_empty = empty_element(temp_pointer%sibling)
    else if (associated(temp_pointer%parent)) then
      return
    else
      write(*, *) 'ERROR: broken tree.'
      stop
    end if

  end function empty_element

end module xml_tools
