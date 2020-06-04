module euitm_xml_parser

!----------------------------------------------------------------------------
! Set of routines for parsing an XML string according to
! EUITM specifications (http://www.efda-taskforce-itm.org)
!
! The parser uses a code specific W3C schema in XML format to build a tree
! which will hold the element names in the structure element cname and
! the value of the element in cvalue.
!
! When parsing the input XML file, the corresponding cvalue fields will be
! allocated and filled.
! Not allocated cvalue indicates a field which was not set or a field not at
! the tip of a branch.
!
! The parser requires version 4.07b of the euitm_schemas and up.
!----------------------------------------------------------------------------
 
  use itm_types
  use string_manipulation_tools

  type interval
    integer(itm_i4) :: ind_start, ind_end
  end type interval

  type tree
    type(element), pointer :: first
  end type tree
  integer(itm_i4) :: n_lines_schema

  logical, save :: verbose_parsing = .FALSE.
contains
  subroutine set_verbose(in)
    logical, intent(in) :: in
    verbose_parsing = in
  end subroutine set_verbose

  subroutine euitm_xml_parse(code_parameters, nparm, parameter_list)
!------------------------------------------------------------------------
!  This subroutine parses the xml string code_parameters%parameters
!  in EUITM format (i.e. array of character strings of a certain length)
!  It first builds the document tree using the XML schema specified
!  in code_parameters%schema.
!  It then parses the xml string, fills the parsed values into
!  the tree from the schema and returns the complete tree as a
!  parameter_list together with the number of parameters nparm.
!------------------------------------------------------------------------
    use euitm_schemas

    type (type_param), intent(in) :: code_parameters
    type(tree) :: parameter_list
    integer(itm_i4) :: nparm

    integer(itm_i4) :: i, length

    if (.not. associated(code_parameters%parameters)) then
      return
    else
      !write(*, *) 'number of XML input lines : ', &
      !size(code_parameters%parameters)
    end if

    call build_xml_tree(code_parameters%schema, parameter_list)

    length = 0
    do i = 1, size(code_parameters%parameters)
      length = length + len(code_parameters%parameters(i))
    end do

    !write(*, *) 'length of parameter_string = ', length

    call parse_xml_string(code_parameters%parameters, length, nparm, &
     parameter_list)

  end subroutine euitm_xml_parse

  subroutine build_xml_tree(schema, parameter_list)
!-------------------------------------------------------------------------
! This subroutine builds a tree from the given XML schema and
! returns it in parameter_list
!-------------------------------------------------------------------------
    implicit none

    type(tree), intent(out) :: parameter_list

    character(len = 132), dimension(:), pointer :: schema
    integer(itm_i4) :: i, length
    integer(itm_i4) :: branches
    logical :: been_here

    type(element), pointer :: temp_pointer

!-- check schema
    if (.not. associated(schema)) then
      stop ' ERROR: No schema specified! Cannot build tree.'
    else
      !write(*, *) 'number of XML lines in schema: ', size(schema)
    end if

!-- calculate length of schema_string
    length = 0
    do i = 1, size(schema)
      length = length + len(schema(i))
    end do

    !write(*, *) 'length of schema_string = ', length

    call build_tree(schema, length, branches, parameter_list)

!-- run through tree (for test purposes only)
    been_here = .false.
    temp_pointer => parameter_list%first
    do
      if (associated(temp_pointer%parent, parameter_list%first) &
       .and. been_here) exit
      if (associated(temp_pointer%child) .and. .not.been_here) then
        temp_pointer => temp_pointer%child
      else if (associated(temp_pointer%sibling)) then
        temp_pointer => temp_pointer%sibling
      else if (associated(temp_pointer%parent)) then
        temp_pointer => temp_pointer%parent
        been_here = .true.
        if (associated(temp_pointer%sibling)) then
          temp_pointer => temp_pointer%sibling
          been_here = .false.
        end if
      end if
      if (.not. associated(temp_pointer%child) &
       .and. .not. associated(temp_pointer%sibling)) been_here = .true.
    end do

  end subroutine build_xml_tree

  subroutine build_tree(schema, length, branches, parameter_list)
!--------------------------------------------------------------------------
! This subroutine builds a tree from the schema stored in schema and
! returns it in parameter_list.
!--------------------------------------------------------------------------
    implicit none

    character(len = *), parameter :: root = 'parameters'  ! document element

    type(tree), intent(out) :: parameter_list
    character(len = 132), dimension(:), pointer :: schema
    integer(itm_i4), intent(in) :: length
    integer(itm_i4), intent(out) :: branches

    type(element), pointer :: temp_pointer
    type(interval) :: full_string
    character(len = length) :: parameter_string
    integer(itm_i4) :: i, start_index, c_length

!-- concatenate strings in schema with spaces instead of line feeds

    parameter_string = ''
    start_index = 1
    do i = 1, size(schema)
      parameter_string(start_index : start_index + len(schema(i)) - 1) &
       = schema(i)
      start_index = start_index + len(schema(i))
    end do
 
!-- replace line feeds
    do i = 1, len(parameter_string)
      if (iachar(parameter_string(i : i)) == 10) &
       parameter_string(i : i) = ' '
    end do

    full_string%ind_start = 1
    full_string%ind_end = len(parameter_string)

!-- start list of parameters
    nullify(parameter_list%first)

    allocate(parameter_list%first)

    if (.not. associated (parameter_list%first)) then
      stop 'ERROR: pointer not associated'
    end if

!-- define document element
    allocate(parameter_list%first%cname(len(root)))
    parameter_list%first%cname = str2char(root)

    temp_pointer => parameter_list%first
    branches = 0

!-- recursively parse the W3C XML schema
    c_length = size(temp_pointer%cname)
    call parse_single_knot(parameter_string, temp_pointer, &
     c_length, branches)

  end subroutine build_tree

  recursive subroutine parse_single_knot(parameter_string, parent, length, &
   branches)
!---------------------------------------------------------------------------
! Recursive subroutine which searches parameter_string for occurrence of
! <xs:element name="temp_pointer%cname"
! if found, test whether bracket is closed with />, i.e. no children
! else look for closure </xs:element>.
! In the latter case: find all children (denoted by ref="child_name"),
! create the child knots and call parse_single_knot on child knots.
!---------------------------------------------------------------------------
    implicit none

    integer(itm_i4), intent(inout) :: branches

    type(element), pointer :: parent
    character(len = *), intent(in) :: parameter_string
    integer(itm_i4) :: length

    type(element), pointer :: child
    type(interval) :: found
    character(len = : ), allocatable :: cname
    integer(itm_i4) :: i, j, no_of_children
    logical :: can_have_children

!-- newborns know no relatives
    nullify(parent%child)
    nullify(parent%sibling)
    nullify(parent%parent)
    cname = char2str(parent%cname)
    found%ind_start = index(parameter_string, '<xs:element name="' &
     // cname // '"')
    if (found%ind_start == 0) then
      if (verbose_parsing) then
      write(*, *) 'ERROR: Element ', parent%cname,' not found.'
      end if
      return
    end if
    can_have_children = .true.
    found%ind_start = found%ind_start + len_trim('<xs:element name="' &
     // cname // '"')
    deallocate(cname)
    i = found%ind_start
    do
      if (parameter_string(i : i + 1) == '/>') then
        can_have_children = .false.
        exit
      else if (parameter_string(i : i) == '>') then
        exit
      else if (parameter_string(i : i) == '<') then
        if (verbose_parsing) then
        write(*, *) 'ERROR: not well-formed XML schema.'
        end if
        return
      end if
      i = i + 1
    end do
    found%ind_start = i
    if (.not. can_have_children) then
      branches = branches + 1
      return               ! tip of branch reached
    else
!--   find end of element block
      found%ind_end = index(parameter_string(found%ind_start &
       : len_trim(parameter_string)), '</xs:element>')
      if (found%ind_end == 0) then
        if (verbose_parsing) then
        write(*, *) 'ERROR: Element ', parent%cname,' not properly closed.'
        end if
        return
      end if
      found%ind_end = found%ind_end + found%ind_start - 1
      no_of_children = 0
!--   scan for occurrences of ' ref="', count number of children
      do
        i = index(parameter_string(found%ind_start : found%ind_end), &
         ' ref="') 
        if (i == 0) then
          if (no_of_children == 0) then
            branches = branches + 1
            return                         ! has no children
          else
            child%parent => parent         ! last child
            return
          end if
        else
          i = i + len(' ref="') + found%ind_start - 1
          j = index(parameter_string(i : found%ind_end), '"')
          if (j == 0) then
            if (verbose_parsing) then
            write(*, *) 'ERROR: in ref, not well-formed XML.'
            end if
            return
          else
            j = j + i - 2
            if (no_of_children == 0) then
              allocate(parent%child)        ! first born child
              child => parent%child
            else
              allocate(child%sibling)       ! next child
              child => child%sibling 
            end if
            allocate(child%cname(j - i + 1))
            child%cname = str2char(parameter_string(i : j))
            no_of_children = no_of_children + 1
            length = size(child%cname)
            call parse_single_knot(parameter_string, child, &
             length, branches)
          end if
        end if
        found%ind_start = j + 2
      end do
    end if

  end subroutine parse_single_knot

  subroutine parse_xml_string(textv, length, nparm, parameter_list)
!---------------------------------------------------------------------------
!  This subroutine does the actual parsing after condensing textv into a
!  single string parameter_string.
!  It requires the resulting string to be a valid XML document following
!  the XML schema specified in code_parameters%schema.
!  It browses through the tree which was built from this schema and
!  fills in the values cvalue from the XML document.
!---------------------------------------------------------------------------

    type(tree), intent(inout) :: parameter_list
    character(len = *), intent(in) :: textv(:)
    integer(itm_i4), intent(in) :: length
    integer(itm_i4), intent(out) :: nparm

    type(element), pointer :: temp_pointer
    type(interval) :: full_string, bounds
    character(len = length) :: parameter_string
    integer(itm_i4) :: i, start_index

!-- concatenate strings in textv with spaces instead of line feeds

    parameter_string = ''
    start_index = 1
    do i = 1, size(textv)
      parameter_string(start_index : start_index + len(textv(i)) - 1) &
       = textv(i)
      start_index = start_index + len(textv(i)) 
    end do    
 
!-- replace line feeds
    do i = 1, len(parameter_string)
      if (iachar(parameter_string(i : i)) == 10) &
       parameter_string(i : i) = ' '
    end do

    full_string%ind_start = 1
    full_string%ind_end = len(parameter_string)

!-- set pointer at root of the tree
    if (.not. associated (parameter_list%first)) then
      stop 'ERROR: pointer not associated'
    else
      temp_pointer => parameter_list%first
    end if
     
!-- parse for document element
    call parse_xml_element(parameter_string, temp_pointer%cname, &
     full_string, bounds)

    if (bounds%ind_start == 0) then
       if (verbose_parsing) then
      write(*, *) 'ERROR: No document element.'
       end if
      return
    end if

!-- start tree
    nparm = 0
    if (.not. associated (temp_pointer%child)) then
      stop 'ERROR: not a valid XML document.'
    else
      temp_pointer => temp_pointer%child
    end if

!-- recursively parse XML string based on tree
    call parse_xml_tree(parameter_string, temp_pointer, bounds, nparm)

!-- consistency check
    if (.not. associated(temp_pointer, parameter_list%first)) then
      if (verbose_parsing) then
      write(*, *) 'ERROR: incomplete tree.'
      end if
      stop
    end if

    if (verbose_parsing) then
       write(*,*) 'number of parameters in XML file: ', nparm
    end if
   
  end subroutine parse_xml_string

  recursive subroutine parse_xml_tree(parameter_string, tree_pointer, &
   search, nparm)
!--------------------------------------------------------------------------
!  This subroutine parses parameter_string based on the tree built
!  from the schema.
!  It operates recursively through the tree following the child and parent
!  pointers of the tree elements. The sibling pointers are processed
!  within a single call to parse_xml_tree until a parent pointer is
!  encountered.
!--------------------------------------------------------------------------
    implicit none

    type(element), pointer :: tree_pointer
    character(len = *), intent(in) :: parameter_string
    type(interval), intent(in) :: search
    integer(itm_i4), intent(inout) :: nparm

    type(interval) :: found

    do
      call parse_xml_element(parameter_string, tree_pointer%cname, &
       search, found)
      if (found%ind_start /= 0) then               ! element found
        if (associated(tree_pointer%child)) then   ! has children
          tree_pointer => tree_pointer%child       ! go down
          call parse_xml_tree(parameter_string, tree_pointer, found, nparm)
        else                                       ! contains data
          allocate(tree_pointer%cvalue(len_trim(adjustl(parameter_string(&
           found%ind_start : found%ind_end)))))
          tree_pointer%cvalue = &
           str2char(trim(adjustl(parameter_string(found%ind_start &
            : found%ind_end))))
          nparm = nparm + 1
        end if
      end if
      if (associated(tree_pointer%sibling)) then   ! has siblings
        tree_pointer => tree_pointer%sibling       ! move right
      else
        if (.not. associated(tree_pointer%parent)) then  ! no parent
          write(*, *) 'ERROR: missing parent.'
          return
        else
          tree_pointer => tree_pointer%parent      ! go up
          exit
        end if
      end if
    end do

  end subroutine parse_xml_tree

  subroutine parse_xml_element(parameter_string, ctag, search, found)
!--------------------------------------------------------------------------
!  This subroutine parses parameter_string for the first occurence of the
!  xml element <tag> within the search bounds search.
!  It ignores attributes to the element.
!  It returns the location of the element in found and 0 if not found
!--------------------------------------------------------------------------
    implicit none

    character(len = *) :: parameter_string
    character, dimension(:) :: ctag
    character(len = size(ctag)) :: tag
    type(interval) :: search, found
    integer(itm_i4) :: i   

    do i = 1, size(ctag)
      tag(i : i) = ctag(i)
    end do
    found%ind_start = index(parameter_string(search%ind_start &
     : search%ind_end), '<' // trim(tag) // '>')    ! try without attributes
    if (found%ind_start == 0) then
      found%ind_start = index(parameter_string(search%ind_start &
       : search%ind_end), '<' // trim(tag) // ' ')  ! try with attributes
    end if
    found%ind_end = index(parameter_string(search%ind_start &
     : search%ind_end), '</' // trim(tag) // '>') - 1
    if (found%ind_start > 0) then
      found%ind_start = search%ind_start + found%ind_start + len_trim(tag) &
       + 1
      do
        if (parameter_string(found%ind_start - 1 : found%ind_start - 1) &
         == '>') exit
        found%ind_start = found%ind_start + 1
      end do
      found%ind_end = search%ind_start + found%ind_end - 1
    end if

  end subroutine parse_xml_element

  subroutine destroy_xml_tree(parameter_list)
!------------------------------------------------------------------------
!  This subroutine deallocates the XML tree in <parameter_list>.
!------------------------------------------------------------------------
    implicit none

    type(tree), intent(inout) :: parameter_list

    type(element), pointer :: temp_pointer
    integer(itm_i4) :: level

    temp_pointer => parameter_list%first
    level = 0

    call destroy_xml_element(temp_pointer, level)

  end subroutine destroy_xml_tree

  recursive subroutine destroy_xml_element(temp_pointer, level)
!-------------------------------------------------------------------------
! This recursive subroutine destroys a single XML element plus all its
! children through recursive calls.
! The destructor deallocates the fields <cname> and <cvalue> if allocated
! and finally deallocates the pointer to that element.
!-------------------------------------------------------------------------
    implicit none

    type(element), pointer :: temp_pointer
    integer(itm_i4) :: level

    type(element), pointer :: save_pointer

    do
      if (associated(temp_pointer%child)) then    ! is parent element
        temp_pointer => temp_pointer%child
        level = level + 1
        call destroy_xml_element(temp_pointer, level)
      end if
      if (allocated(temp_pointer%cname)) deallocate(temp_pointer%cname)
      if (allocated(temp_pointer%cvalue)) deallocate(temp_pointer%cvalue)
      if (associated(temp_pointer%sibling)) then
        save_pointer => temp_pointer
        temp_pointer => temp_pointer%sibling
        deallocate(save_pointer)
      else if (associated(temp_pointer%parent)) then
        level = level - 1
        save_pointer => temp_pointer
        temp_pointer => temp_pointer%parent
        exit
      else
        save_pointer => temp_pointer
        if (level > 0) write(*, *) 'ERROR: broken tree.'
        exit
      end if
    end do

    deallocate(save_pointer)

  end subroutine destroy_xml_element

! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
SUBROUTINE find_element(mko_ptr_to_element, mko_return_ptr, mko_cname)
  CHARACTER(len=132), INTENT(in) :: mko_cname
  CHARACTER(len=:), allocatable :: mko_tmp_cname
  TYPE(element), POINTER, INTENT(in) :: mko_ptr_to_element
  TYPE(element), POINTER, INTENT(out) :: mko_return_ptr
  TYPE(element), POINTER :: mko_tmp_ptr

  mko_tmp_ptr => mko_ptr_to_element

  DO WHILE( ASSOCIATED(mko_tmp_ptr) )
     mko_tmp_cname = char2str(mko_tmp_ptr%cname)
     IF ( mko_cname .EQ. mko_tmp_cname) THEN
        mko_return_ptr => mko_tmp_ptr
        deallocate(mko_tmp_cname)
        RETURN
     END IF
     deallocate(mko_tmp_cname)
     mko_tmp_ptr => mko_tmp_ptr%sibling
   END DO
   mko_return_ptr => mko_tmp_ptr
 END SUBROUTINE find_element
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +


! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
!TEST ROUTINES FROM MICHAL: TO BE REMOVED AFTER
! INTEGRATING THEM INTO XMLLIB !!!!
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +
SUBROUTINE find_parameter(mko_str, mko_value, mko_parameters_ptr)
  CHARACTER(len=132), INTENT(in) :: mko_str
  CHARACTER(len=132), INTENT(out) :: mko_value
  TYPE(element), POINTER :: mko_temp_pointer
  TYPE(element), POINTER :: mko_found_element
  TYPE(element), POINTER, INTENT(in) :: mko_parameters_ptr
  INTEGER :: mko_pos1 = 1, mko_pos2=10000, mko_n = 0, mko_i
  ! there are few strong assumptions
  ! 1. we assume that no parameter will be longer than 132
  ! 2. we assume that the deepth of the tree will be not bigger than 10 levels down
  CHARACTER(len=132)   :: mko_word(30)
  CHARACTER(len = 132) :: mko_cname
  CHARACTER(len = :), allocatable :: mko_value_temp

  mko_value = ''
  mko_word  = ''
  mko_n     = 0
  mko_pos1 = 1
  mko_pos2 = 10000

  mko_temp_pointer => mko_parameters_ptr

  DO
     mko_pos2 = INDEX(mko_str(mko_pos1:), "/")
     IF (mko_pos2 == 0) THEN
        mko_n = mko_n + 1
	mko_word(mko_n) = mko_str(mko_pos1:)
        EXIT
     END IF
     mko_n = mko_n + 1
     mko_word(mko_n) = mko_str(mko_pos1:mko_pos1+mko_pos2-2)
     mko_pos1 = mko_pos2+mko_pos1
  END DO

! we have the whole tree here, now we have to traverse the elements

  DO mko_i = 1, mko_n
     ! at each level, we have to check whether we have correct name
     CALL find_element(mko_temp_pointer, mko_found_element, mko_word(mko_i) )
     IF(ASSOCIATED(mko_found_element) .EQV. .FALSE.) THEN
        mko_value = ''
       RETURN
    ELSE
       IF ( mko_i == mko_n) THEN
          mko_value_temp = char2str(mko_found_element%cvalue)
          write(mko_value,*)trim(mko_value_temp)
          deallocate(mko_value_temp)
          RETURN
       ELSE
          mko_temp_pointer => mko_found_element%child
       END IF
     END IF
  END DO
  mko_value = ''
END SUBROUTINE find_parameter
! + + + + + + + + + + + + + + + + + + + + + + + + + + + +

end module euitm_xml_parser
