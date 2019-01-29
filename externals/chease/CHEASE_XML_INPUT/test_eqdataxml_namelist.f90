program test_eqdataxml
!----------------------------------------------------------------------------
! Test namelist chease in xml
!----------------------------------------------------------------------------

  use prec_const
  use euitm_schemas
  use globals

  implicit none
  INCLUDE 'COMDAT.inc'

  interface
     subroutine assign_code_parameters(codeparameters, return_status)
       use prec_const
       use euitm_schemas
       use euitm_xml_parser  
       implicit none
       type (type_codeparam), intent(in) :: codeparameters
       integer(ikind), intent(out) :: return_status 
     end subroutine assign_code_parameters
  end interface

  type (type_equilibrium), pointer :: euitm_equilibrium_in(:)

  character(len = 132), target :: codename(1) = 'CHEASE'
  character(len = 132), target :: codeversion(1) = '95'

  character(len = 132), allocatable :: parameters(:)
  target :: parameters
  character(len = 132) :: xml_line
  integer(ikind) :: file_length, i, n_lines, ios, ios2, in_xml, istatus, in_namelist
  logical :: ex_dp, ex_fdf, ex_j_tor


!-- read EQDATA_input.xml
  in_xml = 9
  open (unit = in_xml, file = "EQDATA_input.xml", status = 'old', &
        action = 'read', iostat = ios)

  if (ios /= 0) then    
     print *,' WARNING:  EQDATA_input.xml, see if namelist given in file: CHEASE_namelist'
     in_namelist = 10
     open (unit = in_namelist, file = "CHEASE_namelist", status = 'old', &
        action = 'read', iostat = ios2)
     if (ios2 /= 0) then
        print *,' ERROR: CHEASE_namelist does not exist either'
        stop ' ios2 '
     end if
     READ (in_namelist,EQDATA)
     ! write(*,EQDATA)
     print *,' namelist read from file CHEASE_namelist'
     print *,'nitmshot= ',nitmshot
  else
     print *,' namelist will be taken from file EQDATA_input.xml'

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

     allocate(parameters(n_lines))

     do i = 1, n_lines
        read (in_xml, '(a)', iostat = ios) parameters(i)
     end do

     close(in_xml)
     !  print *,'parameters'
     !  print *,parameters(:)

     !-- assign code parameters to internal variables
     allocate(euitm_equilibrium_in(1))
     euitm_equilibrium_in(1)%codeparam%parameters => parameters
     call assign_code_parameters(euitm_equilibrium_in(1)%codeparam,istatus)

     if (istatus /= 0) then
        write(*, *) 'ERROR: Could not assign code parameters.'
        return
     end if

  end if


end program test_eqdataxml
