program chease_prog
  !------------------------------------------------------------------
  ! Wrapper program for chease routine to be called with equil_in, equil_out and codename for parameters
  ! It should allow to run chease independently from the ITM structure, gateway or with libraries 
  ! allowing read/write from ITM structure.
  ! If chease is called from Kepler. Then this program is not run and chease routine directly called with NITMOPT=22
  !
  ! Thus the namelist should be read here and then if needed the equil_in.
  !
  ! Compatible with only ids structures, calling chease(equil_in_ids,equil_out_ids;param_ids_input)
  !
  !------------------------------------------------------------------
  !
  use globals    ! needed here since keep option read(..,eqdata) namelist for backward compatibility
  use ids_schemas                       ! module containing the equilibrium type definitions
  use euitm_schemas ! needed for codeparam used with assign_parameters

  IMPLICIT NONE

  INCLUDE 'COMDAT.inc'  ! include namelist eqdata

  !
  interface 
     subroutine chease(equil_in_ids,equil_out_ids,param_input_ids)
       use globals
       use ids_schemas                       ! module containing the equilibrium type definitions
       IMPLICIT NONE
       type(ids_equilibrium), intent(in) :: equil_in_ids
       type(ids_equilibrium), intent(out) :: equil_out_ids
       type(ids_Parameters_Input), intent(inout) :: param_input_ids
       !
     end subroutine chease

     subroutine assign_code_parameters(codeparameters, return_status)
       use prec_const
       use euitm_schemas
       use euitm_xml_parser  
       implicit none
       type (type_param), intent(in) :: codeparameters
       integer(ikind), intent(out) :: return_status 
     end subroutine assign_code_parameters

     subroutine load_imas(equil_in_ids,kitmopt,kitmshot,kitmrun,citmtree)
       use globals
       use ids_schemas                       ! module containing the equilibrium type definitions
       !avoid for no ITM compatibility       use euITM_routines
       IMPLICIT NONE
       type(ids_equilibrium)      :: equil_in_ids
       character*120  :: citmtree
       integer        :: kitmopt, kitmshot, kitmrun
     end subroutine load_imas

     subroutine write_imas(equil_out_ids,kitmopt,kitmshot,kitmrun,citmtree)
       use globals
       use ids_schemas                       ! module containing the equilibrium type definitions
       !       use euITM_routines
       IMPLICIT NONE
       type(ids_equilibrium)      :: equil_out_ids
       character*120  :: citmtree
       integer        :: kitmopt, kitmshot, kitmrun
     end subroutine write_imas
     !
  end interface
  !
  type(ids_equilibrium)  :: equil_in_ids, equil_out_ids
  type (type_param) :: codeparam_itm
  type (ids_Parameters_Input) :: param_input_ids
  integer :: itmopt, i

  character(len = 132), target :: codename(1) = 'CHEASE'
  character(len = 132), target :: codeversion(1) = 'version 10'

  character(len = 132), allocatable :: parameters(:)
  target :: parameters
  character(len = 132) :: xml_line
  character(len=132) :: file_xml_schema
  integer(ikind) :: file_length, n_lines, ios, ios2, INXML, istatus, INAMELIST
  logical :: ex_dp, ex_fdf, ex_j_tor

  !*********************************************************************
  !
  !
  ! Initialize NITMOPT depending on local installation:
  ! NITMOPT = -1: if no itm stuff/libraries installed (default)
  ! NITMOPT =  0: if linked with ITM/mds modules (so can define ITM shot in namelist)

  NITMOPT = 0

  !
  ! Set up the case
  !
  CALL PRESET
  ! if itmopt<0 allows to force negative value independent of namelist value later
  itmopt = nitmopt

  !*********************************************************************
  ! Read namelist in xml format in file chease_input.xml
  ! or can read namelist as former namelist BUT in file: chease_namelist
  !

  !-- read chease_input.xml
  INXML = 12
  OPEN (unit = INXML, file = "chease_input.xml", status = 'old', &
       action = 'read', iostat = ios)

  IF (ios /= 0) THEN    
     IF (NVERBOSE .GE. 0) write(0,*) ' WARNING:  ''chease_input.xml'' file missing,'
     IF (NVERBOSE .GE. 0) write(0,*) '            now will see if namelist given in file: ''chease_namelist'''
     INAMELIST = 13
     OPEN (unit = INAMELIST, file = "chease_namelist", status = 'old', &
          action = 'read', iostat = ios2)
     IF (ios2 /= 0) THEN
        IF (NVERBOSE .GE. 0) THEN
          write(0,*) ' ERROR: ''chease_namelist'' does not exist either'
          write(0,*) ' Note: CHEASE has been modified to allow for xml type inputs,'
          write(0,*) '       therefore input data should be given through files.'
          write(0,*) ' CHEASE accepts xml type input in file ''chease_input.xml'' or '
          write(0,*) '        namelist type as before but in file named ''chease_namelist'' '
          write(0,*) ' Thus copy your input datafile to the file ''chease_namelist'''
          write(0,*) '      and then run CHEASE with just the command: chease'
        END IF
        stop
     END IF
     rewind(INAMELIST)
     rewind(INAMELIST)
     rewind(INAMELIST)
     READ (INAMELIST,1000) LABEL1
     comments(1) = LABEL1
     READ (INAMELIST,1000) LABEL2
     comments(2) = LABEL2
     READ (INAMELIST,1000) LABEL3
     comments(3) = LABEL3
     READ (INAMELIST,1000) LABEL4
     comments(4) = LABEL4
     rewind(INAMELIST)
1000 FORMAT(A)

     READ (INAMELIST,EQDATA)
     close(INAMELIST)
     IF (NVERBOSE .GE. 1) THEN
       CALL BLINES(10)
       CALL MESAGE(LABEL1)
       CALL BLINES(1)
       CALL MESAGE(LABEL2)
       CALL BLINES(1)
       CALL MESAGE(LABEL3)
       CALL BLINES(1)
       CALL MESAGE(LABEL4)
     END IF
     if (associated(codeparam_itm%parameters)) deallocate(codeparam_itm%parameters)
     ! write(*,EQDATA)
     IF (NVERBOSE .GE. 1) PRINT *,' namelist read from file chease_namelist'
     IF (NVERBOSE .GE. 1) PRINT *,'nitmshot= ',nitmshot
  ELSE
     REWIND(INXML)
     IF (NVERBOSE .GE. 1) PRINT *,' namelist will be taken from file chease_input.xml'
     n_lines = 0
     DO
        READ (INXML, '(a)', iostat = ios) xml_line
        if (ios == 0) then
           n_lines = n_lines + 1
        else
           exit
        end if
     END DO
     rewind INXML
     allocate(parameters(n_lines))
     do i = 1, n_lines
        read (INXML, '(a)', iostat = ios) parameters(i)
     end do
     close(INXML)
     !  PRINT *,'parameters'
     !  PRINT *,parameters(:)

     !-- assign code parameters to internal variables defined in globals
     allocate(codeparam_itm%parameters(n_lines))
     codeparam_itm%parameters = parameters
     allocate(codeparam_itm%default_param(n_lines))
     codeparam_itm%default_param = parameters
     !
     ! need also schemas in codeparam_itm%schema now
     !
     ! file_xml_schema = 'chease_schema.xml'
     OPEN (unit = INXML, file = "chease_schema.xsd", status = 'old', &
          action = 'read', iostat = ios)
     n_lines = 0
     DO
        READ (INXML, '(a)', iostat = ios) xml_line
        if (ios == 0) then
           n_lines = n_lines + 1
        else
           exit
        end if
     END DO
     rewind INXML
     deallocate(parameters)
     allocate(parameters(n_lines))
     do i = 1, n_lines
        read (INXML, '(a)', iostat = ios) parameters(i)
     end do
     close(INXML)
     ! PRINT *,'parameters'
     ! PRINT *,parameters(:)
     allocate(codeparam_itm%schema(n_lines))
     codeparam_itm%schema = parameters
     IF (NVERBOSE .GE. 2) print *,codeparam_itm%parameters
     call assign_code_parameters(codeparam_itm,istatus)

     IF (NVERBOSE .GE. 3) print *,'ntcase= ',ntcase
     if (istatus /= 0) then
       IF (NVERBOSE .GE. 0) write(*, *) 'ERROR: Could not assign some code parameters.'
        stop 'assign_code_parameters'
     end if

     ! Copy input xml to param_input_ids
     allocate(param_input_ids%parameters_value(size(codeparam_itm%parameters)))
     param_input_ids%parameters_value = codeparam_itm%parameters
     allocate(param_input_ids%parameters_default(size(codeparam_itm%parameters)))
     param_input_ids%parameters_default = codeparam_itm%parameters
     allocate(param_input_ids%schema(size(codeparam_itm%schema)))
     param_input_ids%schema = codeparam_itm%schema
     !
  end if

  if (itmopt .lt. 0) then
     ! chease not compiled with itm libraries, so keep itmopt=-1
     NITMOPT = itmopt
  end if

  !
  !   fetch equilibrium from ITM database if required
  !
  IF (NVERBOSE .GE. 1) print *,'NITMOPT= ',NITMOPT
  IF (NVERBOSE .GE. 1) print *,'NITMSHOT= ',NITMSHOT
  IF (NITMOPT.GT.0 .AND. MOD(NITMOPT,10) .EQ. 1) THEN
    IF (NVERBOSE .GE. 1) PRINT *,'load with treeitm(1), nitmshot(1), nitmrun(1)= ', &
         & trim(treeitm(1)), nitmshot(1), nitmrun(1)
    CALL LOAD_IMAS(equil_in_ids,nitmopt,nitmshot(1),nitmrun(1),treeitm(1))
    !
    IF (NVERBOSE .GE. 2) write(*,*) 'equil_in_ids%timebase = ',equil_in_ids%timebase
  else
    ! since did not load it, make sure if associated can work
    IF (NVERBOSE .GE. 1) PRINT *,'equilibrium not taken from ITM structure, thus equil_in_ids not loaded'
  end IF
  !
  call chease(equil_in_ids,equil_out_ids,param_input_ids)
  ! for testing memory leaks run twice:
!!$  write(6,*) 'in between'
!!$  call flush(6)
!!$  call chease(equil_in_ids,equil_out_ids,param_input_ids)
  !
  ! write data to ITM structure if needed
  !
  IF (NITMOPT .GE. 10) THEN
    IF (NVERBOSE .GE. 1) PRINT *,'write with treeitm(2), nitmshot(2), nitmrun(2)= ', &
      & trim(treeitm(2)), nitmshot(2), nitmrun(2)
    CALL WRITE_IMAS(equil_out_ids,nitmopt,nitmshot(2),nitmrun(2),treeitm(2))
  end IF
  !
end program chease_prog
