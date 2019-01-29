program chease_prog
  !------------------------------------------------------------------
  ! Wrapper program for chease routine to be called with equil_in, equil_out and codename for parameters
  ! It should allow to run chease independently from the ITM structure, gateway or with libraries 
  ! allowing read/write from ITM structure.
  ! If chease is called from Kepler. Then this program is not run and chease routine directly called with NITMOPT=22
  !
  ! Thus the namelist should be read here and then if needed the equil_in.
  !------------------------------------------------------------------
  !
  use globals
  use euITM_schemas                       ! module containing the equilibrium type definitions
  use globals    ! needed here since keep option read(..,eqdata) namelist for backward compatibility
  ! use euITM_routines
  use assign_chease_codeparameters_choices_xml2eg

  IMPLICIT NONE

  INCLUDE 'COMDAT.inc'  ! include namelist eqdata

  !
  interface 
     subroutine chease(equil_in,equil_out,param_code)
       use globals
       use euITM_schemas                       ! module containing the equilibrium type definitions
       use euitm_xml_parser
       ! use euitm_routines
       IMPLICIT NONE
       type(type_equilibrium),pointer      :: equil_in(:), equil_out(:)
       type(type_param) :: param_code
     end subroutine chease

     subroutine assign_chease_codeparameters_reflist(codeparameters, return_status)
       use prec_const
       use euitm_schemas
       use euitm_xml_parser  
       implicit none
       type (type_param), intent(in) :: codeparameters
       integer(ikind), intent(out) :: return_status 
     end subroutine assign_chease_codeparameters_reflist

     subroutine load_itm(equil_in,kitmopt,kitmshot,kitmrun,citmtree)
       use globals
       use euITM_schemas                       ! module containing the equilibrium type definitions
       !avoid for no ITM compatibility       use euITM_routines
       IMPLICIT NONE
       type(type_equilibrium),pointer      :: equil_in(:)
       character*120  :: citmtree
       integer        :: kitmopt, kitmshot, kitmrun
     end subroutine load_itm

     subroutine write_itm(equil_out,kitmopt,kitmshot,kitmrun,citmtree)
       use globals
       use euITM_schemas                       ! module containing the equilibrium type definitions
       !       use euITM_routines
       IMPLICIT NONE
       type(type_equilibrium),pointer      :: equil_out(:)
       character*120  :: citmtree
       integer        :: kitmopt, kitmshot, kitmrun
     end subroutine write_itm

  end interface
  !
  type(type_equilibrium),pointer      :: equil_in(:), equil_out(:)
  type (type_param) :: codeparam_param
  integer                             :: itmopt, i

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
     if (associated(codeparam_param%parameters)) deallocate(codeparam_param%parameters)
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
     allocate(codeparam_param%parameters(n_lines))
     codeparam_param%parameters = parameters
     allocate(codeparam_param%default_param(n_lines))
     codeparam_param%default_param = parameters
     !
     ! need also schemas in codeparam_param%schema now
     !
     ! file_xml_schema = 'chease_schema_reflist.xsd'
     ! OPEN (unit = INXML, file = "chease_schema_reflist.xsd", status = 'old', &
     OPEN (unit = INXML, file = "chease_schema_choices.xsd", status = 'old', &
          action = 'read', iostat = ios)
     if (ios .ne. 0) then
       print *,'ERROR: cannot open chease_schema_reflist.xsd'
       stop 'no chease_schema_reflist.xsd file'
     end if
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
     allocate(codeparam_param%schema(n_lines))
     codeparam_param%schema = parameters
     IF (NVERBOSE .GE. 2) print *,codeparam_param%parameters
     ! call assign_chease_codeparameters_reflist(codeparam_param,istatus)
     call assign_chease_codepar_choices(codeparam_param,istatus)

     IF (NVERBOSE .GE. 3) print *,'ntcase= ',ntcase
     if (istatus /= 0) then
       IF (NVERBOSE .GE. 0) write(*, *) 'ERROR: Could not assign some code parameters.'
        stop 'assign_code_parameters'
     end if

  end if

  if (itmopt .lt. 0) then
     ! chease not compiled with itm libraries, so keep itmopt=-1 or 0
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
    CALL LOAD_ITM(equil_in,nitmopt,nitmshot(1),nitmrun(1),treeitm(1))
    IF (NVERBOSE .GE. 2) write(*,*) 'equil_in(1)%time = ',equil_in(1)%time
  else
    ! since did not load it, make sure if associated can work
    nullify(equil_in)
    IF (NVERBOSE .GE. 1) PRINT *,'equilibrium not taken from ITM structure, thus eqchease_in not allocated'
  end if
  !
  IF ((NVERBOSE .GE. 2) .and. (associated(equil_in)) ) PRINT *,'salut1, size(equil_in)= ',size(equil_in)
  ! allocate(eqchease_in(1))
  ! call euITM_copy_slice2slice_equilibrium(equil_in(2),eqchease_in(1))
  eqchease_in => equil_in
  IF ((NVERBOSE .GE. 2) .and. (associated(equil_in)) ) write(*,*) 'equil_in(1)%time = ',equil_in(1)%time
  !
  if ( (associated(equil_in)) .and. (associated(codeparam_param%parameters)) ) then
    equil_in(1)%codeparam%parameters => codeparam_param%parameters
    ! do i=1,size(equil_in(1)%codeparam%parameters)
    !    print *,equil_in(1)%codeparam%parameters(i)
    ! end do
  else
    IF (NVERBOSE .GE. 0) write(0,*) ' parameters not associated'
  end if
  !
  call chease(equil_in,equil_out,codeparam_param)
  !
  ! write data to ITM structure if needed
  !
  IF (NITMOPT .GE. 10) THEN
    IF (NVERBOSE .GE. 1) PRINT *,'write with treeitm(2), nitmshot(2), nitmrun(2)= ', &
      & trim(treeitm(2)), nitmshot(2), nitmrun(2)
    CALL WRITE_ITM(equil_out,nitmopt,nitmshot(2),nitmrun(2),treeitm(2))
  end IF
  !
  if (associated(equil_in)) deallocate(equil_in)
  if (associated(equil_out)) deallocate(equil_out)
  ! 
end program chease_prog
