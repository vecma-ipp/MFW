module muscle_fortran
  !use string_binding
  implicit none

  enum, bind(c)
     enumerator :: MUSCLE_DOUBLE, MUSCLE_FLOAT, MUSCLE_INT32, MUSCLE_INT64, MUSCLE_STRING, MUSCLE_BOOLEAN, MUSCLE_RAW
  endenum

contains

  subroutine muscle_fortran_init()
    implicit none
    integer :: argc, i, prevlen, newlen
    character(len=2560) :: argv
    !character(kind=c_char,len=1), dimension(F_STR_SIZE) :: c_argv
    character(len=255) :: arg

    !print *,'muscle_fortran_init: start'

    prevlen = 0
    argc = command_argument_count()


    do i = 0, argc
       !call get_command_argument(i, arg)
       call getarg(i, arg)
       newlen = len_trim(arg)
       argv = argv(1:prevlen) // arg(1:newlen) // char(0)
       prevlen = prevlen + newlen + 1
    end do

    !print *,'muscle_fortran_init: call MUSCLE_init'
    !print *,'argc=',argc,', argv=',argv

    !call str_f2c(argv,c_argv)

    call MUSCLE_Init(argc, argv(1:prevlen))

    !print *,'muscle_fortran_init: MUSCLE_init ok'
  end subroutine muscle_fortran_init


  subroutine muscle_step_props(init_step, target_step, time_step)
    use iso_c_binding
    implicit none
    integer(4), intent(out) :: init_step, target_step
    real(8), intent(out)    :: time_step
    character(kind=c_char, len=255) :: prop, propName
    
    propName = c_char_"init_step"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i4)') init_step

    propName = c_char_"target_step"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i4)') target_step

    propName = c_char_"time_step"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f15.12)') time_step

  end subroutine muscle_step_props


  ! deprecated
  subroutine muscle_time_props(time0, time_end, tau, step0)
    use iso_c_binding
    implicit none
    real(8), intent(out) :: time0
    real(8), intent(out) :: time_end
    real(8), intent(out) :: tau
    integer(4), intent(out) :: step0
    character(kind=c_char, len=255) :: prop, propName
    
    propName = c_char_"time_init"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f15.12)') time0

    propName = c_char_"time_end"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f15.12)') time_end

    propName = c_char_"time_step"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(f15.12)') tau

    propName = c_char_"step0"//C_NULL_CHAR
    call MUSCLE_Get_Property(propName, prop)
    print *,trim(propName),' = ',trim(prop)
    read(prop,'(i4)') step0

  end subroutine muscle_time_props


end module muscle_fortran
